#!/bin/bash

# tiamat: the chaotic evil static site generator
# version 0.0.1 (it's not done yet)
# by sky <sky@dragn.xyz>
# SPDX-License-Identifier: MIT
# i'm sorry but also not sorry

# "include guard"
if [[ "${tiamat_sourced:-}" -eq 1 ]]; then
  tiamat::verbose 'already sourced, skipping'
  return
fi
declare -ri tiamat_sourced=1

# if we're not already sourced but this isn't the original process, then
# we've restarted - this can be used for config defaults
tiamat_restarted=''
[[ "${TIAMAT_STARTED:-}" ]] && tiamat_restarted=1
export TIAMAT_STARTED=1

if [[ "${BASH_VERSINFO[0]}" -lt 4 ]]; then
  echo "tiamat requires bash >= 4 (running $BASH_VERSION)" >&2
  exit 1
fi

declare -r tiamat_version=0.0.1

tiamat_pid=$BASHPID
tiamat_orig_args=("$@")
tiamat_source=$(realpath "${BASH_SOURCE[0]}")

# clone fds for later use
exec {tiamat_stdin}<&0
exec {tiamat_stdout}>&1
exec {tiamat_stderr}>&2
declare -r tiamat_stdin
declare -r tiamat_stdout
declare -r tiamat_stderr

# shell options
shopt -s inherit_errexit

# tmp file setup
tiamat_temp=$(mktemp -d)
tiamat_depfile=$tiamat_temp/dep

#################
# configuration #
#################

# cli options
tiamat_verbose=''
tiamat_dryrun=''
tiamat_config='tiamat_config.sh'
tiamat_use_npx=''

# file handling
tiamat_output_dir='build'
tiamat_source_dir='src'
tiamat_ignore=(
  # os files
  '.DS_Store' '*/.DS_Store' 'Thumbs.db' '*/Thumbs.db'
  # vcs files
  '.git*' '*/.git*'
  # explicitly ignored + internal doc
  '*.ignore' '*.ignore.*'
  'README*' '*/README*'
)

# tool options
tiamat_npx_opts=(
  tiamat_md_cmd
  tiamat_adoc_cmd
  tiamat_sass_cmd
  tiamat_live_server_cmd
)

tiamat_md_cmd=(marked)
tiamat_md_args=(--gfm)

tiamat_adoc_cmd=(asciidoctor)
tiamat_adoc_args=(
  -s # create embeddable doc
  -a showtitle # output h1
  -a skip-front-matter
)

tiamat_sass_cmd=(sass)
tiamat_sass_args=()

tiamat_live_server_cmd=(live-server)
tiamat_live_server_args=(
  --wait=200
  --entry-file=404.html
  # --quiet # TODO: filter output?
)
# don't open the browser if we hot-reloaded tiamat itself
# this disconnects the socket so requires a reload, but it means the page isn't
# repeatedly opened when you save lmao
[[ "$tiamat_restarted" ]] && tiamat_live_server_args+=(--no-browser)

tiamat_fswatch_cmd=(fswatch)
tiamat_fswatch_args=() # TODO: ignore build dir

# html element list
tiamat_normal_elements+=( # start + end with nested content
  # major divisions / metadata
  html head body title
  # blocks
  div
  h1 h2 h3 h4 h5 h6
  p
  ul ol li
  header main footer
  article section aside nav
  address
  table thead tbody tfoot caption col colgroup
  tr th td
  # inline
  span a i b emph strong
  code
)
tiamat_void_elements+=( # just a start tag
  # metadata
  link meta base
  # content
  img hr br
)
tiamat_raw_elements=(script style) # start + end tag with raw content

function tiamat::postconfig {
  if [[ "$tiamat_use_npx" ]]; then
    for opt in "${tiamat_npx_opts[@]}"; do
      declare -n ref=$opt
      ref=(npx "${ref[@]}")
    done
  fi

  # make paths absolute
  tiamat_source_dir=$(realpath "$tiamat_source_dir")
  tiamat_output_dir=$(realpath "$tiamat_output_dir")
}

###########
# helpers #
###########

# checks if $2.. contains $1
function tiamat::arr_contains {
  local x=$1
  shift
  local i
  for i in "$@"; do
    [[ "$i" = "$x" ]] && return 0
  done
  return 1
}

# check if $2 contains $1
function tiamat::arrstr_contains {
  local i
  while IFS= read -r i; do
    [[ "$i" = "$1" ]] && return 0
  done <<< "$2"
  return 1
}

######################################
# logging, error handling, debugging #
######################################

function tiamat::log {
  echo "[tiamat $BASHPID]" "$@" >& $tiamat_stderr
}

# print a backtrace
function tiamat::backtrace {
  echo "backtrace (pid $BASHPID):" >& $tiamat_stderr
  local n=${1:-0}
  local frame=''
  while frame=$(builtin caller "$n"); do
    echo "  $frame" >& $tiamat_stderr
    n=$(( n + 1 ))
  done
}

# fail with message and backtrace
function tiamat::fail {
  tiamat::log "error: $1" >& $tiamat_stderr
  tiamat::backtrace 1
  exit 1
}

function tiamat::todo {
  tiamat::fail 'unimplemented'
}

# cnf handler (print backtrace)
function tiamat::command_not_found {
  echo "$1: command not found" >& $tiamat_stderr
  tiamat::backtrace 2
  return 127
}

# enter strict mode
function tiamat::strict {
  set -e
  set -o errtrace
  set -u
  function command_not_found_handle {
    tiamat::command_not_found "$@" || return "$?"
  }
  trap tiamat::backtrace ERR
}

# exit strict mode
function tiamat::unstrict {
  set +e
  set +o errtrace
  set +u
  unset -f command_not_found_handle || :
  trap - ERR
}

tiamat::strict

# clean up this process and get ready to exit
function tiamat::cleanup {
  local ret=$?

  # kill remaining jobs if they aren't already killed
  jobs -p | while IFS= read -r job; do
    kill -"$1" "$job" > /dev/null 2> /dev/null || :
  done

  wait
  return "$ret"
}
trap 'tiamat::cleanup INT || exit $?' EXIT SIGINT
trap 'tiamat::cleanup TERM || exit $?' SIGTERM
# trap "trap 'exit 1' SIGINT  && kill -INT -- -$$"  EXIT
# trap "trap -         SIGINT  && kill -INT  -- -$$" SIGINT
# trap "trap -         SIGTERM && kill -TERM -- -$$" SIGTERM

# enter a REPL
function tiamat::debug {
  echo "entering debug repl, exit with 'end'" >& $tiamat_stderr
  local ret=0
  while :; do
    [[ "$ret" -eq 0 ]] && ret=''
    local line
    <& $tiamat_stdin read -rep "tiamat${ret:+ }$ret> " line || return 0
    [[ "$line" == end ]] && return
    if [[ "$line" ]]; then
      tiamat::unstrict
      eval "$line" <& $tiamat_stdin >& $tiamat_stdout 2>& $tiamat_stderr
      ret=$?
      tiamat::strict
    fi
  done
}

# check if something is installed and fail if it isn't
function tiamat::require_tool {
  # use external "which" because it does not get confused by shell functions
  # like "command -v" and "type -t" do
  command which "$1" > /dev/null 2> /dev/null ||
    tiamat::fail "required external tool not found: $1${2:+ (needed for }${2:-}${2:+)}"
}

# prefix all lines of stdin with a string for logging
function tiamat::prefix {
  # exec because this runs in subshell
  exec sed "s/.*/[$1] &/"
}

# run a program and prefix with its name and pid
function tiamat::prefix_exec ( # subshell
  local prefix=$1
  shift
  local pid=$BASHPID
  exec > >(tiamat::prefix "$prefix $pid" >& $tiamat_stderr)
  exec "$@"
)

function tiamat::verbose {
  [[ "${tiamat_verbose:-}" ]] && tiamat::log "$@"
  return 0
}

##############
# templating #
##############

# if args specified, output them to stdout, otherwise cat stdin to stdout
function tiamat::argcat {
  if [[ "$#" -eq 0 ]]; then
    cat
  else
    printf "%s" "$*"
  fi
}

# outputs a raw string to the output file
function tiamat::raw {
  tiamat::argcat "$@" >& $tiamat_render_fd
}

# html-escapes the input from stdin
function tiamat::escape {
  # from ruakh on stackoverflow
  sed 's/&/\&amp;/g; s/</\&lt;/g; s/>/\&gt;/g; s/"/\&quot;/g; s/'"'"'/\&#39;/g'
}

# outputs an escaped string
function tiamat::text {
  tiamat::argcat "$@" | tiamat::escape | tiamat::raw
}

# outputs an html void tag (unclosed tag)
function tiamat::void {
  local e=$1
  shift || tiamat::fail 'no element specified'

  local attr_order=() # order specified by position of first instance
  declare -A attrs
  local arg
  for arg in "$@"; do
    case "$arg" in
      .* )
        [[ "${attrs[class]:-}" ]] && attrs[class]+=' ' # separate w/ spaces
        attrs[class]+=${arg:1} # remove .
        tiamat::arr_contains class "${attr_order[@]}" || attr_order+=(class)
      ;;

      \#* )
        attrs[id]=${arg:1}
        tiamat::arr_contains id "${attr_order[@]}" || attr_order+=(id)
      ;;

      =* | +* ) tiamat::fail 'cannot set content for void tag' ;;

      *+=* )
        local attr=${arg%%+=*}
        local val=${arg#*+=}
        attrs[$attr]+=$val
        tiamat::arr_contains "$attr" "${attr_order[@]}" || attr_order+=("$attr")
      ;;

      *=* )
        local attr=${arg%%=*}
        local val=${arg#*=}
        attrs[$attr]=$val
        tiamat::arr_contains "$attr" "${attr_order[@]}" || attr_order+=("$attr")
      ;;

      * )
        attrs[$arg]=''
        tiamat::arr_contains "$arg" "${attr_order[@]}" || attr_order+=("$arg")
      ;;
    esac
  done

  tiamat::raw "<$e"
  local attr
  for attr in "${attr_order[@]}"; do
    tiamat::raw " $attr"
    if [[ -n "${attrs[$attr]}" ]]; then
      tiamat::raw '="'
      tiamat::text "${attrs[$attr]}"
      tiamat::raw '"'
    fi
  done
  tiamat::raw '>'
}

# outputs an html opening tag and pushes to the element stack
function tiamat::begin {
  local e=$1
  shift
  local nested=''

  case "$e" in
    + )
      nested=1
      e=$1
      shift
    ;;
    +* )
      nested=1
      e=${e:1}
    ;;
  esac

  local args=()
  local content=()
  while [[ "$#" -ne 0 ]]; do
    case "$1" in
      =* | +* )
        content=("$@")
        break
      ;;
      * )
        args+=("$1")
        shift
      ;;
    esac
  done

  tiamat::void "$e" "${args[@]}"

  # push stack: if we're nesting then prepend last entry
  if [[ "$nested" ]]; then
    tiamat_element_stack[-1]=$e ${tiamat_element_stack[-1]}
  else
    tiamat_element_stack+=("$e")
  fi

  # if content specified, output it and close if it's text
  case "${content[0]:-}" in
    # plain content specified: output and close
    =* )
      # strip '=' before text
      content[0]=${content[0]:1}
      tiamat::text "${content[@]}"
      tiamat::end
    ;;

    # nested element specified: open it, prefixing + to mark as nesting
    +* )
      # if the nested tag is void, create it with void and then end
      if tiamat::arr_contains "${content[0]:1}" "${tiamat_void_elements[@]}"; then
        content[0]=${content[0]:1}
        tiamat::void "${content[@]}"
        tiamat::end
      else
        tiamat::begin "${content[@]}"
        # don't close, because the final tiamat::end call will close all of them
      fi
    ;;

    '' ) ;;

    * ) tiamat::fail unreachable
  esac
}

# closes specified elements in order (or obtains from popping stack)
function tiamat::end {
  local e=("$@")

  if [[ "$#" -eq 0 ]]; then
    set +u # seemingly a bash bug, the length of an empty array is "unbound"
    [[ "${#tiamat_element_stack[@]}" -ne 0 ]] ||
      tiamat::fail 'too many closed elements'
    set -u
    e=(${tiamat_element_stack[-1]}) # expand!
    unset tiamat_element_stack[-1]
  fi

  tiamat::raw "</$e>"
}

# outputs a tag pair with raw content included (for i.e. style, script)
function tiamat::rawtag {
  tiamat::begin "$1" # TODO: allow attrs here?
  shift
  tiamat::raw "$@"
  tiamat::end
}

# inline markdown block
function tiamat::markdown {
  # tiamat::require_tool marked 'rendering inline markdown'

  tiamat::argcat "$@" |
    command "${tiamat_md_cmd[@]}" "${tiamat_md_args[@]}" |
    tiamat::raw
}

# inline sass block - outputs a <style>
function tiamat::sass {
  # tiamat::require_tool sass 'building inline sass'

  tiamat::argcat "$@" |
    command "${tiamat_sass_cmd[@]}" --stdin "${tiamat_sass_args[@]}" |
    tiamat::rawtag style
}

# enter template mode in this shell
function tiamat::template {
  # we can't do this stuff with aliases because bash evaluates aliases before
  # this function gets run. hence the evil eval hacks to create functions.

  tiamat_element_stack=() # global

  # Output tools
  local directalias=(
    raw text
    begin end
    markdown sass
    # depend sourcedep
  )

  # atypical elements
  function comment { tiamat::raw  "<!-- $* -->"; }
  function doctype { tiamat::void !DOCTYPE "$@"; }

  local i
  for i in "${directalias[@]}"; do
    eval "function $i { tiamat::$i \"\$@\"; }"
  done

  for i in "${tiamat_normal_elements[@]}"; do
    eval "function $i { tiamat::begin $i \"\$@\"; }"
  done

  for i in "${tiamat_void_elements[@]}"; do
    eval "function $i { tiamat::void $i \"\$@\"; }"
  done

  for i in "${tiamat_raw_elements[@]}"; do
    eval "function $i { tiamat::rawtag $i \"\$@\"; }"
  done
}

# OVERWRITABLE function! This is the root block for rendering
function tiamat::root {
  tiamat::text "$content"
}

# renderer for tiamat's builtin templating system
function tiamat::render_template {
  local out
  out=${1:-}
  [[ "$out" ]] || out=${tiamat_permalink:-}

  out=$tiamat_output_dir$out

  tiamat::log "-> $out"

  [[ "$tiamat_dryrun" ]] && return

  tiamat::mkparents "$out"
  touch "$out" || tiamat::fail "could not open output file '$out'"

  tiamat::root {tiamat_render_fd}>"$out" || tiamat::fail 'failed to render document'

  set +u # seemingly a bash bug, the length of an empty array is "unbound"
  [[ "${#tiamat_element_stack[@]}" -eq 0 ]] ||
    tiamat::fail "unclosed element(s): ${tiamat_element_stack[*]}"
  set -u
}

#################
# file handling #
#################

declare -A tiamat_dependencies

function tiamat::depend {
  realpath "$1" >& $tiamat_depfd
}

# source a script and depend on it
function tiamat::sourcedep {
  source "$1"
  tiamat::depend "$@"
}

# TODO: avoid namespace pollution?
function depend { tiamat::depend "$@"; }
function sourcedep { tiamat::sourcedep "$@"; }

function tiamat::mkparents {
  mkdir -p -- "$(dirname -- "$1")"
}

# takes a source path and makes it relative to the source dir
function tiamat::relative_to_output {
  echo "${1#"$tiamat_source_dir"}"
}

# takes a source path and makes it relative to source dir w/o extension
function tiamat::output_name {
  local f=$(tiamat::relative_to_output "$1")
  echo "${f%%.*}"
}

# default output filename map
function tiamat::map_filename {
  local f=$(tiamat::output_name "$1")

  case "$f" in
    */index ) f=$f.html ;;
    * ) f=$f/index.html ;;
  esac
  tiamat::verbose "extension added: $f" >&2

  echo "$f"
}

# pass thru a file unmodified
function tiamat::build_passthru {
  local out=$tiamat_output_dir$(tiamat::relative_to_output "$1")

  tiamat::log "-> $out"

  [[ "$tiamat_dryrun" ]] && return 0

  tiamat::mkparents "$out"
  cp "$1" "$out"
}

# build a page directly from a script
function tiamat::build_plain {
  # determine output file
  tiamat_permalink=$(tiamat::map_filename "$1")

  tiamat::verbose 'entering subshell'
  (
    tiamat::verbose 'sourcing page'
    source "$1"
    # the script itself contains the render_template call
    # it can also have multiple!
  )
}

function tiamat::read_frontmatter {
  awk '
    $0 == "---" {
      if (inblock) {
        inblock = 0
        exit
      }
      inblock = 1
      next
    }
    {
      if (inblock) print $0
      else exit
    }
    # if not closed, error
    END {
      exit inblock
    }
  ' "$1" || return $?
}

function tiamat::build_adoc {
  # tiamat::require_tool asciidoctor "building adoc file $1"

  # determine output file
  tiamat_permalink=$(tiamat::map_filename "$1")

  tiamat::verbose 'entering subshell'
  (
    # execute frontmatter
    tiamat::verbose 'running frontmatter'
    eval "$(tiamat::read_frontmatter "$1")"

    # render adoc
    tiamat::verbose 'rendering adoc'
    declare -g content=$(
      command "${tiamat_adoc_cmd[@]}" "${tiamat_adoc_args[@]}" -o - "$1"
    )

    # render page template
    tiamat::verbose 'rendering page'
    tiamat::render_template "$tiamat_permalink"
  )
}

function tiamat::build_sass {
  case "$1" in _* ) return 0; esac # ignore sass partials

  # tiamat::require_tool sass 'building sass file'

  local out=$tiamat_output_dir$(tiamat::output_name "$1").css

  tiamat::log "-> $out"

  [[ "$tiamat_dryrun" ]] && return 0

  tiamat::mkparents "$out"
  command "${tiamat_sass_cmd[@]}" "${tiamat_sass_args[@]}" "$1" "$out"
}

# Build the outputs from the specified source file
function tiamat::build_file {
  tiamat::verbose "building $1"

  # check if file is ignored
  local ignore
  for ignore in "${tiamat_ignore[@]}"; do
    case "$1" in $ignore )
      tiamat::verbose 'skipping ignored'
      return 0
    ;; esac
  done

  # open + truncate depfile
  exec {tiamat_depfd}> $tiamat_depfile

  # TODO: for each file, first check if it has an associated .proc.sh file
  # which will be run to process it instead of dealing with the file itself

  # if not, then process the file directly
  case "$1" in
    # files processed by other tools
    # *.md   | *.markdown ) ;;
    *.adoc | *.asciidoc ) tiamat::build_adoc "$1" ;;
    *.sass | *.scss ) tiamat::build_sass "$1" ;;
    # for output html files
    *.html.sh ) tiamat::build_plain "$1" ;;
    # *.proc.sh ) ;;
    # other sh files only run if i.e. sourced

    # plain files to pass through
    *.css | *.jpg | *.png | *.webp | *.svg )
      tiamat::build_passthru "$1"
    ;;

    * )
      tiamat::verbose "skipping unknown"
      exec {tiamat_depfd}>&-
      return
    ;;
  esac

  # store deps
  exec {tiamat_depfd}>&- # close
  tiamat_dependencies[$1]=$(<"$tiamat_depfile")
}

# build a file and files that depend on it
function tiamat::build_file_deps {
  case "$1" in "$tiamat_output_dir"* )
    return
  ;; esac
  tiamat::log "detected change in $1 ..."
  case "$1" in "$tiamat_source_dir"* )
    tiamat::build_file "$1"
    tiamat::log "  rebuilding $1"
  ;; esac

  local file
  for file in "${!tiamat_dependencies[@]}"; do
    if tiamat::arrstr_contains "$1" "${tiamat_dependencies[$file]}"; then
      tiamat::build_file "$file"
      tiamat::log "  rebuilding $file"
    fi
  done
}

# Builds each file specified from stdin (nul-separated)
# should be used with `< <(input process) build_files` to avoid forking
function tiamat::build_files {
  local file
  while IFS= read -rd $'\0' file; do
    tiamat::build_file "$file"
  done
}

# Builds each file, also building its dependencies, and restarting tiamat
# if the tiamat source file changes
function tiamat::build_files_deps {
  local file
  while IFS= read -rd $'\0' file; do
    case "$file" in
      # TODO: fix hot reload, currently it doesn't clean up properly
      # "$tiamat_source" ) kill -USR1 $tiamat_pid ;;
      * ) tiamat::build_file_deps "$file" ;;
    esac
  done
}

#################
# cli interface #
#################

# Discover all source files and build them
function tiamat::build_site {
  tiamat::log 'building site'
  < <(find "$tiamat_source_dir" -type f -print0) tiamat::build_files
}

# Restart the outer tiamat process, reloading all script sources
function tiamat::restart {
  tiamat::log 'restarting tiamat...'
  tiamat::cleanup TERM || :
  exec "$BASH" "$tiamat_source" "${tiamat_orig_args[@]}"
}

# Build site and host a dev server with live reloading
function tiamat::serve_site {
  # tiamat::require_tool fswatch "running live server"
  # tiamat::require_tool live-server "running live server"

  # ensure site is built before starting
  tiamat::build_site

  tiamat::log 'serving site'

  tiamat::prefix_exec live-server "${tiamat_live_server_cmd[@]}" \
    "${tiamat_live_server_args[@]}" "$tiamat_output_dir" &

  # restart on USR1 (for tiamat::restart hot reloading)
  # TODO: fix hot reload, currently it doesn't clean up properly
  # trap 'tiamat::restart' SIGUSR1

  # listen for updates and send to build
  < <(
    command "${tiamat_fswatch_cmd[@]}" "${tiamat_fswatch_args[@]}" -r -0 . || :
  ) tiamat::build_files_deps || : &

  wait
}

function tiamat::show_usage {
  cat > $tiamat_stderr <<end
usage: $0 [global options] command
global opts:
  -V --version: show tiamat version
  -h --help: show this help
  -v --verbose: show extra logging info
  -n --dryrun: don't actually modify files
  -c --config: specify config path (reads tiamat_config.sh by default)
  -x --npx: access js-based tools using npx
commands:
  build [file]
    build the site (or a single specified source file)
  serve
    build the site and serve on localhost (options passed to live-server)
end
  exit "${1:-0}"
}

function tiamat::show_version {
  echo "tiamat $tiamat_version"
  exit 0
}

# main options
while [[ "$#" -gt 0 ]]; do
  case "$1" in
    -V | --version ) tiamat::show_version ;;
    -h | --help    ) tiamat::show_usage 0 ;;
    -v | --verbose ) tiamat_verbose=1 ;;
    -n | --dryrun  ) tiamat_dryrun=1 ;;
    -x | --npx     ) tiamat_use_npx=1 ;;
    -c | --config  )
      shift
      tiamat_config=$2 || tiamat::show_usage 1
    ;;
    -*             ) tiamat::show_usage 1 ;;
    *              ) break ;;
  esac
  shift
done

# TODO: error if not found
[[ -f "$tiamat_config" ]] && source "$tiamat_config"

tiamat::postconfig

# command
case "${1:-}" in
  version ) tiamat::show_version ;;
  help    ) tiamat::show_usage 0 ;;

  build   ) tiamat::build_site ;;
  serve   ) tiamat::serve_site ;;

  *       ) tiamat::show_usage 1 ;;
esac

# vim: syn=bash
