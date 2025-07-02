#!/bin/bash

# tiamat: the chaotic evil static site generator
# version 0.0.1 (it's not done yet)
# by sky <sky@dragn.xyz>
# SPDX-License-Identifier: MIT
# i'm sorry but also not sorry

# "include guard" - don't redefine stuff if it's already been loaded
if [[ "${tiamat_loaded:-}" -eq 1 ]]; then
  tiamat::verbose 'already sourced, skipping'
  return
fi
declare -ri tiamat_loaded=1

# are we being sourced? if so, then we're being loaded as a library
tiamat_sourced=''
[[ ! "${BASH_SOURCE[0]}" -ef "$0" ]] && tiamat_sourced=1

# if we're not already loaded but this isn't the original process, then
# we've restarted - this can be used for config defaults
tiamat_restarted=''
[[ "${TIAMAT_STARTED:-}" ]] && tiamat_restarted=1
export TIAMAT_STARTED=$$

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
shopt -s extglob

# tmp file setup
tiamat_temp=$(mktemp -d)
tiamat_depfile=$tiamat_temp/dep
tiamat_outfile=$tiamat_temp/out

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
# patterns match all subdirs TODO: except if prefixed with /
tiamat_ignore=(
  # os files
  '.DS_Store' 'Thumbs.db'
  # vcs files
  '.git*'
  # editor files
  '*~' '*.swp'
  '+([[:digit:]])' # for some reason nvim keeps making these, idk why
  # explicitly ignored + internal doc
  '*.ignore' '*.ignore.*'
  'README*'
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
# this is here so that custom elements can be added and used
# the same as standard elements
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
  local needle=$1
  # param @: haystack
  # ENDPARAMS
  shift

  local i
  for i in "$@"; do
    [[ "$i" = "$needle" ]] && return 0
  done
  return 1
}

# check if \n-delimited $2 contains a pattern matching $1
function tiamat::arrstr_matches {
  local needle=$1
  local haystack=$2
  # ENDPARAMS

  local pat
  while IFS= read -r pat; do
    [[ "$needle" = $pat ]] && return 0
  done <<< "$haystack"
  return 1
}

######################################
# logging, error handling, debugging #
######################################

function tiamat::log {
  # param @: message
  # ENDPARAMS

  echo "[tiamat $BASHPID]" "$@" >& $tiamat_stderr
}

# log only in verbose mode
function tiamat::verbose {
  [[ "${tiamat_verbose:-}" ]] && tiamat::log "$@"
  return 0
}

# print a backtrace
function tiamat::backtrace {
  local depth=${1:-0}
  # ENDPARAMS

  echo "backtrace (pid $BASHPID):" >& $tiamat_stderr
  local frame=''
  while frame=$(builtin caller "$depth"); do
    echo "  $frame" >& $tiamat_stderr
    depth=$(( depth + 1 ))
  done
}

# fail with message and backtrace
function tiamat::fail {
  # param @: message
  # ENDPARAMS

  tiamat::log "error:" "$@"
  tiamat::backtrace 1
  exit 1
}

# common fail case
function tiamat::todo {
  # no params

  tiamat::fail 'unimplemented'
}

# cnf handler (print backtrace)
function tiamat::command_not_found {
  # param @: failed command
  # endparams

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

# enter strict mode asap if we're not sourced by someone else
[[ ! "$tiamat_sourced" ]] && tiamat::strict

# clean up this process and get ready to exit
function tiamat::cleanup {
  local ret=$?
  local sig=$1 # signal to send to subprocs
  # ENDPARAMS

  # kill remaining jobs if they aren't already killed
  jobs -p | while IFS= read -r job; do
    kill -"$sig" "$job" > /dev/null 2> /dev/null || :
  done

  wait
  return "$ret"
}
trap 'tiamat::cleanup INT || exit $?' EXIT SIGINT
trap 'tiamat::cleanup TERM || exit $?' SIGTERM
# trap "trap 'exit 1' SIGINT  && kill -INT -- -$$"  EXIT
# trap "trap -         SIGINT  && kill -INT  -- -$$" SIGINT
# trap "trap -         SIGTERM && kill -TERM -- -$$" SIGTERM

# enter a mini-REPL
function tiamat::debug {
  # no params

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
  local tool=$1
  local reason=${2:-}
  # ENDPARAMS

  # use external "which" because it does not get confused by shell functions
  # like "command -v" and "type -t" do
  if ! command which "$tool" > /dev/null 2> /dev/null; then
    if [[ "$reason" ]]; then
      tiamat::fail "required external tool not found: $tool (needed for $reason)"
    else
      tiamat::fail "required external tool not found: $tool"
    fi
  fi
}

# prefix all lines of stdin with a string for logging
function tiamat::prefix {
  local prefix=$1
  # ENDPARAMS

  # exec because this fn runs in subshell
  exec sed "s/.*/$prefix&/"
}

# run a command and prefix with its name and pid
function tiamat::log_exec ( # subshell
  local name=$1
  # param @: command to run
  # ENDPARAMS
  shift

  local pid=$BASHPID
  exec > >(tiamat::prefix "[$name $pid] " >& $tiamat_stderr)
  exec "$@"
)

##############
# templating #
##############

# if args specified, output them to stdout, otherwise cat stdin to stdout
function tiamat::argcat {
  # param @: text to output
  # ENDPARAMS

  if [[ "$#" -eq 0 ]]; then
    cat
  else
    printf "%s" "$*"
  fi
}

# outputs a raw string to the output file
function tiamat::raw {
  # param @: text to output
  # ENDPARAMS

  tiamat::argcat "$@" >& $tiamat_render_fd
}

# html-escapes the input from stdin
function tiamat::escape {
  # no params

  # from ruakh on stackoverflow
  sed 's/&/\&amp;/g; s/</\&lt;/g; s/>/\&gt;/g; s/"/\&quot;/g; s/'"'"'/\&#39;/g'
}

# outputs an escaped string
function tiamat::text {
  # param @: text to output
  # ENDPARAMS

  tiamat::argcat "$@" | tiamat::escape | tiamat::raw
}

# outputs an html void tag (unclosed tag)
function tiamat::void {
  local element=$1
  # param @: attribute options
  # ENDPARAMS
  shift

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

  tiamat::raw "<$element"
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
  local element=$1
  # param @: attribute options
  # ENDPARAMS
  shift
  local nested=''

  case "$element" in
    + )
      nested=1
      element=$1
      shift
    ;;
    +* )
      nested=1
      element=${element:1}
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

  tiamat::void "$element" "${args[@]}"

  # push stack: if we're nesting then prepend last entry
  if [[ "$nested" ]]; then
    tiamat_element_stack[-1]=$element ${tiamat_element_stack[-1]}
  else
    tiamat_element_stack+=("$element")
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

# closes specified elements in order (or obtains from popping stack if not specified)
function tiamat::end {
  local element=("$@")
  # ENDPARAMS

  if [[ "$#" -eq 0 ]]; then
    set +u # seemingly a bash bug, the length of an empty array is "unbound"
    [[ "${#tiamat_element_stack[@]}" -ne 0 ]] ||
      tiamat::fail 'too many closed elements'
    set -u
    element=(${tiamat_element_stack[-1]}) # expand!
    unset tiamat_element_stack[-1]
  fi

  tiamat::raw "</$element>"
}

# outputs a tag pair with raw content included (for i.e. style, script)
function tiamat::rawtag {
  local element=$1
  # param @: text within tag
  # ENDPARAMS
  shift

  tiamat::begin "$element" # TODO: allow attrs here?
  tiamat::raw "$@"
  tiamat::end
}

# inline markdown block
function tiamat::markdown {
  # param @: markdown source
  # ENDPARAMS

  # tiamat::require_tool marked 'rendering inline markdown'

  tiamat::argcat "$@" |
    command "${tiamat_md_cmd[@]}" "${tiamat_md_args[@]}" |
    tiamat::raw
}

# inline sass block - outputs a <style>
function tiamat::sass {
  # param @: sass source
  # ENDPARAMS

  # tiamat::require_tool sass 'building inline sass'

  tiamat::argcat "$@" |
    command "${tiamat_sass_cmd[@]}" --stdin "${tiamat_sass_args[@]}" |
    tiamat::rawtag style
}

# enter template mode in this shell
function tiamat::template {
  # no params

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
  # no params

  tiamat::text "$content"
}

# renderer for tiamat's builtin templating system
function tiamat::render_template {
  local out=${1:-}
  # ENDPARAMS

  [[ "$out" ]] || out=${tiamat_permalink:-}

  out="$tiamat_output_dir$out"

  tiamat::log "-> $out"

  [[ "$tiamat_dryrun" ]] && return

  tiamat::mkparents "$out"
  touch "$out" || tiamat::fail "could not open output file '$out'"

  tiamat::output "$out"
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

# footgun: don't use *unescaped* globs here because if a new matching file is added
# to the dir, it does not immediately count as dep... escape them to match at
# file-detect time
function tiamat::depend {
  # param @: files to depend on
  # ENDPARAMS

  for i in "$@"; do
    echo "$i" >& $tiamat_depfd
  done
}

# source a script and depend on it
function tiamat::sourcedep {
  # param 1: file to source and depend on
  # param @: args passed to source
  # ENDPARAMS

  source "$@"
  tiamat::depend "$1"
}

# TODO: avoid namespace pollution?
function depend { tiamat::depend "$@"; }
function sourcedep { tiamat::sourcedep "$@"; }

# keys: output files, values: source originating those files
declare -A tiamat_sources

# declare that a file will be output, and fail if another source file has
# already output it
function tiamat::output {
  local outfile=$1
  # ENDPARAMS

  [[ "${tiamat_sources[$outfile]:-}" ]] &&
    [[ "${tiamat_sources[$outfile]}" != "$tiamat_source_file" ]] &&
    tiamat::fail "output file conflict: \
source file $tiamat_source_file wants to output $outfile, \
but that file is already output by ${tiamat_sources[$outfile]}"

  echo "$outfile" >& $tiamat_outfd
}

function tiamat::mkparents {
  local path=$1
  # ENDPARAMS

  mkdir -p -- "$(dirname -- "$path")"
}

# takes a source path and makes it relative to the source dir
function tiamat::relative_to_output {
  local path=$1
  # ENDPARAMS

  echo "${path#"$tiamat_source_dir"}"
}

# takes a source path and makes it relative to source dir w/o extension
function tiamat::output_name {
  local path=$1
  # ENDPARAMS

  local f=$(tiamat::relative_to_output "$path")
  echo "${f%%.*}"
}

# default output filename map
function tiamat::map_filename {
  local srcfile=$1
  # ENDPARAMS

  local f=$(tiamat::output_name "$srcfile")

  case "$f" in
    */index ) f=$f.html ;;
    * ) f=$f/index.html ;;
  esac
  tiamat::verbose "extension added: $f" >&2

  echo "$f"
}

# pass thru a file unmodified
function tiamat::build_passthru {
  local srcfile=$1
  # ENDPARAMS

  local out=$tiamat_output_dir$(tiamat::relative_to_output "$srcfile")

  tiamat::log "-> $out"

  [[ "$tiamat_dryrun" ]] && return 0

  tiamat::mkparents "$out"
  tiamat::output "$out"
  cp "$srcfile" "$out"
}

# build a page directly from a script
function tiamat::build_plain {
  local srcfile=$1
  # ENDPARAMS

  # determine output file
  tiamat_permalink=$(tiamat::map_filename "$srcfile")

  tiamat::verbose 'entering subshell'
  (
    tiamat::verbose 'sourcing page'
    source "$srcfile"
    # the script itself contains the render_template call
    # it can also have multiple!
  )
}

function tiamat::read_frontmatter {
  local srcfile=$1
  # ENDPARAMS

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
  ' "$srcfile" || return $?
}

function tiamat::build_adoc {
  local srcfile=$1
  # ENDPARAMS

  # tiamat::require_tool asciidoctor "building adoc file $1"

  # determine output file
  tiamat_permalink=$(tiamat::map_filename "$srcfile")

  tiamat::verbose 'entering subshell'
  (
    # execute frontmatter
    tiamat::verbose 'running frontmatter'
    eval "$(tiamat::read_frontmatter "$1")"

    # render adoc
    tiamat::verbose 'rendering adoc'
    declare -g content=$(
      command "${tiamat_adoc_cmd[@]}" "${tiamat_adoc_args[@]}" -o - "$srcfile"
    )

    # render page template
    tiamat::verbose 'rendering page'
    tiamat::render_template "$tiamat_permalink"
  )
}

function tiamat::build_sass {
  local srcfile=$1
  # ENDPARAMS

  # ignore sass partials
  [[ "$srcfile" = */_*([^/]) ]] && return 0

  # tiamat::require_tool sass 'building sass file'

  local out=$tiamat_output_dir$(tiamat::output_name "$srcfile").css

  tiamat::log "-> $out"

  [[ "$tiamat_dryrun" ]] && return 0

  tiamat::mkparents "$out"
  tiamat::output "$out"
  command "${tiamat_sass_cmd[@]}" "${tiamat_sass_args[@]}" "$srcfile" "$out"
}

function tiamat::is_ignored {
  local srcfile=$1
  # ENDPARAMS

  local ignore
  for ignore in "${tiamat_ignore[@]}"; do
    case "$srcfile" in $ignore | */$ignore )
      tiamat::verbose "skipping ignored: $1"
      return 0
    ;; esac
  done
  return 1
}

# Build the outputs from the specified source file
function tiamat::build_file {
  local srcfile=$1
  # ENDPARAMS

  tiamat::is_ignored "$srcfile" && return 0
  tiamat::verbose "building $srcfile"

  # open + truncate depfile
  exec {tiamat_depfd}> $tiamat_depfile {tiamat_outfd}> $tiamat_outfile

  # TODO: for each file, first check if it has an associated .proc.sh file
  # which will be run to process it instead of dealing with the file itself

  # if not, then process the file directly
  tiamat_source_file=$srcfile # for use by scripts
  case "$srcfile" in
    # files processed by other tools
    # *.md   | *.markdown ) ;;
    *.adoc | *.asciidoc ) tiamat::build_adoc "$srcfile" ;;
    *.sass | *.scss ) tiamat::build_sass "$srcfile" ;;
    # for output html files
    *.html.sh ) tiamat::build_plain "$srcfile" ;;
    # *.proc.sh ) ;;
    # other sh files only run if i.e. sourced

    # plain files to pass through
    *.css | *.jpg | *.png | *.webp | *.svg )
      tiamat::build_passthru "$srcfile"
    ;;

    * )
      tiamat::verbose "skipping unknown"
      exec {tiamat_depfd}>&-
      return
    ;;
  esac

  # store deps
  exec {tiamat_depfd}>&- {tiamat_outfd}>&-
  tiamat_dependencies[$srcfile]=$(<"$tiamat_depfile")
  while IFS= read -r out; do
    tiamat_sources[$out]=$1
  done < "$tiamat_outfile"
}

# build a file and files that depend on it
# if the file was deleted, remove from build list
function tiamat::build_file_deps {
  local srcfile=$1
  # ENDPARAMS

  case "$srcfile" in "$tiamat_output_dir"* )
    return
  ;; esac

  tiamat::is_ignored "$srcfile" && return 0

  if [[ -e "$srcfile" ]]; then
    tiamat::log "detected change in $srcfile ..."
    if [[ "$srcfile" = "$tiamat_source_dir/"* ]]; then
      tiamat::build_file "$srcfile"
      tiamat::log "  rebuilding $srcfile"
    fi

    local file
    for file in "${!tiamat_dependencies[@]}"; do
      # don't depend on self
      [[ "$file" -ef "$srcfile" ]] && continue
      if tiamat::arrstr_matches "$srcfile" "${tiamat_dependencies[$file]}"; then
        tiamat::build_file "$file"
        tiamat::log "  rebuilding $file"
      fi
    done
  else
    tiamat::log "file deleted: $srcfile"
    for out in "${!tiamat_sources[@]}"; do
      if [[ "${tiamat_sources[$out]}" = "$srcfile" ]]; then
        unset tiamat_sources[$out]
        tiamat::log "  deleting output file $out"
        [[ ! "$tiamat_dryrun" ]] && rm "$out"
      fi
    done
  fi
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

  tiamat::log_exec live-server "${tiamat_live_server_cmd[@]}" \
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
  local status=${1:-0}
  # ENDPARAMS

  cat >& $tiamat_stderr << end
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
  exit "$status"
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
      tiamat_config=$1 || tiamat::show_usage 1
    ;;
    -*             ) tiamat::show_usage 1 ;;
    *              ) break ;;
  esac
  shift
done

# TODO: error if not found except if default
[[ -f "$tiamat_config" ]] && source "$tiamat_config"

tiamat::postconfig

# run subcommand unless sourced as library
if [[ ! "$tiamat_sourced" ]]; then
  local subcommand=${1:-}
  shift

  case "$subcommand" in
    version ) tiamat::show_version "$@" ;;
    help    ) tiamat::show_usage 0 ;;

    build   ) tiamat::build_site "$@" ;;
    serve   ) tiamat::serve_site "$@" ;;

    *       ) tiamat::show_usage 1 ;;
  esac
fi

# vim: syn=bash
