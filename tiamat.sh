#!/bin/bash

# tiamat: the chaotic evil static site generator
# version 0.0.2 (it pretty much works)
# by sky <sky@dragn.xyz>
# SPDX-License-Identifier: MIT
# i'm sorry but also not sorry

# sc doesn't seem to understand scopes?
# shellcheck disable=SC2178,SC2128

##################
# initialization #
##################

if ! [[ "${BASH_VERSINFO[0]}" -ge 4 ]]; then
  echo "tiamat requires bash >= 4 (running $BASH_VERSION)" >&2
  exit 1
fi

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
export TIAMAT_STARTED=$BASHPID

declare -r tiamat_version=0.0.1

# needed for restarting
tiamat_pid=$BASHPID
tiamat_orig_args=("$@")
tiamat_source=$(realpath -s -- "${BASH_SOURCE[0]}")

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
shopt -s nullglob

# tmp file setup
tiamat_temp="${TMPDIR:-/tmp}/tiamat"
mkdir -p "$tiamat_temp"
tiamat_session=$(mktemp -p "$tiamat_temp" -d -t tiamat.XXXXXXXX)
tiamat_depfile=$tiamat_session/dependencies
tiamat_outfile=$tiamat_session/outputs
tiamat_rundepfile=$tiamat_session/rundependencies
tiamat_shadow=$tiamat_session/shadow_src

#################
# configuration #
#################

# cli options
tiamat_verbose=''
tiamat_dryrun=''
tiamat_config='tiamat_config.sh'
tiamat_use_npx=''
tiamat_build_drafts=''

# file handling
tiamat_root_path="$(realpath -s -- "$(dirname -- "${BASH_SOURCE[0]}")")" # dir containing tiamat.sh
tiamat_output_path='//build' # given as a srcpath (// is root)
tiamat_source_path='//src'   # "
# patterns match all subdirs TODO: except if prefixed with /
tiamat_ignore=(
  # sass partials
  '_*([^/]).@(sass|scss)'
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
tiamat_handlers=(
  '*.md'        tiamat::build_markdown
  '*.markdown'  tiamat::build_markdown
  '*.adoc'      tiamat::build_adoc
  '*.asciidoc'  tiamat::build_adoc
  '*.sass'      tiamat::build_sass
  '*.scss'      tiamat::build_sass
  '*.html.sh'   tiamat::build_plain
  # TODO: regular .sh should not warn
  '*.css'       tiamat::build_passthru
  '*.jpg'       tiamat::build_passthru
  '*.png'       tiamat::build_passthru
  '*.webp'      tiamat::build_passthru
  '*.svg'       tiamat::build_passthru
  '*.gif'       tiamat::build_passthru
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
  -b html5
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
  --quiet # TODO: filter output?
)
# don't open the browser if we hot-reloaded tiamat itself
# this disconnects the socket so requires a reload, but it means the page isn't
# repeatedly opened when you save lmao
[[ "$tiamat_restarted" ]] && tiamat_live_server_args+=(--no-browser)

tiamat_fswatch_cmd=(fswatch)
tiamat_fswatch_args=(
  # TODO: ignore build dir
)

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

# post-process configs into more usable values
function tiamat::postconfig {
  if [[ "$tiamat_use_npx" ]]; then
    for opt in "${tiamat_npx_opts[@]}"; do
      declare -n ref=$opt
      ref=(npx "${ref[@]}")
    done
  fi

  # resolve paths
  tiamat_source_path=$(tiamat::srcpath "$tiamat_source_path")
  tiamat_output_path=$(tiamat::srcpath "$tiamat_output_path")
  tiamat_sass_args+=(--load-path="$tiamat_source_path")

  [[ "$tiamat_build_drafts" ]] || tiamat_ignore+=('*.draft' '*.draft.*')
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

  local entry
  for entry in "$@"; do
    [[ "$needle" = "$entry" ]] && return 0
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
    # shellcheck disable=SC2053
    [[ "$needle" = $pat ]] && return 0
  done <<< "$haystack"
  return 1
}

# print with delimiters
function tiamat::arr_print {
  printf '('
  while [[ "$#" -gt 1 ]]; do
    printf '%s, ' "$1"
    shift
  done
  [[ "$#" -eq 1 ]] && printf '%s' "$1"
  printf ')'
}

######################################
# logging, error handling, debugging #
######################################

tiamat_log_depth=0

function tiamat::log {
  # param @: message
  # ENDPARAMS

  local indent=''
  local i
  for (( i=0; i<tiamat_log_depth; i++ )); do
    indent+='  '
  done

  printf '[tiamat %5d]%s %s\n' "$BASHPID" "$indent" "$*" >& "$tiamat_stderr"
}

function tiamat::loggroup {
  # param @: message
  # ENDPARAMS

  tiamat::log "$@"
  tiamat_log_depth=$(( tiamat_log_depth + 1 ))
}

function tiamat::endgroup {
  # no params
  tiamat_log_depth=$(( tiamat_log_depth - 1 ))
}

function tiamat::warn {
  # param @: message
  # ENDPARAMS

  tiamat::log "warning: $*"
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

  echo "backtrace (pid $BASHPID, tmp in $tiamat_session):" >& "$tiamat_stderr"
  local frame=''
  while frame=$(builtin caller "$depth"); do
    echo "  $frame" >& "$tiamat_stderr"
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

# use like [[ CONDITION ]] || tiamat::assert
function tiamat::assert {
  # no params

  tiamat::fail 'assertion failed'
}

# cnf handler (print backtrace)
function tiamat::command_not_found {
  # param @: failed command
  # endparams

  echo "$1: command not found" >& "$tiamat_stderr"
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
  trap tiamat::backtrace EXIT
}

# exit strict mode
function tiamat::unstrict {
  set +e
  set +o errtrace
  set +u
  unset -f command_not_found_handle || :
  trap - ERR
  trap - EXIT
}

# enter strict mode asap if we're not sourced by someone else
[[ ! "$tiamat_sourced" ]] && tiamat::strict

# clean up jobs and get ready to exit
# should only be called by root process
function tiamat::cleanup {
  local ret=$?
  local sig=$1 # signal to send to subprocs
  # ENDPARAMS

  set +e

  tiamat::verbose 'cleaning up...'
  trap '' SIG"$sig" # don't kill self
  kill -"$sig" -- -"$tiamat_pid"

  # kill remaining jobs if they aren't already killed
  # jobs -p | while IFS= read -r job; do
  #   kill -"$sig" "$job" > /dev/null 2> /dev/null || :
  # done

  wait
  tiamat::verbose 'subprocesses killed'
  return "$ret"
}
# traps only apply to root shell
trap 'tiamat::cleanup INT; exit $?' SIGINT
trap 'tiamat::cleanup TERM; exit $?' SIGTERM

# enter a mini-REPL
function tiamat::debug {
  # no params

  echo "entering debug repl, exit with 'end'" >& "$tiamat_stderr"
  local ret=0
  while :; do
    [[ "$ret" -eq 0 ]] && ret=''
    local line
    <& "$tiamat_stdin" read -rep "tiamat${ret:+ }$ret> " line || return 0
    [[ "$line" == end ]] && return
    if [[ "$line" ]]; then
      tiamat::unstrict
      eval "$line" <& "$tiamat_stdin" >& "$tiamat_stdout" 2>& "$tiamat_stderr"
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

# run an external command with verbose logging
function tiamat::extern {
  tiamat::verbose "running external command: $*"
  command "$@"
}

# prefix all lines of stdin with a string for logging
function tiamat::prefix {
  local prefix=$1
  # ENDPARAMS

  # exec because this fn runs in subshell
  exec sed "s/.*/$prefix&/"
}

# run a command and prefix with its name and pid
# must run in subshell
function tiamat::log_exec {
  local name=$1
  # param @: command to run
  # ENDPARAMS
  shift

  local pid=$BASHPID
  exec > >(tiamat::prefix "[$name $pid] " >& $tiamat_stderr)
  exec "$@"
}

# time a command, storing time in tiamat_time (to avoid subshell)
# this clobbers TIMEFORMAT but whatever
tiamat_time=''
function tiamat::time {
  # param @: command to run
  # ENDPARAMS

  # create new tmp within tmp dir for time
  # needed so that multiple times can run recursively...
  local tmp
  tmp=$(mktemp -p "$tiamat_session" -t 'time.XXXXXXXX')

  # dup outputs to point running command at
  local tmpstdout
  local tmpstderr
  exec {tmpstdout}>&1 {tmpstderr}>&2

  TIMEFORMAT='%R'
  # shellcheck disable=SC2261 # time is just weird like that
  { time "$@" >& "$tmpstdout" 2>& "$tmpstderr"; } 2> "$tmp"

  # close dups
  exec {tmpstdout}>&- {tmpstderr}>&-

  tiamat_time=$(< "$tmp")
}

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

  tiamat::argcat "$@" >& "$tiamat_render_fd"
}

# html-escapes the input from stdin
function tiamat::escape {
  # no params

  # from ruakh on stackoverflow
  sed '
    s/&/\&amp;/g;
    s/</\&lt;/g;
    s/>/\&gt;/g;
    s/"/\&quot;/g;
    s/'"'"'/\&#39;/g
  '
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
    tiamat_element_stack[-1]="$element ${tiamat_element_stack[-1]}"
    # tiamat::log "${tiamat_element_stack[@]/#/:}"
  else
    tiamat_element_stack+=("$element")
  fi

  tiamat::verbose "$(tiamat::arr_print "${tiamat_element_stack[@]}")"

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
    # shellcheck disable=SC2206
    element=(${tiamat_element_stack[-1]}) # expand!
    unset 'tiamat_element_stack[-1]'
  fi

  tiamat::verbose "$(tiamat::arr_print "${tiamat_element_stack[@]}")"

  for el in "${element[@]}"; do
    tiamat::raw "</$el>"
  done
}

# outputs a tag pair with raw content included (for i.e. style, script)
function tiamat::rawtag {
  # TODO: shellcheck says this is the same var as prev, but it
  # might just not understand scopes
  local element=$1
  # param @: text within tag
  # ENDPARAMS
  shift

  tiamat::begin "$element" # TODO: allow attrs here?
  tiamat::raw "$@" # TODO: look for </$element> and warn
  tiamat::end
}

# inline markdown block
function tiamat::markdown {
  # param @: markdown source
  # ENDPARAMS

  # tiamat::require_tool marked 'rendering inline markdown'

  tiamat::argcat "$@" |
    tiamat::extern "${tiamat_md_cmd[@]}" "${tiamat_md_args[@]}" |
    tiamat::raw
}

# inline sass block - outputs a <style>
# TODO: automatic dependencies for sass
function tiamat::sass {
  # param @: sass source
  # ENDPARAMS

  # tiamat::require_tool sass 'building inline sass'

  tiamat::argcat "$@" |
    tiamat::extern "${tiamat_sass_cmd[@]}" --stdin "${tiamat_sass_args[@]}" |
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
  # shellcheck disable=SC2317
  function comment { tiamat::raw  "<!-- $* -->"; }
  # shellcheck disable=SC2317
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

  tiamat::warn "$tiamat_source_file does not overwrite tiamat::root"
  tiamat::text "$content" # TODO: fix shellcheck
}

# renderer for tiamat's builtin templating system
function tiamat::render_template {
  local out=${1:-}
  # ENDPARAMS

  # skip if we're in a file sourced from our template
  [[ "$tiamat_sourcedepth" -gt 1 ]] && return 0

  [[ "$out" ]] || out=$tiamat_permalink

  [[ "$tiamat_dryrun" ]] && return 0

  tiamat::output "$out"

  tiamat::root {tiamat_render_fd}>"$(tiamat::outpath "$out")" ||
    tiamat::fail 'failed to render document'

  set +u # seemingly a bash bug, the length of an empty array is "unbound"
  [[ "${#tiamat_element_stack[@]}" -eq 0 ]] ||
    tiamat::fail "unclosed element(s): ${tiamat_element_stack[*]}"
  set -u
}

#################
# file handling #
#################

# paths

# strip extension
function tiamat::stripext {
  local path=$1
  # ENDPARAMS

  echo "${path%%.*([^/])}"
}

# resolves a source path into an absolute filesystem path
# absolute and relative paths -> resolved like normal
# // paths -> relative to $tiamat_root_path
function tiamat::srcpath {
  local srcpath=$1
  # ENDPARAMS

  case "$srcpath" in
    //* ) realpath -s -- "$tiamat_root_path/${srcpath:2}" ;;
    *   ) realpath -s -- "$srcpath" ;;
  esac
}

# converts a real filesystem path into a root-relative srcpath
function tiamat::to_srcpath {
  local path=$1
  # ENDPARAMS

  # do nothing if already a srcpath
  if [[ "$path" = //* ]]; then
    echo "$path"
    return
  fi

  printf '//'
  realpath -s --relative-to "$tiamat_root_path" -- "$path"
}

# resolves an output path into an absolute filesystem path
# relative paths -> relative to current dir's output path
# absolute paths -> relative to $tiamat_build
function tiamat::outpath {
  local outpath=$1
  # ENDPARAMS

  case "$outpath" in
    /* ) realpath -s -- "$tiamat_output_path$outpath" ;;
    *  ) realpath -s -- "$tiamat_output_path/$(realpath -s --relative-to "$tiamat_source_path" -- "$outpath")" ;;
  esac
}

# directly translate srcpath into outpath, for file as-is
function tiamat::outpath_for {
  local srcpath=$1
  # ENDPARAMS

  # TODO: resolve src being different???
  [[ "$srcpath" = //* ]] || srcpath=$(tiamat::to_srcpath "$srcpath")
  [[ "$srcpath" = //src/* ]] ||
    tiamat::fail "outpath_for must be given a srcpath within src dir: $srcpath"
  echo "${srcpath##//src}" # leaves leading slash
}

# map a srcpath to its final url path (not the actual file, but the logical path)
# TODO: merge with page_outpath
function tiamat::page_url {
  local srcfile=$1
  # ENDPARAMS

  tiamat::stripext "$(tiamat::outpath_for "$srcfile")"
}

# map a srcpath to an output as a page
function tiamat::page_outpath {
  local srcfile=$1
  # ENDPARAMS

  local f
  f=$(tiamat::page_url "$srcfile")

  case "$f" in
    */index ) f=$f.html ;;
    * ) f=$f/index.html ;;
  esac
  tiamat::verbose "extension added: $f" >&2

  echo "$f"
}

# pushd, but goes to the folder containing the given srcpath
function tiamat::pushd {
  local srcpath=$1
  # ENDPARAMS

  local path
  path=$(tiamat::srcpath "$srcpath")
  [[ -d "$path" ]] || path=$(dirname -- "$path")
  tiamat::verbose "pushd $path"
  pushd -- "$path" > /dev/null
}

# get index of folder as actual path
function tiamat::index {
  local srcpath=$1
  # ENDPARAMS

  local indexes=("$(tiamat::srcpath "$srcpath")"/index.*)
  [[ "${#indexes[@]}" -gt 0 ]] ||
    tiamat::fail "srcpath $srcpath does not contain an index"
  [[ "${#indexes[@]}" -eq 1 ]] ||
    tiamat::fail "srcpath $srcpath contains more than 1 index: ${indexes[*]}"
  echo "${indexes[0]}"
}

# popd for symmetry
function tiamat::popd {
  tiamat::verbose 'popd'
  popd > /dev/null
}

# sourcing and frontmatter

# strip frontmatter from between '---' tags for markup files and pipe out
# frontmatter -> stderr, content -> stdout
function tiamat::read_frontmatter {
  local srcfile=$1
  # ENDPARAMS

  awk '
    # begin only on line 1
    # TODO: intial blanks?
    NR == 1 && $0 == "---" {
      fm = 1
      next
    }

    fm && $0 == "---" {
      fm = 0
      next
    }

    {
      if (fm)
        print $0 > "/dev/stderr"
      else
        print $0 > "/dev/stdout"
    }

    # if not closed, error
    END {
      exit fm
    }
  ' "$(tiamat::srcpath "$srcfile")" || return $?
}

# mkdir's the parent dirs of a given file
function tiamat::mkparents {
  local path=$1
  # ENDPARAMS

  mkdir -p -- "$(dirname -- "$path")"
}

# convert a srcpath to a shadow path
function tiamat::shadow_path {
  local srcpath=$1
  # ENDPARAMS
  shift

  local shadowpath
  shadowpath="$tiamat_shadow/$(realpath -s --relative-to "$tiamat_source_path" -- "$(tiamat::srcpath "$srcpath")")"
  [[ "$shadowpath" = ..* ]] && tiamat::assert

  echo "$shadowpath"
}

# split frontmatter and src into separate files in shadow src
function tiamat::shadow_split {
  local srcpath=$1
  # ENDPARAMS
  shift

  local shadowpath
  shadowpath=$(tiamat::shadow_path "$srcpath")
  tiamat::mkparents "$shadowpath" || tiamat::assert
  tiamat::read_frontmatter "$path" 2> "$shadowpath.frontmatter" > "$shadowpath"
}

# source depth: if 0, we know we're running as the root template so
# tiamat::render_template is allowed to run
tiamat_sourcedepth=0

# source with superpowers:
# - accepts a srcpath
# - if the script is a dir then it sources the dir's index
# - automatically cd's to the dir containing the script
# - can source a non-script file containing frontmatter
function tiamat::source {
  local srcpath=$1
  # param @: args
  # ENDPARAMS
  shift

  local path
  path=$(tiamat::srcpath "$srcpath")
  [[ -d "$path" ]] && path=$(tiamat::index "$srcpath")

  tiamat_sourcedepth=$(( tiamat_sourcedepth + 1 ))
  tiamat::pushd "$srcpath"
  tiamat::verbose "source $srcpath"
  case "$srcpath" in

    *.sh ) builtin source "$path" "$@" ;;

    *.adoc | *.md )
      local shadowpath
      tiamat::shadow_split "$srcpath"
      shadowpath=$(tiamat::shadow_path "$srcpath")
      tiamat::verbose "frontmatter at $shadowpath.frontmatter"
      # ignore status (if there's an actual error it should fail past this)
      builtin source "$shadowpath.frontmatter" "$@" || :
    ;;
  esac

  tiamat::popd
  tiamat_sourcedepth=$(( tiamat_sourcedepth - 1 ))
}

# source a file and extract globals from it
# you thought my template trickery was evil, THIS is pure evil.
# this is one of the most evil things i've written. it's so evil.
function tiamat::extract {
  local srcfile=$1
  # param @: vars to extract
  # ENDPARAMS
  shift

  local i
  while IFS= read -rd $'\0' i; do
    declare -n ref=${1%%=*}
    ref=$i
    shift
  done < <(
    tiamat::source "$srcfile"
    local tiamat_var
    for tiamat_var in "$@"; do
      declare -n tiamat_ref=${tiamat_var#*=}
      printf '%s\0' "${tiamat_ref:-}"
    done
  )
}

# TODO: implement a way to do semantic sorting of files

# dependencies

declare -A tiamat_dependencies

# declares that this script depends on another for incremental building
# footgun: don't use *unescaped* globs here because if a new matching file is added
# to the dir, it does not immediately count as dep... escape them to match at
# file-detect time
function tiamat::depend {
  # param @: files to depend on
  # ENDPARAMS

  # TODO: dedup

  # if we're not in a dependency context then nop
  [[ "${tiamat_depfd:-}" ]] || return 0

  local i
  for i in "$@"; do
    i=$(tiamat::to_srcpath "$i")
    echo "$i" >& "$tiamat_depfd"
    tiamat::verbose "added dependency $i"
  done
}

# source a script and depend on it
function tiamat::sourcedep {
  local file=$1
  # param @: args passed to source
  # ENDPARAMS
  shift

  tiamat::source "$file" "$@"
  tiamat::depend "$file"
}

# Depend on a file at runtime: this only affects build file runs, and builds
# the depended file in addition to the files specified
declare -A tiamat_rundeps
tiamat_built_rundeps=()
function tiamat::rundep {
  # param @: files to depend
  # ENDPARAMS

  # if we're not in a dependency context then nop
  [[ "${tiamat_rundepfd:-}" ]] || return 0

  local i
  for i in "$@"; do
    i=$(tiamat::to_srcpath "$i")
    echo "$i" >& "$tiamat_rundepfd"
    tiamat::verbose "added rundep $i"
  done
}

# TODO: avoid namespace pollution?
function depend { tiamat::depend "$@"; }
function sourcedep { tiamat::sourcedep "$@"; }

# misc

# keys: output files, values: source originating those files
declare -A tiamat_sources

# declare that a file will be output, prep output dir,
# and fail if another source file has already output it
# returns non-zero if dryrun
function tiamat::output {
  local outfile=$1
  # ENDPARAMS

  [[ "${tiamat_sources[$outfile]:-}" ]] &&
    [[ "${tiamat_sources[$outfile]}" != "$tiamat_source_file" ]] &&
    tiamat::fail "output file conflict: \
source file $tiamat_source_file wants to output $outfile, \
but that file is already output by ${tiamat_sources[$outfile]}"

  echo "$outfile" >& "$tiamat_outfd"

  [[ "$tiamat_dryrun" ]] && return 1

  tiamat::mkparents "$(tiamat::outpath "$outfile")" || tiamat::assert
}

# sister fn of ::output, marks in globals
function tiamat::markoutput {
  local src=$1
  local out=$2
  # ENDPARAMS

  [[ "${tiamat_sources[$out]:-$src}" = "$src" ]] || tiamat::assert
  tiamat_sources[$out]=$src
}

#################
# file handlers #
#################

# pass thru a file unmodified
function tiamat::build_passthru {
  local srcfile=$1
  # ENDPARAMS

  local out
  out=$(tiamat::outpath_for "$srcfile")

  tiamat::output "$out" || return 0

  cp "$(tiamat::srcpath "$srcfile")" "$(tiamat::outpath "$out")"
}

# build a page directly from a script
function tiamat::build_plain {
  local srcfile=$1
  # ENDPARAMS

  (
    tiamat::strict
    declare -g tiamat_permalink
    tiamat_permalink=$(tiamat::page_outpath "$srcfile")

    tiamat_sourcedepth=0

    declare -g content=''

    tiamat::verbose 'sourcing page'
    tiamat::source "$srcfile"

    # the script itself contains the render_template call
    # it can also have multiple!

    tiamat::unstrict
  )
}

function tiamat::build_markdown {
  local srcfile=$1
  # ENDPARAMS

  (
    tiamat::strict
    # determine output file
    declare -g tiamat_permalink
    tiamat_permalink=$(tiamat::page_outpath "$srcfile")

    # eval frontmatter
    tiamat::source "$srcfile"

    # render markdown
    tiamat::verbose 'rendering markdown'
    declare -g content
    content=$(
      tiamat::extern "${tiamat_md_cmd[@]}" \
        "${tiamat_md_args[@]}" \
        "$(tiamat::shadow_path "$srcfile")"
        # TODO: be more explicit about this originating from ::shadow_split in ::source
    )

    # render page template
    tiamat::verbose 'rendering page'
    tiamat::render_template "$tiamat_permalink"

    tiamat::unstrict
  )
}

function tiamat::build_adoc {
  local srcfile=$1
  # ENDPARAMS

  # tiamat::require_tool asciidoctor "building adoc file $srcfile"
  (
    tiamat::strict
    # determine output file
    declare -g tiamat_permalink
    tiamat_permalink=$(tiamat::page_outpath "$srcfile")

    # eval frontmatter
    tiamat::source "$srcfile"

    # render adoc
    tiamat::verbose 'rendering asciidoc'
    declare -g content
    content=$(
      tiamat::extern "${tiamat_adoc_cmd[@]}" \
        "${tiamat_adoc_args[@]}" \
        -o - \
        "$(tiamat::shadow_path "$srcfile")"
        # TODO: be more explicit about this originating from ::shadow_split in ::source
    )

    # render page template
    tiamat::verbose 'rendering page'
    tiamat::render_template "$tiamat_permalink"

    tiamat::unstrict
  )
}

function tiamat::build_sass {
  local srcfile=$1
  # ENDPARAMS

  # tiamat::require_tool sass 'building sass file'

  local out
  out="$(tiamat::outpath_for "$srcfile")"
  out="$(tiamat::stripext "$out").css"

  tiamat::output "$out" || return 0

  tiamat::extern "${tiamat_sass_cmd[@]}" \
    "${tiamat_sass_args[@]}" \
    "$(tiamat::srcpath "$srcfile")" \
    "$(tiamat::outpath "$out")"
}

###############
# build flows #
###############

function tiamat::is_ignored {
  local srcfile=$1
  # ENDPARAMS

  local ignore
  for ignore in "${tiamat_ignore[@]}"; do
    # shellcheck disable=SC2254
    case "$srcfile" in $ignore | */$ignore )
      tiamat::verbose "skipping ignored: $srcfile"
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
  exec {tiamat_depfd}>    "$tiamat_depfile" \
       {tiamat_rundepfd}> "$tiamat_rundepfile" \
       {tiamat_outfd}>    "$tiamat_outfile"

  # TODO: for each file, first check if it has an associated .proc.sh file
  # which will be run to process it instead of dealing with the file itself

  # if not, then process the file directly
  tiamat_source_file=$srcfile # for use by scripts

  local nhandlers=$(( ${#tiamat_handlers[@]} / 2 ))
  local handled=''
  local i
  for (( i=0; i<nhandlers; i++ )); do
    local pat=${tiamat_handlers[$((i*2))]}
    local handler=${tiamat_handlers[$((i*2+1))]}

    # shellcheck disable=SC2053
    if [[ "$srcfile" = $pat ]]; then
      handled=1
      tiamat::time "$handler" "$srcfile"
      break
    fi
  done

  exec {tiamat_depfd}>&- {tiamat_outfd}>&- {tiamat_rundepfd}>&-

  if [[ ! "$handled" ]]; then
    tiamat::warn "non-ignored file not handled: $srcfile"
    return 0
  fi

  # store deps and list of output files
  tiamat_dependencies[$srcfile]=$(<"$tiamat_depfile")
  tiamat_rundeps[$srcfile]=$(<"$tiamat_rundepfile")

  local outs
  outs=$(< "$tiamat_outfile")
  case "$(wc -l <<< "$outs")" in
    0 )
      tiamat::log "$srcfile -> ??? (${tiamat_time}s)"
      tiamat::warn "no pages output from $srcfile"
    ;;
    1 )
      tiamat::log "$srcfile -> $outs (${tiamat_time}s)"
      tiamat::markoutput "$srcfile" "$outs"
    ;;
    * )
      tiamat::log "$srcfile -> ... (${tiamat_time}s)"
      local out
      while IFS= read -r out; do
        tiamat::log "  -> $out"
        tiamat::markoutput "$srcfile" "$out"
      done <<< "$outs"
    ;;
  esac
}

# build a file and files that depend on it
# if the file was deleted, remove from build list
function tiamat::build_file_deps {
  local srcfile=$1
  # ENDPARAMS

  local realsrc
  realsrc=$(tiamat::srcpath "$srcfile")

  # skip build dir
  # TODO: do this filter in fswatch opts too
  [[ "$realsrc" = "$tiamat_output_path/"* ]] && return 0

  tiamat::is_ignored "$srcfile" && return 0

  # skip dirs
  [[ -d "$realsrc" ]] && return 0

  if [[ -e "$realsrc" ]]; then
    tiamat::loggroup "detected change in $srcfile, rebuilding dependents ..."
    # tiamat::log "${tiamat_dependencies[@]}"
    [[ "$realsrc" = "$tiamat_source_path/"* ]] &&
      tiamat::build_file "$srcfile"

    local dependent
    for dependent in "${!tiamat_dependencies[@]}"; do
      # avoid circular deps
      [[ "$dependent" = "$srcfile" ]] && continue

      if tiamat::arrstr_matches "$srcfile" "${tiamat_dependencies[$dependent]}"; then
        tiamat::build_file "$dependent"
      fi
    done
    tiamat::endgroup
  else
    tiamat::loggroup "file deleted: $srcfile"
    for out in "${!tiamat_sources[@]}"; do
      if [[ "${tiamat_sources[$out]}" = "$srcfile" ]]; then
        unset tiamat_sources\["$out"\]
        tiamat::log "deleting output file $out"
        [[ ! "$tiamat_dryrun" ]] && rm "$(tiamat::outpath "$out")"
      fi
    done
    tiamat::endgroup
  fi
}

# Builds each file specified from stdin (nul-separated)
# should be used with `< <(input process) build_files` to avoid forking
function tiamat::build_files {
  local file
  while IFS= read -rd $'\0' file; do
    tiamat::verbose "$file"
    tiamat::build_file "$(tiamat::to_srcpath "$file")"
  done
}

# Builds each file, also building its dependencies, and restarting tiamat
# if the tiamat source file changes
function tiamat::build_files_deps {
  local file
  while IFS= read -rd $'\0' file; do
    if [[ "$file" -ef "$tiamat_source" ]]; then
      [[ "$tiamat_verbose" ]] && pstree -g3 "$tiamat_pid"
      tiamat::restart
    fi

    local f
    f=$(tiamat::to_srcpath "$file")
    tiamat::build_file_deps "$f"
  done
}

#################
# cli interface #
#################

# Discover all source files and build them
function tiamat::build_site {
  # param @: files to build or none
  # ENDPARAMS

  if [[ "$#" -gt 0 ]]; then # build only these files
    tiamat::build
  else # build whole site
    # clear output dir
    rm -rf "$tiamat_output_path"
    mkdir -p "$tiamat_output_path"

    tiamat::loggroup 'building site'
    tiamat::time < <(find "$tiamat_source_path" -type f -print0) tiamat::build_files
    tiamat::endgroup
    tiamat::log "built site in $tiamat_time seconds"
  fi
}

# Restart the outer tiamat process, reloading all script sources
function tiamat::restart {
  # no params

  # if called from a subprocess, just signal the parent to restart and exit
  if [[ "$tiamat_pid" -eq "$BASHPID" ]]; then
    tiamat::log 'restarting tiamat...'
    tiamat::cleanup TERM
    exec "$BASH" "$tiamat_source" "${tiamat_orig_args[@]}"
  else
    kill -USR1 "$tiamat_pid"
    # stall waiting for kill
    wait
    exit 0
  fi
}
# restart on USR1 (for tiamat::restart hot reloading)
trap 'tiamat::restart' SIGUSR1

# Build site and host a dev server with live reloading
function tiamat::serve_site {
  # no params
  # tiamat::require_tool fswatch "running live server"
  # tiamat::require_tool live-server "running live server"

  # ensure site is built before serving
  tiamat::build_site

  tiamat::loggroup 'serving site'

  (
    tiamat::log_exec live-server "${tiamat_live_server_cmd[@]}" \
      "${tiamat_live_server_args[@]}" "$tiamat_output_path"
  ) &

  # listen for updates and send to build
  < <(
    command "${tiamat_fswatch_cmd[@]}" "${tiamat_fswatch_args[@]}" -r -0 . || :
  ) tiamat::build_files_deps &

  tiamat::endgroup
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
  build [file...]
    build the entire site, or if files specified, then build only those files
    and their runtime deps
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
    -d | --drafts  ) tiamat_build_drafts=1 ;;
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
[[ -f "$tiamat_config" ]] && builtin source "$tiamat_config"

tiamat::postconfig

# run subcommand unless sourced as library
if [[ ! "$tiamat_sourced" ]]; then
  tiamat_subcommand=${1:-}
  shift

  case "$tiamat_subcommand" in
    version ) tiamat::show_version "$@" ;;
    help    ) tiamat::show_usage 0 ;;

    build   ) tiamat::build_site "$@" ;;
    serve   ) tiamat::serve_site "$@" ;;

    *       ) tiamat::show_usage 1 ;;
  esac
fi

# vim: syn=bash
