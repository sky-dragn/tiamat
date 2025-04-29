# tiamat: the chaotic evil static site generator
# version 0.0.1 (it's not done yet)
# by sky <sky@dragn.xyz>
# SPDX-License-Identifier: MIT

# "include guard"
if [[ "${tiamat_sourced:-}" -eq 1 ]]; then
  tiamat::verbose 'already sourced, skipping'
  return
fi
declare -ri tiamat_sourced=1

declare -r tiamat_version=0.1

# shell options
shopt -s inherit_errexit

# shared config options
tiamat_verbose=''
tiamat_output_dir='build'
tiamat_source_dir='src'

# globals
exec {tiamat_fd}> /dev/null

######################################
# logging, error handling, debugging #
######################################

# enter strict mode
function tiamat::strict {
  set -e
  set -o errtrace
  set -u
  trap tiamat::backtrace ERR
}

# exit strict mode
function tiamat::unstrict {
  set +e
  set +o errtrace
  set +u
  trap - ERR
}

tiamat::strict

# print a backtrace
function tiamat::backtrace {
  echo 'backtrace:' >&2
  local frame=${1:-0}
  while caller "$frame" >&2; do
    frame=$(( frame + 1 ))
  done
}

# fail with message and backtrace
function tiamat::fail {
  echo "error: $1" >&2
  tiamat::backtrace 1
  exit 1
}

function tiamat::todo {
  tiamat::fail 'unimplemented'
}

# enter a REPL
function tiamat::debug {
  echo "entering debug repl, exit with 'end'" >&2
  local ret=0
  while :; do
    if [[ "$ret" -eq 0 ]]; then
      ret=
    else
      ret=" $ret"
    fi
    local line
    read -rep "tiamat$ret> " line || return
    [[ "$line" == end ]] && return
    tiamat::unstrict
    eval "$line"
    ret=$?
    tiamat::strict
  done
}

# check if something is installed and fail if it isn't
function tiamat::require_tool {
  command -v "$1" > /dev/null 2> /dev/null ||
    tiamat::fail "required external tool not found: $1"
}

# prefix all lines of stdin with a string for logging
function tiamat::prefix {
  sed "s/.*/[$1] &/"
}

function tiamat::verbose {
  [[ "$tiamat_verbose" ]] && echo "$@"
  return 0
}

##############
# templating #
##############

# globals
declare -a tiamat_element_stack

# outputs a raw string
function tiamat::raw {
  if [[ "$#" -eq 0 ]]; then
    cat >& $tiamat_fd
  else
    printf "%s" "$*" >& $tiamat_fd
  fi
}

# html-escapes the input from stdin
function tiamat::escape {
  # from ruakh on SO
  sed 's/&/\&amp;/g; s/</\&lt;/g; s/>/\&gt;/g; s/"/\&quot;/g; s/'"'"'/\&#39;/g'
}

# outputs an escaped string
function tiamat::text {
  if [[ "$#" -eq 0 ]]; then
    tiamat::escape | tiamat::raw
  else
    printf "%s" "$*" | tiamat::escape | tiamat::raw
  fi
}

# outputs an html void tag (unclosed tag)
function tiamat::void {
  local e="$1"
  shift || tiamat::fail 'no element specified'

  declare -A attrs
  for arg in "$@"; do
    case "$arg" in
      .* )
        set +u # seemingly a bash bug, the length of an empty array is "unbound"
        if [[ "${#attrs[class]}" -ne 0 ]]; then
          attrs[class]+=' '
        fi
        set -u
        attrs[class]+="${arg:1}"
      ;;

      \#* ) attrs[id]="${arg:1}" ;;

      =* ) tiamat::fail 'cannot set content for void tag' ;;

      *=* )
        local attr="${arg%%=*}"
        local val="${arg#*=}"
        attrs[$attr]="$val"
      ;;

      * )
        attrs[$arg]=''
      ;;

    esac
  done

  tiamat::raw "<$e"
  for attr in "${!attrs[@]}"; do
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
function tiamat::open {
  local e="${1:-}"
  shift || tiamat::fail 'no element specified'

  declare -a args=()
  declare -a content=()
  while [[ "$#" -ne 0 ]]; do
    case "$1" in
      =* )
        content=("$@")
        content[0]="${content[0]:1}"
        break
      ;;
      * )
        args+=("$1")
        shift
      ;;
    esac
  done

  tiamat_element_stack+=("$e")
  tiamat::void "$e" "${args[@]}"

  # if content specified, output and close
  set +u # seemingly a bash bug, the length of an empty array is "unbound"
  if [[ "${#content}" -ne 0 ]]; then
    set -u
    tiamat::text "${content[@]}"
    tiamat::end
  fi
  set -u
}

# pops the element stack and outputs a closing tag
function tiamat::end {
  set +u # seemingly a bash bug, the length of an empty array is "unbound"
  [[ "${#tiamat_element_stack[@]}" -ne 0 ]] ||
    tiamat::fail 'too many closed elements'
  set -u

  local e="${tiamat_element_stack[-1]}"
  unset tiamat_element_stack[-1]
  tiamat::raw "</$e>"
}

# enter template mode in this shell
function tiamat::template {
  # we can't do this stuff with aliases because bash evaluates aliases before
  # this function gets run. hence the evil eval hacks to create functions.

  # Output tools
  declare -a directalias=(
    raw
    text
    end
    render
  )

  # Elements
  declare -a els voids
  els+=(
    # major divisions
    html head body
    # blocks
    div
    h1 h2 h3 h4 h5 h6
    p
    ul ol li
    header main footer
    article section aside nav
    address
    # inline
    span a i b emph strong
    code
  )
  voids+=(img hr br)
  # misc / metadata
  els+=(title)
  voids+=(link meta base)

  # atypical elements
  function comment { tiamat::raw  "<!-- $* -->"; }
  function doctype { tiamat::void !DOCTYPE "$@"; }

  for a in "${directalias[@]}"; do
    eval "function $a { tiamat::$a \"\$@\"; }"
  done

  for e in "${els[@]}"; do
    eval "function $e { tiamat::open $e \"\$@\"; }"
  done

  for e in "${voids[@]}"; do
    eval "function $e { tiamat::void $e \"\$@\"; }"
  done
}

# OVERWRITABLE function! This is the root block for rendering
function tiamat::root {
  tiamat::text "$content"
}

tiamat_dryrun='' # config

# renderer for tiamat's builtin templating system
function tiamat::render_template {
  local out
  out="${1:-}"
  [[ "$out" ]] || out="$tiamat_permalink"

  out="$tiamat_output_dir$out"

  echo "-> $out"

  [[ "$tiamat_dryrun" ]] && return

  tiamat::mkparents "$out"
  touch "$out" || tiamat::fail "could not open output file '$out'"

  tiamat::root {tiamat_fd}>"$out" || tiamat::fail 'failed to render document'

  set +u # seemingly a bash bug, the length of an empty array is "unbound"
  [[ "${#tiamat_element_stack[@]}" -eq 0 ]] ||
    tiamat::fail "unclosed element(s): ${tiamat_element_stack[*]}"
  set -u
}

#################
# file handling #
#################

function tiamat::mkparents {
  mkdir -p "$(dirname "$1")"
}

function tiamat::output_name {
  # get path relative to src
  local f="${1#"$tiamat_source_dir"}"
  tiamat::verbose "relative to source: $f" >&2
  # remove extension
  f="${f%%.*}"
  tiamat::verbose "extension removed: $f" >&2

  echo "$f"
}

# default output filename map
function tiamat::map_filename {
  local f="$(tiamat::output_name "$1")"

  case "$f" in
    */index ) f="$f.html" ;;
    * ) f="$f/index.html" ;;
  esac
  tiamat::verbose "extension added: $f" >&2

  echo "$f"
}

# build a page directly from a script
function tiamat::build_plain {
  # determine output file
  tiamat_permalink="$(tiamat::map_filename "$1")"

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

declare -a tiamat_adoc_args=( # config
  -s # create embeddable doc
  -a showtitle # output h1
  -a skip-front-matter
)

function tiamat::build_adoc {
  tiamat::require_tool asciidoctor

  # determine output file
  tiamat_permalink="$(tiamat::map_filename "$1")"

  tiamat::verbose 'entering subshell'
  (
    # execute frontmatter
    tiamat::verbose 'running frontmatter'
    eval "$(tiamat::read_frontmatter "$1")"

    # render adoc
    tiamat::verbose 'rendering adoc'
    declare -g content=$(asciidoctor "${tiamat_adoc_args[@]}" -o - "$1")

    # render page template
    tiamat::verbose 'rendering page'
    tiamat::render_template "$tiamat_permalink"
  )
}

function tiamat::build_sass {
  tiamat::require_tool sass

  local out="$tiamat_output_dir$(tiamat::output_name "$1").css"

  echo "-> $out"

  [[ "$tiamat_dryrun" ]] && return

  sass "$1" "$out"
}

# Build the outputs from the specified source file
declare -a tiamat_ignore=( # config
  '*.ignore.*'
  '.DS_Store' '*/.DS_Store' 'Thumbs.db' '*/Thumbs.db'
  '.git*' '*/.git*'
  'README*' '*/README*'
)
function tiamat::build_file {
  tiamat::verbose "building $1"

  # check if file is ignored
  for ignore in "${tiamat_ignore[@]}"; do
    case "$1" in
      $ignore )
        tiamat::verbose "skipping ignored"
        return 0
    esac
  done

  # TODO: for each file, first check if it has an associated .proc.sh file
  # which will be run to process it

  # if not, then process the file directly
  case "$1" in
    # files processed by other tools
    *.md ) ;;
    *.adoc ) tiamat::build_adoc "$1" ;;
    *.sass | *.scss ) tiamat::build_sass "$1" ;;
    # for output html files
    *.html.sh ) tiamat::build_plain "$1" ;;
    *.proc.sh ) ;;
    # other sh files only run if i.e. sourced
    * ) tiamat::verbose "skipping unknown" ;;
  esac
  # identify file type
  # .html.sh -> run
  # .md -> 
}

# Builds each file specified from stdin (nul-separated)
function tiamat::build_files {
  while read -rd $'\0' file; do
    tiamat::build_file "$file"
  done
}

#################
# cli interface #
#################

# Discover all source files in the site folder and build the site
function tiamat::build_site {
  tiamat::verbose 'building site'
  find "$tiamat_source_dir" -type f -print0 | tiamat::build_files |
    tiamat::prefix tiamat
}

# Build site and host a live server
function tiamat::serve_site {
  tiamat::require_tool fswatch
  tiamat::require_tool live-server

  # ensure site is built before starting
  tiamat::build_site

  # start server, kill on exit
  live-server "$tiamat_output_dir" "$@" | tiamat::prefix live-server &
  # local server_job="$!"
  # trap "kill $server_job" EXIT

  # listen for updates and send to build
  fswatch "$tiamat_source_dir" -r -0 | tiamat::build_files |
    tiamat::prefix tiamat || :
}

function tiamat::usage {
  cat <<EOF
usage: $0 [global options] command
global opts:
  -h --help: show this help
  -v --verbose: show extra logging info
  -n --dryrun: don't actually modify files
  -c --config: specify config path (reads tiamat_config.sh by default)
commands:
  build [file]
    build the site (or a single specified source file)
  serve
    build the site and serve on localhost (options passed to live-server)
EOF
  exit "${1:-0}"
}

tiamat_config='tiamat_config.sh' # config

# early options
while [[ "$#" -gt 0 ]]; do
  case "$1" in
    -h | --help    ) tiamat::usage 0 ;;
    -v | --verbose ) tiamat_verbose=1 ;;
    -n | --dryrun  ) tiamat_dryrun=1 ;;
    -c | --config  )
      shift
      tiamat_config="$2" || tiamat::usage 1
    ;;
    -*             ) tiamat::usage 1 ;;
    *              ) break ;;
  esac
  shift
done

# TODO: error if not found
[[ -f "$tiamat_config" ]] && source "$tiamat_config"

# make paths absolute
tiamat_source_dir="$(realpath "$tiamat_source_dir")"
tiamat_output_dir="$(realpath "$tiamat_output_dir")"

# command
case "${1:-}" in
  help  ) tiamat::usage 0 ;;
  build ) tiamat::build_site ;;
  serve ) tiamat::serve_site ;;
  *     ) tiamat::usage 1 ;;
esac

# vim: syn=bash
