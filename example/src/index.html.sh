source template.sh

title='the homepage'

function block_main {
  h1 ='tiamat example site'

  p ='this is the index page, it can have stuff generated from arbitrary Bash constructs'
  p
    text "for example, here's a list of items generated from a "
    code ='for'
    text ' loop:'
  end
  ul
    for (( i=1; i<=10; i++ )); do
      li ="$i"
    done
  end
  p
    text "since it's a bash script, you can also just include arbitrary program output: "
    text "this page was built at $(date +%c)"
  end
  p
    text 'also check out the other page '
    a href=/other_page ='here'
    text ' which is written in asciidoc'
  end
}

tiamat::render_template