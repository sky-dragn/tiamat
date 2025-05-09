source template.sh
tiamat::depend template.sh

title='the homepage'

function block_main {
  h1 ='tiamat example site'

  p ="this is the index page, it's written in Bash so it
    can have stuff generated from arbitrary Bash constructs"
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
    text <<end
since it's a bash script, you can also just include arbitrary program output.
for example, two random words chosen at build time are "$(shuf -n 2 /usr/share/dict/words)",
and this page was built at $(date +%c)
end
  end
  p
    text 'also check out the other page '
    a href=/other_page ='here'
    text ' which is written in asciidoc'
  end

  p
    text "oh also there's escaping happening: if i do <i>markup</i> in here it is escaped, "
    raw 'but if i do it here it is <b>not escaped!</b>'
  end

  code =':3'

  markdown <<end
## also
this is a \`markdown\` block, which i'm writing **inline** with the rest of this
template! teehee
end
}

tiamat::render_template