source tiamat.sh

function tiamat::root {
  tiamat::template

  doctype html
  html
    head
      title ="${title:-untitled page}"
      link rel=stylesheet href=/style.css
    end

    body
      block_body
    end
  end
}

function block_body {
  div .container
    block_container
  end
}

function block_container {
  header
    div ='this is a header that appears on every page'
    a href=/ ='go home'
  end

  main
    block_main
  end
}

function block_main {
  raw "$content"
}