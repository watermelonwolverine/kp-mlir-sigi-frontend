

stagedir := "target/universal/stage/bin"

build:
    sbt stage

alias b := build

toMlir EXPR:
    echo "{{EXPR}}" | {{stagedir}}/sigi-to-mlir

repl:
    {{stagedir}}/repl

test:
    sbt test
