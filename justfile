

stagedir := "target/universal/stage/bin"

test:
    sbt test

build:
    sbt stage

alias b := build

exprToMlir EXPR:
    echo "{{EXPR}}" | {{stagedir}}/sigi-to-mlir -

sigiToMlir *ARGS:
    {{stagedir}}/sigi-to-mlir {{ARGS}}

interpretSigi *ARGS:
    {{stagedir}}/interpret-sigi {{ARGS}}

repl:
    {{stagedir}}/repl

# setup commands

# Install sdkman if you haven't already
installSdkman:
    #!bin/bash
    sdkman_auto_answer=true curl -s "https://get.sdkman.io" | $SHELL

# Install required dev tools, given an existing sdkman installation
installRequirementsWithSdkman:
    #!bin/bash
    set -e
    source "$HOME/.sdkman/bin/sdkman-init.sh"
    # get scala
    sdk i scala 3.2.1
    sdk i sbt
    # get graalvm
    sdk i java 22.3.r19-grl
    # get native image
    gu install native-image


