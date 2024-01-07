

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

## Used in combination with the "Attach" launch configuration in VSCode
debugRepl:
    {{stagedir}}/repl -agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=localhost:5005

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

cleanMetalsFiles:
    #!/bin/bash
    todelete=$(find . -wholename "**/.bloop")
    if [  -n "${todelete}" ]; then 
        rm -r ${todelete}
    fi
    
    todelete=$(find . -wholename "**/metals.sbt")
    if [  -n "${todelete}" ]; then 
        rm -r ${todelete}
    fi

    todelete=$(find . -wholename "**/.metals")
    if [  -n "${todelete}" ]; then 
        rm -r ${todelete}
    fi
    
    echo "Remember to import build"
    

cleanTargetFolders:
    #!/bin/bash
    todelete=$(find . -wholename "**/target")
    if [  -n "${todelete}" ]; then 
        rm -r ${todelete}
    fi

