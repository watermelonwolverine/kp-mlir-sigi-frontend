missing_env_file_msg := "
=====================================================================
= Missing file: .justfile.env. Check README.md for more information =
====================================================================="

############################################################################
## These recipes are for working with the Dev Container plugin for vscode ##
############################################################################
prepareDockerfile:
    #!/bin/bash
    source .justfile.env 2> /dev/null || (echo "{{missing_env_file_msg}}"; exit 1)

    __USER__=$(whoami)

    __PROJECT_NAME__=$(basename $(pwd))

    cp Dockerfile.template Dockerfile
    sed -i -e "s,__BASE_IMAGE__,${__BASE_IMAGE__},g" Dockerfile
    sed -i -e "s,__USER__,${__USER__},g" Dockerfile