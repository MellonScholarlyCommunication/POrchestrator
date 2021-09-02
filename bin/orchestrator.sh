#!/bin/bash

PROJDIR=$(dirname $0)

cd ${PROJDIR}/..

# If we get a directory as second argument, glob over all rule files 
if [ -d $2 ]; then
    ./prolog/orchestrator.pl -- $1 $2/*.n3
else
    ./prolog/orchestrator.pl -- "$@"
fi
