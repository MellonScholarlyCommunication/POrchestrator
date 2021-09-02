#!/bin/bash

PROJDIR=$(dirname $0)

cd ${PROJDIR}/..

./prolog/orchestrator.pl -- "$@"
