#!/bin/bash

PROJDIR=$(dirname $0)

${PROJDIR}/../prolog/orchestrator.pl -- "$@"
