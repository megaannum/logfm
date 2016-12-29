#!/bin/bash
declare -r SCRIPT=$(basename $0)
declare -r BIN_DIR=$(cd "$(dirname $0)"; pwd)

if [[ $# -ne 1 ]]; then
  echo "usage: $SCRIPT <number>"
  exit 1
fi

$SCALA_HOME/bin/scala com.megaannum.logging.logfm.LoggerTest $@
