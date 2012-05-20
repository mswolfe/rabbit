#!/bin/bash
# 
# Builds a racket executable.
#
##########################################

print_usage() {
cat << EOF
  USAGE : `basename $0` [OPTIONS]
  Creates a racket executable for the rabbit-cli script.
  
  OPTIONS
    -dev         : Creates a launcher that runs the code from source.
    -help        : Prints out this help
EOF
return;
}

# Get user supplied options.
while [ "$#" -ne 0 ]
do
  if [ "$1" == "-dev" ]; then
    DEV="true"
    shift
  elif [ "$1" == "-help" ]; then
    print_usage
    exit 1
  else
    echo "Unrecognized option..."
    print_usage
    exit 1
  fi
done

# Get the directory where the user started this script from.
PRG="$0"
while [ -h "$PRG" ]; do
  ls=`ls -ld "$PRG"`
  link=`expr "$ls" : '.*-> \(.*\)$'`
  if expr "$link" : '/.*' > /dev/null; then
    PRG="$link"
  else
    PRG=`dirname "$PRG"`/"$link"
  fi
done

# Get the directory where the user started this script from.
PROGDIR=`dirname "$PRG"`

IN_FILE="$PROGDIR/../src/rabbit-cli.rkt"
OUT_FILE="-o $PWD/rabbit-cli"
ARGS=""

if [ "$DEV" == "true" ] ; then
  ARGS="-l"
fi

CMD="raco exe $ARGS $OUT_FILE $IN_FILE"

echo "Building rabbit-cli: $CMD"
$CMD