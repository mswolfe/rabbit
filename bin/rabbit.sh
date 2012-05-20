#!/bin/bash
# 
# Runs the racket executable found in the
# local directory with a database in the
# current directory.
#
##########################################

print_usage() {
cat << EOF
  USAGE : `basename $0` [OPTIONS]
  Runs the rabbit-cli executable.
  
  OPTIONS
    --database <filename> : Supercedes the database value set by this script.
    <?>                   : All other options are passed to the rabbit executable.
    -help                 : Prints out this help
EOF
return;
}

DB_PATH=""
ARGS=""

# Get user supplied options.
while [ "$#" -ne 0 ]
do
  if [ "$1" == "--database" ]; then
    if [ "$#" -gt 1 ]; then
      DB_PATH=$2
      shift
      shift
    else
      echo "--database requires an additional argument"
      echo
      print_usage
      exit 1
    fi
  elif [ "$1" == "-help" ]; then
    print_usage
    exit 1
  else
    ARGS="$ARGS $1"
    shift
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

if [ "$DB_PATH" == "" ]; then
  DB_PATH="$PROGDIR/rabbit.db"
fi

CMD="$PROGDIR/rabbit-cli --database $DB_PATH $ARGS"
$CMD