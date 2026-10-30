#!/bin/sh
# Compile and run the unit tests with instantfpc.
# Usage: ./run_tests.sh [runner options]   (default: -a)

cd "$(dirname "$0")" || exit 1

# -B: always rebuild, the cache only tracks the main program source.
export INSTANTFPCOPTIONS="-B -Fu.. -Fu../../../FastPlaz/src/library"

if [ $# -eq 0 ]; then
  set -- -a
fi
exec instantfpc regex_tester_test.lpr "$@"
