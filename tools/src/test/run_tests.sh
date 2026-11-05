#!/bin/sh
# Compile and run the unit tests with instantfpc.
# Usage: ./run_tests.sh [runner options]   (default: -a)

cd "$(dirname "$0")" || exit 1

# -B on the instantfpc command line: always rebuild. The cache only tracks the
# main program source, and -B inside INSTANTFPCOPTIONS does not force a build.
export INSTANTFPCOPTIONS="-Fu.. -Fu../ai_plane -Fu../../../FastPlaz/src/library -Fu../../../FastPlaz/test"

# The Agent Plane tests talk to this fake agent.
echo "Compiling mock_acp_agent..."
mkdir -p lib
fpc -O1 -FUlib mock_acp_agent.lpr > lib/mock_acp_agent.txt 2>&1 \
  || { cat lib/mock_acp_agent.txt; exit 1; }

echo "Compiling (instantfpc)..."
if [ $# -eq 0 ]; then
  set -- -a
fi
exec instantfpc -B tools_test.lpr "$@"
