#!/bin/bash

DIR=$1

PARENT_DIR="$(dirname $(realpath $0))"

PARSER="$PARENT_DIR/ginger-result-parser"
(cd "$(dirname $(realpath $0))"; go build)

echo "Parametric fragments discovered (Table 1 - Column 2)"
$PARSER -name "Channel"  -regexp "Profiling Go program parametricity:\nProgram is ((channel loop parametric|capacity parametric)( and )?)+\n" -dir $1
$PARSER -name "WaitGroup:"  -regexp "Profiling Go program parametricity:\nProgram is ((WaitGroup loop parametric|add parametric)( and )?)+" -dir $1
$PARSER -name "Both"  -regexp "Profiling Go program parametricity:\nProgram is ((channel loop parametric|capacity parametric)( and )?)+( and )?((WaitGroup loop parametric|add parametric)( and )?)+" -dir $1
$PARSER -name "Total" -regexp "Profiling Go program parametricity:\nProgram is [^n]" -dir $1
echo ""
echo "Expressible fragments (Table 1 - Column 3)"
$PARSER -name "Channel"  -regexp "Profiling Go program parametricity:\nProgram is ((channel loop parametric|capacity parametric)( and )?)+\n+VIRGo translation:" -dir $1
$PARSER -name "WaitGroup"  -regexp "Profiling Go program parametricity:\nProgram is ((WaitGroup loop parametric|add parametric)( and )?)+\n+VIRGo translation:" -dir $1
$PARSER -name "Both"  -regexp "Profiling Go program parametricity:\nProgram is ((channel loop parametric|capacity parametric)( and )?)+( and )?((WaitGroup loop parametric|add parametric)( and )?)+\n+VIRGo translation:" -dir $1
$PARSER -name "Total"  -regexp "Profiling Go program parametricity:\nProgram is [^n][^\n]+\n+VIRGo translation:" -dir $1
echo ""
echo "Efficacy of verification back-end (Table 1 - Column 4)"
$PARSER -name "Channel"  -regexp "\[ SUCCESS \| [^|]+ \| .* \| ((chan-loop|capacity)(; )?)+ \]" -dir $1
$PARSER -name "WaitGroup"  -regexp "\[ SUCCESS \| [^|]+ \| .* \| ((wg-add|wg-loop)(; )?)+ \]" -dir $1
$PARSER -name "Both" -regexp "\[ SUCCESS \| [^|]+ \| .* \| ((chan-loop|capacity)(; )?)+((wg-add|wg-loop)(; )?)+((chan-loop|capacity)(; )?)* \]" -dir $1
$PARSER -name "Total"  -regexp "\[ SUCCESS \| [^|]+ \| .* \| [^n][^|]* \]" -dir $1
echo ""
echo "Efficacy of Strat. 1 (Table 1 - Column 5)"
$PARSER -name "Channel"  -regexp "\[ SUCCESS \| strat-1 \| .* \| ((chan-loop|capacity)(; )?)+ \]" -dir $1
$PARSER -name "WaitGroup"  -regexp "\[ SUCCESS \| strat-1 \| .* \| ((wg-add|wg-loop)(; )?)+ \]" -dir $1
$PARSER -name "Both" -regexp "\[ SUCCESS \| strat-1 \| .* \| ((chan-loop|capacity)(; )?)+((wg-add|wg-loop)(; )?)+((chan-loop|capacity)(; )?)* \]" -dir $1
$PARSER -name "Total"  -regexp "\[ SUCCESS \| strat-1 \| .* \| [^n]" -dir $1
echo ""
echo "Efficacy of Strat. 2 (Table 1 - Column 6)"
$PARSER -name "Channel"  -regexp "\[ SUCCESS \| strat-2 \| .* \| ((chan-loop|capacity)(; )?)+ \]" -dir $1
$PARSER -name "WaitGroup"  -regexp "\[ SUCCESS \| strat-2 \| .* \| ((wg-add|wg-loop)(; )?)+ \]" -dir $1
$PARSER -name "Both" -regexp "\[ SUCCESS \| strat-2 \| .* \| ((chan-loop|capacity)(; )?)+((wg-add|wg-loop)(; )?)+((chan-loop|capacity)(; )?)* \]" -dir $1
$PARSER -name "Total"  -regexp "\[ SUCCESS \| strat-2 \| .* \| [^n][^|]* \]" -dir $1
echo ""
echo "Efficacy of Strat. 3 (Table 1 - Column 7)"
$PARSER -name "Channel"  -regexp "\[ SUCCESS \| strat-3 \| .* \| ((chan-loop|capacity)(; )?)+ \]" -dir $1
$PARSER -name "WaitGroup"  -regexp "\[ SUCCESS \| strat-3 \| .* \| ((wg-add|wg-loop)(; )?)+ \]" -dir $1
$PARSER -name "Both" -regexp "\[ SUCCESS \| strat-3 \| .* \| ((chan-loop|capacity)(; )?)+((wg-add|wg-loop)(; )?)+((chan-loop|capacity)(; )?)* \]" -dir $1
$PARSER -name "Total"  -regexp "\[ SUCCESS \| strat-3 \| .* \| [^n][^|]* \]" -dir $1
echo ""
$PARSER -name "Verification errors"  -regexp "\[ VERIFICATION ERROR \| .* \| [^n][^|]* \]" -dir $1
echo ""
echo "Verification time (Table 2)"
$PARSER -name "Strat. 1" -regexp "strat-1 verification time: \d+ms\n([^\n]*\n)+\[ (SUCCESS|VERIFICATION ERROR) \| .* \| [^n][^|]* \]" -dir $1
$PARSER -name "Strat. 2" -regexp "strat-2 verification time: \d+ms\n([^\n]*\n)+\[ (SUCCESS|VERIFICATION ERROR) \| .* \| [^n][^|]* \]" -dir $1
$PARSER -name "Strat. 3" -regexp "strat-3 verification time: \d+ms\n([^\n]*\n)+\[ (SUCCESS|VERIFICATION ERROR) \| .* \| [^n][^|]* \]" -dir $1
echo ""
echo "Complexity metrics"
$PARSER -name "FV occurrences" -regexp "FV occurrences:" -dir $1
$PARSER -name "Unique FVs"  -regexp "Unique FVs:" -dir $1
