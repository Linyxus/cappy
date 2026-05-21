#!/usr/bin/env bash
# Compile (reuse agent bundle if present, else recompile) + runtime-survey every
# bench. Runs each op once and records OK/EXC. Parallel. Writes a summary.
set -u
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." >/dev/null && pwd)"
cd "$ROOT" || exit 1
OUT="/tmp/benchsweep"; RES="$OUT/results"
rm -rf "$OUT"; mkdir -p "$RES"
SRCROOT="bench-py/src/main/scala/dotty/tools/benchmarks/py"
export ROOT OUT RES SRCROOT

do_one() {
  cat="$1"; name="$2"; ops="$3"; size="$4"
  src="$SRCROOT/$cat/$name.scala"
  res="$RES/$cat.$name.txt"
  # Prefer the agent's already-built bundle; else recompile.
  bundle="/tmp/benchcheck-$cat/$name/$name.py"
  if [ ! -f "$bundle" ]; then
    out="$OUT/$cat-$name"; mkdir -p "$out"
    ./bin/spc -d "$out" "$src" >"$OUT/$cat-$name.compile.log" 2>&1
    bundle="$out/$name.py"
  fi
  if [ ! -f "$bundle" ]; then
    echo "COMPILE-FAIL $cat.$name" >"$res"; return
  fi
  run="$OUT/$cat-$name.run.log"
  BENCH_BUNDLE="$bundle" BENCH_QUAL="dotty.tools.benchmarks.py.$cat.$name" \
    BENCH_SIZE="$size" BENCH_OPS="$ops" \
    uv run --project "$ROOT" --no-sync python -W ignore \
      bench-py/python-shim/survey_run.py >"$run" 2>&1
  rc=$?
  nexc=$(grep -c '^  EXC ' "$run" 2>/dev/null); nexc=${nexc:-0}
  nsetup=$(grep -c 'SETUPFAIL\|LOADFAIL' "$run" 2>/dev/null); nsetup=${nsetup:-0}
  if [ "$nsetup" != 0 ]; then
    echo "RUN-SETUPFAIL $cat.$name" >"$res"
  elif [ "$nexc" != 0 ]; then
    echo "RUN-EXC $cat.$name ($nexc op(s))" >"$res"
  else
    echo "OK $cat.$name" >"$res"
  fi
  { echo "=== $cat.$name (size=$size) ==="; cat "$run"; } >>"$OUT/run-all.log"
}
export -f do_one

cat >"$OUT/manifest.txt" <<'EOF'
mathalg SieveBench sieve 256
mathalg GcdBench gcdLoop,lcmLoop 256
mathalg ModPowBench modPow,intPow 256
mathalg IntSqrtBench isqrtNewton,isqrtBit 256
mathalg BitTwiddleBench popcount,reverseBits,leadingZeros 512
mathalg MatMulBench matmul16x16 128
mathalg CollatzBench collatzMax,collatzSum 256
mathalg ArrayArithBench dotProduct,prefixSum,maxScan 256
sorting InsertionSortBench insertionSort 64
sorting QuicksortBench quicksort 64
sorting MergeSortBench mergeSort 64
sorting BinarySearchBench binarySearchHit,binarySearchMiss,linearSearch 64
sorting StdlibSortBench listSorted,vectorSorted,listSortWith,vectorSortBy 64
sorting RecordSortBench sortByKey,sortBySecondary,sortedCustom 64
sorting CountingSortBench countingSort,quickselect 64
datastruct ConsListBench build,sumFold,length,reverse 256
datastruct BSTBench lookup,insertFresh,depth,inOrderSum 128
datastruct IntHeapBench heapSort,pushPop,minPeek 512
datastruct UnionFindBench findAll,unionBatch,connected 512
datastruct PersistentStackBench pushPop,sumTop,copyStack 256
datastruct HeapTreeBench insertBatch,deleteMinSeq,mergeTrees 128
datastruct RingBufferBench enqDeq,slideWindow,peekLoop 512
graphdp BfsBench bfsFull 256
graphdp DfsBench dfsIterative 256
graphdp FloydWarshallBench floydWarshall 256
graphdp KnapsackBench knapsack01 256
graphdp EditDistBench editDist 256
graphdp CoinChangeBench coinChangeMin,coinChangeCount 256
graphdp LisBench lisLength 256
graphdp UnionFindBench unionFind 256
text CharFreqBench charFreqArray,charFreqMap 256
text CharScanBench vowelCount,digitCount,upperCount 512
text StringReverseBench builderReverse,arrayReverse 128
text CaesarCipherBench caesarBuilder,caesarArray 256
text RunLengthBench encode,decode 256
text NaiveSubstringSearchBench naiveSearch,indexOfSearch 512
text WordSplitBench manualSplit,charCountWords,reverseWords 64
pipelines MapFilterFoldBench map1,mapFilter,mapFilterFold,chain4 256
pipelines ViewVsStrictBench strictMapFilter,viewMapFilter,viewChain3,strictChain3 512
pipelines FlatMapExpansionBench flatMapPair,flatMapFilter,flatMapNested 256
pipelines GroupByAggregateBench groupBySize,groupByBucketSum,groupMapReduce,groupMap 256
pipelines ZipIndexSumBench zipWithIndex,zipTwoVecs,zipFoldLeft 256
pipelines ScanLeftWindowBench scanLeftSum,scanLeftProd,slidingSum,groupedSum 256
pipelines PartitionSortTopKBench partition,sortTake,sortWithTake,partSortSum 128
pipelines ForComprehensionBench forYieldTwo,forYieldIf,withFilter 256
interp StackVMBench run 128
interp BytecodeVMBench run 256
interp RecursiveDescentBench parse 64
interp BrainfuckBench run 256
interp NFAMatcherBench simulate 16
interp FSMBench run 256
interp CellularAutomatonBench rule90,rule110 64
dispatch ShapeDispatchBench megaArea,monoArea,megaPerimeter,bimorphicArea 64
dispatch TraitDefaultBench inheritedScore,overriddenScore,mixedScore,abstractBonus 64
dispatch DeepInheritanceBench shallowCompute,deepCompute,deepMixedCompute 64
dispatch VisitorBench sum,count 64
dispatch FunctionVsMethodBench closureBimorphic,methodBimorphic,closureMono,methodMono 64
dispatch InterfaceMixinBench hashMixed,compareMixed,hashMono,allThreeMixed 64
dispatch DecoratorChainBench depth1,depth2,depth4 64
dispatch IsInstanceOfBench patMatchMega,patMatchMono,isInstanceMega,isInstanceMono 64
errorflow OptionPipelineBench mapFlatMap,chainedOps,foldGetOrElse,mapFold 256
errorflow OptionForCompBench forYield,optionForComp 32
errorflow EitherValidationBench mapFlatMap,chainedFlat,foldBoth,getOrElseFold 256
errorflow TryParseBench tryParse,tryMapFlat,tryGetOrElse,tryRecover 128
errorflow ThrowCatchVsEitherBench throwCatch,optionDiv,eitherDiv,customThrow 256
errorflow OptionFlattenBench flatten,sequence,orElseChain,mapN 256
errorflow BoundaryVsOptionBench boundaryEarlyExit,optionFind,boundaryNoExit,optionFindMiss 256
hasheq CaseClassHashBench sumHashCodes,countEqual,countDistinctHash 64
hasheq HandRolledHashBench manualMurmur,stdlibHashInt,pairHashSum 64
hasheq HashSetCaseKeyBench buildSet,containsHit,containsMiss 64
hasheq SortByOrderingBench sortMultiKey,sortByDept,minByDept,maxBySalary 64
hasheq TreeSetCustomOrderBench buildTreeSet,insertions,headTail,rangeQuery 64
hasheq HashMapCaseKeyBench buildMap,lookupAll,updateAll 64
hasheq GroupByRecordBench groupByKind,groupByKindSize,groupMap,groupMapReduce 64
hasheq DeduplicateVectorBench toHashSet,toDistinct,unionSelf,sizeAfterDedup 64
controlflow WhileVsForeachSumBench whileLoop,rangeForEach,rangeFoldLeft,rangeSum 64
controlflow TailRecVsWhileBench whileSum,tailRecSum,forLoopSum 64
controlflow RangeStridesBench strideBy1,strideBy2,strideBy3,strideDesc 64
controlflow EarlyExitSearchBench whileEarlyExit,foreachNonLocalRet,existsCombinator,findCombinator 64
controlflow NestedLoopBench nestedWhile,nestedForEach,flatWhile,nestedFoldLeft 64
controlflow MatchInLoopBench matchInWhile,ifChainInWhile,matchInForeach 64
controlflow ForeachNonLocalReturnBench nonLocalRetForeach,nonLocalRetEarlyHit,whileIndexScan,existsScan 64
controlflow BoundaryBreakBench boundaryBreakEarlyExit,whileEarlyExit,existsEarlyExit 64
EOF

echo "[sweep] $(wc -l < "$OUT/manifest.txt") benches; running with -P 4 ..."
< "$OUT/manifest.txt" xargs -P 4 -L1 bash -c 'do_one "$@"' _

echo "============================================================"
cat "$RES"/*.txt | sort | sed 's/^/  /'
echo "============================================================"
total=$(cat "$RES"/*.txt | wc -l | tr -d ' ')
ok=$(grep -l '^OK ' "$RES"/*.txt 2>/dev/null | wc -l | tr -d ' ')
echo "benches: $total   fully-OK: $ok   with-issues: $((total-ok))"
echo "details: $OUT/run-all.log    compile logs: $OUT/*.compile.log"
