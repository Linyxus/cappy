package dotty.tools.benchmarks.py

/** Registry of self-contained pattern benchmarks. Each entry maps to one
 *  `.scala` source (one bench class per file) under
 *  `bench-py/src/main/scala/dotty/tools/benchmarks/py/<category>/`. Ops and
 *  sizes are per-bench (no fixed build/access/transform/mutate vocabulary). */
object Catalog:
  final case class Entry(
      benchId:    String,        // "<category>.<ClassName>"; drives qual + bundle dir
      bundleName: String,        // "<SourceBaseName>.py"
      source:     String,        // repo-relative .scala path
      ops:        List[String],
      sizes:      List[Int],
  )

  private val defaultSizes: List[Int] = List(64, 1024)

  /** category = sub-package under dotty.tools.benchmarks.py; name = class /
   *  source base name (kept identical, so one bundle file per bench). */
  private def bench(
      category: String,
      name:     String,
      ops:      List[String],
      sizes:    List[Int] = defaultSizes,
  ): Entry = Entry(
    benchId    = s"$category.$name",
    bundleName = s"$name.py",
    source     = s"bench-py/src/main/scala/dotty/tools/benchmarks/py/$category/$name.scala",
    ops        = ops,
    sizes      = sizes,
  )

  val entries: List[Entry] = List(
    // --- Phase 1 starter set ---
    bench("numeric",     "NumericLoopBench",  List("sumLoop", "mulAccum", "divmod")),
    bench("recursion",   "FibBench",          List("recursive", "iterative")),
    bench("recursion",   "TailRecBench",      List("countDown", "sumTail")),
    bench("patmat",      "ExprEvalBench",     List("eval", "render")),
    bench("closures",    "HigherOrderBench",  List("mapInc", "filterEven", "foldSum", "mapFilterFold")),
    bench("strings",     "StringBuildBench",  List("builder", "interpolate", "mkString")),
    bench("collections", "ImmutableSeqBench", List("listBuild", "listFold", "vectorBuild", "vectorAccess")),

    // --- numeric & math algorithms ---
    bench("mathalg", "SieveBench",      List("sieve"),                            List(256, 512, 1024)),
    bench("mathalg", "GcdBench",        List("gcdLoop", "lcmLoop"),               List(256, 512, 1024)),
    bench("mathalg", "ModPowBench",     List("modPow", "intPow"),                 List(256, 512, 1024)),
    bench("mathalg", "IntSqrtBench",    List("isqrtNewton", "isqrtBit"),          List(256, 512, 1024)),
    bench("mathalg", "BitTwiddleBench", List("popcount", "reverseBits", "leadingZeros"), List(512, 1024)),
    bench("mathalg", "MatMulBench",     List("matmul16x16"),                      List(128, 256, 512)),
    bench("mathalg", "CollatzBench",    List("collatzMax", "collatzSum"),         List(256, 512, 1024)),
    bench("mathalg", "ArrayArithBench", List("dotProduct", "prefixSum", "maxScan"), List(256, 512, 1024)),

    // --- sorting & searching ---
    bench("sorting", "InsertionSortBench", List("insertionSort"),                 List(64, 256)),
    bench("sorting", "QuicksortBench",     List("quicksort"),                     List(64, 1024)),
    bench("sorting", "MergeSortBench",     List("mergeSort"),                     List(64, 1024)),
    bench("sorting", "BinarySearchBench",  List("binarySearchHit", "binarySearchMiss", "linearSearch"), List(64, 1024)),
    bench("sorting", "StdlibSortBench",    List("listSorted", "vectorSorted", "listSortWith", "vectorSortBy"), List(64, 1024)),
    bench("sorting", "RecordSortBench",    List("sortByKey", "sortBySecondary", "sortedCustom"), List(64, 1024)),
    bench("sorting", "CountingSortBench",  List("countingSort", "quickselect"),   List(64, 1024)),

    // --- custom data structures ---
    bench("datastruct", "ConsListBench",        List("build", "sumFold", "length", "reverse"), List(256, 512, 1024)),
    bench("datastruct", "BSTBench",             List("lookup", "insertFresh", "depth", "inOrderSum"), List(128, 256, 512)),
    bench("datastruct", "IntHeapBench",         List("heapSort", "pushPop", "minPeek"), List(512, 1024, 2048)),
    bench("datastruct", "UnionFindBench",       List("findAll", "unionBatch", "connected"), List(512, 1024, 2048)),
    bench("datastruct", "PersistentStackBench", List("pushPop", "sumTop", "copyStack"), List(256, 512, 1024)),
    bench("datastruct", "HeapTreeBench",        List("insertBatch", "deleteMinSeq", "mergeTrees"), List(128, 256, 512)),
    bench("datastruct", "RingBufferBench",      List("enqDeq", "slideWindow", "peekLoop"), List(512, 1024, 2048)),

    // --- graph & dynamic programming ---
    bench("graphdp", "BfsBench",           List("bfsFull"),                       List(256, 512, 1024)),
    bench("graphdp", "DfsBench",           List("dfsIterative"),                  List(256, 512, 1024)),
    bench("graphdp", "FloydWarshallBench", List("floydWarshall"),                 List(256, 384, 512)),
    bench("graphdp", "KnapsackBench",      List("knapsack01"),                    List(256, 512, 1024)),
    bench("graphdp", "EditDistBench",      List("editDist"),                      List(256, 512, 1024)),
    bench("graphdp", "CoinChangeBench",    List("coinChangeMin", "coinChangeCount"), List(256, 512, 1024)),
    bench("graphdp", "LisBench",           List("lisLength"),                     List(256, 512, 1024)),
    bench("graphdp", "UnionFindBench",     List("unionFind"),                     List(256, 512, 1024)),

    // --- text & string processing ---
    bench("text", "CharFreqBench",            List("charFreqArray", "charFreqMap"), List(256, 512, 1024)),
    bench("text", "CharScanBench",            List("vowelCount", "digitCount", "upperCount"), List(512, 1024, 2048)),
    bench("text", "StringReverseBench",       List("builderReverse", "arrayReverse"), List(128, 512, 1024)),
    bench("text", "CaesarCipherBench",        List("caesarBuilder", "caesarArray"), List(256, 512, 1024)),
    bench("text", "RunLengthBench",           List("encode", "decode"),           List(256, 512, 1024)),
    bench("text", "NaiveSubstringSearchBench", List("naiveSearch", "indexOfSearch"), List(512, 1024, 2048)),
    bench("text", "WordSplitBench",           List("manualSplit", "charCountWords", "reverseWords"), List(64, 128, 256)),

    // --- functional collection pipelines ---
    bench("pipelines", "MapFilterFoldBench",    List("map1", "mapFilter", "mapFilterFold", "chain4"), List(256, 512, 1024)),
    bench("pipelines", "ViewVsStrictBench",     List("strictMapFilter", "viewMapFilter", "viewChain3", "strictChain3"), List(512, 1024)),
    bench("pipelines", "FlatMapExpansionBench", List("flatMapPair", "flatMapFilter", "flatMapNested"), List(256, 512, 1024)),
    bench("pipelines", "GroupByAggregateBench", List("groupBySize", "groupByBucketSum", "groupMapReduce", "groupMap"), List(256, 512, 1024)),
    bench("pipelines", "ZipIndexSumBench",      List("zipWithIndex", "zipTwoVecs", "zipFoldLeft"), List(256, 512, 1024)),
    bench("pipelines", "ScanLeftWindowBench",   List("scanLeftSum", "scanLeftProd", "slidingSum", "groupedSum"), List(256, 512, 1024)),
    bench("pipelines", "PartitionSortTopKBench", List("partition", "sortTake", "sortWithTake", "partSortSum"), List(128, 256, 512)),
    bench("pipelines", "ForComprehensionBench", List("forYieldTwo", "forYieldIf", "withFilter"), List(256, 512, 1024)),

    // --- interpreters, VMs & state machines ---
    bench("interp", "StackVMBench",          List("run"),                List(128, 512, 1024)),
    bench("interp", "BytecodeVMBench",       List("run"),                List(256, 1024, 2048)),
    bench("interp", "RecursiveDescentBench", List("parse"),              List(64, 256, 512)),
    bench("interp", "BrainfuckBench",        List("run"),                List(256, 1024, 4096)),
    bench("interp", "NFAMatcherBench",       List("simulate"),           List(16, 64, 256)),
    bench("interp", "FSMBench",              List("run"),                List(256, 1024, 4096)),
    bench("interp", "CellularAutomatonBench", List("rule90", "rule110"), List(64, 256, 1024)),

    // --- polymorphism & virtual dispatch ---
    bench("dispatch", "ShapeDispatchBench",    List("megaArea", "monoArea", "megaPerimeter", "bimorphicArea")),
    bench("dispatch", "TraitDefaultBench",     List("inheritedScore", "overriddenScore", "mixedScore", "abstractBonus")),
    bench("dispatch", "DeepInheritanceBench",  List("shallowCompute", "deepCompute", "deepMixedCompute")),
    bench("dispatch", "VisitorBench",          List("sum", "count")),
    bench("dispatch", "FunctionVsMethodBench", List("closureBimorphic", "methodBimorphic", "closureMono", "methodMono")),
    bench("dispatch", "InterfaceMixinBench",   List("hashMixed", "compareMixed", "hashMono", "allThreeMixed")),
    bench("dispatch", "DecoratorChainBench",   List("depth1", "depth2", "depth4")),
    bench("dispatch", "IsInstanceOfBench",     List("patMatchMega", "patMatchMono", "isInstanceMega", "isInstanceMono")),

    // --- Option / Either / Try & error-handling control flow ---
    bench("errorflow", "OptionPipelineBench",     List("mapFlatMap", "chainedOps", "foldGetOrElse", "mapFold"), List(256, 512, 1024)),
    bench("errorflow", "OptionForCompBench",      List("forYield", "optionForComp"), List(32, 64, 256)),
    bench("errorflow", "EitherValidationBench",   List("mapFlatMap", "chainedFlat", "foldBoth", "getOrElseFold"), List(256, 512, 1024)),
    bench("errorflow", "TryParseBench",           List("tryParse", "tryMapFlat", "tryGetOrElse", "tryRecover"), List(128, 256, 512)),
    bench("errorflow", "ThrowCatchVsEitherBench", List("throwCatch", "optionDiv", "eitherDiv", "customThrow"), List(256, 512, 1024)),
    bench("errorflow", "OptionFlattenBench",      List("flatten", "sequence", "orElseChain", "mapN"), List(256, 512, 1024)),
    bench("errorflow", "BoundaryVsOptionBench",   List("boundaryEarlyExit", "optionFind", "boundaryNoExit", "optionFindMiss"), List(256, 512, 1024)),

    // --- hashing, equality & ordering ---
    bench("hasheq", "CaseClassHashBench",      List("sumHashCodes", "countEqual", "countDistinctHash")),
    bench("hasheq", "HandRolledHashBench",     List("manualMurmur", "stdlibHashInt", "pairHashSum")),
    bench("hasheq", "HashSetCaseKeyBench",     List("buildSet", "containsHit", "containsMiss")),
    bench("hasheq", "SortByOrderingBench",     List("sortMultiKey", "sortByDept", "minByDept", "maxBySalary")),
    bench("hasheq", "TreeSetCustomOrderBench", List("buildTreeSet", "insertions", "headTail", "rangeQuery"), List(64, 512)),
    bench("hasheq", "HashMapCaseKeyBench",     List("buildMap", "lookupAll", "updateAll")),
    bench("hasheq", "GroupByRecordBench",      List("groupByKind", "groupByKindSize", "groupMap", "groupMapReduce")),
    bench("hasheq", "DeduplicateVectorBench",  List("toHashSet", "toDistinct", "unionSelf", "sizeAfterDedup")),

    // --- control flow & iteration patterns ---
    bench("controlflow", "WhileVsForeachSumBench",     List("whileLoop", "rangeForEach", "rangeFoldLeft", "rangeSum")),
    bench("controlflow", "TailRecVsWhileBench",        List("whileSum", "tailRecSum", "forLoopSum")),
    bench("controlflow", "RangeStridesBench",          List("strideBy1", "strideBy2", "strideBy3", "strideDesc")),
    bench("controlflow", "EarlyExitSearchBench",       List("whileEarlyExit", "foreachNonLocalRet", "existsCombinator", "findCombinator")),
    bench("controlflow", "NestedLoopBench",            List("nestedWhile", "nestedForEach", "flatWhile", "nestedFoldLeft")),
    bench("controlflow", "MatchInLoopBench",           List("matchInWhile", "ifChainInWhile", "matchInForeach")),
    bench("controlflow", "ForeachNonLocalReturnBench", List("nonLocalRetForeach", "nonLocalRetEarlyHit", "whileIndexScan", "existsScan")),
    bench("controlflow", "BoundaryBreakBench",         List("boundaryBreakEarlyExit", "whileEarlyExit", "existsEarlyExit")),
  )
