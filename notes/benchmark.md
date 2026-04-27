# Benchmark Results

JVM vs Python backend, taking the maximal size (4096) for each `bench.op`.

| benchmark | Java op/s | Python op/s | Java/Python |
| --- | ---: | ---: | ---: |
| immutable.HashMapBench.access | 93,223,825.15 | 71,459.06 | 1,304.58 |
| immutable.HashMapBench.build | 2,825.10 | 5.53 | 510.87 |
| immutable.HashMapBench.mutate | 14,515,804.76 | 32,573.73 | 445.63 |
| immutable.HashMapBench.transform | 2,846.72 | 5.20 | 547.45 |
| immutable.HashSetBench.access | 102,677,726.11 | 76,554.18 | 1,341.24 |
| immutable.HashSetBench.build | 3,618.08 | 5.89 | 614.28 |
| immutable.HashSetBench.mutate | 16,728,149.49 | 33,325.13 | 501.97 |
| immutable.HashSetBench.transform | 3,191.73 | 5.38 | 593.26 |
| immutable.LazyListBench.access | 132,497.65 | 21.30 | 6,220.55 |
| immutable.LazyListBench.build | 217,416,660.69 | 20,818.17 | 10,443.60 |
| immutable.LazyListBench.mutate | 248,515,363.74 | 248,606.54 | 999.63 |
| immutable.LazyListBench.transform | 9,742.67 | 2.37 | 4,110.83 |
| immutable.ListBench.access | 266,118.71 | 89.73 | 2,965.77 |
| immutable.ListBench.build | 76,427.19 | 60.82 | 1,256.61 |
| immutable.ListBench.mutate | 397,078,693.69 | 273,619.81 | 1,451.21 |
| immutable.ListBench.transform | 69,965.05 | 56.91 | 1,229.40 |
| immutable.TreeMapBench.access | 65,666,312.45 | 13,518.32 | 4,857.58 |
| immutable.TreeMapBench.build | 1,877.57 | 4.33 | 433.62 |
| immutable.TreeMapBench.mutate | 16,039,910.62 | 24,524.42 | 654.04 |
| immutable.TreeMapBench.transform | 2,684.08 | 7.47 | 359.31 |
| immutable.VectorBench.access | 409,475,419.38 | 565,181.86 | 724.50 |
| immutable.VectorBench.build | 108,958.87 | 502.11 | 217.00 |
| immutable.VectorBench.mutate | 50,562,093.39 | 35,306.15 | 1,432.10 |
| immutable.VectorBench.transform | 109,881.95 | 458.23 | 239.80 |
| mutable.ArrayBufferBench.access | 793,211,970.29 | 265,815.73 | 2,984.07 |
| mutable.ArrayBufferBench.build | 153,563.68 | 307.52 | 499.36 |
| mutable.ArrayBufferBench.mutate | 343,119,914.54 | 232,896.37 | 1,473.27 |
| mutable.ArrayBufferBench.transform | 75,493.67 | 103.18 | 731.67 |
| mutable.ArrayDequeBench.access | 770,411,180.67 | 216,639.23 | 3,556.19 |
| mutable.ArrayDequeBench.build | 135,974.77 | 135.61 | 1,002.69 |
| mutable.ArrayDequeBench.mutate | 350,451,932.56 | 170,181.06 | 2,059.29 |
| mutable.ArrayDequeBench.transform | 57,693.86 | 67.15 | 859.18 |
| mutable.HashMapBench.access | 241,405,421.54 | 49,762.16 | 4,851.18 |
| mutable.HashMapBench.build | 34,998.35 | 24.90 | 1,405.56 |
| mutable.HashMapBench.mutate | 166,413,223.14 | 32,155.69 | 5,175.23 |
| mutable.HashMapBench.transform | 23,510.30 | 18.35 | 1,281.22 |
| mutable.HashSetBench.access | 223,112,663.10 | 74,698.99 | 2,986.82 |
| mutable.HashSetBench.build | 37,313.19 | 24.77 | 1,506.39 |
| mutable.HashSetBench.mutate | 325,920,042.98 | 118,347.44 | 2,753.93 |
| mutable.HashSetBench.transform | 28,318.31 | 20.27 | 1,397.06 |
| mutable.ListBufferBench.access | 686,606,714.36 | 886,164.40 | 774.81 |
| mutable.ListBufferBench.build | 80,608.50 | 24.24 | 3,325.43 |
| mutable.ListBufferBench.mutate | 217,295,126.99 | 168,967.13 | 1,286.02 |
| mutable.ListBufferBench.transform | 64,328.83 | 21.20 | 3,034.38 |
| mutable.StringBuilderBench.access | 797,629,005.42 | 809,342.32 | 985.53 |
| mutable.StringBuilderBench.build | 368,145.98 | 171.30 | 2,149.13 |
| mutable.StringBuilderBench.mutate | 656,687,072.41 | 824,602.95 | 796.37 |
| mutable.StringBuilderBench.transform | 6,173,427.05 | 477.54 | 12,927.56 |
