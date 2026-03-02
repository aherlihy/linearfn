# Benchmark Data

This directory contains data generation scripts for the RestrictedFn benchmarks.

## Quick Start

To generate all benchmark data and run benchmarks:

```bash
# 1. Generate benchmark data (takes ~10-20 seconds)
./bench/data/setup.sh

# 2. Run Souffle execution benchmarks
sbt "bench/Jmh/run bench.SouffleExecutionBenchmark"

# 3. Run Datalog generation benchmarks
sbt "bench/Jmh/run bench.DatalogGenerationBenchmark"
```

## Generated Data

The setup script generates three benchmark datasets in `bench/data/files/`:

### Transitive Closure (TC)
- **Location**: `bench/data/files/tc-facts/edge.facts`
- **Size**: ~41 KB (4,431 edges)
- **Structure**: Chains with branches and cross-edges
- **Expected execution time**: ~6-7 seconds

### Single-Source Shortest Path (SSSP)
- **Location**: `bench/data/files/sssp-facts/base.facts` and `bench/data/files/sssp-facts/edge.facts`
- **Size**: ~344 KB (29,601 edges)
- **Structure**: 100×100 grid graph with random edge costs
- **Expected execution time**: ~2-3 seconds

### Ancestry
- **Location**: `bench/data/files/ancestry-facts/parents.facts`
- **Size**: ~73 MB (4,914,600 parent-child relationships)
- **Structure**: 300 binary trees, each with depth 13
- **Expected execution time**: ~1.5 seconds

## Manual Generation

You can also generate individual datasets:

```bash
# Generate TC data (defaults to bench/data/files/tc-facts)
python3 bench/data/generators/generate_tc_data.py

# Generate SSSP data (defaults to bench/data/files/sssp-facts)
python3 bench/data/generators/generate_sssp_data.py

# Generate Ancestry data (defaults to bench/data/files/ancestry-facts)
python3 bench/data/generators/generate_ancestry_data.py

# Or specify custom output directories:
python3 bench/data/generators/generate_tc_data.py /custom/path/tc-facts
python3 bench/data/generators/generate_sssp_data.py /custom/path/sssp-facts
python3 bench/data/generators/generate_ancestry_data.py /custom/path/ancestry-facts
```

## Requirements

- Python 3.x
- Souffle (for running execution benchmarks)

## Why Not Commit Data Files?

The Ancestry dataset is 73 MB, which is too large for Git. Instead, we commit the generation scripts and generate the data on-demand. The TC and SSSP datasets are small but we keep everything consistent by generating all data using the setup script.

## Benchmark Results

With the generated data, you should see benchmark times similar to:

| Benchmark | Restricted | Unrestricted | Difference |
|-----------|-----------|--------------|------------|
| Ancestry | 1484 ms | 1490 ms | +0.4% |
| SSSP | 2207 ms | 2211 ms | +0.2% |
| TC | 6660 ms | 6816 ms | +2.3% |

These results demonstrate that the restricted (type-safe) implementation has virtually no performance overhead compared to the unrestricted version.
