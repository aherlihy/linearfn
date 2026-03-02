#!/bin/bash
# Setup script for generating benchmark data
# This script generates all necessary data files for the Souffle execution benchmarks.

set -e  # Exit on error

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
GENERATORS_DIR="$SCRIPT_DIR/generators"
DATA_DIR="/tmp"

echo "=== Benchmark Data Setup ==="
echo "Generating benchmark data in $DATA_DIR"
echo ""

# Check if Python 3 is available
if ! command -v python3 &> /dev/null; then
    echo "Error: python3 is required but not found"
    exit 1
fi

# Make generators executable
chmod +x "$GENERATORS_DIR"/*.py

# Generate TC data
echo "Generating Transitive Closure data..."
python3 "$GENERATORS_DIR/generate_tc_data.py" "$DATA_DIR/tc-facts"
echo ""

# Generate SSSP data
echo "Generating Single-Source Shortest Path data..."
python3 "$GENERATORS_DIR/generate_sssp_data.py" "$DATA_DIR/sssp-facts"
echo ""

# Generate Ancestry data (this takes a moment)
echo "Generating Ancestry data (this may take 10-20 seconds)..."
python3 "$GENERATORS_DIR/generate_ancestry_data.py" "$DATA_DIR/ancestry-facts"
echo ""

echo "=== Setup Complete ==="
echo ""
echo "Data files created:"
echo "  - $DATA_DIR/tc-facts/edge.facts"
echo "  - $DATA_DIR/sssp-facts/base.facts"
echo "  - $DATA_DIR/sssp-facts/edge.facts"
echo "  - $DATA_DIR/ancestry-facts/parents.facts"
echo ""
echo "You can now run benchmarks with:"
echo "  sbt \"bench/Jmh/run bench.SouffleExecutionBenchmark\""
echo "  sbt \"bench/Jmh/run bench.DatalogGenerationBenchmark\""
