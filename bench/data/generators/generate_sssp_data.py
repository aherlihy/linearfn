#!/usr/bin/env python3
"""
Generate Single-Source Shortest Path benchmark data.
Creates a 100x100 grid graph with random edge costs.
Target: ~23,067 edges, ~2 seconds execution time.
"""

import os
import sys
import random

def generate_sssp_data(output_dir):
    os.makedirs(output_dir, exist_ok=True)

    # Set random seed for reproducibility
    random.seed(42)

    # Generate base.facts (starting node)
    with open(os.path.join(output_dir, 'base.facts'), 'w') as f:
        f.write("0\t0\n")

    # Generate edge.facts (100x100 grid)
    with open(os.path.join(output_dir, 'edge.facts'), 'w') as f:
        rows, cols = 100, 100
        total_edges = 0

        for r in range(rows):
            for c in range(cols):
                node_id = r * cols + c

                # Right edge
                if c < cols - 1:
                    cost = random.randint(1, 10)
                    f.write(f"{node_id}\t{node_id + 1}\t{cost}\n")
                    total_edges += 1

                # Down edge
                if r < rows - 1:
                    cost = random.randint(1, 10)
                    f.write(f"{node_id}\t{node_id + cols}\t{cost}\n")
                    total_edges += 1

                # Diagonal edge (down-right)
                if r < rows - 1 and c < cols - 1:
                    cost = random.randint(1, 10)
                    f.write(f"{node_id}\t{node_id + cols + 1}\t{cost}\n")
                    total_edges += 1

        print(f"Generated SSSP data with {total_edges} edges")
        print(f"Output: {output_dir}/base.facts, {output_dir}/edge.facts")

if __name__ == "__main__":
    # Default to bench/data/files/sssp-facts relative to script location
    script_dir = os.path.dirname(os.path.abspath(__file__))
    default_dir = os.path.join(script_dir, "..", "files", "sssp-facts")
    output_dir = sys.argv[1] if len(sys.argv) > 1 else default_dir
    generate_sssp_data(output_dir)
