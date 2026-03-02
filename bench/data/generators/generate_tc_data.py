#!/usr/bin/env python3
"""
Generate Transitive Closure benchmark data.
Creates a medium-sized graph with chains and branches.
Target: ~4,431 edges, ~6-7 seconds execution time.
"""

import os
import sys

def generate_tc_data(output_dir):
    os.makedirs(output_dir, exist_ok=True)

    with open(os.path.join(output_dir, 'edge.facts'), 'w') as f:
        total_edges = 0

        # Create a few long chains
        for i in range(1, 2000):
            f.write(f"{i}\t{i+1}\n")
            total_edges += 1

        for i in range(5000, 7000):
            f.write(f"{i}\t{i+1}\n")
            total_edges += 1

        # Add branching paths
        for i in range(1, 1800, 10):
            f.write(f"{i}\t{i+20000}\n")
            f.write(f"{i+20000}\t{i+20001}\n")
            total_edges += 2

        # Add cross-edges between chains
        for i in range(100, 1900, 50):
            f.write(f"{i}\t{i+100}\n")
            f.write(f"{i}\t{i+5000}\n")
            total_edges += 2

        print(f"Generated TC data with {total_edges} edges")
        print(f"Output: {output_dir}/edge.facts")

if __name__ == "__main__":
    output_dir = sys.argv[1] if len(sys.argv) > 1 else "/tmp/tc-facts"
    generate_tc_data(output_dir)
