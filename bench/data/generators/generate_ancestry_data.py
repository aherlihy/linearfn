#!/usr/bin/env python3
"""
Generate Ancestry benchmark data.
Creates 300 separate binary trees, each with depth 13.
Target: ~4.9M parent-child relationships, ~1.5 seconds execution time.
"""

import os
import sys

def generate_ancestry_data(output_dir):
    os.makedirs(output_dir, exist_ok=True)

    with open(os.path.join(output_dir, 'parents.facts'), 'w') as f:
        total_edges = 0

        # Create 300 separate trees, each with depth 13
        for tree_num in range(300):
            offset = tree_num * 20000
            max_generations = 13

            for gen in range(max_generations):
                gen_start = 2**gen + offset
                gen_end = 2**(gen+1) + offset
                for parent in range(gen_start, gen_end):
                    child1 = (parent - offset) * 2 + offset
                    child2 = (parent - offset) * 2 + 1 + offset
                    f.write(f"{parent}\t{child1}\n")
                    f.write(f"{parent}\t{child2}\n")
                    total_edges += 2

        print(f"Generated Ancestry data with {total_edges} parent-child relationships")
        print(f"Output: {output_dir}/parents.facts")

if __name__ == "__main__":
    # Default to bench/data/files/ancestry-facts relative to script location
    script_dir = os.path.dirname(os.path.abspath(__file__))
    default_dir = os.path.join(script_dir, "..", "files", "ancestry-facts")
    output_dir = sys.argv[1] if len(sys.argv) > 1 else default_dir
    generate_ancestry_data(output_dir)
