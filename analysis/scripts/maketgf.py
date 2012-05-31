#!/usr/bin/python
import re
import sys

## Usage: cat nodes.txt | ./maketgf.py

def main():
    input_text = sys.stdin.read()
    stripped_names = [x.strip() for x in re.split("\n|->", input_text)]
    nodes = sorted([x for x in set(stripped_names) if x])
    node_id = dict(zip(nodes, [x+1 for x in range(len(nodes))]))
    edges = [x.strip().split(" -> ") for x in re.split("\n", input_text)
             if x.strip()]
    for node in nodes:
        print node_id[node], node
    print "#"
    for (from_node, to_node) in edges:
        print node_id[from_node], node_id[to_node]

main()
