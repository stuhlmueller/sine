#!/usr/bin/python
import re
import sys

def parseline(line):
    elements = [x.strip(")") for x in re.split(" |\(", line[3:-1]) if x]
    first = elements[0]
    if len(elements) == 2:
        rest = [elements[1]]
        operator = ""
    else:
        rest = elements[2:]
        operator = elements[1]
    return [first, operator, rest]

def node_name(node):
    if node.startswith("sum"):
        return "+"
    elif node.startswith("product"):
        return "*"
    else:
        return node

def main():
    input_text = input_text = sys.stdin.read()
    special = ["+", "logsumexp"]
    nodes = sorted(set([x.strip() for x in re.split("\n|\s|\)|\(|=", input_text) if x.strip()]))
    node_id = dict(zip(nodes, [x+1 for x in range(len(nodes))]))
    for node in nodes:
        if node not in special:
            print node_id[node], node_name(node)
    print "#"
    xs = [parseline(x.strip()) for x in re.split("\n", input_text) if x.strip()]
    for (node, operator, targets) in xs:
        for target in targets:
            print node_id[node], node_id[target] #, operator

main()
