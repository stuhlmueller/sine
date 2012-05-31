#!/usr/bin/python
import re

# stripped_names = [x.strip() for x in re.split("\n|->", input_text_2)]
# nodes = sorted([x for x in set(stripped_names) if x])
# node_id = dict(zip(nodes, [x+1 for x in range(len(nodes))]))

# edges = [x.strip().split(" -> ") for x in re.split("\n", input_text_2) if x.strip()]

# for node in nodes:
#     print node_id[node], node
# print "#"
# for (from_node, to_node) in edges:
#     print node_id[from_node], node_id[to_node]

input_text = """
(= rootter77 sum-188ter77)
(= rootter78 sum-188ter78)
(= product-409ter77 (+ sum-188ter77 product-212ter125))
(= sum-188ter77 (logsumexp product-209ter127 product-409ter77))
(= product-409ter78 (+ sum-188ter78 product-212ter125))
(= product-212ter125 (+ -0.6931471805599453 -0.6931471805599453))
(= product-212ter126 (+ -0.6931471805599453 -0.6931471805599453))
(= sum-224ter127 (logsumexp -0.6931471805599453 -0.6931471805599453))
(= product-209ter127 (+ sum-224ter127 -0.6931471805599453))
(= sum-188ter78 (logsumexp product-212ter126 product-409ter78))
"""

input_text_2 = """
(= rootter91 sum-240ter91)
(= product-258ter91 (+ sum-240ter91 -0.6931471805599453))
(= sum-240ter91 (logsumexp product-258ter91 -0.6931471805599453))
"""

special = ["+", "logsumexp"]
nodes = sorted(set([x.strip() for x in re.split("\n|\s|\)|\(|=", input_text_2) if x.strip()]))
node_id = dict(zip(nodes, [x+1 for x in range(len(nodes))]))

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

for node in nodes:
    if node not in special:
        print node_id[node], node_name(node)
print "#"

for (node, operator, targets) in [parseline(x.strip()) for x in re.split("\n", input_text_2) if x.strip()]:
    for target in targets:
        print node_id[node], node_id[target] #, operator
    # print (node, operator, targets)
 
#. 2 -0.6931471805599453
#. 4 product-209ter127
#. 5 product-212ter125
#. 6 product-212ter126
#. 7 product-409ter77
#. 8 product-409ter78
#. 9 rootter77
#. 10 rootter78
#. 11 sum-188ter77
#. 12 sum-188ter78
#. 13 sum-224ter127
#. #
#. 9 11 
#. 10 12 
#. 7 11 +
#. 7 5 +
#. 11 4 logsumexp
#. 11 7 logsumexp
#. 8 12 +
#. 8 5 +
#. 5 2 +
#. 5 2 +
#. 6 2 +
#. 6 2 +
#. 13 2 logsumexp
#. 13 2 logsumexp
#. 4 13 +
#. 4 2 +
#. 12 6 logsumexp
#. 12 8 logsumexp
#. 
