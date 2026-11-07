"""09_gephi_gexf.py: graphml (from 07_citespace_like.R) -> GEXF for Gephi.
Lower-case labels, integer cluster, frequency-based size, Wong colours for the 8 largest clusters, grey for the rest."""
import math
import networkx as nx

wong = ["#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00", "#F0E442", "#0072B2", "#000000"]
other = "#BBBBBB"   # clusters beyond the 8 largest
g = nx.read_graphml("gephi/keyword_network.graphml")
h = nx.Graph()
for n, d in g.nodes(data=True):
    c = int(d["cluster"]); f = int(d["freq"])
    r, gg, b = (int((wong[c - 1] if c <= len(wong) else other)[i:i + 2], 16) for i in (1, 3, 5))
    h.add_node(d["name"], label=d["name"].lower(), cluster=f"C{c}", freq=f,
               viz={"color": {"r": r, "g": gg, "b": b, "a": 1.0}, "size": 4 + 3 * math.sqrt(f / 10)})
for u, v, d in g.edges(data=True):
    h.add_edge(g.nodes[u]["name"], g.nodes[v]["name"], weight=float(d["weight"]))
nx.write_gexf(h, "gephi/keyword_network.gexf")
print(h.number_of_nodes(), "nodes,", h.number_of_edges(), "edges")
