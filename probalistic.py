import urllib.request


urllib.request.urlretrieve('https://github.com/MiyainNYC/Graph-Thinking/raw/master/worker_G.graphml','worker_G.graphml')

from graph_tool.all import *

g = load_graph('worker_G.graphml')

state = minimize_blockmodel_dl(g, deg_corr=False)
state.entropy()
# entropy: 68796.50778086044
# groups : 52



blocks = state.get_blocks()
# entropy: 67503.250630552488
# groups : 61

block_list = []
for v in g.vertices():
    block_list.append(blocks[v])
    
from collections import Counter
Counter(block_list)

state = minimize_blockmodel_dl(g, deg_corr=True)


state = minimize_nested_blockmodel_dl(g, deg_corr=False)
state.entropy()
# entropy: 58349.392019931613
l: 0, N: 2621, B: 1
l: 1, N: 123, B: 50
l: 2, N: 50, B: 22
l: 3, N: 22, B: 11
l: 4, N: 11, B: 6
l: 5, N: 6, B: 2
l: 6, N: 2, B: 1
            
state = minimize_nested_blockmodel_dl(g, deg_corr=True)
state.entropy()
# entropy: 58447.470967214511
l: 0, N: 2621, B: 1
l: 1, N: 109, B: 39
l: 2, N: 39, B: 15
l: 3, N: 15, B: 6
l: 4, N: 6, B: 2
l: 5, N: 2, B: 1