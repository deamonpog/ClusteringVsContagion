import numpy as np
import networkx as nx

def get_neighbors(in_node, in_edges_df):
    setA = set( in_edges_df[in_edges_df["Target"] == in_node]["Source"].unique() )
    setA.update( in_edges_df[in_edges_df["Source"] == in_node]["Target"].unique() )
    return setA

def pick_ego_network(in_root_node, in_edges_df, in_limit_nodes):
    queue = [in_root_node]
    net_nodes = set([in_root_node])
    while len(queue) > 0 and len(net_nodes) < in_limit_nodes:
        node = queue.pop(0)
        net_nodes.add(node)
        nbrs = get_neighbors(node, in_edges_df)
        for nbr in nbrs:
            if nbr not in net_nodes:
                queue.append(nbr)
    net_edges = in_edges_df[in_edges_df["Source"].isin(net_nodes) & in_edges_df["Target"].isin(net_nodes)]
    return net_nodes, net_edges
    
def sample_network(in_root_node, in_edges_df, in_num_nodes):
    nodes, edges = pick_ego_network(in_root_node, in_edges_df, in_num_nodes)
    g = nx.from_edgelist(edges.values)
    # nx.draw(g)
    c = nx.clustering(g)
    ccdist = np.array([c[node] for node in c])
    return (in_root_node, ccdist.mean(), ccdist.std(), nodes, g)