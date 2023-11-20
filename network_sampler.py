import numpy as np
import networkx as nx

def get_neighbors(in_node, in_edges_df):
    setA = set( in_edges_df[in_edges_df["Target"] == in_node]["Source"].unique() )
    setA.update( in_edges_df[in_edges_df["Source"] == in_node]["Target"].unique() )
    return setA

def pick_DFS_ego_network(in_root_node, in_edges_df, in_limit_nodes):
    queue = [in_root_node]
    visited_nodes = set([in_root_node])
    while len(queue) > 0 and len(visited_nodes) < in_limit_nodes:
        node = queue.pop(0)
        visited_nodes.add(node)
        nbrs = get_neighbors(node, in_edges_df)
        for nbr in nbrs:
            if nbr not in visited_nodes:
                queue.append(nbr)
    net_edges = in_edges_df[in_edges_df["Source"].isin(visited_nodes) & in_edges_df["Target"].isin(visited_nodes)]
    return visited_nodes, net_edges

def pick_RNN_ego_network(in_root_node, in_edges_df, in_limit_nodes):
    queue = [in_root_node]
    visited_nodes = set([in_root_node])
    while len(queue) > 0 and len(visited_nodes) < in_limit_nodes:
        r = np.random.randint(0, len(queue))
        node = queue.pop(r)
        visited_nodes.add(node)
        nbrs = get_neighbors(node, in_edges_df)
        # new_nbrs = nbrs.difference(visited_nodes)
        # queue.extend(new_nbrs)
        # visited_nodes.update(new_nbrs)
        for nbr in nbrs:
            if nbr not in visited_nodes:
                queue.append(nbr)
                visited_nodes.add(nbr)
    net_edges = in_edges_df[in_edges_df["Source"].isin(visited_nodes) & in_edges_df["Target"].isin(visited_nodes)]
    return visited_nodes, net_edges
    
    
def sample_network(in_sampling_method, in_root_node, in_edges_df, in_num_nodes):
    if in_sampling_method == "DFS":
        nodes, edges = pick_DFS_ego_network(in_root_node, in_edges_df, in_num_nodes)
    elif in_sampling_method == "RNN":
        nodes, edges = pick_RNN_ego_network(in_root_node, in_edges_df, in_num_nodes)
    g = nx.from_edgelist(edges.values)
    # nx.draw(g)
    c = nx.clustering(g)
    ccdist = np.array([c[node] for node in c])
    return (in_root_node, ccdist.mean(), ccdist.std(), nodes, g)