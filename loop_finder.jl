using Graphs

"""
    nodes_in_loops(A::AbstractMatrix{<:Integer})

Return a boolean vector indicating which nodes are part of at least one
directed simple cycle in the adjacency matrix `A`.
Self-loops (A[i,i] = 1) are ignored.
"""
function nodes_in_loops(A::AbstractMatrix{<:Integer})
    n = size(A, 1)
    @assert size(A, 1) == size(A, 2) "Adjacency matrix must be square"

    # Remove self-loops (cannibalism)
    B = transpose(copy(A))
    for i in 1:n
        B[i,i] = 0
    end

    # Create directed graph
    G = DiGraph(B)

    # Find strongly connected components
    comps = strongly_connected_components(G)

    # Nodes in SCCs of size ≥ 2 are guaranteed to be in at least one cycle
    node_on_cycle = falses(n)
    for comp in comps
        if length(comp) > 1
            node_on_cycle[comp] .= true
        end
    end

    return node_on_cycle
end