# Defines recursive function to find depth
    #    'current_node': The species we are looking at
    #    'visited': A list of species already in this specific food chain

function get_max_chain_depth(node_id, matrix, visited, start_time, time_limit)
        
    # Find all prey for this node (Where matrix row has 1s)
    prey_list = findall(matrix[node_id, :] .== 1)
        
    # If no prey, this is the end of the line (Basal)
    if isempty(prey_list)
        return 1.0
    end
    
    max_prey_depth = 0.0
    
    for prey in prey_list
        # --- TIME CHECK ---
        # Check time inside the loop. 
        # If time runs out, we BREAK the loop but keep our current 'max_prey_depth'.
        if (time() - start_time) > time_limit
            break 
        end
        # ---------------------------

        # Only proceed if we haven't eaten this species in this chain yet
        if !(prey in visited)
            
            # Add prey to visited list for the next step
            push!(visited, prey)
            
            # Go deeper
            depth = get_max_chain_depth(prey, matrix, visited, start_time, time_limit)
            
            # Keep the largest depth found
            if depth > max_prey_depth
                max_prey_depth = depth
            end
            
            # Backtrack: Remove prey from visited so other paths can use it
            delete!(visited, prey)
        end
    end
    
    # My depth is 1 (me) + the deepest path below me
    return 1.0 + max_prey_depth
end

# Computes the longest simple chain terminating at each species.
# Returns a vector of length no_species
# maxtime = time limit for each species search, in seconds
function chain_depths(sp_A, no_species, SLN_ID; maxtime = 10)
    trophic_height = zeros(Float64, no_species)
    for i in 1:no_species
        # Capture the start time for this specific search
        t0 = time()
        trophic_height[i] = get_max_chain_depth(i, sp_A, Set([i]), t0, maxtime)
        if (time() - t0) > maxtime
            println("  Warning: search for species $i in $SLN_ID timed out. ",
                    "Result may be underestimated.")
        end
    end
    return trophic_height
end