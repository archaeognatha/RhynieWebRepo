function SLN_maker(A_matrix,P_matrix,no_guilds,no_species)
    A = A_matrix
    P = P_matrix
    meta_SLN = Array{Int64}(undef,no_guilds,no_species)
    for i = 1:no_guilds
		tally2 = 1
		for j = 1:no_guilds
	    	for k = 1:P[j,:G]  # loop through each species in guild j            
	        meta_SLN[i,tally2] = A[i,j] 
			tally2+=1
	    	end
		end
    end
    return meta_SLN
end
