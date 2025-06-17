function SLN_maker(A_matrix,P_matrix,no_guilds,no_species)
    A = A_matrix
    P = P_matrix
    meta_SLN = Array{Int64}(undef,no_guilds,no_species)
    for i = 1:no_guilds
	SLN_vector = Array{Int64}(undef,1,no_species)
	tally2 = 1
	for j = 1:no_guilds
	    for k = 1:P[j,:G]              
	        if A[i,j] == 0
	            meta_SLN[i,tally2] = 0
	        elseif A[i,j] == 1
	            meta_SLN[i,tally2] = 1
	        end
	        tally2+=1
	    end
	end
    end
    return meta_SLN
end
