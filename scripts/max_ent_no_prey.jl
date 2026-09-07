function max_ent_no_prey(M)
    if M == 0
        k = 0
    else
        k_set = []
        # binomial total
        binom_denom = 0
        for i = 1:M
            binom_denom = binom_denom + binomial(BigInt(M),i)
        end
        # binomial fractions
        for i = 1:M
            k_temp = convert(Int64,ceil(M*binomial(BigInt(M),i)/binom_denom))
            push!(k_set,k_temp)
        end
        # generate sample space
        space = []
        for i = 1:M
            for j = 1:k_set[i]
                push!(space,i)
            end
        end
        shuffle!(space)
        k = space[1]
    end
    return k
end
        
