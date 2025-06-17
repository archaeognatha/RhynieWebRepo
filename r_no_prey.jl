function r_no_prey(M,gamma)
    if M == 0
        r = 0
        else
        rand_p = rand()
        r = round(-1 * (M^(1-(1/gamma))) * log(rand_p))
        if r > M
            r = M
        end
        if r == 0
            r = 1
        end
    end
    return r
end
