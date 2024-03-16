MAX = 739397
primes = trues(MAX)
primes[1] = false
for i in 2:length(primes)
    if !primes[i]
        continue
    end
    for j in i*2:i:length(primes)
        primes[j] = false
    end
end

digits(x) = floor(Int, log10(x)) + 1
tr(x) = floor(Int, x / 10)
tl(x) = x % (10^(digits(x) - 1))

function is_tlr(x)
    tlr = true
    a = x
    while a > 10
        a = tl(a)
        tlr = tlr && primes[a]
    end
    a = x
    while a > 10
        a = tr(a)
        tlr = tlr && primes[a]
    end
    return tlr
end

function main()
    s = 0
    for i in 10:length(primes)
        if !primes[i]
            continue
        end
        if is_tlr(i)
            s += i
        end
    end
    println(s)
end

main()
