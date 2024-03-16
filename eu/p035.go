package main

import (
    "fmt"
    "math"
)

func digits(i int) int {
    return int(math.Floor(math.Log10(float64(i)))) + 1
}

func rotate(i int) int {
    m := i % 10
    return (m * int(math.Pow(10, float64(digits(i) - 1)))) + (i / 10)
}

func main() {
    prime := [1000001]bool{}
    for i := 0; i < 1000001; i++ {
        prime[i] = true
    }
    for i := 2; i < 1000000; i++ {
        if !prime[i] {
            continue
        }
        for j := i*2; j < 1000000; j += i {
            prime[j] = false
        }
    }

    ncp := 0
    for p := 2; p < 1000000; p++ {
        if !prime[p] {
            continue
        }
        q := p
        is_circular_prime := true
        for d := 0; d < digits(q); d++ {
            if q % 10 == 0 {
                is_circular_prime = false
                break
            }
            q = rotate(q)
            is_circular_prime = is_circular_prime && prime[q]
        }
        if is_circular_prime {
            ncp++
        }
    }
    fmt.Println(ncp)
}
