// 16.1
let notDivisible (m,n) = m % n = 0

let rec testPrime = function
 | (n,m) when m*m > n -> n % m <> 0
 | (n,m) when n % m = 0 -> n % m <> 0
 | (n,m) -> testPrime(n, m + 2)

// 16.2
let  prime n = (n > 1 && n < 4) || (n > 3 && n % 2 <> 0 && testPrime(n,3))


printfn "%b" (notDivisible(2,4))