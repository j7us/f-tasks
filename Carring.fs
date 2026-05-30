// 20.3.1
let vat n x = x * (1.0 + float n / 100.0)

// 20.3.2
let unvat n x = x / (1.0 + float n / 100.0)

// 20.3.3
let min f = 
    let rec findMin = function
     | (f,n) when f n = 0 -> n
     | (f,n) -> findMin(f,n+1)
    findMin(f,1)
