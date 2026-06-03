// 23.4.1
let (.+.) x y = 
    let sumMed ((a,b,c), (q,w,e)) = a*20*12 + q*20*12 + b*12 + w*12 + c + e
    let countFull allMed = (allMed / (20 * 12), (allMed % (20 * 12)) / 12, allMed % 12)
    countFull (sumMed (x,y))


let (.-.) x y = 
    let minusMed ((a,b,c), (q,w,e)) = a*20*12 - q*20*12 + b*12 - w*12 + c - e
    let countFull f =  (f / (20 * 12), (f % (20 * 12)) / 12, f % 12)
    countFull (minusMed (x,y))

// 23.4.2
let (.+) (x: float * float) (y: float * float) = 
    let sum ((a,b), (c,d)) = (a+c, b+d)
    sum (x,y)

let (.-) (x: float * float) (y: float * float) = 
    let minus (a,b) = (a * (-1.0), b* (-1.0))
    x .+ (minus y)

let (.*) (x: float * float) (y: float * float) = 
    let sum ((a,b), (c,d)) = (a*c - b*d, b*c + a*d)
    sum (x,y)

let (./) (x: float * float) (y: float * float) = 
    let del (a,b) = (a / (a * a + b * b), -b / (a * a + b * b))
    x .* (del y)