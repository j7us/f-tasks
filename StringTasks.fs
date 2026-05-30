// 17.1
let rec pow = function
 | (s,0) -> ""
 | (s, n) -> s + pow(s,n-1) 

// 17.2
let rec isIthChar(s: string,n,c) = s.[n] = c

let calc = function
 | (s:string,n,c) when s.[n] = c -> 1
 | (s,n,c) -> 0

// 17.3
let rec occFromIth = function
 | (s,n,c) when n >= String.length s -> 0
 | (s,n,c) -> calc(s,n,c) + occFromIth(s, (n+1), c)