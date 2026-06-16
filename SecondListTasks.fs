// 39.1
let rmodd n = 
  let rec witheven = function
    | ([], _) -> []
    | (x1 :: x2, true) -> [x1] @ witheven (x2, false)
    | (x1 :: x2, false) -> [] @ witheven (x2, true)
  witheven (n, false)
  

// 39.2
let rec del_even = function
  | x1 :: x2 when x1 % 2 = 0 -> [] @ del_even x2
  | x1 :: x2 -> [x1] @ del_even x2
  | _ -> []

// 39.3
let rec multiplicity x xs = 
  match xs with
  | x1 :: x2 when x1 <> x -> 0 + multiplicity x x2
  | x1 :: x2 when x1 = x -> 1 + multiplicity x x2
  | _ -> 0


// 39.4
let  split n =
  let rec witheven = function
    | ([], _) -> []
    | (x1 :: x2, true) -> [x1] @ witheven (x2, false)
    | (x1 :: x2, false) -> [] @ witheven (x2, true)
  (witheven (n, true), witheven (n, false)) 


exception NotEqualsLists
// 39.5
let rec zip (xs1,xs2) = 
  match (xs1, xs2) with
  | (x1::x2,x3::x4) -> (x1,x3) :: zip (x2,x4)
  | ([],[]) -> []
  | _ -> raise NotEqualsLists