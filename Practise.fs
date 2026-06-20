// 40.1
let rec sum (p, xs) = 
    match (p,xs) with
    | (p, []) -> 0
    | (p,head :: tail) when p head -> head + sum (p,tail)
    | (p, head :: tail) -> sum (p,tail)

// 40.2.1
let rec count (xs, n: int) = 
    match (xs, n) with
    | ([], n) -> 0
    | (head :: tail, n) when head > n -> 0
    | (head :: tail, n) when head < n -> count (tail, n)
    | (head :: tail, n) -> 1 + count (tail, n)

// 40.2.2
let rec insert (xs, n: int) = 
    match (xs, n) with
    | ([], n) -> [n]
    | (head :: tail, n) when n > head -> head :: insert (tail, n)
    | (head :: tail, n) when n = head -> head :: n :: tail
    | (head :: tail, n) when n < head -> n :: head :: tail

// 40.2.3
let rec intersect (xs1: int list, xs2) = 
    match (xs1, xs2) with
    | ([], t) -> []
    | (t, []) -> []
    | (head :: tail, head2 :: tail2) when head = head2 -> head :: intersect (tail, tail2)
    | (head :: tail, head2 :: tail2) when head < head2 -> intersect (tail, head2 :: tail2)
    | (head :: tail, head2 :: tail2) when head > head2 -> intersect (head :: tail, tail2)

// 40.2.4
let rec plus (xs1: int list, xs2) =
    match (xs1, xs2) with
    | ([], t) -> t
    | (t, []) -> t
    | (head :: tail, head2 :: tail2) when head = head2 -> head :: head2 :: plus(tail, tail2)
    | (head :: tail, head2 :: tail2) when head < head2 -> head :: plus (tail, head2 :: tail2)
    | (head :: tail, head2 :: tail2) when head > head2 ->head2 :: plus (head :: tail, tail2)

// 40.2.5
let rec minus (xs1: int list, xs2) = 
    match (xs1, xs2) with
    | ([], _) -> []
    | (t, []) -> t
    | (head :: tail, head2 :: tail2) when head = head2 -> minus (tail, tail2)
    | (head :: tail, head2 :: tail2) when head < head2 -> head :: minus (tail, head2 :: tail2)
    | (head :: tail, _ :: tail2) -> minus (head :: tail, tail2)

// 40.3.1
let smallest (l: int list) : int option = 
    let rec findSmal = function
        | ([], m: int) -> m
        | (head :: tail, m) when head < m -> findSmal (tail, head)
        | (_ :: tail, m) -> findSmal (tail, m)

    match l with
    | [] -> None
    | head :: tail -> Some (findSmal (tail, head))
    


// 40.3.2
let rec delete (n: int, xs) = 
    match (n, xs) with
    | (n, []) -> []
    | (n, head :: tail) when n = head -> tail
    | (n, head :: tail) -> head :: delete(n, tail)

// 40.3.3
let rec sort xs =
    match smallest xs with
    | None -> []
    | Some m -> m :: sort (delete (m, xs))

// 40.4
let rec revrev (l: 'a list list) : 'a list list= 
    match l with
    | [] -> []
    | head :: tail -> revrev tail @ [List.rev head]
    