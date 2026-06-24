// 41.4.1
let list_filter f xs: 'a list = List.foldBack (fun a b -> if f a then a :: b else b ) xs []

// 41.4.2
let sum (p, xs) = List.fold (fun a b -> if p b then a + b else a ) 0 xs

let srev l = List.fold (fun a b -> b :: a) [] l
// 41.4.3
let revrev l = List.fold (fun a b -> (srev b) :: a) [] l
