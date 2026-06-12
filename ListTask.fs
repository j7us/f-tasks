// 34.1
let upto n = 
    let rec listCreator = function
      | x1 :: _ when x1 <= 0 -> []
      | x1 :: x2 when x1 = 1 -> x1 :: x2
      | x1 :: x2 -> listCreator ((x1 - 1) :: x1 :: x2)
    listCreator (n :: [])


// 34.2
let dnto n  =
  let rec listCreator = function
    | (x , a) when a > n -> x
    | (x, a) -> listCreator (a :: x, a + 1)
  if n < 1 then []
  else listCreator ([1], 2)

// 34.3
let evenn n = 
  let rec listEvCreator = function
    | (x, a) when a = 0 -> []
    | (x, a) -> x :: listEvCreator (x + 2, a - 1)
  if n > 0 then listEvCreator (0, n) else []

