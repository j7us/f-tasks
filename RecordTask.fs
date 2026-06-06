type TimeOfDay = { hours: int; minutes: int; f: string }

let (.>.) x y = (x.f = "PM" && y.f = "AM") || (x.f = y.f && (x.hours > y.hours || (x.hours = y.hours && x.minutes > y.minutes)))

let first = {hours = 1; minutes = 30; f = "AM"} 
let second = {hours = 1; minutes = 10; f = "PM"} 

printfn "%b" (first .>. second)

