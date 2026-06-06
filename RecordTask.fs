type TimeOfDay = { hours: int; minutes: int; f: string }

let (.>.) x y = (x.f = "PM" && y.f = "AM") || (x.f = y.f && (x.hours > y.hours || (x.hours = y.hours && x.minutes > y.minutes)))
