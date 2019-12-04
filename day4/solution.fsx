//Input -> 171309-643603
//Rules
//    It is a six-digit number.
//    The value is within the range given in your puzzle input.
//    Two adjacent digits are the same (like 22 in 122345).
//    Going from left to right, the digits never decrease; they only ever increase or stay the same (like 111123 or 135679).


let i = [171309..643603]
let mutable j = 0


let turnToIntArray (x: string) = 
   let chars =  x.ToCharArray()
   chars
    |> Array.map (string >> int)
    |> Array.toList

let adjacentDigits (x: int list) =
    if  x.[0] = x.[1] || x.[1] = x.[2] || x.[2] = x.[3] || x.[3] = x.[4] || x.[4] = x.[5] then 1 else 0

let higherToRight (x: int list) = 
    if x.[0] <= x.[1] && x.[1] <= x.[2] && x.[2] <= x.[3] && x.[3] <= x.[4] && x.[4] <= x.[5] then 1 else 0


        
let list1 = 
    i 
    |> List.map (string >> turnToIntArray >> adjacentDigits) 

let list2 = 
    i
    |> List.map (string >> turnToIntArray >> higherToRight)

let pairLists list1 list2 = 
    List.zip list1 list2 

let tupleList = pairLists list1 list2

tupleList
|> List.filter (fun x -> fst x = 1 && snd x =1 )
|> List.sumBy (fun x -> if fst x = snd x then 1 else 0)

//Second part

let getIsolatedPair (x: int list) =
    if ((x.[0]=x.[1] && x.[1]<>x.[2]) ||
           (x.[1]=x.[2] && x.[1]<>x.[0] && x.[2]<>x.[3]) ||
           (x.[2]=x.[3] && x.[2]<>x.[1] && x.[3]<>x.[4]) ||
           (x.[3]=x.[4] && x.[3]<>x.[2]&& x.[4]<>x.[5]) ||
           (x.[4]=x.[5] && x.[4]<>x.[3])) then 1 else 0


let list11 = 
    i
    |> List.map (string >>turnToIntArray >>getIsolatedPair)


let tupleList2 = List.zip list11 list2

tupleList2
|> List.filter (fun x -> fst x = 1 && snd x =1 )
|> List.sumBy (fun x -> if fst x = snd x then 1 else 0)
