open System.IO

let isStartPosition y = 
    y = 0 || y % 4 = 0

let calculate (input: array<int * int>) = 
    for (ele,index) in input do
        if isStartPosition index then
            match ele with
            | 1 ->  
                let firstTupleOne = input.[index + 1]
                let secondTupleOne = input.[index + 2]
                let thirdTupleOne = input.[index + 3]
                input.[fst thirdTupleOne] <- (fst input.[fst firstTupleOne] + fst input.[fst secondTupleOne], index)   
            | 2 ->  
                let firstTupleTwo = input.[index + 1]
                let secondTupleTwo = input.[index + 2]
                let thirdTupleTwo = input.[index + 3]
                input.[fst thirdTupleTwo] <- (fst input.[fst firstTupleTwo] * fst input.[fst secondTupleTwo], index) 
            | _ -> ()
    fst input.[0]


for i in 0..99 do
    for j in 0..99 do
        let mutable input = 
            File.ReadAllLines("day2/input2.txt")
            |> Seq.toArray
            |> Array.map int
            |> Array.mapi (fun i ele -> ele, i)
        input.[1] <- (i, 1)
        input.[2] <- (j, 2)
        let result = calculate input
        if result = 19690720 then
            printfn "%i" (100 * i + j)

                 
        