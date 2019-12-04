open System.IO

let calculateMass mass = 
    mass / 3 - 2

let input = 
    File.ReadAllLines("day1/input.txt")
    |> Seq.toList
    |> List.map int
    |> List.sumBy calculateMass


//Second part:

//Pseudocode:
// let calculateNewMass mass = 
// fuelNeeded = mass / 3 - 2   //save fuelNeeded (accumulator)
// if (fuelneeded) <= 0 return totalFuelNeeded
// else calculateNewMass fuelneeded

let calculateNewMass mass = 
    let rec tailRec mass totalFuel = 
        match mass < 0 with
        | true -> totalFuel
        | false -> tailRec (mass / 3 - 2) totalFuel
    tailRec mass 0

let inputTwo = 
    File.ReadAllLines("day1/input.txt")
    |> Seq.toList
    |> List.map int
    |> List.sumBy calculateNewMass


 //Test with 89122

let test = calculateNewMass 14



let test2 = 29705 / 3 - 2 //9899
let test3 = 9899 / 3 - 2 //3297 etc