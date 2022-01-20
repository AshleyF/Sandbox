open System
open System.IO

// helper function-generating functions for building the constraints list
let exclude letters = Seq.forall (fun c -> not (Seq.contains c letters)) // not containing given letters
let green letter index word = Array.get word index = letter // contains letter at index 
let yellow letter index word = Seq.contains letter word && Array.get word index <> letter // contains letter but *not* at index

// list of constraint predicate functions which must *all* return true
let constraints = [
    // 1st guess: AROSE (highest ranked of 5883 potential solutions)
    exclude "AOS"
    green 'R' 1
    yellow 'E' 4
    // 2st guess: CREED (of 54 potential solutions)
    exclude "D"
    yellow 'C' 0
    green 'E' 2
    ] // -> must be WRECK or ERECT

// candidate solutions from the dictionary
let candidates =
    File.ReadAllLines "words.txt" // from /usr/share/dict
    |> Seq.map (fun w -> w.ToUpper()) // normalize case
    |> Seq.distinct // remove duplicates
    |> Seq.filter (fun w -> w.Length = 5) // 5-letter words only
    |> Seq.filter (fun w -> Seq.forall Char.IsLetter w) // including only letters
    |> Seq.filter (fun w -> Seq.forall (fun c -> Seq.toArray w |> c) constraints) // matching *all* constraints

// count letter occurances across words
let frequency =
    let countLettersInWord =
        let increment = function Some i -> Some (i + 1) | None -> Some 1
        let countLetter tally' c = Map.change c increment tally'
        Seq.fold countLetter // fold over single word
    Seq.fold countLettersInWord Map.empty candidates // fold over all words

// rank word by letter frequency and number of distinct letters
let rank word =
    let distinct = Seq.distinct word |> Seq.length // num distinct letters
    let score = word |> Seq.map (fun c -> Map.find c frequency) |> Seq.sum // sum of letter frequencies
    score * distinct

// print ranked possible solutions
let solutions = candidates |> Seq.sortByDescending rank |> Seq.toArray
printfn "Solutions: %A (%i)" solutions solutions.Length

