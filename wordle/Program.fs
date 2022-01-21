// solver for both:
// - Wordle (https://wordlegame.org/)
// - Primel (https://converged.yt/primel/)

open System
open System.IO

// helper function-generating functions for building the constraints list
let exclude letters = Seq.forall (fun c -> not (Seq.contains c letters)) // not containing given letters
let green letter index word = Array.get word index = letter // contains letter at index 
let yellow letter index word = Seq.contains letter word && Array.get word index <> letter // contains letter but *not* at index

// list of constraint predicate functions which must *all* return true
(*
let constraints = [ // Wordle
    // 1st guess: AROSE (highest ranked of 5883 potential solutions)
    exclude "AOS"
    green 'R' 1
    yellow 'E' 4
    // 2st guess: CREED (of 54 potential solutions)
    exclude "D"
    yellow 'C' 0
    green 'E' 2
    ] // -> must be WRECK or ERECT
*)

let constraints = [ // Primel
    // 1st guess: 12379
    exclude "127"
    yellow '3' 2
    green '9' 4
    // 2nd guess: 34589
    exclude "4"
    yellow '3' 0
    yellow '5' 2
    yellow '8' 3
    // 3rd guess: 50839
    exclude "0"
    yellow '5' 0
    green '8' 2
    green '3' 3
    ] // -> must be 65839

// dictionary words (for Wordle)
let words =
    File.ReadAllLines "words.txt" // from /usr/share/dict
    |> Seq.map (fun w -> w.ToUpper()) // normalize case
    |> Seq.distinct // remove duplicates
    |> Seq.filter (fun w -> w.Length = 5) // 5-letter words only
    |> Seq.filter (Seq.forall Char.IsLetter) // including only letters

// 5-digit primes (for Primel)
let primes =
    Seq.fold (fun p n -> List.filter (fun i -> i % n <> 0) p) [10007..99999] [2..317]
    |> Seq.map (fun n -> n.ToString())

// candidate solutions from the dictionary
let candidates = 
    // words // Wordle
    primes // Primel
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

