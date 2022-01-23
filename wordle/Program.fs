// solver for Wordle and Primel

open System
open System.IO

// generate ranked possible solutions, given a set of constraints and initial list
let solver (words : string seq) constraints =
    let solutions constraints words =
        let candidates = // candidate solutions from the dictionary
            let apply w = Seq.forall (fun c -> Seq.toArray w |> c) constraints
            Seq.filter apply words // matching *all* constraints

        let frequency = // count letter occurances across words
            let countLettersInWord =
                let increment = function Some i -> Some (i + 1) | None -> Some 1
                let countLetter tally' c = Map.change c increment tally'
                Seq.fold countLetter // fold over single word
            Seq.fold countLettersInWord Map.empty candidates // fold over all words

        let rank word = // rank word by letter frequency and number of distinct letters
            let vowels = Seq.filter (fun (c : char) -> "AEIOU".Contains(c)) word |> Seq.length // number of vowels
            let distinct = Seq.distinct word |> Seq.length // num distinct letters
            let score = word |> Seq.map (fun c -> Map.find c frequency) |> Seq.sum // sum of letter frequencies
            score * (vowels + 1) * distinct * distinct

        // print ranked possible solutions
        let solutions = candidates |> Seq.sortByDescending rank |> Seq.toArray
        printfn "Solutions: %A (%i)" solutions solutions.Length

    words
    |> Seq.map (fun w -> w.ToUpper()) // normalize case
    |> Seq.distinct // remove duplicates
    |> Seq.filter (fun w -> w.Length = 5) // 5-letter/digit only
    |> solutions constraints

// helper function-generating functions for building the constraints list
let exclude letters = Seq.forall (fun c -> not (Seq.contains c letters)) // not containing given letters
let green letter index word = Array.get word index = letter // contains letter at index 
let yellow letter index word = Seq.contains letter word && Array.get word index <> letter // contains letter but *not* at index

// --------------------------------------------------------------------------------
// Wordle (https://wordlegame.org/)

let wordle =
    // "words.txt" // from /usr/share/dict
    "words-cheat.txt" // from source: https://wordlegame.org/assets/js/wordle/en.js
    |> File.ReadAllLines
    |> Seq.filter (Seq.forall Char.IsLetter) // including only letters
    |> solver

let wordleDemo () =
    [ // 1st guess: AROSE (highest ranked of 5883 potential solutions)
        exclude "AOS"
        green 'R' 1
        yellow 'E' 4
        // 2st guess: CREED (of 54 potential solutions)
        exclude "D"
        yellow 'C' 0
        green 'E' 2
    ] |> wordle// -> must be ERECT or WRECK

// --------------------------------------------------------------------------------
// Primel (https://converged.yt/primel/)

let primel =
    Seq.fold (fun p n -> List.filter (fun i -> i % n <> 0) p) [10007..99999] [2..317] // 5-digit primes
    |> Seq.map (fun n -> n.ToString())
    |> solver

let primelDemo () =
    [   // 1st guess: 12379
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
    ] |> primel// -> must be 65839

wordleDemo ()
// primelDemo ()
