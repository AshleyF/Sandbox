// generate words containing only subset of characters (for Morse code practice)

open System.IO

let morse (letters : string) =
    "words.txt" // from /usr/share/dict
    |> File.ReadAllLines
    |> Seq.filter (fun w -> w.Length > 1)
    |> Seq.map (fun w -> w.ToUpper())
    |> Seq.distinct
    |> Seq.filter (Seq.forall (fun c -> letters.Contains(c)))
    |> Seq.sortBy(fun w -> w.Length)
    |> Seq.iter (printfn "%s")

morse "ETANOIS14"
