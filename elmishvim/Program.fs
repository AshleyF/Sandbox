open System
open Keyboard

type Mode = Normal | NormalPending of Key | Insert

type Token = Char of char | Tab | Return

type Direction = Forward | Backward

type Model = {
    Beep   : bool
    Mode   : Mode
    Find   : (Direction * char) option
    Before : Token list
    After  : Token list }

type Input = {
    Key: Key
    Modifier: Modifier option }

let init () = {
    Beep = false
    Mode = Normal
    Find = None
    Before = [] 
    After = [] }

let tokenToChar = function
    | Char c -> c
    | Tab -> '\t'
    | Return -> '\n'

let view model =
    if model.Beep then Console.Beep()
    let toString = Seq.map tokenToChar >> String.Concat
    Console.Clear()
    model.Before |> Seq.rev |> toString |> Console.Write
    let x, y = Console.CursorLeft, Console.CursorTop
    model.After |> toString |> Console.Write
    Console.WriteLine()
    match model.Mode with
    | Normal -> printf "NORMAL"
    | NormalPending key -> printf $"NORMAL (pending {keyToString key})"
    | Insert -> printf "INSERT"
    printfn "Model: %A" model
    Console.CursorLeft <- x
    Console.CursorTop <- y

let update model input =
    let model = { model with Beep = false }
    let scan predicate direction terminate line upto including model =
        let rec scan' count model =
            let forward = direction = Forward
            match if forward then model.After else model.Before with
            | h :: tail ->
                if line && h = Return then
                    if terminate then None else Some (count, model)
                else
                    let model' =
                        if forward
                        then { model with Before = h :: model.Before; After = tail }
                        else { model with Before = tail; After = h :: model.After }
                    if predicate h && (count <> 0 || including)
                    then Some (count, if upto then model else model')
                    else scan' (count + 1) model'
            | [] -> if terminate then None else Some (count, model)
        scan' 0 model
    let rec left n beep model =
        if n = 0 then model else
        match model.Before with
        | Return :: _ // cannot move past start of line
        | [] -> { model with Beep = beep }
        | c :: before -> left (n - 1) beep { model with Before = before; After = c :: model.After }
    let rec right n beep model =
        if n = 0 then model else
        match model.After with
        | _ :: [] // cannot move past last token of buffer
        | _ :: Return :: _  // cannot move past end of line (note: second to last)
        | [] -> { model with Beep = beep }
        | c :: after -> right (n - 1) beep { model with After = after; Before = c :: model.Before }
    match model.Mode with
    | Normal ->
        match input.Key with
        | Key.I -> { model with Mode = Insert } // insert
        | Key.H | Key.Backspace | Key.Left -> left 1 true model // left
        | Key.L | Key.Space | Key.Right -> right 1 true model // right
        | Key.K | Key.Up ->
            match scan ((=) Return) Backward true false false true model with
            | Some (count, model) -> // end of previous line
                // scan predicate direction terminate line upto including model =
                match scan ((=) Return) Backward false false false false model with
                | Some (_, model) -> right count false model // attempt to alighn
                | None -> failwith "Should never happen with scan terminate=false"
            | None -> { model with Beep = true }
        | Key.J | Key.Down -> model // down TODO
        | Key.F -> { model with Mode = NormalPending Key.F }
        | Key.CapF -> { model with Mode = NormalPending Key.CapF }
        | Key.T -> { model with Mode = NormalPending Key.T }
        | Key.CapT -> { model with Mode = NormalPending Key.CapT }
        | _ -> Console.WriteLine($"UNHANDLED: Normal {input.Key}"); model
    | NormalPending key ->
        let normal = { model with Mode = Normal }
        let find direction upto =
            match keyToChar input.Key with
            | Some c ->
                match scan ((=) (Char c)) direction true true upto false normal with
                | Some (_, normal) -> if direction = Forward then left 1 false normal else normal
                | None -> { normal with Beep = true }
            | None -> { normal with Beep = true }
        if input.Key = Key.Esc then normal else
        match key with
        | Key.F    -> find Forward  false
        | Key.CapF -> find Backward false
        | Key.T    -> find Forward  true
        | Key.CapT -> find Backward true
        | _ -> failwith "UNHANDLED: NormalPending {key}"
    | Insert ->
        match input.Key with
        | Key.Esc -> { left 1 false model with Mode = Normal }
        | Key.Return -> { model with Before = Return :: model.Before }
        | k ->
            match keyToChar k with
            | Some c -> { model with Before = Char c :: model.Before }
            | None -> model
        | _ -> Console.WriteLine($"UNHANDLED: Insert {input.Key}"); model

keys ()
|> Seq.map (fun (k, m) -> { Key = k; Modifier = m })
|> Seq.scan update (init ())
|> Seq.iter view