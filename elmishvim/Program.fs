open System
open Keyboard

type Mode = Normal | NormalPending of Key | Insert

type Direction = Forward | Backward

type Model = {
    Beep   : bool
    Mode   : Mode
    Find   : (Direction * char) option
    Before : char list
    After  : char list }

type Input = {
    Key: Key
    Modifier: Modifier option }

let init () = {
    Beep = false
    Mode = Normal
    Find = None
    Before = [] 
    After = [] }

let view model =
    if model.Beep then Console.Beep()
    Console.Clear()
    model.Before |> Seq.rev |> String.Concat |> Console.Write
    let x, y = Console.CursorLeft, Console.CursorTop
    model.After |> String.Concat |> Console.Write
    Console.WriteLine()
    Console.WriteLine()
    match model.Mode with
    | Normal -> printfn "NORMAL"
    | NormalPending key -> printfn $"NORMAL (pending {keyToString key})"
    | Insert -> printfn "INSERT"
    Console.WriteLine()
    printfn "Model: %A" model
    Console.CursorLeft <- x
    Console.CursorTop <- y

let update model input =
    let model = { model with Beep = false }
    let rec scan direction predicate model =
        let recurse = scan direction predicate
        if predicate model then Some model
        elif direction = Forward
        then match model.After with | h :: tail -> recurse { model with Before = h :: model.Before; After = tail } | [] -> None
        else match model.Before with | h :: tail -> recurse { model with Before = tail; After = h :: model.After } | [] -> None
    let expectSome = function Some x -> x | None -> failwith "Expected some value"
    let rec left beep model =
        match scan Backward (fun m -> m.Before.Length = model.Before.Length - 1) model with
        | Some model -> model
        | None -> { model with Beep = beep }
    let rec right beep model =
        match scan Forward (fun m -> m.After.Length > 0 (* under cursor *) && m.After.Length = model.After.Length - 1) model with
        | Some model -> model
        | None -> { model with Beep = beep }
    match model.Mode with
    | Normal ->
        match input.Key with
        | Key.I -> { model with Mode = Insert } // insert
        | Key.H | Key.Backspace | Key.Left -> left true model // left
        | Key.L | Key.Space | Key.Right -> right true model // right
        | Key.D0 -> scan Backward (fun m -> m.Before.Length = 0) model |> expectSome // column zero
        | Key.Dollar -> scan Forward (fun m -> m.After.Length <= 1) model |> expectSome // end of line
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
                let pred m =
                    match direction, upto with
                    | Forward,  false
                    | Backward, false -> m.After <> model.After && match m.After with h :: _ -> h = c | [] -> false
                    | Forward,  true  -> match m.After with _ :: h :: _ -> h = c | _ -> false
                    | Backward, true  -> match m.Before with h :: _ -> h = c | [] -> false
                match scan direction pred normal with
                | Some normal -> normal
                | None -> { normal with Beep = true }
            | None -> { normal with Beep = true }
        if input.Key = Key.Esc then normal else
        match key with
        | Key.F    -> find Forward  false
        | Key.CapF -> find Backward false
        | Key.T    -> find Forward  true
        | Key.CapT -> find Backward true
        | _ -> failwith $"UNHANDLED: NormalPending {key}"
    | Insert ->
        match input.Key with
        | Key.Esc -> { left false model with Mode = Normal }
        | k ->
            match keyToChar k with
            | Some c -> { model with Before = c :: model.Before }
            | None -> model

keys ()
|> Seq.map (fun (k, m) -> { Key = k; Modifier = m })
|> Seq.scan update (init ())
|> Seq.iter view