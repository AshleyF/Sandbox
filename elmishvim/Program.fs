open System
open Keyboard

type Mode = Normal | NormalPending of Modifier option * Key | Insert

type Direction = Forward | Backward

type Model = {
    Beep   : bool
    Mode   : Mode
    Find   : (Direction * char) option
    Before : char list
    After  : char list }

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
    | NormalPending (modifier, key) -> printfn $"NORMAL (pending {keyToString (modifier, key)})"
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
    let isWordChar c = c = '_' || Char.IsLetterOrDigit(c)
    let isEsc (m, k) = k = Key.Esc || (m = Some Control && k = Key.LSquare)
    match model.Mode with
    | Normal ->
        match input with
        | None, Key.I -> { model with Mode = Insert } // insert
        | None, Key.H | None, Key.Backspace | None, Key.Left -> left true model // left
        | None, Key.L | None, Key.Space | None, Key.Right -> right true model // right
        | None, Key.D0 -> scan Backward (fun m -> m.Before.Length = 0) model |> expectSome // column zero
        | None, Key.Dollar -> scan Forward (fun m -> m.After.Length <= 1) model |> expectSome // end of line
        | None, Key.F -> { model with Mode = NormalPending (None, Key.F) } // find
        | Some Shift, Key.F -> { model with Mode = NormalPending (Some Shift, Key.F) } // find-reverse
        | None, Key.T -> { model with Mode = NormalPending (None, Key.T) } // to
        | Some Shift, Key.T -> { model with Mode = NormalPending (Some Shift, Key.T) } // to-reverse
        | None, Key.X ->
            let model = if model.After.Length > 0 then { model with After = model.After.Tail } else model // delete under cursor
            if model.After.Length = 0 then left false model else model // move left if off end
        | None, Key.W | Some Shift, Key.Right -> // word (note: spans lines)
            model
            |> scan Forward (fun m -> m.After.Length <= 1 || not (isWordChar m.After[0])) |> expectSome // first non-word char (or end)
            |> scan Forward (fun m -> m.After.Length <= 1 || isWordChar m.After[0]) |> expectSome // first word char following (or end)
        | None, Key.B | Some Shift, Key.Left -> // back word (note: spans lines)
            model
            |> left false
            |> scan Backward (fun m -> m.Before.Length = 0 || not (isWordChar m.Before[0])) |> expectSome // first non-word char (or end)
        | _ -> Console.WriteLine($"UNHANDLED: Normal {input}"); model
    | NormalPending (modifier, key) ->
        let normal = { model with Mode = Normal }
        let find direction upto =
            match keyToChar input with
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
        if isEsc input then normal else
        match (modifier, key) with
        | None,       Key.F -> find Forward  false
        | Some Shift, Key.F -> find Backward false
        | None,       Key.T -> find Forward  true
        | Some Shift, Key.T -> find Backward true
        | _ -> failwith $"UNHANDLED: NormalPending {input}"
    | Insert ->
        if isEsc input then { left false model with Mode = Normal } else
        match input with
        | k ->
            match keyToChar k with
            | Some c -> { model with Before = c :: model.Before }
            | None -> model

keys ()
|> Seq.scan update (init ())
|> Seq.iter view