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
    After = ['N';'o';'w';' ';'t';'h';'e';' ';'t';'i';'m';'e';' ';'f';'o';'r';' ';'a';'l';'l';' ';'g';'o';'o';'d';' ';'m';'e';'n';' ';'t';'o';' ';'c';'o';'m';'e';'.';'.';'.'] }

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

let rec update model input =
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
    let play inputs model = inputs |> Seq.fold update model
    match model.Mode with
    | Normal ->
        match input with
        | None, Key.A -> // insert after
            match model.After with
            | b :: after -> { model with Before = b :: model.Before; After = after; Mode = Insert }
            | [] -> { model with Mode = Insert }
        | Some Shift, Key.A -> model |> play [(None, Key.Dollar); (None, Key.A)] // insert after line ($a)
        | None, Key.B | Some Shift, Key.Left -> // back word (note: spans lines) // TODO: beep when at start of document?
            let toNonWordChar = scan Backward (fun m -> m.Before.Length = 0 || not (isWordChar m.Before[0])) >> expectSome
            if model.Before.Length > 0 && isWordChar model.Before[0]
            then model |> toNonWordChar // first non-word char (or start)
            else model |> scan Backward (fun m -> m.Before.Length = 0 || isWordChar m.Before[0]) |> expectSome |> toNonWordChar // first non-word char (or start)
        | Some Shift, Key.I -> model |> play [(None, Key.Carot); (None, Key.I)] // insert before line (^i)
        | None, Key.Carot   -> model |> play [(None, Key.D0); (None, Key.W)] // start of line (^w)
        | None, Key.D0 -> scan Backward (fun m -> m.Before.Length = 0) model |> expectSome // column zero
        | None, Key.Dollar -> scan Forward (fun m -> m.After.Length <= 1) model |> expectSome // end of line
        | None, Key.E -> // end of word
            let isEnd = function
                | w :: n :: _ when isWordChar w && not (isWordChar n) -> true // end of word
                | n :: [] when not (isWordChar n) -> true // end of line // TODO span lines
                | [] -> true
                | _ -> false
            model |> right false |> scan Forward (fun m -> isEnd m.After) |> expectSome
        | None, Key.F -> { model with Mode = NormalPending (None, Key.F) } // find
        | Some Shift, Key.F -> { model with Mode = NormalPending (Some Shift, Key.F) } // find-reverse
        | None, Key.G -> { model with Mode = NormalPending (None, Key.G) } // g pending
        | None, Key.H | None, Key.Backspace | None, Key.Left -> left true model // left
        | None, Key.I -> { model with Mode = Insert } // insert
        | None, Key.L | None, Key.Space | None, Key.Right -> right true model // right
        | None, Key.R -> { model with Mode = NormalPending (None, Key.R) } // replace
        | None, Key.T -> { model with Mode = NormalPending (None, Key.T) } // to
        | Some Shift, Key.T -> { model with Mode = NormalPending (Some Shift, Key.T) } // to-reverse
        | None, Key.W | Some Shift, Key.Right -> // word (note: spans lines)
            model
            |> scan Forward (fun m -> m.After.Length <= 1 || not (isWordChar m.After[0])) |> expectSome // first non-word char (or end)
            |> scan Forward (fun m -> m.After.Length <= 1 || isWordChar m.After[0]) |> expectSome // first word char following (or end)
        | None, Key.X -> // delete under cursor
            let model = if model.After.Length > 0 then { model with After = model.After.Tail } else model
            if model.After.Length = 0 then left false model else model // move left if off end
        | Some Shift, Key.X -> if model.Before.Length > 0 then { model with Before = model.Before.Tail } else model // delete left of cursor
        | _ -> { model with Beep = true } // TODO vim behavior?
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
        | None, Key.F -> find Forward  false
        | Some Shift, Key.F -> find Backward false
        | None, Key.G ->
            match input with
            | None, Key.E -> // go end (previous word)
                normal
                |> scan Backward (fun m -> m.Before.Length = 0 || m.After.Length = 0 || not (isWordChar m.After[0])) |> expectSome
                |> scan Backward (fun m -> m.Before.Length = 0 || m.After.Length = 0 || isWordChar m.After[0]) |> expectSome
            | _ -> { normal with Beep = true } // TODO vim behavior?
        | None, Key.R ->
            let insert = if normal.After.Length > 1 then Key.I else Key.A
            match keyToChar input with
            | Some _ -> normal |> play [(None, Key.X); (None, insert); input; (None, Key.Esc)] // e.g. replace with z -> xiz<Esc> or xaz<Esc> when at the end
            | None -> normal // replaced with non-char // TODO handle <CR>, <Tab>, ...
        | None, Key.T -> find Forward  true
        | Some Shift, Key.T -> find Backward true
        | _ -> { normal with Beep = true } // TODO vim behavior?
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

(*
. dot
dd
G
gg
> (>G)
;
,
@: (repeat ex command)
& (repeat last substitution)
q{char} q
:%s/foo/bar/g
*
#
c{motion}
cc
aw (and other text objects)
(counts, on mothions + actions)
y{motion}
yy
*)