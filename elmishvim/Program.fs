open System
open Keyboard

type Mode = Normal | Insert

type Action =
    | Pending | Beep | NotImplemented
    | Left | Right | Up | Down
    | InsertBefore | InsertBeforeLine
    | InsertAfter | InsertAfterLine
    | ForwardWord | ForwardBigWord
    | BackWord | BackBigWord
    | ForwardEndWord | BackEndWord
    | ColumnZero | StartOfLine | EndOfLine
    | Find of char | FindReverse of char
    | To of char | ToReverse of char
    | ReplaceWith of char
    | DeleteForward | DeleteBack
    | ResetState
    | NormalMode | Type of char

let action pending mode key =
    let expectChar k action = match keyToChar k with Some c -> action c | None -> Beep
    match mode with
    | Normal ->
        match key :: pending with
        | Key.Esc        :: p
        | Key.CLSquare   :: p -> ResetState                  , [] // forget pending
        | Key.H          :: p
        | Key.Left       :: p
        | Key.Backspace  :: p -> Left                        , p
        | Key.L          :: p
        | Key.Right      :: p
        | Key.Space      :: p -> Right                       , p
        | Key.K          :: p
        | Key.Up         :: p -> Up                          , p
        | Key.J          :: p
        | Key.Down       :: p -> Down                        , p
        | Key.I          :: p -> InsertBefore                , p
        | Key.CapI       :: p -> InsertBeforeLine            , p
        | Key.A          :: p -> InsertAfter                 , p
        | Key.CapA       :: p -> InsertAfterLine             , p
        | Key.W          :: p
        | Key.SRight     :: p -> ForwardWord                 , p
        | Key.CapW       :: p -> ForwardBigWord              , p
        | Key.B          :: p
        | Key.SLeft      :: p -> BackWord                    , p
        | Key.CapB       :: p -> BackBigWord                 , p
        | Key.E :: Key.G :: p -> BackEndWord                 , p
        | Key.E          :: p -> ForwardEndWord              , p
        | Key.D0         :: p -> ColumnZero                  , p
        | Key.Carot      :: p -> StartOfLine                 , p
        | Key.Dollar     :: p -> EndOfLine                   , p
        | Key.X          :: p -> DeleteForward               , p
        | Key.CapX       :: p -> DeleteBack                  , p
        | k :: Key.F     :: p -> Find        |> expectChar k , p
        | k :: Key.CapF  :: p -> FindReverse |> expectChar k , p
        | k :: Key.T     :: p -> To          |> expectChar k , p
        | k :: Key.CapT  :: p -> ToReverse   |> expectChar k , p
        | k :: Key.R     :: p -> ReplaceWith |> expectChar k , p
        |                   p -> Pending                     , p
    | Insert ->
        match key :: pending with
        | Key.Esc        :: p
        | Key.CLSquare   :: p -> NormalMode                  , p
        | k              :: p -> Type |> expectChar k        , p
        |                   p -> NotImplemented              , p

type Direction = Forward | Backward

type Model = {
    Beep    : bool
    Mode    : Mode
    Pending : Key list
    Action  : Action // debugging
    Find    : (Direction * char) option
    Before  : char list
    After   : char list }

let init () = {
    Beep = false
    Mode = Normal
    Pending = []
    Action = Pending
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
    | Insert -> printfn "INSERT"
    Console.WriteLine()
    printfn "Model: %A" model
    Console.CursorLeft <- x
    Console.CursorTop <- y

let rec update model input =
    let action, pending = action model.Pending model.Mode input
    let model = { model with Pending = pending; Action = action; Beep = false }
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
    let isEsc k = k = Key.Esc || k = Key.CLSquare
    let play inputs model = inputs |> Seq.fold update model
    let normal = { model with Mode = Normal }
    let find direction upto c =
        let pred m =
            match direction, upto with
            | Forward,  false
            | Backward, false -> m.After <> model.After && match m.After with h :: _ -> h = c | [] -> false
            | Forward,  true  -> match m.After with _ :: h :: _ -> h = c | _ -> false
            | Backward, true  -> match m.Before with h :: _ -> h = c | [] -> false
        match scan direction pred normal with
        | Some normal -> normal
        | None -> { normal with Beep = true }
    if isEsc input then normal else
    match model.Mode with
    | Normal ->
        match action with
        | ResetState -> model // TODO
        | InsertAfter ->
            match model.After with
            | b :: after -> { model with Before = b :: model.Before; After = after; Mode = Insert }
            | [] -> { model with Mode = Insert }
        | InsertAfterLine -> model |> play [Key.Dollar; Key.A]
        | BackWord ->
            let toNonWordChar = scan Backward (fun m -> m.Before.Length = 0 || not (isWordChar m.Before[0])) >> expectSome
            if model.Before.Length > 0 && isWordChar model.Before[0]
            then model |> toNonWordChar // first non-word char (or start)
            else model |> scan Backward (fun m -> m.Before.Length = 0 || isWordChar m.Before[0]) |> expectSome |> toNonWordChar // first non-word char (or start)
        | InsertBeforeLine -> model |> play [Key.Carot; Key.I]
        | StartOfLine -> model |> play [Key.D0; Key.W]
        | ColumnZero -> scan Backward (fun m -> m.Before.Length = 0) model |> expectSome
        | EndOfLine -> scan Forward (fun m -> m.After.Length <= 1) model |> expectSome
        | ForwardEndWord ->
            let isEnd = function
                | w :: n :: _ when isWordChar w && not (isWordChar n) -> true // end of word
                | n :: [] when not (isWordChar n) -> true // end of line // TODO span lines
                | [] -> true
                | _ -> false
            model |> right false |> scan Forward (fun m -> isEnd m.After) |> expectSome
        | Find c -> find Forward false c
        | FindReverse c -> find Backward false c
        | BackEndWord ->
            normal
            |> scan Backward (fun m -> m.Before.Length = 0 || m.After.Length = 0 || not (isWordChar m.After[0])) |> expectSome
            |> scan Backward (fun m -> m.Before.Length = 0 || m.After.Length = 0 || isWordChar m.After[0]) |> expectSome
        | ReplaceWith c ->
            let insert = if normal.After.Length > 1 then Key.I else Key.A
            let model' = play [Key.X; insert] normal
            { model' with Before = c :: model'.Before; Mode = Normal }
        | To c -> find Forward true c
        | ToReverse c -> find Backward true c
        | Left -> left true model
        | InsertBefore -> { model with Mode = Insert }
        | Right -> right true model
        | ForwardWord -> // TODO span lines
            model
            |> scan Forward (fun m -> m.After.Length <= 1 || not (isWordChar m.After[0])) |> expectSome // first non-word char (or end)
            |> scan Forward (fun m -> m.After.Length <= 1 || isWordChar m.After[0]) |> expectSome // first word char following (or end)
        | DeleteForward ->
            let model = if model.After.Length > 0 then { model with After = model.After.Tail } else model
            if model.After.Length = 0 then left false model else model // move left if off end
        | DeleteBack -> if model.Before.Length > 0 then { model with Before = model.Before.Tail } else model
        | Pending -> model
        | _ -> { model with Beep = true }
    | Insert ->
        match action with
        | NormalMode -> { model with Mode = Normal }
        | Type c -> { model with Before = c :: model.Before }
        | _ -> { model with Beep = true }

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