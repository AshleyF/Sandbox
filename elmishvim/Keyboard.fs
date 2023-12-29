module Keyboard

open System

// keyboard input mapped to Key, Modifier pairs

type Key =
    | A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
    | Esc | Tab | Backspace | Return | Space | Left | Right | Up | Down
    | Tilde | Bang | At | Pound | Dollar | Percent | Carot | Ampersand | Star | LParen | RParen | Underscore | Plus | Backtick
    | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9 | D0
    | Minus | Equals | LCurly | RCurly | Pipe | LSquare | RSquare | Backslash
    | Colon | Quote | Semicolon | Tick | LAngle | RAngle | Question | Comma | Dot | Slash

type Modifier = Shift | Control | Meta

let toKey (k : ConsoleKeyInfo) =
    let m, c = 
        match k.Modifiers with
        | ConsoleModifiers.None -> None, false
        | ConsoleModifiers.Shift -> Some Shift, true
        | ConsoleModifiers.Control -> Some Control, false
        | ConsoleModifiers.Alt -> Some Meta, false
        | _ -> failwith $"Unexpected modifier ({k.Modifiers})"
    match k.Key, k.KeyChar with
    | ConsoleKey.A, _ -> m, A
    | ConsoleKey.B, _ -> m, B
    | ConsoleKey.C, _ -> m, C
    | ConsoleKey.D, _ -> m, D
    | ConsoleKey.E, _ -> m, E
    | ConsoleKey.F, _ -> m, F
    | ConsoleKey.G, _ -> m, G
    | ConsoleKey.H, _ -> m, H
    | ConsoleKey.I, _ -> m, I
    | ConsoleKey.J, _ -> m, J
    | ConsoleKey.K, _ -> m, K
    | ConsoleKey.L, _ -> m, L
    | ConsoleKey.M, _ -> m, M
    | ConsoleKey.N, _ -> m, N
    | ConsoleKey.O, _ -> m, O
    | ConsoleKey.P, _ -> m, P
    | ConsoleKey.Q, _ -> m, Q
    | ConsoleKey.R, _ -> m, R
    | ConsoleKey.S, _ -> m, S
    | ConsoleKey.T, _ -> m, T
    | ConsoleKey.U, _ -> m, U
    | ConsoleKey.V, _ -> m, V
    | ConsoleKey.W, _ -> m, W
    | ConsoleKey.X, _ -> m, X
    | ConsoleKey.Y, _ -> m, Y
    | ConsoleKey.Z, _ -> m, Z
    | ConsoleKey.Escape, _ -> m, Esc
    | ConsoleKey.Tab, _ -> m, Tab
    | ConsoleKey.Backspace, _ -> m, Backspace
    | ConsoleKey.Enter, _ -> m, Return
    | ConsoleKey.Spacebar, _ -> m, Space
    | ConsoleKey.LeftArrow, _ -> m, Left
    | ConsoleKey.RightArrow, _ -> m, Right
    | ConsoleKey.UpArrow, _ -> m, Up
    | ConsoleKey.DownArrow, _ -> m, Down
    | _, '~' -> m, Tilde
    | _, '!' -> m, Bang
    | _, '@' -> m, At
    | _, '#' -> m, Pound
    | _, '$' -> m, Dollar
    | _, '%' -> m, Percent
    | _, '^' -> m, Carot
    | _, '&' -> m, Ampersand
    | _, '*' -> m, Star
    | _, '(' -> m, LParen
    | _, ')' -> m, RParen
    | _, '_' -> m, Underscore
    | _, '+' -> m, Plus
    | _, '`' -> m, Backtick
    | _, '1' -> m, D1
    | _, '2' -> m, D2
    | _, '3' -> m, D3
    | _, '4' -> m, D4
    | _, '5' -> m, D5
    | _, '6' -> m, D6
    | _, '7' -> m, D7
    | _, '8' -> m, D8
    | _, '9' -> m, D9
    | _, '0' -> m, D0
    | _, '-' -> m, Minus
    | _, '=' -> m, Equals
    | _, '{' -> m, LCurly
    | _, '}' -> m, RCurly
    | _, '|' -> m, Pipe
    | _, '[' -> m, LSquare
    | _, ']' -> m, RSquare
    | _, '\\' -> m, Backslash
    | _, ':' -> m, Colon
    | _, '"' -> m, Quote
    | _, ';' -> m, Semicolon
    | _, '\'' -> m, Tick
    | _, '<' -> m, LAngle
    | _, '>' -> m, RAngle
    | _, '?' -> m, Question
    | _, ',' -> m, Comma
    | _, '.' -> m, Dot
    | _, '/' -> m, Slash
    | _ -> failwith $"Unexpected console key ({k.Key} {k.KeyChar})"

let keyToString = function
    | None, A -> "a"
    | None, B -> "b"
    | None, C -> "c"
    | None, D -> "d"
    | None, E -> "e"
    | None, F -> "f"
    | None, G -> "g"
    | None, H -> "h"
    | None, I -> "i"
    | None, J -> "j"
    | None, K -> "k"
    | None, L -> "l"
    | None, M -> "m"
    | None, N -> "n"
    | None, O -> "o"
    | None, P -> "p"
    | None, Q -> "q"
    | None, R -> "r"
    | None, S -> "s"
    | None, T -> "t"
    | None, U -> "u"
    | None, V -> "v"
    | None, W -> "w"
    | None, X -> "x"
    | None, Y -> "y"
    | None, Z -> "z"
    | Some Shift, A -> "A"
    | Some Shift, B -> "B"
    | Some Shift, C -> "C"
    | Some Shift, D -> "D"
    | Some Shift, E -> "E"
    | Some Shift, F -> "F"
    | Some Shift, G -> "G"
    | Some Shift, H -> "H"
    | Some Shift, I -> "I"
    | Some Shift, J -> "J"
    | Some Shift, K -> "K"
    | Some Shift, L -> "L"
    | Some Shift, M -> "M"
    | Some Shift, N -> "N"
    | Some Shift, O -> "O"
    | Some Shift, P -> "P"
    | Some Shift, Q -> "Q"
    | Some Shift, R -> "R"
    | Some Shift, S -> "S"
    | Some Shift, T -> "T"
    | Some Shift, U -> "U"
    | Some Shift, V -> "V"
    | Some Shift, W -> "W"
    | Some Shift, X -> "X"
    | Some Shift, Y -> "Y"
    | Some Shift, Z -> "Z"
    | None, Esc -> "<esc>"
    | None, Tab -> "<tab>"
    | None, Backspace -> "<bs>"
    | None, Return -> "<cr>"
    | None, Space -> " " // <space>
    | None, Left -> "<left>"
    | None, Right -> "<right>"
    | None, Up -> "<up>"
    | None, Down -> "<down>"
    | None, Tilde -> "~"
    | None, Bang -> "!"
    | None, At -> "@"
    | None, Pound -> "#"
    | None, Dollar -> "$"
    | None, Percent -> "%"
    | None, Carot -> "^"
    | None, Ampersand -> "&"
    | None, Star -> "*"
    | None, LParen -> "("
    | None, RParen -> ")"
    | None, Underscore -> "_"
    | None, Plus -> "+"
    | None, Backtick -> "`"
    | None, D1 -> "1"
    | None, D2 -> "2"
    | None, D3 -> "3"
    | None, D4 -> "4"
    | None, D5 -> "5"
    | None, D6 -> "6"
    | None, D7 -> "7"
    | None, D8 -> "8"
    | None, D9 -> "9"
    | None, D0 -> "0"
    | None, Minus -> "-"
    | None, Equals -> "="
    | None, LCurly -> "{"
    | None, RCurly -> "}"
    | None, Pipe -> "|"
    | None, LSquare -> "["
    | None, RSquare -> "]"
    | None, Backslash -> "\\"
    | None, Colon -> ":"
    | None, Quote -> "\""
    | None, Semicolon -> ";"
    | None, Tick -> "'"
    | None, LAngle -> "<"
    | None, RAngle -> ">"
    | None, Question -> "?"
    | None, Comma -> ","
    | None, Dot -> "."
    | None, Slash -> "/"
    | Some Shift, Tab -> "<S-Tab>"
    | Some Shift, Left -> "<S-Left>"
    | Some Shift, Right -> "<S-Right>"
    | m, k -> failwith $"Unexpected console key ({m} {k})"

let keyToChar key = let str = keyToString key in if str.Length = 1 then Some str[0] else None

let rec keys () = seq {
    yield Console.ReadKey(true) |> toKey
    yield! keys () }

//while true do
//    let k = Console.ReadKey()
//    Console.WriteLine($"KEY: {k.Key} {k.KeyChar} {k.Modifiers}")