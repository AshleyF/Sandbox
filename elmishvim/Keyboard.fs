module Keyboard

open System

// keyboard input mapped to Key, Modifier pairs
// Shift is not a modifier, instead separate CapX and X (lowercase) Keys

type Key =
    | A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
    | CapA | CapB | CapC | CapD | CapE | CapF | CapG | CapH | CapI | CapJ | CapK | CapL | CapM | CapN | CapO | CapP | CapQ | CapR | CapS | CapT | CapU | CapV | CapW | CapX | CapY | CapZ
    | Esc | Tab | Backspace | Return | Space | Left | Right | Up | Down
    | Tilde | Bang | At | Pound | Dollar | Percent | Carot | Ampersand | Star | LParen | RParen | Underscore | Plus | Backtick
    | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9 | D0
    | Minus | Equals | LCurly | RCurly | Pipe | LSquare | RSquare | Backslash
    | Colon | Quote | Semicolon | Tick | LAngle | RAngle | Question | Comma | Dot | Slash

type Modifier = Control | Meta

let toKey (k : ConsoleKeyInfo) =
    let m, c = 
        match k.Modifiers with
        | ConsoleModifiers.None -> None, false
        | ConsoleModifiers.Shift -> None, true // not treated as modifier
        | ConsoleModifiers.Control -> Some Control, false
        | ConsoleModifiers.Alt -> Some Meta, false
        | _ -> failwith $"Unexpected modifier ({k.Modifiers})"
    let cap lower upper = if c then upper else lower
    match k.Key, k.KeyChar with
    | ConsoleKey.A, _ -> cap A CapA, m
    | ConsoleKey.B, _ -> cap B CapB, m
    | ConsoleKey.C, _ -> cap C CapC, m
    | ConsoleKey.D, _ -> cap D CapD, m
    | ConsoleKey.E, _ -> cap E CapE, m
    | ConsoleKey.F, _ -> cap F CapF, m
    | ConsoleKey.G, _ -> cap G CapG, m
    | ConsoleKey.H, _ -> cap H CapH, m
    | ConsoleKey.I, _ -> cap I CapI, m
    | ConsoleKey.J, _ -> cap J CapJ, m
    | ConsoleKey.K, _ -> cap K CapK, m
    | ConsoleKey.L, _ -> cap L CapL, m
    | ConsoleKey.M, _ -> cap M CapM, m
    | ConsoleKey.N, _ -> cap N CapN, m
    | ConsoleKey.O, _ -> cap O CapO, m
    | ConsoleKey.P, _ -> cap P CapP, m
    | ConsoleKey.Q, _ -> cap Q CapQ, m
    | ConsoleKey.R, _ -> cap R CapR, m
    | ConsoleKey.S, _ -> cap S CapS, m
    | ConsoleKey.T, _ -> cap T CapT, m
    | ConsoleKey.U, _ -> cap U CapU, m
    | ConsoleKey.V, _ -> cap V CapV, m
    | ConsoleKey.W, _ -> cap W CapW, m
    | ConsoleKey.X, _ -> cap X CapX, m
    | ConsoleKey.Y, _ -> cap Y CapY, m
    | ConsoleKey.Z, _ -> cap Z CapZ, m
    | ConsoleKey.Escape, _ -> Esc, m
    | ConsoleKey.Tab, _ -> Tab, m
    | ConsoleKey.Backspace, _ -> Backspace, m
    | ConsoleKey.Enter, _ -> Return, m
    | ConsoleKey.Spacebar, _ -> Space, m
    | ConsoleKey.LeftArrow, _ -> Left, m
    | ConsoleKey.RightArrow, _ -> Right, m
    | ConsoleKey.UpArrow, _ -> Up, m
    | ConsoleKey.DownArrow, _ -> Down, m
    | _, '~' -> Tilde, m
    | _, '!' -> Bang, m
    | _, '@' -> At, m
    | _, '#' -> Pound, m
    | _, '$' -> Dollar, m
    | _, '%' -> Percent, m
    | _, '^' -> Carot, m
    | _, '&' -> Ampersand, m
    | _, '*' -> Star, m
    | _, '(' -> LParen, m
    | _, ')' -> RParen, m
    | _, '_' -> Underscore, m
    | _, '+' -> Plus, m
    | _, '`' -> Backtick, m
    | _, '1' -> D1, m
    | _, '2' -> D2, m
    | _, '3' -> D3, m
    | _, '4' -> D4, m
    | _, '5' -> D5, m
    | _, '6' -> D6, m
    | _, '7' -> D7, m
    | _, '8' -> D8, m
    | _, '9' -> D9, m
    | _, '0' -> D0, m
    | _, '-' -> Minus, m
    | _, '=' -> Equals, m
    | _, '{' -> LCurly, m
    | _, '}' -> RCurly, m
    | _, '|' -> Pipe, m
    | _, '[' -> LSquare, m
    | _, ']' -> RSquare, m
    | _, '\\' -> Backslash, m
    | _, ':' -> Colon, m
    | _, '"' -> Quote, m
    | _, ';' -> Semicolon, m
    | _, '\'' -> Tick, m
    | _, '<' -> LAngle, m
    | _, '>' -> RAngle, m
    | _, '?' -> Question, m
    | _, ',' -> Comma, m
    | _, '.' -> Dot, m
    | _, '/' -> Slash, m
    | _ -> failwith $"Unexpected console key ({k.Key} {k.KeyChar})"

let keyToString = function
    | A -> "a"
    | B -> "b"
    | C -> "c"
    | D -> "d"
    | E -> "e"
    | F -> "f"
    | G -> "g"
    | H -> "h"
    | I -> "i"
    | J -> "j"
    | K -> "k"
    | L -> "l"
    | M -> "m"
    | N -> "n"
    | O -> "o"
    | P -> "p"
    | Q -> "q"
    | R -> "r"
    | S -> "s"
    | T -> "t"
    | U -> "u"
    | V -> "v"
    | W -> "w"
    | X -> "x"
    | Y -> "y"
    | Z -> "z"
    | CapA -> "A"
    | CapB -> "B"
    | CapC -> "C"
    | CapD -> "D"
    | CapE -> "E"
    | CapF -> "F"
    | CapG -> "G"
    | CapH -> "H"
    | CapI -> "I"
    | CapJ -> "J"
    | CapK -> "K"
    | CapL -> "L"
    | CapM -> "M"
    | CapN -> "N"
    | CapO -> "O"
    | CapP -> "P"
    | CapQ -> "Q"
    | CapR -> "R"
    | CapS -> "S"
    | CapT -> "T"
    | CapU -> "U"
    | CapV -> "V"
    | CapW -> "W"
    | CapX -> "X"
    | CapY -> "Y"
    | CapZ -> "Z"
    | Esc -> "<esc>"
    | Tab -> "<tab>"
    | Backspace -> "<bs>"
    | Return -> "<cr>"
    | Space -> " " // <space>
    | Left -> "<left>"
    | Right -> "<right>"
    | Up -> "<up>"
    | Down -> "<down>"
    | Tilde -> "~"
    | Bang -> "!"
    | At -> "@"
    | Pound -> "#"
    | Dollar -> "$"
    | Percent -> "%"
    | Carot -> "^"
    | Ampersand -> "&"
    | Star -> "*"
    | LParen -> "("
    | RParen -> ")"
    | Underscore -> "_"
    | Plus -> "+"
    | Backtick -> "`"
    | D1 -> "1"
    | D2 -> "2"
    | D3 -> "3"
    | D4 -> "4"
    | D5 -> "5"
    | D6 -> "6"
    | D7 -> "7"
    | D8 -> "8"
    | D9 -> "9"
    | D0 -> "0"
    | Minus -> "-"
    | Equals -> "="
    | LCurly -> "{"
    | RCurly -> "}"
    | Pipe -> "|"
    | LSquare -> "["
    | RSquare -> "]"
    | Backslash -> "\\"
    | Colon -> ":"
    | Quote -> "\""
    | Semicolon -> ";"
    | Tick -> "'"
    | LAngle -> "<"
    | RAngle -> ">"
    | Question -> "?"
    | Comma -> ","
    | Dot -> "."
    | Slash -> "/"

let keyToChar key = let str = keyToString key in if str.Length = 1 then Some str[0] else None

let rec keys () = seq {
    yield Console.ReadKey(true) |> toKey
    yield! keys () }