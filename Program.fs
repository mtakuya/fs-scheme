open System
let EOF = null
let PLUS = "+"
let MINUS = "-"
let LPAREN = "("
let RPAREN = ")"
let ASSIGN = "="
let ASTERISK = "*"
let SLASH = "/"
let LT = "<"
let GT = ">"
let BAN = "!"
let ILLEGAL = "ILLEGAL"
let WHITESPACE = "WHITESPACE"
let OBJECT = "OBJECT"
let BLANK = "BLANK"
let LET = "LET"
let FN = "FN"
let INT = "INT"
let keywords = Map [("fn", FN); ("let", LET)]

let lookupIdent ident = 
    let result = Map.tryFind ident keywords
    match result with
    | Some value -> value
    | None -> OBJECT

type ScmTokenType = string

type Token(scmtype, literal) = 
    member this.scmtype = scmtype
    member this.literal = literal

type Lexer(input: string) =
    let input : string = input
    let mutable position = 0
    let mutable readPosition = 0
    let mutable ch = ""

    member this.readChar() =
        if readPosition >= input.Length then
            ch <- null
        else
            ch <- input.[readPosition].ToString()

        position <- readPosition
        readPosition <- readPosition + 1

    member this.newToken(tokenType, ch) =
        Token(tokenType, ch)

    member this.NextToken() =
        //this.skipWhitespace()
        this.readChar()
        let rec tok = 
            match ch with
            | "(" -> this.newToken(LPAREN, ch.ToString())
            | ")" -> this.newToken(RPAREN, ch.ToString())
            | "+" -> this.newToken(PLUS, ch.ToString())
            | "-" -> this.newToken(MINUS, ch.ToString())
            | "=" -> this.newToken(ASSIGN, ch.ToString())
            | "<" -> this.newToken(LT, ch.ToString())
            | ">" -> this.newToken(GT, ch.ToString())
            | "*" -> this.newToken(ASTERISK, ch.ToString())
            | "!" -> this.newToken(BAN, ch.ToString())
            | ""  -> this.newToken(BLANK, "")
            | " " -> this.NextToken()
            | "\t" -> this.NextToken()
            | "\n" -> this.NextToken()
            | "\r" -> this.NextToken()
            | "0" -> this.newToken(INT, ch.ToString())
            | "1" -> this.newToken(INT, ch.ToString())
            | "2" -> this.newToken(INT, ch.ToString())
            | "3" -> this.newToken(INT, ch.ToString())
            | "4" -> this.newToken(INT, ch.ToString())
            | "5" -> this.newToken(INT, ch.ToString())
            | "6" -> this.newToken(INT, ch.ToString())
            | "7" -> this.newToken(INT, ch.ToString())
            | "8" -> this.newToken(INT, ch.ToString())
            | "9" -> this.newToken(INT, ch.ToString())
            | null -> this.newToken(EOF, "")
            | _   -> this.newToken(OBJECT, ch.ToString())
        tok

let PROMPT = ">>> "

[<EntryPoint>]
let main args =
    while true do
        printf "%s" PROMPT
        let scanned = Console.ReadLine()
        let lexer = new Lexer(scanned)
        let mutable tok = lexer.NextToken()
        while tok.scmtype <> EOF do
            printfn "literal: %A, scmtype: %A" tok.literal tok.scmtype
            tok <- lexer.NextToken()
    0