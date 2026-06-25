namespace Bjolang

module Lexer =
    open System

    // 1-based lines, 0-based columns (aligns with FSharp.Compiler.Text.Position)
    type Position = { Line: int; Column: int }
    type Range = { Start: Position; End: Position }

    type Token =
        | Quote
        | LParen
        | RParen
        | LBracket
        | RBracket
        | Comma
        | Colon
        | Dot
        | Spread
        | StringLit of string
        | CharLit of char
        | NumberLit of string
        | Keyword of string
        | Symbol of string
        | TypeVar of string
        | QuotedSymbol of string

    type LexedToken = { Token: Token; Range: Range }

    let tokenize (input: string) : LexedToken list =
        let length = input.Length

        let isSymbolChar c =
            not (Char.IsWhiteSpace c)
            && not (List.contains c [ '('; ')'; '['; ']'; ','; ':'; '"'; ';'; '\'' ])

        let rec following charList pos =
            if List.isEmpty charList then
                true
            elif pos >= String.length input then
                false
            elif (List.head charList) = input[pos] then
                following (List.tail charList) (pos + 1)
            else
                false


        let rec readSymbol p =
            if p < length && isSymbolChar input[p] then
                readSymbol (p + 1)
            else
                p

        // Calculates the new line and column after consuming a chunk of text
        let advance (text: string) startLine startCol =
            let mutable l = startLine
            let mutable c = startCol

            for i = 0 to text.Length - 1 do
                if text[i] = '\n' then
                    l <- l + 1
                    c <- 0
                else
                    c <- c + 1

            l, c

        let rec loop pos line col tokens =
            if pos >= length then
                List.rev tokens
            else
                let c = input[pos]

                // Helper to emit a token and automatically calculate its range
                let emit t len =
                    let text = input.Substring(pos, len)
                    let endLine, endCol = advance text line col

                    let range =
                        { Start = { Line = line; Column = col }
                          End = { Line = endLine; Column = endCol } }

                    loop (pos + len) endLine endCol ({ Token = t; Range = range } :: tokens)




                match c with
                // Whitespace tracking
                | '\n' -> loop (pos + 1) (line + 1) 0 tokens
                | '\r' -> loop (pos + 1) line col tokens
                | _ when Char.IsWhiteSpace c -> loop (pos + 1) line (col + 1) tokens

                // Comments
                | ';' ->
                    let rec skipLine p =
                        if p >= length || input[p] = '\n' then
                            p
                        else
                            skipLine (p + 1)

                    let nextPos = skipLine pos
                    let len = nextPos - pos
                    let text = input.Substring(pos, len)
                    let endLine, endCol = advance text line col
                    loop nextPos endLine endCol tokens


                // Delimiters
                | '(' -> emit LParen 1
                | ')' -> emit RParen 1
                | '[' -> emit LBracket 1
                | ']' -> emit RBracket 1
                | ',' -> emit Comma 1
                // There are two types of keywords right now...
                | ':' when pos + 1 < length && isSymbolChar input[pos + 1] ->
                    let nextPos = readSymbol (pos + 1)
                    let len = nextPos - pos
                    emit (Keyword(input.Substring(pos + 1, len - 1))) len
                | ':' -> emit Colon 1

                // Spread Operator
                | '.' when pos + 2 < length && input[pos + 1] = '.' && input[pos + 2] = '.' -> emit Spread 3
                | '.' -> emit Dot 1

                // Strings
                | '"' ->
                    let rec readString p =
                        if p >= length then
                            failwithf $"Unterminated string at line %d{line}, col %d{col}"
                        elif input[p] = '"' then
                            p + 1
                        elif input[p] = '\\' && p + 1 < length then
                            readString (p + 2)
                        else
                            readString (p + 1)

                    let nextPos = readString (pos + 1)
                    let len = nextPos - pos
                    let rawStr = input.Substring(pos + 1, len - 2)

                    // Simple unescaping for the final AST value
                    let unescaped =
                        rawStr.Replace("\\n", "\n").Replace("\\t", "\t").Replace("\\\"", "\"").Replace("\\\\", "\\")

                    let text = input.Substring(pos, len)
                    let endLine, endCol = advance text line col

                    let range =
                        { Start = { Line = line; Column = col }
                          End = { Line = endLine; Column = endCol } }

                    loop
                        nextPos
                        endLine
                        endCol
                        ({ Token = StringLit unescaped
                           Range = range }
                         :: tokens)

                // This emits a quote. The Parser will have to understand whether it is a quoted symbol
                // or a type var
                | '\'' ->
                    // If followed by a valid symbol character, read it as a single token
                    if pos + 1 < length && isSymbolChar input[pos + 1] then

                        let nextPos = readSymbol (pos + 1)
                        let len = nextPos - pos
                        let varName = input.Substring(pos + 1, len - 1)
                        emit (QuotedSymbol varName) len // Or QuotedSymbol varName
                    else
                        // Otherwise, it's a standalone quote for lists: '(1 2 3)<
                        raise (NotImplementedException "Quoteda symbols are not yet implemented")
                        emit Quote 1
                // Numbers
                | _ when Char.IsDigit c || (c = '-' && pos + 1 < length && Char.IsDigit input[pos + 1]) ->
                    let rec readNumber p =
                        if
                            p < length
                            && (Char.IsLetterOrDigit input[p] || input[p] = '.' || input[p] = '-')
                        then
                            readNumber (p + 1)
                        else
                            p

                    let nextPos = readNumber pos
                    let len = nextPos - pos
                    emit (NumberLit(input.Substring(pos, len))) len

                // Hashtag prefixes (#:, #\, etc.)
                | '#' when pos + 1 < length ->
                    match input[pos + 1] with
                    | ':' -> // Keywords (#:keyword)
                        let nextPos = readSymbol (pos + 2)
                        let len = nextPos - pos
                        emit (Keyword(input.Substring(pos + 2, len - 2))) len

                    | '\\' -> // Scheme Character Literals (#\c, #\space)
                        let rec readCharLiteral p =
                            if p < length && isSymbolChar input[p] then
                                readCharLiteral (p + 1)
                            else
                                p

                        let nextPos = readCharLiteral (pos + 2)
                        let len = nextPos - pos
                        let charStr = input.Substring(pos + 2, len - 2)

                        let charVal =
                            match charStr.ToLowerInvariant() with
                            | "space" -> ' '
                            | "newline" -> '\n'
                            | "tab" -> '\t'
                            | "return" -> '\r'
                            | _ when charStr.Length = 1 -> charStr[0]
                            | _ -> failwithf $"Invalid character literal #\\%s{charStr} at line %d{line}, col %d{col}"

                        emit (CharLit charVal) len

                    | _ -> // Fallback for booleans (#t, #f) or symbols starting with #
                        let nextPos = readSymbol pos
                        let len = nextPos - pos
                        emit (Symbol(input.Substring(pos, len))) len

                // Symbols
                | _ ->
                    let nextPos = readSymbol pos
                    let len = nextPos - pos
                    emit (Symbol(input.Substring(pos, len))) len

        loop 0 1 0 []
