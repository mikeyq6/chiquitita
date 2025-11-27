open System
open System.IO

type Token =
    | Print | String of string | If | Elsif | Else | While | Foreach | In | True | False
    | Int | Text | Float | Bool | Void | Number of int | FloatNum of float
    | Identifier of string | Assign | Plus | Minus | Multiply | Divide | Mod | Dot | Chiquitita
    | GreaterThan | LessThan | GreaterThanOrEqual | LessThanOrEqual
    | And | Or | Xor | Equals
    | LeftParen | RightParen | LeftBracket | RightBracket | Comma | Indent | Dedent | EOF

type VarType = IntType | TextType | FloatType | BoolType | VoidType
             | IntArrayType | TextArrayType | FloatArrayType | BoolArrayType

type Value =
    | IntVal of int | TextVal of string | FloatVal of float | BoolVal of bool
    | ArrayVal of Value list * VarType

type Method = {
    Name: string
    ReturnType: VarType
    Parameters: (string * VarType) list
    Body: Token list
}

let processStringInterpolation (str: string) =
    let mutable result = str
    let mutable pos = 0
    while pos < result.Length - 1 do
        if result.[pos] = '{' && result.[pos + 1] = '{' then
            let startPos = pos + 2
            let mutable endPos = startPos
            let mutable depth = 0
            let mutable found = false
            while endPos < result.Length - 1 && not found do
                if result.[endPos] = '{' then depth <- depth + 1
                elif result.[endPos] = '}' then
                    if depth > 0 then depth <- depth - 1
                    elif result.[endPos + 1] = '}' then
                        found <- true
                endPos <- endPos + 1
            if found then
                let content = result.Substring(startPos, endPos - startPos - 1).Trim()
                let placeholder = $"<<EXPR:{content}>>"
                result <- result.Substring(0, pos) + placeholder + result.Substring(endPos + 1)
                pos <- pos + placeholder.Length
            else
                pos <- pos + 1
        else
            pos <- pos + 1
    result

let tokenize (input: string) =
    let lines = input.Split([|'\n'; '\r'|], StringSplitOptions.RemoveEmptyEntries)
    let mutable tokens = []
    let mutable indentStack = [0]
    
    for line in lines do
        let trimmed = line.TrimStart()
        if not (trimmed.StartsWith("//")) && trimmed <> "" then
            let indent = line.Length - trimmed.Length
            let currentIndent = List.head indentStack
            
            if indent > currentIndent then
                indentStack <- indent :: indentStack
                tokens <- Indent :: tokens
            elif indent < currentIndent then
                let mutable newStack = indentStack
                while List.head newStack > indent do
                    newStack <- List.tail newStack
                    tokens <- Dedent :: tokens
                indentStack <- newStack
            
            // Check if line contains array literal (after =) and handle specially
            let hasArrayLiteral = trimmed.Contains("=") && trimmed.Contains("[") && trimmed.Contains("]")
            let parts = 
                if hasArrayLiteral then
                    let equalsIdx = trimmed.IndexOf('=')
                    let bracketStart = trimmed.IndexOf('[', equalsIdx)
                    let bracketEnd = trimmed.LastIndexOf(']')
                    if bracketEnd > bracketStart && bracketStart > equalsIdx then
                        let beforeBracket = trimmed.Substring(0, bracketStart).Trim()
                        let insideBracket = trimmed.Substring(bracketStart, bracketEnd - bracketStart + 1)
                        let afterBracket = if bracketEnd + 1 < trimmed.Length then trimmed.Substring(bracketEnd + 1).Trim() else ""
                        let beforeParts = if beforeBracket <> "" then beforeBracket.Split([|' '|], StringSplitOptions.RemoveEmptyEntries) else [||]
                        let afterParts = if afterBracket <> "" then afterBracket.Split([|' '|], StringSplitOptions.RemoveEmptyEntries) else [||]
                        Array.concat [beforeParts; [|insideBracket|]; afterParts]
                    else
                        trimmed.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
                else
                    trimmed.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
            let mutable i = 0
            while i < parts.Length do
                let word = parts.[i].Trim()
                let prevToken = if tokens.IsEmpty then EOF else List.head tokens
                if word <> "" then
                    match word with
                    | "print" -> tokens <- Print :: tokens
                    | "if" -> tokens <- If :: tokens | "elsif" -> tokens <- Elsif :: tokens | "else" -> tokens <- Else :: tokens
                    | "while" -> tokens <- While :: tokens
                    | "foreach" -> tokens <- Foreach :: tokens | "in" -> tokens <- In :: tokens
                    | "true" -> tokens <- True :: tokens | "false" -> tokens <- False :: tokens
                    | "and" -> tokens <- And :: tokens | "or" -> tokens <- Or :: tokens | "xor" -> tokens <- Xor :: tokens
                    | "mod" -> tokens <- Mod :: tokens | "equals" -> tokens <- Equals :: tokens
                    | "int" -> tokens <- Int :: tokens | "text" -> tokens <- Text :: tokens
                    | "float" -> tokens <- Float :: tokens | "bool" -> tokens <- Bool :: tokens
                    | "void" -> tokens <- Void :: tokens
                    | "=" -> tokens <- Assign :: tokens | "+" -> tokens <- Plus :: tokens
                    | "-" -> tokens <- Minus :: tokens | "*" -> tokens <- Multiply :: tokens
                    | "/" -> tokens <- Divide :: tokens | "." -> tokens <- Dot :: tokens
                    | ">=" -> tokens <- GreaterThanOrEqual :: tokens | "<=" -> tokens <- LessThanOrEqual :: tokens
                    | ">" -> tokens <- GreaterThan :: tokens | "<" -> tokens <- LessThan :: tokens
                    | "chiquitita" -> tokens <- Chiquitita :: tokens | "(" -> tokens <- LeftParen :: tokens
                    | ")" -> tokens <- RightParen :: tokens | "[" -> tokens <- LeftBracket :: tokens
                    | "]" -> tokens <- RightBracket :: tokens | "," -> tokens <- Comma :: tokens
                    | s when s.StartsWith("\"") && s.EndsWith("\"") && s.Length >= 2 -> 
                        let content = s.Substring(1, s.Length - 2)
                        let processedString = processStringInterpolation content
                        tokens <- String processedString :: tokens
                    | s when s.StartsWith("\"") ->
                        // Find the closing quote in the original trimmed line
                        let mutable pos = 0
                        let mutable foundStart = false
                        let mutable startPos = 0
                        let mutable endPos = -1
                        while pos < trimmed.Length && endPos = -1 do
                            if trimmed.[pos] = '"' then
                                if not foundStart then
                                    foundStart <- true
                                    startPos <- pos + 1
                                else
                                    endPos <- pos
                            pos <- pos + 1
                        if endPos > startPos then
                            let fullString = trimmed.Substring(startPos, endPos - startPos)
                            let processedString = processStringInterpolation fullString
                            tokens <- String processedString :: tokens
                        else
                            failwith "Unterminated string literal"
                    | s when System.Text.RegularExpressions.Regex.IsMatch(s, @"^-?\d+$") ->
                        tokens <- Number (int s) :: tokens
                    | s when System.Text.RegularExpressions.Regex.IsMatch(s, @"^-?\d+\.\d+$") ->
                        tokens <- FloatNum (float s) :: tokens
                    | s when s.Contains(".") && not (s.StartsWith("\"")) && (System.Text.RegularExpressions.Regex.IsMatch(s, @"^[a-zA-Z][a-zA-Z0-9_]*\.[a-zA-Z][a-zA-Z0-9_]*$")) ->
                        let dotIdx = s.IndexOf('.')
                        let beforeDot = s.Substring(0, dotIdx)
                        let afterDot = s.Substring(dotIdx + 1)
                        tokens <- Identifier beforeDot :: tokens
                        tokens <- Dot :: tokens
                        tokens <- Identifier afterDot :: tokens
                    | s when s.StartsWith("[") && s.EndsWith("]") && s.Length > 2 && s.Contains(",") ->
                        // Handle array literal [elements]
                        let content = s.Substring(1, s.Length - 2)
                        tokens <- LeftBracket :: tokens
                        // Parse array elements
                        let mutable remaining = content
                        while remaining <> "" do
                            remaining <- remaining.TrimStart()
                            if remaining.StartsWith("\"") then
                                let quoteEnd = remaining.IndexOf('"', 1)
                                if quoteEnd > 0 then
                                    let strContent = remaining.Substring(1, quoteEnd - 1)
                                    let processedString = processStringInterpolation strContent
                                    tokens <- String processedString :: tokens
                                    remaining <- remaining.Substring(quoteEnd + 1).TrimStart()
                                    if remaining.StartsWith(",") then
                                        tokens <- Comma :: tokens
                                        remaining <- remaining.Substring(1)
                                else
                                    failwith "Unterminated string in array"
                            elif System.Text.RegularExpressions.Regex.IsMatch(remaining, @"^-?\d+\.\d+") then
                                let m = System.Text.RegularExpressions.Regex.Match(remaining, @"^-?\d+\.\d+")
                                tokens <- FloatNum (float m.Value) :: tokens
                                remaining <- remaining.Substring(m.Length).TrimStart()
                                if remaining.StartsWith(",") then
                                    tokens <- Comma :: tokens
                                    remaining <- remaining.Substring(1)
                            elif System.Text.RegularExpressions.Regex.IsMatch(remaining, @"^x[0-9a-fA-F]+") then
                                let m = System.Text.RegularExpressions.Regex.Match(remaining, @"^x[0-9a-fA-F]+")
                                let hexValue = System.Convert.ToInt32(m.Value.Substring(1), 16)
                                tokens <- Number hexValue :: tokens
                                remaining <- remaining.Substring(m.Length).TrimStart()
                                if remaining.StartsWith(",") then
                                    tokens <- Comma :: tokens
                                    remaining <- remaining.Substring(1)
                            elif System.Text.RegularExpressions.Regex.IsMatch(remaining, @"^b[01]+") then
                                let m = System.Text.RegularExpressions.Regex.Match(remaining, @"^b[01]+")
                                let binValue = System.Convert.ToInt32(m.Value.Substring(1), 2)
                                tokens <- Number binValue :: tokens
                                remaining <- remaining.Substring(m.Length).TrimStart()
                                if remaining.StartsWith(",") then
                                    tokens <- Comma :: tokens
                                    remaining <- remaining.Substring(1)
                            elif System.Text.RegularExpressions.Regex.IsMatch(remaining, @"^o[0-7]+") then
                                let m = System.Text.RegularExpressions.Regex.Match(remaining, @"^o[0-7]+")
                                let octValue = System.Convert.ToInt32(m.Value.Substring(1), 8)
                                tokens <- Number octValue :: tokens
                                remaining <- remaining.Substring(m.Length).TrimStart()
                                if remaining.StartsWith(",") then
                                    tokens <- Comma :: tokens
                                    remaining <- remaining.Substring(1)
                            elif System.Text.RegularExpressions.Regex.IsMatch(remaining, @"^-?\d+") then
                                let m = System.Text.RegularExpressions.Regex.Match(remaining, @"^-?\d+")
                                tokens <- Number (int m.Value) :: tokens
                                remaining <- remaining.Substring(m.Length).TrimStart()
                                if remaining.StartsWith(",") then
                                    tokens <- Comma :: tokens
                                    remaining <- remaining.Substring(1)
                            elif remaining.StartsWith("true") then
                                tokens <- True :: tokens
                                remaining <- remaining.Substring(4).TrimStart()
                                if remaining.StartsWith(",") then
                                    tokens <- Comma :: tokens
                                    remaining <- remaining.Substring(1)
                            elif remaining.StartsWith("false") then
                                tokens <- False :: tokens
                                remaining <- remaining.Substring(5).TrimStart()
                                if remaining.StartsWith(",") then
                                    tokens <- Comma :: tokens
                                    remaining <- remaining.Substring(1)
                            elif System.Text.RegularExpressions.Regex.IsMatch(remaining, @"^[a-zA-Z][a-zA-Z0-9_]*") then
                                let m = System.Text.RegularExpressions.Regex.Match(remaining, @"^[a-zA-Z][a-zA-Z0-9_]*")
                                tokens <- Identifier m.Value :: tokens
                                remaining <- remaining.Substring(m.Length).TrimStart()
                                if remaining.StartsWith(",") then
                                    tokens <- Comma :: tokens
                                    remaining <- remaining.Substring(1)
                            elif remaining = "" then
                                ()
                            else
                                failwith $"Unexpected content in array: {remaining}"
                        tokens <- RightBracket :: tokens
                    | s when s.Contains("(") || s.Contains(")") || s.Contains("[") || s.Contains("]") || s.Contains(",") || s.Contains(">") || s.Contains("<") || s.Contains("*") || s.Contains("/") ->
                        // Handle identifiers with parentheses or commas
                        let mutable remaining = s
                        while remaining <> "" do
                            if remaining.StartsWith("\"") then
                                let quoteEnd = remaining.IndexOf('"', 1)
                                if quoteEnd > 0 then
                                    let content = remaining.Substring(1, quoteEnd - 1)
                                    let processedString = processStringInterpolation content
                                    tokens <- String processedString :: tokens
                                    remaining <- remaining.Substring(quoteEnd + 1)
                                else
                                    failwith "Unterminated string literal"
                            elif remaining.StartsWith("(") then
                                tokens <- LeftParen :: tokens
                                remaining <- remaining.Substring(1)
                            elif remaining.StartsWith(")") then
                                tokens <- RightParen :: tokens
                                remaining <- remaining.Substring(1)
                            elif remaining.StartsWith("[") then
                                tokens <- LeftBracket :: tokens
                                remaining <- remaining.Substring(1)
                            elif remaining.StartsWith("]") then
                                tokens <- RightBracket :: tokens
                                remaining <- remaining.Substring(1)
                            elif remaining.StartsWith(",") then
                                tokens <- Comma :: tokens
                                remaining <- remaining.Substring(1)
                            elif remaining.StartsWith(">=") then
                                tokens <- GreaterThanOrEqual :: tokens
                                remaining <- remaining.Substring(2)
                            elif remaining.StartsWith("<=") then
                                tokens <- LessThanOrEqual :: tokens
                                remaining <- remaining.Substring(2)
                            elif remaining.StartsWith(">") then
                                tokens <- GreaterThan :: tokens
                                remaining <- remaining.Substring(1)
                            elif remaining.StartsWith("<") then
                                tokens <- LessThan :: tokens
                                remaining <- remaining.Substring(1)
                            elif remaining.StartsWith("*") then
                                tokens <- Multiply :: tokens
                                remaining <- remaining.Substring(1)
                            elif remaining.StartsWith("/") then
                                tokens <- Divide :: tokens
                                remaining <- remaining.Substring(1)
                            else
                                let endIdx = 
                                    let parenIdx = if remaining.Contains("(") then remaining.IndexOf('(') else remaining.Length
                                    let closeIdx = if remaining.Contains(")") then remaining.IndexOf(')') else remaining.Length
                                    let leftBracketIdx = if remaining.Contains("[") then remaining.IndexOf('[') else remaining.Length
                                    let rightBracketIdx = if remaining.Contains("]") then remaining.IndexOf(']') else remaining.Length
                                    let commaIdx = if remaining.Contains(",") then remaining.IndexOf(',') else remaining.Length
                                    let gtIdx = if remaining.Contains(">") then remaining.IndexOf('>') else remaining.Length
                                    let ltIdx = if remaining.Contains("<") then remaining.IndexOf('<') else remaining.Length
                                    let multIdx = if remaining.Contains("*") then remaining.IndexOf('*') else remaining.Length
                                    let divIdx = if remaining.Contains("/") then remaining.IndexOf('/') else remaining.Length
                                    min (min (min (min (min (min (min (min parenIdx closeIdx) leftBracketIdx) rightBracketIdx) commaIdx) gtIdx) ltIdx) multIdx) divIdx
                                if endIdx > 0 then
                                    let part = remaining.Substring(0, endIdx)
                                    if part.StartsWith("\"") && part.EndsWith("\"") && part.Length >= 2 then
                                        let content = part.Substring(1, part.Length - 2)
                                        let processedString = processStringInterpolation content
                                        tokens <- String processedString :: tokens
                                    elif System.Text.RegularExpressions.Regex.IsMatch(part, @"^x[0-9a-fA-F]+$") then
                                        let hexValue = System.Convert.ToInt32(part.Substring(1), 16)
                                        tokens <- Number hexValue :: tokens
                                    elif System.Text.RegularExpressions.Regex.IsMatch(part, @"^b[01]+$") then
                                        let binValue = System.Convert.ToInt32(part.Substring(1), 2)
                                        tokens <- Number binValue :: tokens
                                    elif System.Text.RegularExpressions.Regex.IsMatch(part, @"^o[0-7]+$") then
                                        let octValue = System.Convert.ToInt32(part.Substring(1), 8)
                                        tokens <- Number octValue :: tokens
                                    elif System.Text.RegularExpressions.Regex.IsMatch(part, @"^-?\d+$") then
                                        tokens <- Number (int part) :: tokens
                                    elif System.Text.RegularExpressions.Regex.IsMatch(part, @"^-?\d+\.\d+$") then
                                        tokens <- FloatNum (float part) :: tokens
                                    elif part = "int" then tokens <- Int :: tokens
                                    elif part = "text" then tokens <- Text :: tokens
                                    elif part = "float" then tokens <- Float :: tokens
                                    elif part = "bool" then tokens <- Bool :: tokens
                                    elif part = "void" then tokens <- Void :: tokens
                                    elif part = "true" then tokens <- True :: tokens
                                    elif part = "false" then tokens <- False :: tokens
                                    elif part = "and" then tokens <- And :: tokens
                                    elif part = "or" then tokens <- Or :: tokens
                                    elif part = "xor" then tokens <- Xor :: tokens
                                    elif part = "if" then tokens <- If :: tokens
                                    elif part = "elsif" then tokens <- Elsif :: tokens
                                    elif part = "else" then tokens <- Else :: tokens
                                    elif part = "while" then tokens <- While :: tokens
                                    elif part = "foreach" then tokens <- Foreach :: tokens
                                    elif part = "in" then tokens <- In :: tokens
                                    elif part = "print" then tokens <- Print :: tokens
                                    elif part = "chiquitita" then tokens <- Chiquitita :: tokens
                                    elif System.Text.RegularExpressions.Regex.IsMatch(part, @"^[a-zA-Z][a-zA-Z0-9_]*$") then
                                        tokens <- Identifier part :: tokens
                                    remaining <- remaining.Substring(endIdx)
                                else
                                    remaining <- ""
                    | s when System.Text.RegularExpressions.Regex.IsMatch(s, @"^[a-zA-Z][a-zA-Z0-9_]*$") ->
                        tokens <- Identifier s :: tokens
                    | _ -> ()
                i <- i + 1
    
    for _ in 1 .. (List.length indentStack - 1) do
        tokens <- Dedent :: tokens
    
    tokens <- EOF :: tokens
    List.rev tokens

let execute tokens =
    let mutable variables = Map.empty<string, Value * VarType>
    let mutable methods = Map.empty<string, Method>
    
    let keywords = ["print"; "if"; "elsif"; "else"; "while"; "foreach"; "in"; "true"; "false"; "int"; "text"; "float"; "bool"; "void"; "chiquitita"; "and"; "or"; "xor"; "mod"; "equals"]
    let isKeyword name = List.contains name keywords
    
    let tryParseNumberLiteral (s: string) =
        if System.Text.RegularExpressions.Regex.IsMatch(s, @"^x[0-9a-fA-F]+$") then
            Some (IntVal (System.Convert.ToInt32(s.Substring(1), 16)))
        elif System.Text.RegularExpressions.Regex.IsMatch(s, @"^b[01]+$") then
            Some (IntVal (System.Convert.ToInt32(s.Substring(1), 2)))
        elif System.Text.RegularExpressions.Regex.IsMatch(s, @"^o[0-7]+$") then
            Some (IntVal (System.Convert.ToInt32(s.Substring(1), 8)))
        else
            None
    
    // First pass: collect all method definitions
    let rec collectMethods tokens =
        match tokens with
        | Int :: Print :: LeftParen :: _ | Int :: If :: LeftParen :: _ | Int :: Elsif :: LeftParen :: _
        | Int :: Else :: LeftParen :: _ | Int :: True :: LeftParen :: _ | Int :: False :: LeftParen :: _
        | Int :: Int :: LeftParen :: _ | Int :: Text :: LeftParen :: _ | Int :: Float :: LeftParen :: _
        | Int :: Bool :: LeftParen :: _ | Int :: Chiquitita :: LeftParen :: _ ->
            failwith "Cannot use keyword as method name"
        | Float :: Print :: LeftParen :: _ | Float :: If :: LeftParen :: _ | Float :: Elsif :: LeftParen :: _
        | Float :: Else :: LeftParen :: _ | Float :: True :: LeftParen :: _ | Float :: False :: LeftParen :: _
        | Float :: Int :: LeftParen :: _ | Float :: Text :: LeftParen :: _ | Float :: Float :: LeftParen :: _
        | Float :: Bool :: LeftParen :: _ | Float :: Chiquitita :: LeftParen :: _ ->
            failwith "Cannot use keyword as method name"
        | Text :: Print :: LeftParen :: _ | Text :: If :: LeftParen :: _ | Text :: Elsif :: LeftParen :: _
        | Text :: Else :: LeftParen :: _ | Text :: True :: LeftParen :: _ | Text :: False :: LeftParen :: _
        | Text :: Int :: LeftParen :: _ | Text :: Text :: LeftParen :: _ | Text :: Float :: LeftParen :: _
        | Text :: Bool :: LeftParen :: _ | Text :: Chiquitita :: LeftParen :: _ ->
            failwith "Cannot use keyword as method name"
        | Bool :: Print :: LeftParen :: _ | Bool :: If :: LeftParen :: _ | Bool :: Elsif :: LeftParen :: _
        | Bool :: Else :: LeftParen :: _ | Bool :: True :: LeftParen :: _ | Bool :: False :: LeftParen :: _
        | Bool :: Int :: LeftParen :: _ | Bool :: Text :: LeftParen :: _ | Bool :: Float :: LeftParen :: _
        | Bool :: Bool :: LeftParen :: _ | Bool :: Chiquitita :: LeftParen :: _ ->
            failwith "Cannot use keyword as method name"
        | Int :: Identifier methodName :: LeftParen :: rest ->
            if isKeyword methodName then failwith $"Cannot use keyword '{methodName}' as method name"
            let (parameters, afterParams) = parseMethodParams rest []
            match afterParams with
            | Indent :: methodBody ->
                let (body, remaining) = parseMethodBlock methodBody []
                let method = { Name = methodName; ReturnType = IntType; Parameters = parameters; Body = body }
                methods <- Map.add methodName method methods
                collectMethods remaining
            | _ -> collectMethods rest
        | Float :: Identifier methodName :: LeftParen :: rest ->
            if isKeyword methodName then failwith $"Cannot use keyword '{methodName}' as method name"
            let (parameters, afterParams) = parseMethodParams rest []
            match afterParams with
            | Indent :: methodBody ->
                let (body, remaining) = parseMethodBlock methodBody []
                let method = { Name = methodName; ReturnType = FloatType; Parameters = parameters; Body = body }
                methods <- Map.add methodName method methods
                collectMethods remaining
            | _ -> collectMethods rest
        | Text :: Identifier methodName :: LeftParen :: rest ->
            if isKeyword methodName then failwith $"Cannot use keyword '{methodName}' as method name"
            let (parameters, afterParams) = parseMethodParams rest []
            match afterParams with
            | Indent :: methodBody ->
                let (body, remaining) = parseMethodBlock methodBody []
                let method = { Name = methodName; ReturnType = TextType; Parameters = parameters; Body = body }
                methods <- Map.add methodName method methods
                collectMethods remaining
            | _ -> collectMethods rest
        | Bool :: Identifier methodName :: LeftParen :: rest ->
            if isKeyword methodName then failwith $"Cannot use keyword '{methodName}' as method name"
            let (parameters, afterParams) = parseMethodParams rest []
            match afterParams with
            | Indent :: methodBody ->
                let (body, remaining) = parseMethodBlock methodBody []
                let method = { Name = methodName; ReturnType = BoolType; Parameters = parameters; Body = body }
                methods <- Map.add methodName method methods
                collectMethods remaining
            | _ -> collectMethods rest
        | Void :: Print :: LeftParen :: _ | Void :: If :: LeftParen :: _ | Void :: Elsif :: LeftParen :: _
        | Void :: Else :: LeftParen :: _ | Void :: True :: LeftParen :: _ | Void :: False :: LeftParen :: _
        | Void :: Int :: LeftParen :: _ | Void :: Text :: LeftParen :: _ | Void :: Float :: LeftParen :: _
        | Void :: Bool :: LeftParen :: _ | Void :: Chiquitita :: LeftParen :: _ | Void :: Void :: LeftParen :: _ ->
            failwith "Cannot use keyword as method name"
        | Void :: Identifier methodName :: LeftParen :: rest ->
            if isKeyword methodName then failwith $"Cannot use keyword '{methodName}' as method name"
            let (parameters, afterParams) = parseMethodParams rest []
            match afterParams with
            | Indent :: methodBody ->
                let (body, remaining) = parseMethodBlock methodBody []
                let method = { Name = methodName; ReturnType = VoidType; Parameters = parameters; Body = body }
                methods <- Map.add methodName method methods
                collectMethods remaining
            | _ -> collectMethods rest
        | [] -> ()
        | _ :: rest -> collectMethods rest
    
    and parseMethodParams tokens acc =
        match tokens with
        | RightParen :: rest -> (List.rev acc, rest)
        | varType :: Identifier paramName :: Comma :: rest ->
            if isKeyword paramName then failwith $"Cannot use keyword '{paramName}' as parameter name"
            let paramType = 
                match varType with
                | Int -> IntType | Float -> FloatType | Text -> TextType | Bool -> BoolType
                | _ -> failwith "Invalid parameter type"
            parseMethodParams rest ((paramName, paramType) :: acc)
        | varType :: Identifier paramName :: RightParen :: rest ->
            if isKeyword paramName then failwith $"Cannot use keyword '{paramName}' as parameter name"
            let paramType = 
                match varType with
                | Int -> IntType | Float -> FloatType | Text -> TextType | Bool -> BoolType
                | _ -> failwith "Invalid parameter type"
            (List.rev ((paramName, paramType) :: acc), rest)
        | RightParen :: rest when List.isEmpty acc -> ([], rest)
        | _ -> failwith "Invalid parameter syntax"
    
    and parseMethodBlock tokens acc =
        match tokens with
        | Dedent :: rest -> (List.rev acc, rest)
        | EOF :: _ -> (List.rev acc, tokens)
        | token :: rest -> parseMethodBlock rest (token :: acc)
        | [] -> (List.rev acc, [])
    
    collectMethods tokens
    
    let rec parseExpression tokens =
        match tokens with
        | Identifier name :: LeftParen :: rest ->
            let (args, remaining) = parseArgs rest []
            match remaining with
            | RightParen :: afterParen ->
                let result = callMethod name args
                (result, afterParen)
            | _ -> failwith "Expected closing parenthesis"
        | Number n :: Plus :: Number m :: rest -> (IntVal (n + m), rest)
        | Number n :: Plus :: FloatNum f :: rest -> (FloatVal (float n + f), rest)
        | FloatNum f :: Plus :: Number n :: rest -> (FloatVal (f + float n), rest)
        | FloatNum f1 :: Plus :: FloatNum f2 :: rest -> (FloatVal (f1 + f2), rest)
        | Identifier var :: Plus :: FloatNum f :: rest ->
            match Map.tryFind var variables with
            | Some (IntVal n, IntType) -> (FloatVal (float n + f), rest)
            | Some (FloatVal v, FloatType) -> (FloatVal (v + f), rest)
            | _ -> failwith "Type mismatch in addition"
        | Number n :: Minus :: Number m :: rest -> (IntVal (n - m), rest)
        | Number n :: Multiply :: Number m :: rest -> (IntVal (n * m), rest)
        | Number n :: Multiply :: FloatNum f :: rest -> (FloatVal (float n * f), rest)
        | FloatNum f :: Multiply :: Number n :: rest -> (FloatVal (f * float n), rest)
        | FloatNum f1 :: Multiply :: FloatNum f2 :: rest -> (FloatVal (f1 * f2), rest)
        | Number n :: Divide :: Number m :: rest -> (IntVal (n / m), rest)
        | Number n :: Mod :: Number m :: rest -> (IntVal (n % m), rest)
        | Number n :: Divide :: FloatNum f :: rest -> (FloatVal (float n / f), rest)
        | FloatNum f :: Divide :: Number n :: rest -> (FloatVal (f / float n), rest)
        | FloatNum f1 :: Divide :: FloatNum f2 :: rest -> (FloatVal (f1 / f2), rest)
        | Number n1 :: GreaterThan :: Number n2 :: rest -> (BoolVal (n1 > n2), rest)
        | Number n :: GreaterThan :: FloatNum f :: rest -> (BoolVal (float n > f), rest)
        | FloatNum f :: GreaterThan :: Number n :: rest -> (BoolVal (f > float n), rest)
        | FloatNum f1 :: GreaterThan :: FloatNum f2 :: rest -> (BoolVal (f1 > f2), rest)
        | Number n1 :: LessThan :: Number n2 :: rest -> (BoolVal (n1 < n2), rest)
        | Number n :: LessThan :: FloatNum f :: rest -> (BoolVal (float n < f), rest)
        | FloatNum f :: LessThan :: Number n :: rest -> (BoolVal (f < float n), rest)
        | FloatNum f1 :: LessThan :: FloatNum f2 :: rest -> (BoolVal (f1 < f2), rest)
        | Number n1 :: GreaterThanOrEqual :: Number n2 :: rest -> (BoolVal (n1 >= n2), rest)
        | Number n :: GreaterThanOrEqual :: FloatNum f :: rest -> (BoolVal (float n >= f), rest)
        | FloatNum f :: GreaterThanOrEqual :: Number n :: rest -> (BoolVal (f >= float n), rest)
        | FloatNum f1 :: GreaterThanOrEqual :: FloatNum f2 :: rest -> (BoolVal (f1 >= f2), rest)
        | Number n1 :: LessThanOrEqual :: Number n2 :: rest -> (BoolVal (n1 <= n2), rest)
        | Number n1 :: Equals :: Number n2 :: rest -> (BoolVal (n1 = n2), rest)
        | FloatNum f1 :: Equals :: FloatNum f2 :: rest -> (BoolVal (f1 = f2), rest)
        | Number n :: Equals :: FloatNum f :: rest -> (BoolVal (float n = f), rest)
        | FloatNum f :: Equals :: Number n :: rest -> (BoolVal (f = float n), rest)
        | Number n :: LessThanOrEqual :: FloatNum f :: rest -> (BoolVal (float n <= f), rest)
        | FloatNum f :: LessThanOrEqual :: Number n :: rest -> (BoolVal (f <= float n), rest)
        | FloatNum f1 :: LessThanOrEqual :: FloatNum f2 :: rest -> (BoolVal (f1 <= f2), rest)
        | Identifier var :: Multiply :: FloatNum f :: rest ->
            match Map.tryFind var variables with
            | Some (IntVal n, IntType) -> (FloatVal (float n * f), rest)
            | Some (FloatVal v, FloatType) -> (FloatVal (v * f), rest)
            | _ -> failwith "Type mismatch in multiplication"
        | Identifier var :: Plus :: Number n :: rest ->
            match Map.tryFind var variables with
            | Some (IntVal v, IntType) -> (IntVal (v + n), rest)
            | Some (FloatVal v, FloatType) -> (FloatVal (v + float n), rest)
            | _ -> failwith "Type mismatch in addition"
        | Identifier var :: Minus :: Number n :: rest ->
            match Map.tryFind var variables with
            | Some (IntVal v, IntType) -> (IntVal (v - n), rest)
            | Some (FloatVal v, FloatType) -> (FloatVal (v - float n), rest)
            | _ -> failwith "Type mismatch in subtraction"
        | Identifier var :: Multiply :: Number n :: rest ->
            match Map.tryFind var variables with
            | Some (IntVal v, IntType) -> (IntVal (v * n), rest)
            | Some (FloatVal v, FloatType) -> (FloatVal (v * float n), rest)
            | _ -> failwith "Type mismatch in multiplication"
        | Identifier var :: Divide :: Number n :: rest ->
            match Map.tryFind var variables with
            | Some (IntVal v, IntType) -> (IntVal (v / n), rest)
            | Some (FloatVal v, FloatType) -> (FloatVal (v / float n), rest)
            | _ -> failwith "Type mismatch in division"
        | Identifier var :: Mod :: Number n :: rest ->
            match Map.tryFind var variables with
            | Some (IntVal v, IntType) -> (IntVal (v % n), rest)
            | _ -> failwith "Modulus requires integer operands"
        | Identifier var :: Divide :: FloatNum f :: rest ->
            match Map.tryFind var variables with
            | Some (IntVal n, IntType) -> (FloatVal (float n / f), rest)
            | Some (FloatVal v, FloatType) -> (FloatVal (v / f), rest)
            | _ -> failwith "Type mismatch in division"
        | Identifier var1 :: Plus :: Identifier var2 :: rest ->
            match Map.tryFind var1 variables, Map.tryFind var2 variables with
            | Some (IntVal v1, IntType), Some (IntVal v2, IntType) -> (IntVal (v1 + v2), rest)
            | Some (FloatVal v1, FloatType), Some (FloatVal v2, FloatType) -> (FloatVal (v1 + v2), rest)
            | Some (IntVal v1, IntType), Some (FloatVal v2, FloatType) -> (FloatVal (float v1 + v2), rest)
            | Some (FloatVal v1, FloatType), Some (IntVal v2, IntType) -> (FloatVal (v1 + float v2), rest)
            | _ -> failwith "Type mismatch in addition"
        | Identifier var1 :: Multiply :: Identifier var2 :: rest ->
            match Map.tryFind var1 variables, Map.tryFind var2 variables with
            | Some (IntVal v1, IntType), Some (IntVal v2, IntType) -> (IntVal (v1 * v2), rest)
            | Some (FloatVal v1, FloatType), Some (FloatVal v2, FloatType) -> (FloatVal (v1 * v2), rest)
            | Some (IntVal v1, IntType), Some (FloatVal v2, FloatType) -> (FloatVal (float v1 * v2), rest)
            | Some (FloatVal v1, FloatType), Some (IntVal v2, IntType) -> (FloatVal (v1 * float v2), rest)
            | _ -> failwith "Type mismatch in multiplication"
        | Identifier var1 :: Divide :: Identifier var2 :: rest ->
            match Map.tryFind var1 variables, Map.tryFind var2 variables with
            | Some (IntVal v1, IntType), Some (IntVal v2, IntType) -> (IntVal (v1 / v2), rest)
            | Some (FloatVal v1, FloatType), Some (FloatVal v2, FloatType) -> (FloatVal (v1 / v2), rest)
            | Some (IntVal v1, IntType), Some (FloatVal v2, FloatType) -> (FloatVal (float v1 / v2), rest)
            | Some (FloatVal v1, FloatType), Some (IntVal v2, IntType) -> (FloatVal (v1 / float v2), rest)
            | _ -> failwith "Type mismatch in division"
        | Identifier var1 :: Mod :: Identifier var2 :: rest ->
            match Map.tryFind var1 variables, Map.tryFind var2 variables with
            | Some (IntVal v1, IntType), Some (IntVal v2, IntType) -> (IntVal (v1 % v2), rest)
            | _ -> failwith "Modulus requires integer operands"
        | Identifier var1 :: GreaterThan :: Identifier var2 :: rest ->
            match Map.tryFind var1 variables, Map.tryFind var2 variables with
            | Some (IntVal v1, IntType), Some (IntVal v2, IntType) -> (BoolVal (v1 > v2), rest)
            | Some (FloatVal v1, FloatType), Some (FloatVal v2, FloatType) -> (BoolVal (v1 > v2), rest)
            | Some (IntVal v1, IntType), Some (FloatVal v2, FloatType) -> (BoolVal (float v1 > v2), rest)
            | Some (FloatVal v1, FloatType), Some (IntVal v2, IntType) -> (BoolVal (v1 > float v2), rest)
            | _ -> failwith "Type mismatch in comparison"
        | Identifier var :: LessThan :: Number n :: rest ->
            match Map.tryFind var variables with
            | Some (IntVal v, IntType) -> (BoolVal (v < n), rest)
            | Some (FloatVal v, FloatType) -> (BoolVal (v < float n), rest)
            | _ -> failwith "Type mismatch in comparison"
        | Identifier var :: GreaterThan :: Number n :: rest ->
            match Map.tryFind var variables with
            | Some (IntVal v, IntType) -> (BoolVal (v > n), rest)
            | Some (FloatVal v, FloatType) -> (BoolVal (v > float n), rest)
            | _ -> failwith "Type mismatch in comparison"
        | Identifier var :: Equals :: Number n :: rest ->
            match Map.tryFind var variables with
            | Some (IntVal v, IntType) -> (BoolVal (v = n), rest)
            | Some (FloatVal v, FloatType) -> (BoolVal (v = float n), rest)
            | _ -> failwith "Type mismatch in comparison"
        | Identifier var :: Equals :: FloatNum f :: rest ->
            match Map.tryFind var variables with
            | Some (IntVal v, IntType) -> (BoolVal (float v = f), rest)
            | Some (FloatVal v, FloatType) -> (BoolVal (v = f), rest)
            | _ -> failwith "Type mismatch in comparison"
        | Identifier var1 :: LessThan :: Identifier var2 :: rest ->
            match Map.tryFind var1 variables, Map.tryFind var2 variables with
            | Some (IntVal v1, IntType), Some (IntVal v2, IntType) -> (BoolVal (v1 < v2), rest)
            | Some (FloatVal v1, FloatType), Some (FloatVal v2, FloatType) -> (BoolVal (v1 < v2), rest)
            | Some (IntVal v1, IntType), Some (FloatVal v2, FloatType) -> (BoolVal (float v1 < v2), rest)
            | Some (FloatVal v1, FloatType), Some (IntVal v2, IntType) -> (BoolVal (v1 < float v2), rest)
            | _ -> failwith "Type mismatch in comparison"
        | Identifier var1 :: GreaterThanOrEqual :: Identifier var2 :: rest ->
            match Map.tryFind var1 variables, Map.tryFind var2 variables with
            | Some (IntVal v1, IntType), Some (IntVal v2, IntType) -> (BoolVal (v1 >= v2), rest)
            | Some (FloatVal v1, FloatType), Some (FloatVal v2, FloatType) -> (BoolVal (v1 >= v2), rest)
            | Some (IntVal v1, IntType), Some (FloatVal v2, FloatType) -> (BoolVal (float v1 >= v2), rest)
            | Some (FloatVal v1, FloatType), Some (IntVal v2, IntType) -> (BoolVal (v1 >= float v2), rest)
            | _ -> failwith "Type mismatch in comparison"
        | Identifier var1 :: LessThanOrEqual :: Identifier var2 :: rest ->
            match Map.tryFind var1 variables, Map.tryFind var2 variables with
            | Some (IntVal v1, IntType), Some (IntVal v2, IntType) -> (BoolVal (v1 <= v2), rest)
            | Some (FloatVal v1, FloatType), Some (FloatVal v2, FloatType) -> (BoolVal (v1 <= v2), rest)
            | Some (IntVal v1, IntType), Some (FloatVal v2, FloatType) -> (BoolVal (float v1 <= v2), rest)
            | Some (FloatVal v1, FloatType), Some (IntVal v2, IntType) -> (BoolVal (v1 <= float v2), rest)
            | _ -> failwith "Type mismatch in comparison"
        | Identifier var1 :: Equals :: Identifier var2 :: rest ->
            match Map.tryFind var1 variables, Map.tryFind var2 variables with
            | Some (IntVal v1, IntType), Some (IntVal v2, IntType) -> (BoolVal (v1 = v2), rest)
            | Some (FloatVal v1, FloatType), Some (FloatVal v2, FloatType) -> (BoolVal (v1 = v2), rest)
            | Some (IntVal v1, IntType), Some (FloatVal v2, FloatType) -> (BoolVal (float v1 = v2), rest)
            | Some (FloatVal v1, FloatType), Some (IntVal v2, IntType) -> (BoolVal (v1 = float v2), rest)
            | Some (TextVal s1, TextType), Some (TextVal s2, TextType) -> (BoolVal (s1 = s2), rest)
            | Some (BoolVal b1, BoolType), Some (BoolVal b2, BoolType) -> (BoolVal (b1 = b2), rest)
            | _ -> failwith "Type mismatch in comparison"
        | Identifier var1 :: And :: Identifier var2 :: rest ->
            match Map.tryFind var1 variables, Map.tryFind var2 variables with
            | Some (BoolVal b1, BoolType), Some (BoolVal b2, BoolType) -> (BoolVal (b1 && b2), rest)
            | _ -> failwith "Logical operators require boolean operands"
        | Identifier var1 :: Or :: Identifier var2 :: rest ->
            match Map.tryFind var1 variables, Map.tryFind var2 variables with
            | Some (BoolVal b1, BoolType), Some (BoolVal b2, BoolType) -> (BoolVal (b1 || b2), rest)
            | _ -> failwith "Logical operators require boolean operands"
        | Identifier var1 :: Xor :: Identifier var2 :: rest ->
            match Map.tryFind var1 variables, Map.tryFind var2 variables with
            | Some (BoolVal b1, BoolType), Some (BoolVal b2, BoolType) -> (BoolVal (b1 <> b2), rest)
            | _ -> failwith "Logical operators require boolean operands"
        | Identifier var :: And :: True :: rest ->
            match Map.tryFind var variables with
            | Some (BoolVal b, BoolType) -> (BoolVal (b && true), rest)
            | _ -> failwith "Logical operators require boolean operands"
        | Identifier var :: And :: False :: rest ->
            match Map.tryFind var variables with
            | Some (BoolVal b, BoolType) -> (BoolVal (b && false), rest)
            | _ -> failwith "Logical operators require boolean operands"
        | Identifier var :: Or :: True :: rest ->
            match Map.tryFind var variables with
            | Some (BoolVal b, BoolType) -> (BoolVal (b || true), rest)
            | _ -> failwith "Logical operators require boolean operands"
        | Identifier var :: Or :: False :: rest ->
            match Map.tryFind var variables with
            | Some (BoolVal b, BoolType) -> (BoolVal (b || false), rest)
            | _ -> failwith "Logical operators require boolean operands"
        | Identifier var :: Xor :: True :: rest ->
            match Map.tryFind var variables with
            | Some (BoolVal b, BoolType) -> (BoolVal (b <> true), rest)
            | _ -> failwith "Logical operators require boolean operands"
        | Identifier var :: Xor :: False :: rest ->
            match Map.tryFind var variables with
            | Some (BoolVal b, BoolType) -> (BoolVal (b <> false), rest)
            | _ -> failwith "Logical operators require boolean operands"
        | True :: Xor :: Identifier var :: rest ->
            match Map.tryFind var variables with
            | Some (BoolVal b, BoolType) -> (BoolVal (true <> b), rest)
            | _ -> failwith "Logical operators require boolean operands"
        | False :: Xor :: Identifier var :: rest ->
            match Map.tryFind var variables with
            | Some (BoolVal b, BoolType) -> (BoolVal (false <> b), rest)
            | _ -> failwith "Logical operators require boolean operands"
        | True :: Equals :: Identifier var :: rest ->
            match Map.tryFind var variables with
            | Some (BoolVal b, BoolType) -> (BoolVal (true = b), rest)
            | _ -> failwith "Equals requires matching types"
        | False :: Equals :: Identifier var :: rest ->
            match Map.tryFind var variables with
            | Some (BoolVal b, BoolType) -> (BoolVal (false = b), rest)
            | _ -> failwith "Equals requires matching types"
        | Identifier var :: Equals :: True :: rest ->
            match Map.tryFind var variables with
            | Some (BoolVal b, BoolType) -> (BoolVal (b = true), rest)
            | _ -> failwith "Equals requires matching types"
        | Identifier var :: Equals :: False :: rest ->
            match Map.tryFind var variables with
            | Some (BoolVal b, BoolType) -> (BoolVal (b = false), rest)
            | _ -> failwith "Equals requires matching types"
        | Identifier name :: LeftParen :: rest when not (Map.containsKey name variables) ->
            let (args, remaining) = parseArgs rest []
            match remaining with
            | RightParen :: And :: afterParen ->
                let result = callMethod name args
                match result with
                | BoolVal b -> 
                    let (rightVal, finalRest) = parseExpression afterParen
                    match rightVal with
                    | BoolVal b2 -> (BoolVal (b && b2), finalRest)
                    | _ -> failwith "Logical operators require boolean operands"
                | _ -> failwith "Logical operators require boolean operands"
            | RightParen :: Or :: afterParen ->
                let result = callMethod name args
                match result with
                | BoolVal b -> 
                    let (rightVal, finalRest) = parseExpression afterParen
                    match rightVal with
                    | BoolVal b2 -> (BoolVal (b || b2), finalRest)
                    | _ -> failwith "Logical operators require boolean operands"
                | _ -> failwith "Logical operators require boolean operands"
            | RightParen :: afterParen ->
                let result = callMethod name args
                (result, afterParen)
            | _ -> failwith "Expected closing parenthesis"
        | True :: And :: Identifier name :: LeftParen :: rest ->
            let (args, remaining) = parseArgs rest []
            match remaining with
            | RightParen :: afterParen ->
                let result = callMethod name args
                match result with
                | BoolVal b -> (BoolVal (true && b), afterParen)
                | _ -> failwith "Logical operators require boolean operands"
            | _ -> failwith "Expected closing parenthesis"
        | False :: And :: Identifier name :: LeftParen :: rest ->
            let (args, remaining) = parseArgs rest []
            match remaining with
            | RightParen :: afterParen ->
                let result = callMethod name args
                match result with
                | BoolVal b -> (BoolVal (false && b), afterParen)
                | _ -> failwith "Logical operators require boolean operands"
            | _ -> failwith "Expected closing parenthesis"
        | True :: Or :: Identifier name :: LeftParen :: rest ->
            let (args, remaining) = parseArgs rest []
            match remaining with
            | RightParen :: afterParen ->
                let result = callMethod name args
                match result with
                | BoolVal b -> (BoolVal (true || b), afterParen)
                | _ -> failwith "Logical operators require boolean operands"
            | _ -> failwith "Expected closing parenthesis"
        | False :: Or :: Identifier name :: LeftParen :: rest ->
            let (args, remaining) = parseArgs rest []
            match remaining with
            | RightParen :: afterParen ->
                let result = callMethod name args
                match result with
                | BoolVal b -> (BoolVal (false || b), afterParen)
                | _ -> failwith "Logical operators require boolean operands"
            | _ -> failwith "Expected closing parenthesis"
        | True :: And :: Identifier var :: rest ->
            match Map.tryFind var variables with
            | Some (BoolVal b, BoolType) -> (BoolVal (true && b), rest)
            | _ -> failwith "Logical operators require boolean operands"
        | False :: And :: Identifier var :: rest ->
            match Map.tryFind var variables with
            | Some (BoolVal b, BoolType) -> (BoolVal (false && b), rest)
            | _ -> failwith "Logical operators require boolean operands"
        | True :: Or :: Identifier var :: rest ->
            match Map.tryFind var variables with
            | Some (BoolVal b, BoolType) -> (BoolVal (true || b), rest)
            | _ -> failwith "Logical operators require boolean operands"
        | False :: Or :: Identifier var :: rest ->
            match Map.tryFind var variables with
            | Some (BoolVal b, BoolType) -> (BoolVal (false || b), rest)
            | _ -> failwith "Logical operators require boolean operands"
        | Number n :: rest -> (IntVal n, rest)
        | FloatNum f :: rest -> (FloatVal f, rest)
        | String s :: rest -> (TextVal s, rest)
        | True :: And :: True :: rest -> (BoolVal true, rest)
        | True :: And :: False :: rest -> (BoolVal false, rest)
        | False :: And :: True :: rest -> (BoolVal false, rest)
        | False :: And :: False :: rest -> (BoolVal false, rest)
        | True :: Or :: True :: rest -> (BoolVal true, rest)
        | True :: Or :: False :: rest -> (BoolVal true, rest)
        | False :: Or :: True :: rest -> (BoolVal true, rest)
        | False :: Or :: False :: rest -> (BoolVal false, rest)
        | True :: Xor :: True :: rest -> (BoolVal false, rest)
        | True :: Xor :: False :: rest -> (BoolVal true, rest)
        | False :: Xor :: True :: rest -> (BoolVal true, rest)
        | False :: Xor :: False :: rest -> (BoolVal false, rest)
        | True :: Equals :: True :: rest -> (BoolVal true, rest)
        | True :: Equals :: False :: rest -> (BoolVal false, rest)
        | False :: Equals :: True :: rest -> (BoolVal false, rest)
        | False :: Equals :: False :: rest -> (BoolVal true, rest)
        | True :: rest -> (BoolVal true, rest)
        | False :: rest -> (BoolVal false, rest)
        | Identifier varName :: LeftBracket :: Number index :: RightBracket :: rest ->
            match Map.tryFind varName variables with
            | Some (ArrayVal (elements, _), _) ->
                if index < 1 || index > List.length elements then
                    failwith $"Array index {index} out of bounds (1-{List.length elements})"
                (List.item (index - 1) elements, rest)
            | _ -> failwith $"Variable '{varName}' is not an array"
        | Identifier varName :: LeftBracket :: Identifier indexVar :: RightBracket :: rest ->
            match Map.tryFind varName variables, Map.tryFind indexVar variables with
            | Some (ArrayVal (elements, _), _), Some (IntVal index, IntType) ->
                if index < 1 || index > List.length elements then
                    failwith $"Array index {index} out of bounds (1-{List.length elements})"
                (List.item (index - 1) elements, rest)
            | Some (ArrayVal _, _), _ -> failwith $"Array index must be an integer"
            | _ -> failwith $"Variable '{varName}' is not an array"
        | Identifier varName :: Dot :: Identifier "length" :: rest ->
            match Map.tryFind varName variables with
            | Some (ArrayVal (elements, _), _) -> (IntVal (List.length elements), rest)
            | _ -> failwith $"Variable '{varName}' is not an array"
        | Identifier varName :: rest ->
            match Map.tryFind varName variables with
            | Some (value, _) -> (value, rest)
            | None -> 
                match rest with
                | LeftParen :: _ -> failwith $"Method '{varName}' not found"
                | _ -> failwith $"Variable '{varName}' not found"
        | _ -> failwith "Invalid expression"
    
    and parseArrayElements tokens acc =
        match tokens with
        | RightBracket :: rest -> (List.rev acc, rest)
        | Number n :: Comma :: rest -> parseArrayElements rest (IntVal n :: acc)
        | Number n :: RightBracket :: rest -> (List.rev (IntVal n :: acc), RightBracket :: rest)
        | FloatNum f :: Comma :: rest -> parseArrayElements rest (FloatVal f :: acc)
        | FloatNum f :: RightBracket :: rest -> (List.rev (FloatVal f :: acc), RightBracket :: rest)
        | String s :: Comma :: rest -> parseArrayElements rest (TextVal s :: acc)
        | String s :: RightBracket :: rest -> (List.rev (TextVal s :: acc), RightBracket :: rest)
        | True :: Comma :: rest -> parseArrayElements rest (BoolVal true :: acc)
        | True :: RightBracket :: rest -> (List.rev (BoolVal true :: acc), RightBracket :: rest)
        | False :: Comma :: rest -> parseArrayElements rest (BoolVal false :: acc)
        | False :: RightBracket :: rest -> (List.rev (BoolVal false :: acc), RightBracket :: rest)
        | Identifier varName :: Comma :: rest ->
            match Map.tryFind varName variables with
            | Some (value, _) -> parseArrayElements rest (value :: acc)
            | None ->
                match tryParseNumberLiteral varName with
                | Some value -> parseArrayElements rest (value :: acc)
                | None -> failwith $"Variable '{varName}' not found"
        | Identifier varName :: RightBracket :: rest ->
            match Map.tryFind varName variables with
            | Some (value, _) -> (List.rev (value :: acc), RightBracket :: rest)
            | None ->
                match tryParseNumberLiteral varName with
                | Some value -> (List.rev (value :: acc), RightBracket :: rest)
                | None -> failwith $"Variable '{varName}' not found"
        | [] -> failwith "Unexpected end of array"
        | token :: _ -> failwith $"Invalid array element: {token}"
    
    and parseArgs tokens acc =
        match tokens with
        | RightParen :: _ -> (List.rev acc, tokens)
        | [] -> (List.rev acc, [])
        | Number n :: Comma :: rest -> parseArgs rest (IntVal n :: acc)
        | Number n :: rest -> parseArgs rest (IntVal n :: acc)
        | FloatNum f :: Comma :: rest -> parseArgs rest (FloatVal f :: acc)
        | FloatNum f :: rest -> parseArgs rest (FloatVal f :: acc)
        | String s :: Comma :: rest -> parseArgs rest (TextVal s :: acc)
        | String s :: rest -> parseArgs rest (TextVal s :: acc)
        | Identifier varName :: Comma :: rest ->
            match Map.tryFind varName variables with
            | Some (value, _) -> parseArgs rest (value :: acc)
            | None -> failwith $"Variable '{varName}' not found"
        | Identifier varName :: rest ->
            match Map.tryFind varName variables with
            | Some (value, _) -> parseArgs rest (value :: acc)
            | None -> failwith $"Variable '{varName}' not found"
        | _ -> failwith "Invalid argument"
    
    and callMethod name args =
        match Map.tryFind name methods with
        | Some method ->
            if List.length args <> List.length method.Parameters then
                failwith $"Method '{name}' expects {List.length method.Parameters} arguments, got {List.length args}"
            for (arg, (paramName, paramType)) in List.zip args method.Parameters do
                variables <- Map.add paramName (arg, paramType) variables
            let result = executeMethodBody method.Body method.ReturnType
            for (paramName, _) in method.Parameters do
                variables <- Map.remove paramName variables
            result
        | None -> 
            let methodNames = Map.keys methods |> String.concat ", "
            failwith $"Method '{name}' not found. Available methods: {methodNames}"
    
    and executeMethodBody tokens returnType =
        let hasChiq = tokens |> List.exists (fun t -> t = Chiquitita)
        match returnType with
        | VoidType ->
            if hasChiq then
                // Check if chiq has a value after it
                let rec checkChiq toks =
                    match toks with
                    | Chiquitita :: Dedent :: _ | Chiquitita :: EOF :: _ | Chiquitita :: [] -> () // chiquitita without value is ok
                    | Chiquitita :: Number _ :: _ | Chiquitita :: FloatNum _ :: _ | Chiquitita :: Identifier _ :: _ 
                    | Chiquitita :: String _ :: _ | Chiquitita :: True :: _ | Chiquitita :: False :: _ -> 
                        failwith "Void method cannot return a value"
                    | _ :: rest -> checkChiq rest
                    | [] -> ()
                checkChiq tokens
            // Execute the void method body
            executeHelper tokens
            IntVal 0 // Dummy return for void
        | _ ->
            if not hasChiq then failwith "Non-void method must have a return statement"
            match tokens with
            | Chiquitita :: rest ->
                let (value, _) = parseExpression rest
                value
            | _ :: rest -> executeMethodBody rest returnType
            | [] -> failwith "Non-void method must have a return statement"
    
    and interpolateString (text: string) =
        let mutable result = text
        let pattern = @"<<EXPR:(.+?)>>"
        let matches = System.Text.RegularExpressions.Regex.Matches(text, pattern)
        for m in matches do
            let expr = m.Groups.[1].Value
            let value = evaluateStringExpr expr
            let stringValue = 
                match value with
                | IntVal n -> string n | TextVal s -> s | FloatVal f -> string f | BoolVal b -> string b
                | ArrayVal _ -> "[array]"
            result <- result.Replace(m.Value, stringValue)
        result
    
    and evaluateStringExpr (expr: string) =
        if expr.Contains("[") && expr.Contains("]") && not (expr.Contains("(")) then
            let bracketStart = expr.IndexOf('[')
            let bracketEnd = expr.LastIndexOf(']')
            if bracketEnd > bracketStart then
                let varName = expr.Substring(0, bracketStart).Trim()
                let indexStr = expr.Substring(bracketStart + 1, bracketEnd - bracketStart - 1).Trim()
                match Map.tryFind varName variables with
                | Some (ArrayVal (elements, _), _) ->
                    let index = 
                        if System.Text.RegularExpressions.Regex.IsMatch(indexStr, @"^\d+$") then
                            int indexStr
                        else
                            match Map.tryFind indexStr variables with
                            | Some (IntVal i, IntType) -> i
                            | _ -> failwith $"Array index must be an integer"
                    if index < 1 || index > List.length elements then
                        failwith $"Array index {index} out of bounds (1-{List.length elements})"
                    List.item (index - 1) elements
                | _ -> failwith $"Variable '{varName}' is not an array"
            else
                failwith $"Invalid array access: {expr}"
        elif expr.Contains(".length") && not (expr.Contains("[")) then
            let varName = expr.Replace(".length", "").Trim()
            match Map.tryFind varName variables with
            | Some (ArrayVal (elements, _), _) -> IntVal (List.length elements)
            | _ -> failwith $"Variable '{varName}' is not an array"
        elif expr.Contains("(") && expr.Contains(")") then
            let parenStart = expr.IndexOf('(')
            let parenEnd = expr.LastIndexOf(')')
            if parenStart > 0 && parenEnd > parenStart then
                let methodName = expr.Substring(0, parenStart).Trim()
                let argsStr = expr.Substring(parenStart + 1, parenEnd - parenStart - 1).Trim()
                let args = 
                    if argsStr = "" then []
                    else argsStr.Split(',') |> Array.map (fun s -> parseStringValue (s.Trim())) |> Array.toList
                callMethod methodName args
            else
                failwith $"Invalid method call: {expr}"
        elif expr.Contains("*") then
            let parts = expr.Split('*')
            if parts.Length = 2 then
                let v1 = parseStringValue (parts.[0].Trim())
                let v2 = parseStringValue (parts.[1].Trim())
                match v1, v2 with
                | IntVal n1, IntVal n2 -> IntVal (n1 * n2)
                | IntVal n, FloatVal f -> FloatVal (float n * f)
                | FloatVal f, IntVal n -> FloatVal (f * float n)
                | FloatVal f1, FloatVal f2 -> FloatVal (f1 * f2)
                | _ -> failwith "Type mismatch in multiplication"
            else
                failwith $"Invalid expression: {expr}"
        elif expr.Contains("/") then
            let parts = expr.Split('/')
            if parts.Length = 2 then
                let v1 = parseStringValue (parts.[0].Trim())
                let v2 = parseStringValue (parts.[1].Trim())
                match v1, v2 with
                | IntVal n1, IntVal n2 -> IntVal (n1 / n2)
                | IntVal n, FloatVal f -> FloatVal (float n / f)
                | FloatVal f, IntVal n -> FloatVal (f / float n)
                | FloatVal f1, FloatVal f2 -> FloatVal (f1 / f2)
                | _ -> failwith "Type mismatch in division"
            else
                failwith $"Invalid expression: {expr}"
        elif expr.Contains(" mod ") then
            let parts = expr.Split([|" mod "|], StringSplitOptions.None)
            if parts.Length = 2 then
                let v1 = parseStringValue (parts.[0].Trim())
                let v2 = parseStringValue (parts.[1].Trim())
                match v1, v2 with
                | IntVal n1, IntVal n2 -> IntVal (n1 % n2)
                | _ -> failwith "Modulus requires integer operands"
            else
                failwith $"Invalid expression: {expr}"
        elif expr.Contains(">=") then
            let parts = expr.Split([|">=" |], StringSplitOptions.None)
            if parts.Length = 2 then
                let v1 = parseStringValue (parts.[0].Trim())
                let v2 = parseStringValue (parts.[1].Trim())
                match v1, v2 with
                | IntVal n1, IntVal n2 -> BoolVal (n1 >= n2)
                | IntVal n, FloatVal f -> BoolVal (float n >= f)
                | FloatVal f, IntVal n -> BoolVal (f >= float n)
                | FloatVal f1, FloatVal f2 -> BoolVal (f1 >= f2)
                | _ -> failwith "Type mismatch in comparison"
            else
                failwith $"Invalid expression: {expr}"
        elif expr.Contains("<=") then
            let parts = expr.Split([|"<=" |], StringSplitOptions.None)
            if parts.Length = 2 then
                let v1 = parseStringValue (parts.[0].Trim())
                let v2 = parseStringValue (parts.[1].Trim())
                match v1, v2 with
                | IntVal n1, IntVal n2 -> BoolVal (n1 <= n2)
                | IntVal n, FloatVal f -> BoolVal (float n <= f)
                | FloatVal f, IntVal n -> BoolVal (f <= float n)
                | FloatVal f1, FloatVal f2 -> BoolVal (f1 <= f2)
                | _ -> failwith "Type mismatch in comparison"
            else
                failwith $"Invalid expression: {expr}"
        elif expr.Contains(">") then
            let parts = expr.Split('>')
            if parts.Length = 2 then
                let v1 = parseStringValue (parts.[0].Trim())
                let v2 = parseStringValue (parts.[1].Trim())
                match v1, v2 with
                | IntVal n1, IntVal n2 -> BoolVal (n1 > n2)
                | IntVal n, FloatVal f -> BoolVal (float n > f)
                | FloatVal f, IntVal n -> BoolVal (f > float n)
                | FloatVal f1, FloatVal f2 -> BoolVal (f1 > f2)
                | _ -> failwith "Type mismatch in comparison"
            else
                failwith $"Invalid expression: {expr}"
        elif expr.Contains("<") then
            let parts = expr.Split('<')
            if parts.Length = 2 then
                let v1 = parseStringValue (parts.[0].Trim())
                let v2 = parseStringValue (parts.[1].Trim())
                match v1, v2 with
                | IntVal n1, IntVal n2 -> BoolVal (n1 < n2)
                | IntVal n, FloatVal f -> BoolVal (float n < f)
                | FloatVal f, IntVal n -> BoolVal (f < float n)
                | FloatVal f1, FloatVal f2 -> BoolVal (f1 < f2)
                | _ -> failwith "Type mismatch in comparison"
            else
                failwith $"Invalid expression: {expr}"
        elif expr.Contains(" and ") then
            let parts = expr.Split([|" and " |], StringSplitOptions.None)
            if parts.Length = 2 then
                let v1 = parseStringValue (parts.[0].Trim())
                let v2 = parseStringValue (parts.[1].Trim())
                match v1, v2 with
                | BoolVal b1, BoolVal b2 -> BoolVal (b1 && b2)
                | _ -> failwith "Logical operators require boolean operands"
            else
                failwith $"Invalid expression: {expr}"
        elif expr.Contains(" or ") then
            let parts = expr.Split([|" or " |], StringSplitOptions.None)
            if parts.Length = 2 then
                let v1 = parseStringValue (parts.[0].Trim())
                let v2 = parseStringValue (parts.[1].Trim())
                match v1, v2 with
                | BoolVal b1, BoolVal b2 -> BoolVal (b1 || b2)
                | _ -> failwith "Logical operators require boolean operands"
            else
                failwith $"Invalid expression: {expr}"
        elif expr.Contains(" xor ") then
            let parts = expr.Split([|" xor " |], StringSplitOptions.None)
            if parts.Length = 2 then
                let v1 = parseStringValue (parts.[0].Trim())
                let v2 = parseStringValue (parts.[1].Trim())
                match v1, v2 with
                | BoolVal b1, BoolVal b2 -> BoolVal (b1 <> b2)
                | _ -> failwith "Logical operators require boolean operands"
            else
                failwith $"Invalid expression: {expr}"
        elif expr.Contains(" equals ") then
            let parts = expr.Split([|" equals " |], StringSplitOptions.None)
            if parts.Length = 2 then
                let v1 = parseStringValue (parts.[0].Trim())
                let v2 = parseStringValue (parts.[1].Trim())
                match v1, v2 with
                | IntVal n1, IntVal n2 -> BoolVal (n1 = n2)
                | FloatVal f1, FloatVal f2 -> BoolVal (f1 = f2)
                | IntVal n, FloatVal f -> BoolVal (float n = f)
                | FloatVal f, IntVal n -> BoolVal (f = float n)
                | TextVal s1, TextVal s2 -> BoolVal (s1 = s2)
                | BoolVal b1, BoolVal b2 -> BoolVal (b1 = b2)
                | _ -> failwith "Equals requires matching types"
            else
                failwith $"Invalid expression: {expr}"
        elif expr.Contains("+") then
            let parts = expr.Split('+')
            if parts.Length = 2 then
                let v1 = parseStringValue (parts.[0].Trim())
                let v2 = parseStringValue (parts.[1].Trim())
                match v1, v2 with
                | IntVal n1, IntVal n2 -> IntVal (n1 + n2)
                | IntVal n, FloatVal f -> FloatVal (float n + f)
                | FloatVal f, IntVal n -> FloatVal (f + float n)
                | FloatVal f1, FloatVal f2 -> FloatVal (f1 + f2)
                | _ -> failwith "Type mismatch in addition"
            else
                failwith $"Invalid expression: {expr}"
        else
            parseStringValue expr
    
    and parseStringValue (str: string) =
        let trimmed = str.Trim()
        if trimmed = "true" then
            BoolVal true
        elif trimmed = "false" then
            BoolVal false
        elif System.Text.RegularExpressions.Regex.IsMatch(trimmed, @"^-?\d+$") then
            IntVal (int trimmed)
        elif System.Text.RegularExpressions.Regex.IsMatch(trimmed, @"^-?\d+\.\d+$") then
            FloatVal (float trimmed)
        elif System.Text.RegularExpressions.Regex.IsMatch(trimmed, @"^[a-zA-Z][a-zA-Z0-9_]*$") then
            match Map.tryFind trimmed variables with
            | Some (value, _) -> value
            | None ->
                match tryParseNumberLiteral trimmed with
                | Some value -> value
                | None -> failwith $"Variable '{trimmed}' not found"
        else
            failwith $"Invalid value: {trimmed}"
    
    and executeHelper tokens =
        match tokens with
        | Indent :: rest -> executeHelper rest
        | Dedent :: rest -> executeHelper rest
        | Print :: String text :: rest ->
            let interpolatedText = interpolateString text
            Console.WriteLine(interpolatedText)
            executeHelper rest
        | Print :: Identifier varName :: rest ->
            match Map.tryFind varName variables with
            | Some (TextVal s, TextType) -> 
                let interpolatedText = interpolateString s
                Console.WriteLine(interpolatedText)
            | Some (IntVal n, IntType) -> Console.WriteLine(n)
            | Some (FloatVal f, FloatType) -> Console.WriteLine(f)
            | Some (BoolVal b, BoolType) -> Console.WriteLine(b)
            | None -> failwith $"Variable '{varName}' not found"
            | _ -> failwith "Cannot print this type"
            executeHelper rest
        | Int :: Identifier _ :: LeftParen :: rest
        | Float :: Identifier _ :: LeftParen :: rest
        | Text :: Identifier _ :: LeftParen :: rest
        | Bool :: Identifier _ :: LeftParen :: rest
        | Void :: Identifier _ :: LeftParen :: rest ->
            // Skip method definitions in second pass
            let (_, afterParams) = parseMethodParams rest []
            match afterParams with
            | Indent :: methodBody ->
                let (_, remaining) = parseMethodBlock methodBody []
                executeHelper remaining
            | _ -> executeHelper rest
        | Int :: Print :: _ | Int :: If :: _ | Int :: Elsif :: _ | Int :: Else :: _
        | Int :: True :: _ | Int :: False :: _ | Int :: Int :: _ | Int :: Text :: _
        | Int :: Float :: _ | Int :: Bool :: _ | Int :: Chiquitita :: _ ->
            failwith "Cannot use keyword as variable name"
        | Text :: Print :: _ | Text :: If :: _ | Text :: Elsif :: _ | Text :: Else :: _
        | Text :: True :: _ | Text :: False :: _ | Text :: Int :: _ | Text :: Text :: _
        | Text :: Float :: _ | Text :: Bool :: _ | Text :: Chiquitita :: _ ->
            failwith "Cannot use keyword as variable name"
        | Float :: Print :: _ | Float :: If :: _ | Float :: Elsif :: _ | Float :: Else :: _
        | Float :: True :: _ | Float :: False :: _ | Float :: Int :: _ | Float :: Text :: _
        | Float :: Float :: _ | Float :: Bool :: _ | Float :: Chiquitita :: _ ->
            failwith "Cannot use keyword as variable name"
        | Bool :: Print :: _ | Bool :: If :: _ | Bool :: Elsif :: _ | Bool :: Else :: _
        | Bool :: True :: _ | Bool :: False :: _ | Bool :: Int :: _ | Bool :: Text :: _
        | Bool :: Float :: _ | Bool :: Bool :: _ | Bool :: Chiquitita :: _ | Bool :: Void :: _ ->
            failwith "Cannot use keyword as variable name"
        | If :: rest ->
            let (condition, _) = parseExpression rest
            let rec findIndent tokens =
                match tokens with
                | Indent :: body -> (body, tokens)
                | _ :: remaining -> findIndent remaining
                | [] -> failwith "Expected indented block after if"
            let (ifBody, _) = findIndent rest
            let (body, remaining) = parseMethodBlock ifBody []
            match condition with
            | BoolVal true ->
                executeHelper body
                // Skip else/elsif blocks
                let rec skipElse tokens =
                    match tokens with
                    | Elsif :: _ | Else :: _ ->
                        let rec skipBlock tokens depth =
                            match tokens with
                            | Indent :: rest -> skipBlock rest (depth + 1)
                            | Dedent :: rest when depth > 1 -> skipBlock rest (depth - 1)
                            | Dedent :: rest -> skipElse rest
                            | _ :: rest -> skipBlock rest depth
                            | [] -> []
                        skipBlock tokens 0
                    | _ :: rest -> skipElse rest
                    | [] -> []
                executeHelper (skipElse remaining)
            | BoolVal false ->
                // Check for elsif/else
                let rec findElseBlock tokens =
                    match tokens with
                    | Elsif :: rest ->
                        let (elsifCondition, _) = parseExpression rest
                        let (elsifBody, _) = findIndent rest
                        let (body, remaining) = parseMethodBlock elsifBody []
                        match elsifCondition with
                        | BoolVal true ->
                            executeHelper body
                            let rec skipRest tokens =
                                match tokens with
                                | Elsif :: _ | Else :: _ ->
                                    let rec skipBlock tokens depth =
                                        match tokens with
                                        | Indent :: rest -> skipBlock rest (depth + 1)
                                        | Dedent :: rest when depth > 1 -> skipBlock rest (depth - 1)
                                        | Dedent :: rest -> skipRest rest
                                        | _ :: rest -> skipBlock rest depth
                                        | [] -> []
                                    skipBlock tokens 0
                                | _ :: rest -> skipRest rest
                                | [] -> []
                            executeHelper (skipRest remaining)
                        | BoolVal false -> findElseBlock remaining
                        | _ -> failwith "Elsif condition must be a boolean"
                    | Else :: rest ->
                        let (elseBody, _) = findIndent rest
                        let (body, remaining) = parseMethodBlock elseBody []
                        executeHelper body
                        executeHelper remaining
                    | _ :: rest -> findElseBlock rest
                    | [] -> executeHelper []
                findElseBlock remaining
            | _ -> failwith "If condition must be a boolean"
        | Elsif :: _ | Else :: _ ->
            // These should be handled by if statement, skip if encountered standalone
            executeHelper []
        | While :: rest ->
            let conditionTokens = rest
            let rec findIndent tokens =
                match tokens with
                | Indent :: body -> (body, tokens)
                | _ :: remaining -> findIndent remaining
                | [] -> failwith "Expected indented block after while"
            let (loopBody, indentStart) = findIndent rest
            let (body, remaining) = parseMethodBlock loopBody []
            let rec executeWhile () =
                let (currentCondition, _) = parseExpression conditionTokens
                match currentCondition with
                | BoolVal true ->
                    executeHelper body
                    executeWhile ()
                | BoolVal false -> ()
                | _ -> failwith "While condition must be a boolean"
            executeWhile ()
            executeHelper remaining
        | Foreach :: varType :: Identifier varName :: In :: Identifier arrayName :: rest ->
            if isKeyword varName then failwith $"Cannot use keyword '{varName}' as variable name"
            match Map.tryFind arrayName variables with
            | Some (ArrayVal (elements, elemType), _) ->
                let expectedType = 
                    match varType with
                    | Int -> IntType | Text -> TextType | Float -> FloatType | Bool -> BoolType
                    | _ -> failwith "Invalid foreach variable type"
                if expectedType <> elemType then
                    failwith $"Foreach variable type must match array element type"
                let rec findIndent tokens =
                    match tokens with
                    | Indent :: body -> (body, tokens)
                    | _ :: remaining -> findIndent remaining
                    | [] -> failwith "Expected indented block after foreach"
                let (loopBody, _) = findIndent rest
                let (body, remaining) = parseMethodBlock loopBody []
                for element in elements do
                    variables <- Map.add varName (element, expectedType) variables
                    executeHelper body
                variables <- Map.remove varName variables
                executeHelper remaining
            | _ -> failwith $"Variable '{arrayName}' is not an array"
        | Void :: _ ->
            failwith "Void can only be used as a method return type"
        | Identifier methodName :: LeftParen :: rest ->
            // Method call as statement (for void methods)
            let (args, remaining) = parseArgs rest []
            match remaining with
            | RightParen :: afterParen ->
                callMethod methodName args |> ignore
                executeHelper afterParen
            | _ -> executeHelper rest
        | Identifier varName :: Assign :: rest ->
            if isKeyword varName then failwith $"Cannot use keyword '{varName}' as variable name"
            match Map.tryFind varName variables with
            | Some (_, varType) ->
                let (value, remaining) = 
                    match rest with
                    | Identifier litName :: restAfter when not (Map.containsKey litName variables) ->
                        match tryParseNumberLiteral litName with
                        | Some litVal -> (litVal, restAfter)
                        | None -> parseExpression rest
                    | _ -> parseExpression rest
                let valueObj = 
                    match varType, value with
                    | IntType, IntVal n -> IntVal n
                    | TextType, TextVal s -> TextVal s
                    | FloatType, FloatVal f -> FloatVal f
                    | BoolType, BoolVal b -> BoolVal b
                    | FloatType, IntVal n -> FloatVal (float n)
                    | _ -> failwith $"Type mismatch in reassignment"
                variables <- Map.add varName (valueObj, varType) variables
                executeHelper remaining
            | None -> failwith $"Variable '{varName}' not declared"
        | varType :: LeftBracket :: RightBracket :: Identifier varName :: Assign :: LeftBracket :: rest ->
            if isKeyword varName then failwith $"Cannot use keyword '{varName}' as variable name"
            if Map.containsKey varName variables then failwith $"Variable '{varName}' is already declared"
            let (arrayElements, remaining) = parseArrayElements rest []
            let (arrayType, arrayVal) = 
                match varType with
                | Int -> (IntArrayType, ArrayVal (arrayElements, IntType))
                | Text -> (TextArrayType, ArrayVal (arrayElements, TextType))
                | Float -> (FloatArrayType, ArrayVal (arrayElements, FloatType))
                | Bool -> (BoolArrayType, ArrayVal (arrayElements, BoolType))
                | _ -> failwith "Invalid array type"
            variables <- Map.add varName (arrayVal, arrayType) variables
            executeHelper remaining
        | varType :: Identifier varName :: Assign :: rest ->
            if isKeyword varName then failwith $"Cannot use keyword '{varName}' as variable name"
            if Map.containsKey varName variables then failwith $"Variable '{varName}' is already declared"
            let (value, remaining) = 
                match rest with
                | Identifier litName :: restAfter when not (Map.containsKey litName variables) ->
                    match tryParseNumberLiteral litName with
                    | Some litVal -> (litVal, restAfter)
                    | None -> parseExpression rest
                | _ -> parseExpression rest
            let (varTypeEnum, valueObj) = 
                match varType, value with
                | Int, IntVal n -> (IntType, IntVal n)
                | Text, TextVal s -> (TextType, TextVal s)
                | Float, FloatVal f -> (FloatType, FloatVal f)
                | Bool, BoolVal b -> (BoolType, BoolVal b)
                | Float, IntVal n -> (FloatType, FloatVal (float n))
                | _ -> failwith $"Type mismatch in declaration: cannot assign {value} to {varType}"
            variables <- Map.add varName (valueObj, varTypeEnum) variables
            executeHelper remaining
        | EOF :: [] -> ()
        | [] -> ()
        | _ :: rest -> executeHelper rest
    

    
    executeHelper tokens

[<EntryPoint>]
let main args =
    match args with
    | [| filename |] when File.Exists(filename) ->
        try
            let content = File.ReadAllText(filename)
            let tokens = tokenize content
            execute tokens
            0
        with
        | ex -> 
            Console.WriteLine($"Error: {ex.Message}")
            1
    | [| filename |] ->
        Console.WriteLine($"File not found: {filename}")
        1
    | _ ->
        Console.WriteLine("Usage: dotnet run <filename.chiq>")
        Console.WriteLine("Example: dotnet run hello.chiq")
        1