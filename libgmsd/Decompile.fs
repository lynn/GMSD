namespace libgmsd

open Disassemble

/// Functions related to turning a Game Maker VM disassembly into human-
/// readable code. This stuff is a big hack, for now -- you'll see.
/// 
/// Note: decompilation forgets *all* type info for now, because GMSD is
/// more about you being able to read code than the result actually being
/// executable.

module Decompile =
    // We'll turn the bytecode (which is stack-based) into Statements, which
    // operate on Expressions, by folding tiny pieces of code together:
    //
    //     push 4           
    //     push a.b         push 4 + a.b  
    //     add         ->   push "x"       ->   push f(4 + a.b, "x")
    //     push "x"         call[2] f     
    //     call[2] f    
    
    /// A decompiled expression.
    type Expression =
    // Push instructions <-> value expressions
    | EDouble of double
    | EInt64 of int64
    | EFloat of single
    | EInt32 of int32
    | EBoolean of bool
    | EInt24 of int32
    | EString of uint32
    | EInt16 of int16
    | EVariable of InstanceType * Reference
    // Math stack operations <-> operator expressions
    | EMul of Expression * Expression
    | EDiv of Expression * Expression
    | EIdv of Expression * Expression
    | ERem of Expression * Expression
    | EAdd of Expression * Expression
    | ESub of Expression * Expression
    | EAnd of Expression * Expression
    | EOr  of Expression * Expression
    | EXor of Expression * Expression
    | ENeg of Expression
    | ENot of Expression
    | EShl of Expression * Expression
    | EShr of Expression * Expression
    | ESlt of Expression * Expression
    | ESle of Expression * Expression
    | ESeq of Expression * Expression
    | ESne of Expression * Expression
    | ESge of Expression * Expression
    | ESgt of Expression * Expression
    // Call instructions <-> call expressions (function and argument list)
    | ECall of Reference * Expression list
    | EIndex of InstanceType * Reference * Expression
    
    let tryDoubleOpToUnaryExpression : DoubleOp -> (Expression -> Expression) option =
        function
        | Neg -> Some ENeg
        | Not -> Some ENot
        | _   -> None

    let tryDoubleOpToBinaryExpression : DoubleOp -> (Expression * Expression -> Expression) option =
        function
        | Mul -> Some EMul
        | Div -> Some EDiv
        | Idv -> Some EIdv
        | Rem -> Some ERem
        | Add -> Some EAdd
        | Sub -> Some ESub
        | And -> Some EAnd
        | Xor -> Some EXor
        | Or  -> Some EOr 
        | Shl -> Some EShl
        | Shr -> Some EShr
        | Slt -> Some ESlt
        | Sle -> Some ESle
        | Seq -> Some ESeq
        | Sne -> Some ESne
        | Sge -> Some ESge
        | Sgt -> Some ESgt
        | _   -> None

    // Show an expression at some "precedence level". (I totally stole this
    // from Haskell. Essentially, it decides whether or not to surround an
    // operator with parentheses: if the inner operator's precedence is
    // lesser than that of the outer operator, we need parens, as in the
    // expression `(3 + 4) * 5`.)
    let rec showExpression' (form : Unpack.Form) (prec : int) : Expression -> string =
        let showRef = showReference form
        
        let showUn (p : int) (op : string) (r : Expression) : string =
            let s = sprintf "%s%s" op (showExpression' form p r)
            sprintf (if (p < prec) then "(%s)" else "%s") s
        let showBin (p : int) (op : string) (l : Expression) (r : Expression) : string =
            let s = sprintf "%s %s %s" (showExpression' form p l) op (showExpression' form p r)
            sprintf (if (p < prec) then "(%s)" else "%s") s
        
        function
        | EDouble(x)       -> sprintf "%A" x
        | EInt64(x)        -> sprintf "%A" x
        | EFloat(x)        -> sprintf "%A" x
        | EInt32(x)        -> sprintf "%A" x
        | EBoolean(x)      -> sprintf "%A" x
        | EInt24(x)        -> sprintf "%A" x
        | EString(i)       -> sprintf "%A" (form.StringIndexMap.[i])
        | EInt16(x)        -> sprintf "%A" x
        | EVariable(it, r) -> sprintf "%A.%s" it (showRef r)

        | ENeg(a) -> showUn 8 "-" a
        | ENot(a) -> showUn 8 "!" a
        | EMul(a, b) -> showBin 7 "*" a b
        | EDiv(a, b) -> showBin 7 "/" a b
        | EIdv(a, b) -> showBin 7 "div" a b
        | ERem(a, b) -> showBin 7 "%" a b
        | EAdd(a, b) -> showBin 6 "+" a b
        | ESub(a, b) -> showBin 6 "-" a b
        | EAnd(a, b) -> showBin 5 "&" a b
        | EXor(a, b) -> showBin 4 "^" a b
        | EOr (a, b) -> showBin 3 "|" a b
        | EShl(a, b) -> showBin 2 "<<" a b
        | EShr(a, b) -> showBin 2 ">>" a b
        | ESlt(a, b) -> showBin 1 "<" a b
        | ESle(a, b) -> showBin 1 "<=" a b
        | ESeq(a, b) -> showBin 1 "==" a b
        | ESne(a, b) -> showBin 1 "!=" a b
        | ESge(a, b) -> showBin 1 ">=" a b
        | ESgt(a, b) -> showBin 1 ">" a b
        | ECall(r, args) ->
            let funcString = showRef r
            let argsString =
                args |> List.map (showExpression' form 0)
                     |> String.concat ", "

            sprintf "%s(%s)" funcString argsString
        | EIndex(it, r, i) ->
            sprintf "%A.%s[%A]" it (showRef r) (showExpression' form 0 i)

    let showExpression form : Expression -> string =
        showExpression' form 0

    /// A decompiled statement.
    type Statement =
    // All of the old instructions, for the initial conversions...
    | DoubleTypeInstructionStatement of DoubleOp
    | DupInstructionStatement
    | RetInstructionStatement
    | PopzInstructionStatement
    | BranchInstructionStatement of Word * BranchConditionality
    | PopInstructionStatement of InstanceType * Reference
    | PushExpressionStatement of Expression
    | CallInstructionStatement of int * Reference
    | BreakInstructionStatement of int16
    | UnknownInstructionStatement of Word
    // ... and new, structured ones.
    | ExpressionStatement of Expression
    | ReturnStatement of Expression
    | SetArrayStatement of (InstanceType * Reference * Expression) * Expression
    | SetVariableStatement of (InstanceType * Reference) * Expression

    let showStatement (form : Unpack.Form) : Statement -> string =
        let showRef = showReference form
        let showExpr = showExpression form
        let showIT = showInstanceType
        function
        | DoubleTypeInstructionStatement(op)       -> sprintf "%A" op
        | DupInstructionStatement                  -> "dup"
        | RetInstructionStatement                  -> "return pop"
        | PopzInstructionStatement                 -> "pop"
        | BranchInstructionStatement(goal, bc)     -> sprintf "%A goto %08x" bc goal
        | PopInstructionStatement(it, r)           -> sprintf "%s.%s = pop" (showIT it) (showRef r)
        | PushExpressionStatement(e)               -> sprintf "push %s" (showExpr e)
        | CallInstructionStatement(n, r)           -> sprintf "call[%d] %s" n (showRef r)
        | BreakInstructionStatement(s)             -> sprintf "break %A" s
        | UnknownInstructionStatement(w)           -> sprintf "<%08x>" w
        | ExpressionStatement(e)                   -> sprintf "%s" (showExpr e)
        | ReturnStatement(e)                       -> sprintf "return %s" (showExpr e)
        | SetVariableStatement((it, r), e)         -> sprintf "%s.%s = %s" (showIT it) (showRef r) (showExpr e)
        | SetArrayStatement((it, r, i), e)         -> sprintf "%s.%s[%s] = %s" (showIT it) (showRef r) (showExpr i) (showExpr e)

    let instructionToStatement : Instruction -> Statement =
        function
        | DoubleTypeInstruction(op, _)     -> DoubleTypeInstructionStatement(op)
        | SingleTypeInstruction(Dup, _)    -> DupInstructionStatement
        | SingleTypeInstruction(Ret, _)    -> RetInstructionStatement
        | SingleTypeInstruction(Exit, _)   -> ReturnStatement(EInt32 0)
        | SingleTypeInstruction(Popz, _)   -> PopzInstructionStatement
        | BranchInstruction(goal, bc)      -> BranchInstructionStatement(goal, bc)
        | PopInstruction(it, _, rf)        -> PopInstructionStatement(it, rf)
        | PushDoubleInstruction(x)         -> PushExpressionStatement(EDouble x)
        | PushInt64Instruction(x)          -> PushExpressionStatement(EInt64 x)
        | PushFloatInstruction(x)          -> PushExpressionStatement(EFloat x)
        | PushInt32Instruction(x)          -> PushExpressionStatement(EInt32 x)
        | PushBooleanInstruction(x)        -> PushExpressionStatement(EBoolean x)
        | PushInt24Instruction(x)          -> PushExpressionStatement(EInt24 x)
        | PushStringInstruction(x)         -> PushExpressionStatement(EString x)
        | PushInt16Instruction(x)          -> PushExpressionStatement(EInt16 x)
        | PushVariableInstruction(it, rf)  -> PushExpressionStatement(EVariable(it, rf))
        | CallInstruction(_, n, rf)        -> CallInstructionStatement(n, rf)
        | BreakInstruction(s)              -> BreakInstructionStatement(s)
        | UnknownInstruction(w)            -> UnknownInstructionStatement(w)

    /// A Statement, tagged with its position in the code.
    type PosStatement =
        {Position: CodeAddress;
         Statement: Statement}
    
    let showPosStatement (form : Unpack.Form) (ps : PosStatement) : string =
        sprintf "%08x %s" ps.Position (showStatement form ps.Statement)

    let posInstructionToStatement : PosInstruction -> PosStatement =
        function
        | { Position = p; Instruction = i } ->
            { Position = p; Statement = instructionToStatement i }

    /// Simplify a program (a list of PosStatements). This simply looks for
    /// simplifications to perform through complicated pattern matching, and
    /// performs them as it goes along.
    let simplify : PosStatement list -> PosStatement list =
        let rec go (acc : PosStatement list) : PosStatement list -> PosStatement list =
            function
            // Eliminate conversions.
            | {Position = p; Statement = DoubleTypeInstructionStatement(Con)} :: xs ->
                go acc xs
            
            // Turn "push -5s; push i; push it.r;" into "push(it.r[i])".
            | {Position = p; Statement = PushExpressionStatement(EInt16(-5s))}
              :: {Statement = PushExpressionStatement(i)}
              :: {Statement = PushExpressionStatement(EVariable(it, r))} :: xs when r.Type = Array ->
                go ({Position = p; Statement = PushExpressionStatement(EIndex(it, r, i))} :: acc) xs
        
            // Turn "push r; push(-pop())" into "push(-r)".
            | {Position = p; Statement = PushExpressionStatement(r)} as head
              :: (({Statement = DoubleTypeInstructionStatement(op)}
                  :: xs) as tail) ->
                match tryDoubleOpToUnaryExpression op with
                | Some e -> go ({Position = p; Statement = PushExpressionStatement(e(r))} :: acc) xs
                | None   -> go (head :: acc) tail
            
            // Turn "push l; push r; push(pop() * pop())" into "push(l * r)".
            | {Position = p; Statement = PushExpressionStatement(l)} as head
              :: (({Statement = PushExpressionStatement(r)}
                   :: {Statement = DoubleTypeInstructionStatement(op)}
                   :: xs) as tail) ->
                match tryDoubleOpToBinaryExpression op with
                | Some e -> go ({Position = p; Statement = PushExpressionStatement(e(l, r))} :: acc) xs
                | None   -> go (head :: acc) tail
            
            // Turn "push e; popz" into "e".
            | {Position = p; Statement = PushExpressionStatement(e)}
              :: {Statement = PopzInstructionStatement} :: xs ->
                go ({Position = p; Statement = ExpressionStatement(e)} :: acc) xs
            // Turn "push e; return pop" into "return e".
            | {Position = p; Statement = PushExpressionStatement(e)}
              :: {Statement = RetInstructionStatement} :: xs ->
                go ({Position = p; Statement = ReturnStatement(e)} :: acc) xs
            // Turn "push e; pop(it, r)" into "it.r = e".
            | {Position = p; Statement = PushExpressionStatement(e)}
              :: {Statement = PopInstructionStatement(it, r)} :: xs ->
                go ({Position = p; Statement = SetVariableStatement((it, r), e)} :: acc) xs
           
            // Turn "push i; push -5s; pop(it, r)" into "it.r[i] = e".
            | {Position = p; Statement = PushExpressionStatement(e)}
              :: {Statement = PushExpressionStatement(EInt16(-5s))}
              :: {Statement = SetVariableStatement((it, r), i)} :: xs when r.Type = Array ->
                go ({Position = p; Statement = SetArrayStatement((it, r, i), e)} :: acc) xs
            
            // Turn "call[0] f" into "push f()".
            | {Position = p; Statement = CallInstructionStatement(0, f)} :: xs ->
                go ({Position = p; Statement = PushExpressionStatement(ECall(f, []))} :: acc) xs
            // Turn "push e1; call[1] f" into "push f(e1)".
            | {Position = p; Statement = PushExpressionStatement(e1)}
              :: {Statement = CallInstructionStatement(1, f)} :: xs ->
                go ({Position = p; Statement = PushExpressionStatement(ECall(f, [e1]))} :: acc) xs
            // Turn "push e1; push e2; call[2] f" into "push f(e1, e2)".
            | {Position = p; Statement = PushExpressionStatement(e1)}
              :: {Statement = PushExpressionStatement(e2)}
              :: {Statement = CallInstructionStatement(2, f)} :: xs ->
                go ({Position = p; Statement = PushExpressionStatement(ECall(f, [e1; e2]))} :: acc) xs
            // Turn "push e1; push e2; push e3; call[3] f" into "push f(e1, e2, e3)".
            | {Position = p; Statement = PushExpressionStatement(e1)}
              :: {Statement = PushExpressionStatement(e2)}
              :: {Statement = PushExpressionStatement(e3)}
              :: {Statement = CallInstructionStatement(3, f)} :: xs ->
                go ({Position = p; Statement = PushExpressionStatement(ECall(f, [e1; e2; e3]))} :: acc) xs
            // Turn "push e1; push e2; push e3; push e4; call[4] f" into "push f(e1, e2, e3, e4)".
            | {Position = p; Statement = PushExpressionStatement(e1)}
              :: {Statement = PushExpressionStatement(e2)}
              :: {Statement = PushExpressionStatement(e3)}
              :: {Statement = PushExpressionStatement(e4)}
              :: {Statement = CallInstructionStatement(4, f)} :: xs ->
                go ({Position = p; Statement = PushExpressionStatement(ECall(f, [e1; e2; e3; e4]))} :: acc) xs
            
            // If we couldn't find anything, move on.
            | x :: xs -> go (x :: acc) xs
            | [] -> List.rev acc
        go []

    // Decompile a chunk of bytecode at a given address into tagged
    // statements. (This simply applies `simplify` until the result stops
    // changing.)
    let decompile (start : uint32) (ws : Word []) : PosStatement list =
        disassemble start ws
        |> List.map posInstructionToStatement
        |> Utility.untilEqual simplify