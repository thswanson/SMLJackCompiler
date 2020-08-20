structure calcAS =
struct

datatype
    AST = class' of string * (string * string * string list) list * AST list
        | staticvar' of string * string list
        | fieldvar' of string * string list
        | int'
        | char'
        | bool'
        | void'
        | id' of string
        | identifierlist' of string list
        | constructor' of string * string * (string * string) list * (string * string list) list * AST list
        | function' of string * string * (string * string) list * (string * string list) list  * AST list
        | method' of string * string * (string * string) list * (string * string list) list  * AST list
        | parameterlist' of (string * string) list
        | varnamelist' of (string * string list) list
        | letstatement' of AST
        | ifstatement' of AST
        | whilestatement' of AST
        | dostatement' of AST
        | returnstatement' of AST
        | letstate' of string * AST
        | letexpr' of string * AST * AST
        | if' of AST * AST list
        | ifelse' of AST * AST list * AST list
        | while' of AST * AST list
        | do' of AST
        | returnvoid'
        | return' of AST
        | add' of AST * AST
        | sub' of AST * AST
        | mul' of AST * AST
        | div' of AST * AST
        | and' of AST * AST
        | or' of AST * AST
        | lt' of AST * AST
        | gt' of AST * AST
        | equal' of AST * AST
        | negate' of AST
        | not' of AST
        | integer' of int
        | string' of string
        | subroutine' of AST
        | idarray' of string * AST
        | subcall' of AST
        | subcall1' of string * AST list
        | subcall2' of string * string * AST list
        | expressionlist' of AST list
        | parenexpr' of AST
        | true'
        | false'
        | null'
        | this'





(*fun show(recall') = "recall"
  | show(letval'(s,e1,e2)) = "letval'("^s^","^show(e1)^","^show(e2)^")"
  | show(valref'(s)) = "valref'("^s^")"
  | show(get') = "get'"
  | show(integer'(i)) = "integer("^Int.toString(i)^")"
  | show(store'(a)) = "store("^show(a)^")"
  | show(negate'(a)) = "negate("^show(a)^")"
  | show(div'(a,b)) = "div("^show(a)^","^show(b)^")"
  | show(prod'(a,b)) = "prod("^show(a)^","^show(b)^")"
  | show(sub'(a,b)) = "sub("^show(a)^","^show(b)^")"
  | show(add'(a,b)) = "add("^show(a)^","^show(b)^")"
  | show(ifElse'(expr1, RelOp, expr2, expr3, expr4)) = "ifElse'("^show(expr1)^","^RelOp^","^show(expr2)^","^show(expr3)^","^show(expr4)^")"
  | show(letfun'(Identifier1, Identifier2, Expr1, Expr2)) = "holder Text"
  | show(funref'(Identifier, Expr)) = "holder text 2"

end;*)
end;
