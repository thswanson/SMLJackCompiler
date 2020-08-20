signature calc_TOKENS =
sig
type ('a,'b) token
type svalue
val Tilde:  'a * 'a -> (svalue,'a) token
val GreaterThan:  'a * 'a -> (svalue,'a) token
val LessThan:  'a * 'a -> (svalue,'a) token
val Div:  'a * 'a -> (svalue,'a) token
val Times:  'a * 'a -> (svalue,'a) token
val Minus:  'a * 'a -> (svalue,'a) token
val Plus:  'a * 'a -> (svalue,'a) token
val Equals:  'a * 'a -> (svalue,'a) token
val RParen:  'a * 'a -> (svalue,'a) token
val LParen:  'a * 'a -> (svalue,'a) token
val Pipe:  'a * 'a -> (svalue,'a) token
val Ampersand:  'a * 'a -> (svalue,'a) token
val SemiColon:  'a * 'a -> (svalue,'a) token
val Comma:  'a * 'a -> (svalue,'a) token
val Period:  'a * 'a -> (svalue,'a) token
val RSquare:  'a * 'a -> (svalue,'a) token
val LSquare:  'a * 'a -> (svalue,'a) token
val RCurly:  'a * 'a -> (svalue,'a) token
val LCurly:  'a * 'a -> (svalue,'a) token
val Identifier: (string) *  'a * 'a -> (svalue,'a) token
val StringConstant: (string) *  'a * 'a -> (svalue,'a) token
val IntConstant: (int) *  'a * 'a -> (svalue,'a) token
val Return: (string) *  'a * 'a -> (svalue,'a) token
val While: (string) *  'a * 'a -> (svalue,'a) token
val Else: (string) *  'a * 'a -> (svalue,'a) token
val If: (string) *  'a * 'a -> (svalue,'a) token
val Do: (string) *  'a * 'a -> (svalue,'a) token
val Let: (string) *  'a * 'a -> (svalue,'a) token
val This: (string) *  'a * 'a -> (svalue,'a) token
val Null: (string) *  'a * 'a -> (svalue,'a) token
val False: (string) *  'a * 'a -> (svalue,'a) token
val True: (string) *  'a * 'a -> (svalue,'a) token
val Void: (string) *  'a * 'a -> (svalue,'a) token
val Boolean: (string) *  'a * 'a -> (svalue,'a) token
val Char: (string) *  'a * 'a -> (svalue,'a) token
val Int: (string) *  'a * 'a -> (svalue,'a) token
val Var: (string) *  'a * 'a -> (svalue,'a) token
val Static: (string) *  'a * 'a -> (svalue,'a) token
val Field: (string) *  'a * 'a -> (svalue,'a) token
val Method: (string) *  'a * 'a -> (svalue,'a) token
val Function: (string) *  'a * 'a -> (svalue,'a) token
val Constructor: (string) *  'a * 'a -> (svalue,'a) token
val Class: (string) *  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
end
signature calc_LRVALS=
sig
structure Tokens : calc_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
