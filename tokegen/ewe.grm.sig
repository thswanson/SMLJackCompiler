signature ewe_TOKENS =
sig
type ('a,'b) token
type svalue
val PC:  'a * 'a -> (svalue,'a) token
val M:  'a * 'a -> (svalue,'a) token
val Break:  'a * 'a -> (svalue,'a) token
val Halt:  'a * 'a -> (svalue,'a) token
val WriteStr:  'a * 'a -> (svalue,'a) token
val String: (string) *  'a * 'a -> (svalue,'a) token
val ReadStr:  'a * 'a -> (svalue,'a) token
val WriteInt:  'a * 'a -> (svalue,'a) token
val ReadInt:  'a * 'a -> (svalue,'a) token
val Goto:  'a * 'a -> (svalue,'a) token
val Then:  'a * 'a -> (svalue,'a) token
val If:  'a * 'a -> (svalue,'a) token
val Identifier: (string) *  'a * 'a -> (svalue,'a) token
val Integer: (int) *  'a * 'a -> (svalue,'a) token
val RightBracket:  'a * 'a -> (svalue,'a) token
val LeftBracket:  'a * 'a -> (svalue,'a) token
val Modulo:  'a * 'a -> (svalue,'a) token
val Divide:  'a * 'a -> (svalue,'a) token
val Mul:  'a * 'a -> (svalue,'a) token
val Sub:  'a * 'a -> (svalue,'a) token
val Plus:  'a * 'a -> (svalue,'a) token
val NotEqual:  'a * 'a -> (svalue,'a) token
val Equal:  'a * 'a -> (svalue,'a) token
val Less:  'a * 'a -> (svalue,'a) token
val LessEqual:  'a * 'a -> (svalue,'a) token
val Greater:  'a * 'a -> (svalue,'a) token
val GreaterEqual:  'a * 'a -> (svalue,'a) token
val RightParen:  'a * 'a -> (svalue,'a) token
val LeftParen:  'a * 'a -> (svalue,'a) token
val Comma:  'a * 'a -> (svalue,'a) token
val Colon:  'a * 'a -> (svalue,'a) token
val Equate:  'a * 'a -> (svalue,'a) token
val Assign:  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
end
signature ewe_LRVALS=
sig
structure Tokens : ewe_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
