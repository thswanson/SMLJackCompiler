(* ewe.lex -- lexer spec *)

exception tokenError;
type pos = int
type svalue = Tokens.svalue
type ('a, 'b) token = ('a, 'b) Tokens.token
type lexresult = (svalue, pos) token
val pos = ref 1
val error = fn x => 
   (TextIO.output(TextIO.stdErr, x ^ " found on line " ^ Int.toString(!pos) ^ "\n");
    raise tokenError)
val eof = fn () => Tokens.EOF(!pos, !pos)
fun sval([], r) = r
  | sval(a::s, r) = sval (s, r*10+(ord(a) - ord(#"0")));
fun negval (#"-"::s) = s
  | negval (l) = l;

%%

%header (functor eweLexFun(structure Tokens : ewe_TOKENS));
alpha=[A-Za-z];
digit=[0-9];
alphanumeric=[A-Za-z0-9_];
ws=[\ \t];
strdel = ['\"];

%%

\n  => (pos := (!pos) + 1; lex());
"#"[^\n]* => (lex());
{ws}+  => (lex());
":="  => (Tokens.Assign(!pos,!pos));
":" => (Tokens.Colon(!pos,!pos));
"("  => (Tokens.LeftParen(!pos,!pos));
")"  => (Tokens.RightParen(!pos,!pos));
">="  => (Tokens.GreaterEqual(!pos,!pos));
">"  => (Tokens.Greater(!pos,!pos));
"<="  => (Tokens.LessEqual(!pos,!pos));
"<"  => (Tokens.Less(!pos,!pos));
"="  => (Tokens.Equal(!pos,!pos));
"<>"  => (Tokens.NotEqual(!pos,!pos));
"+"  => (Tokens.Plus(!pos,!pos));
"-"  => (Tokens.Sub(!pos,!pos));
"*"  => (Tokens.Mul(!pos,!pos));
"/"  => (Tokens.Divide(!pos,!pos));
"%"  => (Tokens.Modulo(!pos,!pos));
"["  => (Tokens.LeftBracket(!pos,!pos));
"]"  => (Tokens.RightBracket(!pos,!pos));
","  => (Tokens.Comma(!pos,!pos));
{digit}+  => (Tokens.Integer(sval(explode yytext,0),!pos,!pos));
-{digit}+  => (Tokens.Integer(~1*sval(negval(explode yytext),0),!pos,!pos));
{strdel}.*{strdel} => (Tokens.String(yytext,!pos,!pos));
{alpha}{alphanumeric}* => 
          (let val tok = String.implode (List.map (Char.toLower) (String.explode yytext))
           in
           if      tok="if"      then Tokens.If(!pos,!pos)
           else if tok="then"    then Tokens.Then(!pos,!pos)
           else if tok="goto"    then Tokens.Goto(!pos,!pos)
           else if tok="readint"    then Tokens.ReadInt(!pos,!pos)
           else if tok="writeint" then Tokens.WriteInt(!pos,!pos)
           else if tok="readstr" then Tokens.ReadStr(!pos,!pos)
           else if tok="writestr" then Tokens.WriteStr(!pos,!pos)
           else if tok="halt"    then Tokens.Halt(!pos,!pos)
           else if tok="m"       then Tokens.M(!pos,!pos)
           else if tok="pc"      then Tokens.PC(!pos,!pos)
           else if tok="equ"     then Tokens.Equate(!pos,!pos)
           else if tok="break"   then Tokens.Break(!pos,!pos)
           else Tokens.Identifier(yytext,!pos,!pos)
           end);
.  => (error ("error: bad token "^yytext); lex());
