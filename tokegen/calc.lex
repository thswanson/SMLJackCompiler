(* calc.lex -- lexer spec *)

type pos = int
type svalue = Tokens.svalue
type ('a, 'b) token = ('a, 'b) Tokens.token
type lexresult = (svalue, pos) token
val pos = ref 1
val error = fn x => TextIO.output(TextIO.stdErr, x ^ "\n")
val eof = fn () => Tokens.EOF(!pos, !pos)
fun sval([], r) = r
  | sval(a::s, r) = sval (s, r*10+(ord(a) - ord(#"0")));

%%
%s MULTI;


%header (functor calcLexFun(structure Tokens : calc_TOKENS));
alpha=[A-Za-z];
alphanumeric=[A-Za-z0-9_];
digit=[0-9];
ws=[\ \t];
nl=[\n];

qs=[\"];
notbad=[^\"\n];
slash = [\/];

%%

\n  => (pos := (!pos) + 1; lex());
<INITIAL>{ws}+  => (lex());
<INITIAL>"{"  => (Tokens.LCurly(!pos,!pos));
<INITIAL>"}"  => (Tokens.RCurly(!pos,!pos));
<INITIAL>"("  => (Tokens.LParen(!pos,!pos));
<INITIAL>")"  => (Tokens.RParen(!pos,!pos));
<INITIAL>"["  => (Tokens.LSquare(!pos,!pos));
<INITIAL>"]"  => (Tokens.RSquare(!pos,!pos));
<INITIAL>"."  => (Tokens.Period(!pos,!pos));
<INITIAL>","  => (Tokens.Comma(!pos,!pos));
<INITIAL>";"  => (Tokens.SemiColon(!pos,!pos));
<INITIAL>"+"  => (Tokens.Plus(!pos,!pos));
<INITIAL>"-"  => (Tokens.Minus(!pos,!pos));
<INITIAL>"*"  => (Tokens.Times(!pos,!pos));
<INITIAL>"/"  => (Tokens.Div(!pos,!pos));
<INITIAL>"&"  => (Tokens.Ampersand(!pos,!pos));
<INITIAL>"|"  => (Tokens.Pipe(!pos,!pos));
<INITIAL>"<"  => (Tokens.LessThan(!pos,!pos));
<INITIAL>">"  => (Tokens.GreaterThan(!pos,!pos));
<INITIAL>"="  => (Tokens.Equals(!pos,!pos));
<INITIAL>"~"  => (Tokens.Tilde(!pos,!pos));

\/\/.*\n => (pos := (!pos) + 1; lex());
<INITIAL>\/\* => (YYBEGIN MULTI; lex());
<MULTI>\*\/   => (YYBEGIN INITIAL; lex());
<INITIAL>{qs}{notbad}*{qs} => (Tokens.StringConstant(yytext,!pos,!pos));
<INITIAL>{digit}+  => (Tokens.IntConstant(sval(explode yytext,0),!pos,!pos));
<INITIAL>{alpha}{alphanumeric}* =>
   (let val tok = String.implode (List.map (Char.toLower)
             (String.explode yytext))
    in
      if      tok = "class" then Tokens.Class(yytext,!pos,!pos)
      else if tok = "constructor" then Tokens.Constructor(yytext,!pos,!pos)
	    else if tok = "function" then Tokens.Function(yytext,!pos,!pos)
      else if tok = "method" then Tokens.Method(yytext,!pos,!pos)
      else if tok = "field" then Tokens.Field(yytext,!pos,!pos)
      else if tok = "static" then Tokens.Static(yytext,!pos,!pos)
      else if tok = "var" then Tokens.Var(yytext,!pos,!pos)
      else if tok = "int" then Tokens.Int(yytext,!pos,!pos)
      else if tok = "char" then Tokens.Char(yytext,!pos,!pos)
      else if tok = "boolean" then Tokens.Boolean(yytext,!pos,!pos)
      else if tok = "void" then Tokens.Void(yytext,!pos,!pos)
      else if tok = "true" then Tokens.True(yytext,!pos,!pos)
      else if tok = "false" then Tokens.False(yytext,!pos,!pos)
      else if tok = "null" then Tokens.Null(yytext,!pos,!pos)
      else if tok = "this" then Tokens.This(yytext,!pos,!pos)
      else if tok = "let" then Tokens.Let(yytext,!pos,!pos)
      else if tok = "do" then Tokens.Do(yytext,!pos,!pos)
      else if tok = "if" then Tokens.If(yytext,!pos,!pos)
      else if tok = "else" then Tokens.Else(yytext,!pos,!pos)
      else if tok = "while" then Tokens.While(yytext,!pos,!pos)
      else if tok = "return" then Tokens.Return(yytext,!pos,!pos)
      else Tokens.Identifier(yytext,!pos,!pos)
    end);
<INITIAL>.  => (error ("error: bad token "^yytext); lex());
<MULTI>.  => (lex());
