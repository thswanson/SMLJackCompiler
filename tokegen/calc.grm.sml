functor calcLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : calc_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(* calc.grm - parser spec *)
(*https://people.mpi-sws.org/~rossberg/sml.html*)

open calcAS;


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\001\000\167\000\000\000\
\\001\000\002\000\003\000\000\000\
\\001\000\003\000\168\000\004\000\168\000\005\000\168\000\027\000\168\000\000\000\
\\001\000\003\000\169\000\004\000\169\000\005\000\169\000\006\000\009\000\
\\007\000\008\000\027\000\169\000\000\000\
\\001\000\003\000\170\000\004\000\170\000\005\000\170\000\006\000\170\000\
\\007\000\170\000\027\000\170\000\000\000\
\\001\000\003\000\171\000\004\000\171\000\005\000\171\000\006\000\171\000\
\\007\000\171\000\027\000\171\000\000\000\
\\001\000\003\000\182\000\004\000\182\000\005\000\182\000\027\000\182\000\000\000\
\\001\000\003\000\183\000\004\000\183\000\005\000\183\000\027\000\183\000\000\000\
\\001\000\003\000\184\000\004\000\184\000\005\000\184\000\027\000\184\000\000\000\
\\001\000\003\000\014\000\004\000\013\000\005\000\012\000\027\000\181\000\000\000\
\\001\000\008\000\191\000\017\000\191\000\018\000\191\000\019\000\191\000\
\\021\000\191\000\022\000\191\000\000\000\
\\001\000\008\000\057\000\017\000\190\000\018\000\190\000\019\000\190\000\
\\021\000\190\000\022\000\190\000\000\000\
\\001\000\009\000\021\000\010\000\020\000\011\000\019\000\012\000\018\000\
\\025\000\017\000\000\000\
\\001\000\009\000\021\000\010\000\020\000\011\000\019\000\012\000\018\000\
\\025\000\017\000\036\000\187\000\000\000\
\\001\000\013\000\092\000\014\000\091\000\015\000\090\000\016\000\089\000\
\\023\000\088\000\024\000\087\000\025\000\086\000\032\000\085\000\
\\035\000\084\000\039\000\083\000\044\000\082\000\000\000\
\\001\000\013\000\092\000\014\000\091\000\015\000\090\000\016\000\089\000\
\\023\000\088\000\024\000\087\000\025\000\086\000\035\000\084\000\000\000\
\\001\000\013\000\092\000\014\000\091\000\015\000\090\000\016\000\089\000\
\\023\000\088\000\024\000\087\000\025\000\086\000\035\000\084\000\
\\036\000\233\000\039\000\083\000\044\000\082\000\000\000\
\\001\000\013\000\092\000\014\000\091\000\015\000\090\000\016\000\089\000\
\\023\000\088\000\024\000\087\000\025\000\086\000\035\000\084\000\
\\039\000\083\000\044\000\082\000\000\000\
\\001\000\017\000\189\000\018\000\189\000\019\000\189\000\021\000\189\000\
\\022\000\189\000\000\000\
\\001\000\017\000\197\000\018\000\197\000\019\000\197\000\021\000\197\000\
\\022\000\197\000\027\000\197\000\000\000\
\\001\000\017\000\198\000\018\000\198\000\019\000\198\000\021\000\198\000\
\\022\000\198\000\027\000\198\000\000\000\
\\001\000\017\000\199\000\018\000\199\000\019\000\199\000\021\000\199\000\
\\022\000\199\000\027\000\199\000\000\000\
\\001\000\017\000\200\000\018\000\200\000\019\000\200\000\021\000\200\000\
\\022\000\200\000\027\000\200\000\000\000\
\\001\000\017\000\201\000\018\000\201\000\019\000\201\000\021\000\201\000\
\\022\000\201\000\027\000\201\000\000\000\
\\001\000\017\000\202\000\018\000\202\000\019\000\202\000\021\000\202\000\
\\022\000\202\000\027\000\202\000\000\000\
\\001\000\017\000\203\000\018\000\203\000\019\000\203\000\021\000\203\000\
\\022\000\203\000\027\000\203\000\000\000\
\\001\000\017\000\204\000\018\000\204\000\019\000\204\000\020\000\161\000\
\\021\000\204\000\022\000\204\000\027\000\204\000\000\000\
\\001\000\017\000\205\000\018\000\205\000\019\000\205\000\021\000\205\000\
\\022\000\205\000\027\000\205\000\000\000\
\\001\000\017\000\206\000\018\000\206\000\019\000\206\000\021\000\206\000\
\\022\000\206\000\027\000\206\000\000\000\
\\001\000\017\000\207\000\018\000\207\000\019\000\207\000\021\000\207\000\
\\022\000\207\000\027\000\207\000\000\000\
\\001\000\017\000\208\000\018\000\208\000\019\000\208\000\021\000\208\000\
\\022\000\208\000\027\000\208\000\000\000\
\\001\000\017\000\209\000\018\000\209\000\019\000\209\000\021\000\209\000\
\\022\000\209\000\027\000\209\000\000\000\
\\001\000\017\000\072\000\018\000\071\000\019\000\070\000\021\000\069\000\
\\022\000\068\000\000\000\
\\001\000\017\000\072\000\018\000\071\000\019\000\070\000\021\000\069\000\
\\022\000\068\000\027\000\196\000\000\000\
\\001\000\025\000\175\000\032\000\175\000\000\000\
\\001\000\025\000\176\000\032\000\176\000\000\000\
\\001\000\025\000\177\000\032\000\177\000\000\000\
\\001\000\025\000\178\000\032\000\178\000\000\000\
\\001\000\025\000\179\000\032\000\179\000\000\000\
\\001\000\025\000\004\000\000\000\
\\001\000\025\000\029\000\032\000\174\000\000\000\
\\001\000\025\000\031\000\000\000\
\\001\000\025\000\032\000\000\000\
\\001\000\025\000\033\000\000\000\
\\001\000\025\000\048\000\000\000\
\\001\000\025\000\096\000\000\000\
\\001\000\025\000\097\000\000\000\
\\001\000\025\000\099\000\032\000\194\000\000\000\
\\001\000\025\000\137\000\000\000\
\\001\000\026\000\005\000\000\000\
\\001\000\026\000\052\000\000\000\
\\001\000\026\000\053\000\000\000\
\\001\000\026\000\054\000\000\000\
\\001\000\026\000\148\000\000\000\
\\001\000\026\000\149\000\000\000\
\\001\000\026\000\163\000\000\000\
\\001\000\027\000\180\000\000\000\
\\001\000\027\000\195\000\000\000\
\\001\000\027\000\023\000\000\000\
\\001\000\027\000\077\000\000\000\
\\001\000\027\000\100\000\000\000\
\\001\000\027\000\101\000\000\000\
\\001\000\027\000\158\000\000\000\
\\001\000\027\000\159\000\000\000\
\\001\000\027\000\165\000\000\000\
\\001\000\028\000\117\000\029\000\228\000\030\000\116\000\031\000\228\000\
\\032\000\228\000\033\000\228\000\034\000\228\000\035\000\115\000\
\\036\000\228\000\037\000\228\000\038\000\228\000\039\000\228\000\
\\040\000\228\000\041\000\228\000\042\000\228\000\043\000\228\000\000\000\
\\001\000\028\000\122\000\037\000\121\000\000\000\
\\001\000\029\000\210\000\031\000\210\000\032\000\210\000\033\000\110\000\
\\034\000\109\000\036\000\210\000\037\000\108\000\038\000\107\000\
\\039\000\106\000\040\000\105\000\041\000\104\000\042\000\103\000\
\\043\000\102\000\000\000\
\\001\000\029\000\211\000\031\000\211\000\032\000\211\000\036\000\211\000\000\000\
\\001\000\029\000\212\000\031\000\212\000\032\000\212\000\036\000\212\000\000\000\
\\001\000\029\000\213\000\031\000\213\000\032\000\213\000\036\000\213\000\000\000\
\\001\000\029\000\214\000\031\000\214\000\032\000\214\000\036\000\214\000\000\000\
\\001\000\029\000\215\000\031\000\215\000\032\000\215\000\036\000\215\000\000\000\
\\001\000\029\000\216\000\031\000\216\000\032\000\216\000\036\000\216\000\000\000\
\\001\000\029\000\217\000\031\000\217\000\032\000\217\000\036\000\217\000\000\000\
\\001\000\029\000\218\000\031\000\218\000\032\000\218\000\036\000\218\000\000\000\
\\001\000\029\000\219\000\031\000\219\000\032\000\219\000\036\000\219\000\000\000\
\\001\000\029\000\220\000\031\000\220\000\032\000\220\000\036\000\220\000\000\000\
\\001\000\029\000\221\000\031\000\221\000\032\000\221\000\036\000\221\000\000\000\
\\001\000\029\000\222\000\031\000\222\000\032\000\222\000\033\000\222\000\
\\034\000\222\000\036\000\222\000\037\000\222\000\038\000\222\000\
\\039\000\222\000\040\000\222\000\041\000\222\000\042\000\222\000\
\\043\000\222\000\000\000\
\\001\000\029\000\223\000\031\000\223\000\032\000\223\000\033\000\223\000\
\\034\000\223\000\036\000\223\000\037\000\223\000\038\000\223\000\
\\039\000\223\000\040\000\223\000\041\000\223\000\042\000\223\000\
\\043\000\223\000\000\000\
\\001\000\029\000\224\000\031\000\224\000\032\000\224\000\033\000\224\000\
\\034\000\224\000\036\000\224\000\037\000\224\000\038\000\224\000\
\\039\000\224\000\040\000\224\000\041\000\224\000\042\000\224\000\
\\043\000\224\000\000\000\
\\001\000\029\000\225\000\031\000\225\000\032\000\225\000\033\000\225\000\
\\034\000\225\000\036\000\225\000\037\000\225\000\038\000\225\000\
\\039\000\225\000\040\000\225\000\041\000\225\000\042\000\225\000\
\\043\000\225\000\000\000\
\\001\000\029\000\226\000\031\000\226\000\032\000\226\000\033\000\226\000\
\\034\000\226\000\036\000\226\000\037\000\226\000\038\000\226\000\
\\039\000\226\000\040\000\226\000\041\000\226\000\042\000\226\000\
\\043\000\226\000\000\000\
\\001\000\029\000\227\000\031\000\227\000\032\000\227\000\033\000\227\000\
\\034\000\227\000\036\000\227\000\037\000\227\000\038\000\227\000\
\\039\000\227\000\040\000\227\000\041\000\227\000\042\000\227\000\
\\043\000\227\000\000\000\
\\001\000\029\000\229\000\031\000\229\000\032\000\229\000\033\000\229\000\
\\034\000\229\000\036\000\229\000\037\000\229\000\038\000\229\000\
\\039\000\229\000\040\000\229\000\041\000\229\000\042\000\229\000\
\\043\000\229\000\000\000\
\\001\000\029\000\230\000\031\000\230\000\032\000\230\000\033\000\230\000\
\\034\000\230\000\036\000\230\000\037\000\230\000\038\000\230\000\
\\039\000\230\000\040\000\230\000\041\000\230\000\042\000\230\000\
\\043\000\230\000\000\000\
\\001\000\029\000\234\000\031\000\234\000\032\000\234\000\033\000\234\000\
\\034\000\234\000\036\000\234\000\037\000\234\000\038\000\234\000\
\\039\000\234\000\040\000\234\000\041\000\234\000\042\000\234\000\
\\043\000\234\000\000\000\
\\001\000\029\000\235\000\031\000\235\000\032\000\235\000\033\000\235\000\
\\034\000\235\000\036\000\235\000\037\000\235\000\038\000\235\000\
\\039\000\235\000\040\000\235\000\041\000\235\000\042\000\235\000\
\\043\000\235\000\000\000\
\\001\000\029\000\236\000\031\000\236\000\032\000\236\000\033\000\236\000\
\\034\000\236\000\036\000\236\000\037\000\236\000\038\000\236\000\
\\039\000\236\000\040\000\236\000\041\000\236\000\042\000\236\000\
\\043\000\236\000\000\000\
\\001\000\029\000\237\000\031\000\237\000\032\000\237\000\033\000\237\000\
\\034\000\237\000\036\000\237\000\037\000\237\000\038\000\237\000\
\\039\000\237\000\040\000\237\000\041\000\237\000\042\000\237\000\
\\043\000\237\000\000\000\
\\001\000\029\000\147\000\000\000\
\\001\000\029\000\151\000\000\000\
\\001\000\030\000\116\000\035\000\115\000\000\000\
\\001\000\031\000\188\000\036\000\188\000\000\000\
\\001\000\031\000\035\000\032\000\172\000\000\000\
\\001\000\031\000\046\000\036\000\185\000\000\000\
\\001\000\031\000\124\000\032\000\192\000\000\000\
\\001\000\031\000\145\000\036\000\231\000\000\000\
\\001\000\032\000\173\000\000\000\
\\001\000\032\000\193\000\000\000\
\\001\000\032\000\034\000\000\000\
\\001\000\032\000\036\000\000\000\
\\001\000\032\000\111\000\000\000\
\\001\000\032\000\120\000\000\000\
\\001\000\032\000\123\000\000\000\
\\001\000\032\000\150\000\000\000\
\\001\000\032\000\162\000\000\000\
\\001\000\035\000\037\000\000\000\
\\001\000\035\000\038\000\000\000\
\\001\000\035\000\039\000\000\000\
\\001\000\035\000\093\000\000\000\
\\001\000\035\000\094\000\000\000\
\\001\000\035\000\146\000\000\000\
\\001\000\036\000\186\000\000\000\
\\001\000\036\000\232\000\000\000\
\\001\000\036\000\047\000\000\000\
\\001\000\036\000\049\000\000\000\
\\001\000\036\000\050\000\000\000\
\\001\000\036\000\134\000\000\000\
\\001\000\036\000\139\000\000\000\
\\001\000\036\000\140\000\000\000\
\\001\000\036\000\144\000\000\000\
\\001\000\036\000\157\000\000\000\
\\001\000\037\000\156\000\000\000\
\"
val actionRowNumbers =
"\002\000\040\000\050\000\004\000\
\\010\000\004\000\013\000\013\000\
\\059\000\010\000\013\000\013\000\
\\013\000\003\000\041\000\039\000\
\\038\000\037\000\036\000\035\000\
\\041\000\001\000\057\000\042\000\
\\043\000\044\000\102\000\096\000\
\\103\000\109\000\110\000\111\000\
\\005\000\041\000\006\000\014\000\
\\014\000\014\000\100\000\097\000\
\\117\000\045\000\118\000\119\000\
\\014\000\051\000\095\000\052\000\
\\053\000\115\000\012\000\012\000\
\\012\000\012\000\033\000\013\000\
\\033\000\033\000\019\000\024\000\
\\023\000\022\000\021\000\020\000\
\\034\000\060\000\015\000\112\000\
\\113\000\046\000\047\000\048\000\
\\061\000\062\000\058\000\009\000\
\\082\000\084\000\068\000\104\000\
\\016\000\016\000\018\000\031\000\
\\066\000\081\000\080\000\091\000\
\\090\000\089\000\088\000\018\000\
\\018\000\105\000\094\000\067\000\
\\106\000\098\000\008\000\007\000\
\\018\000\018\000\018\000\018\000\
\\018\000\018\000\018\000\018\000\
\\018\000\032\000\079\000\078\000\
\\120\000\017\000\049\000\018\000\
\\121\000\122\000\030\000\018\000\
\\018\000\011\000\048\000\076\000\
\\075\000\072\000\071\000\070\000\
\\069\000\077\000\074\000\073\000\
\\085\000\123\000\099\000\114\000\
\\092\000\054\000\055\000\107\000\
\\093\000\101\000\086\000\017\000\
\\017\000\083\000\033\000\033\000\
\\025\000\125\000\116\000\124\000\
\\063\000\064\000\018\000\087\000\
\\029\000\027\000\108\000\056\000\
\\026\000\033\000\065\000\028\000\
\\000\000"
val gotoT =
"\
\\001\000\164\000\000\000\
\\000\000\
\\000\000\
\\002\000\005\000\003\000\004\000\000\000\
\\005\000\009\000\006\000\008\000\000\000\
\\002\000\005\000\003\000\013\000\000\000\
\\004\000\014\000\000\000\
\\004\000\020\000\000\000\
\\000\000\
\\005\000\009\000\006\000\022\000\000\000\
\\004\000\023\000\000\000\
\\004\000\024\000\000\000\
\\004\000\025\000\000\000\
\\000\000\
\\012\000\026\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\012\000\028\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\012\000\038\000\000\000\
\\000\000\
\\004\000\041\000\007\000\040\000\008\000\039\000\000\000\
\\004\000\041\000\007\000\042\000\008\000\039\000\000\000\
\\004\000\041\000\007\000\043\000\008\000\039\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\041\000\007\000\049\000\008\000\039\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\054\000\011\000\053\000\000\000\
\\010\000\056\000\011\000\053\000\000\000\
\\010\000\057\000\011\000\053\000\000\000\
\\010\000\058\000\011\000\053\000\000\000\
\\014\000\065\000\015\000\064\000\016\000\063\000\017\000\062\000\
\\018\000\061\000\019\000\060\000\020\000\059\000\000\000\
\\004\000\071\000\000\000\
\\014\000\072\000\015\000\064\000\016\000\063\000\017\000\062\000\
\\018\000\061\000\019\000\060\000\020\000\059\000\000\000\
\\014\000\073\000\015\000\064\000\016\000\063\000\017\000\062\000\
\\018\000\061\000\019\000\060\000\020\000\059\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\014\000\074\000\015\000\064\000\016\000\063\000\017\000\062\000\
\\018\000\061\000\019\000\060\000\020\000\059\000\000\000\
\\000\000\
\\021\000\079\000\022\000\078\000\023\000\077\000\025\000\076\000\000\000\
\\000\000\
\\000\000\
\\023\000\093\000\000\000\
\\000\000\
\\013\000\096\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\022\000\110\000\023\000\077\000\025\000\076\000\000\000\
\\022\000\111\000\023\000\077\000\025\000\076\000\000\000\
\\021\000\112\000\022\000\078\000\023\000\077\000\025\000\076\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\021\000\116\000\022\000\078\000\023\000\077\000\025\000\076\000\000\000\
\\021\000\117\000\022\000\078\000\023\000\077\000\025\000\076\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\021\000\123\000\022\000\078\000\023\000\077\000\025\000\076\000\000\000\
\\021\000\124\000\022\000\078\000\023\000\077\000\025\000\076\000\000\000\
\\021\000\125\000\022\000\078\000\023\000\077\000\025\000\076\000\000\000\
\\021\000\126\000\022\000\078\000\023\000\077\000\025\000\076\000\000\000\
\\021\000\127\000\022\000\078\000\023\000\077\000\025\000\076\000\000\000\
\\021\000\128\000\022\000\078\000\023\000\077\000\025\000\076\000\000\000\
\\021\000\129\000\022\000\078\000\023\000\077\000\025\000\076\000\000\000\
\\021\000\130\000\022\000\078\000\023\000\077\000\025\000\076\000\000\000\
\\021\000\131\000\022\000\078\000\023\000\077\000\025\000\076\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\021\000\134\000\022\000\078\000\023\000\077\000\024\000\133\000\
\\025\000\076\000\000\000\
\\000\000\
\\021\000\136\000\022\000\078\000\023\000\077\000\025\000\076\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\021\000\139\000\022\000\078\000\023\000\077\000\025\000\076\000\000\000\
\\021\000\140\000\022\000\078\000\023\000\077\000\025\000\076\000\000\000\
\\000\000\
\\013\000\141\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\021\000\134\000\022\000\078\000\023\000\077\000\024\000\150\000\
\\025\000\076\000\000\000\
\\021\000\134\000\022\000\078\000\023\000\077\000\024\000\151\000\
\\025\000\076\000\000\000\
\\000\000\
\\014\000\152\000\015\000\064\000\016\000\063\000\017\000\062\000\
\\018\000\061\000\019\000\060\000\020\000\059\000\000\000\
\\014\000\153\000\015\000\064\000\016\000\063\000\017\000\062\000\
\\018\000\061\000\019\000\060\000\020\000\059\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\021\000\158\000\022\000\078\000\023\000\077\000\025\000\076\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\014\000\162\000\015\000\064\000\016\000\063\000\017\000\062\000\
\\018\000\061\000\019\000\060\000\020\000\059\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 165
val numrules = 71
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit | Identifier of  (string)
 | StringConstant of  (string) | IntConstant of  (int)
 | Return of  (string) | While of  (string) | Else of  (string)
 | If of  (string) | Do of  (string) | Let of  (string)
 | This of  (string) | Null of  (string) | False of  (string)
 | True of  (string) | Void of  (string) | Boolean of  (string)
 | Char of  (string) | Int of  (string) | Var of  (string)
 | Static of  (string) | Field of  (string) | Method of  (string)
 | Function of  (string) | Constructor of  (string)
 | Class of  (string) | KeywordConstant of  (AST)
 | ExpressionList of  (AST list) | SubroutineCall of  (AST)
 | Term of  (AST) | Expression of  (AST) | ReturnStatement of  (AST)
 | DoStatement of  (AST) | WhileStatement of  (AST)
 | IfStatement of  (AST) | LetStatement of  (AST)
 | Statement of  (AST) | Statements of  (AST list)
 | IdentifierList of  (string list) | VarNameList of  (string list)
 | VarDec of  ( ( string * string list ) )
 | VarDecList of  ( ( string * string list )  list)
 | SubroutineBody of  (AST) | Parameter of  ( ( string * string ) )
 | ParameterList of  ( ( string * string )  list)
 | SubroutineDecList of  (AST list) | SubroutineDec of  (AST)
 | Type of  (string)
 | ClassVarDecList of  ( ( string * string * string list )  list)
 | ClassVarDec of  ( ( string * string * string list ) )
 | ClassDec of  (AST)
end
type svalue = MlyValue.svalue
type result = AST
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "Class"
  | (T 2) => "Constructor"
  | (T 3) => "Function"
  | (T 4) => "Method"
  | (T 5) => "Field"
  | (T 6) => "Static"
  | (T 7) => "Var"
  | (T 8) => "Int"
  | (T 9) => "Char"
  | (T 10) => "Boolean"
  | (T 11) => "Void"
  | (T 12) => "True"
  | (T 13) => "False"
  | (T 14) => "Null"
  | (T 15) => "This"
  | (T 16) => "Let"
  | (T 17) => "Do"
  | (T 18) => "If"
  | (T 19) => "Else"
  | (T 20) => "While"
  | (T 21) => "Return"
  | (T 22) => "IntConstant"
  | (T 23) => "StringConstant"
  | (T 24) => "Identifier"
  | (T 25) => "LCurly"
  | (T 26) => "RCurly"
  | (T 27) => "LSquare"
  | (T 28) => "RSquare"
  | (T 29) => "Period"
  | (T 30) => "Comma"
  | (T 31) => "SemiColon"
  | (T 32) => "Ampersand"
  | (T 33) => "Pipe"
  | (T 34) => "LParen"
  | (T 35) => "RParen"
  | (T 36) => "Equals"
  | (T 37) => "Plus"
  | (T 38) => "Minus"
  | (T 39) => "Times"
  | (T 40) => "Div"
  | (T 41) => "LessThan"
  | (T 42) => "GreaterThan"
  | (T 43) => "Tilde"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 43) $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38) $$ (T 37)
 $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30)
 $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( _, _, RCurly1right)) :: ( _, ( 
MlyValue.SubroutineDecList SubroutineDecList, _, _)) :: ( _, ( 
MlyValue.ClassVarDecList ClassVarDecList, _, _)) :: _ :: ( _, ( 
MlyValue.Identifier Identifier, _, _)) :: ( _, ( _, Class1left, _)) ::
 rest671)) => let val  result = MlyValue.ClassDec (
class'(Identifier,ClassVarDecList,SubroutineDecList))
 in ( LrTable.NT 0, ( result, Class1left, RCurly1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.ClassVarDecList ClassVarDecList, _, 
ClassVarDecList1right)) :: ( _, ( MlyValue.ClassVarDec ClassVarDec, 
ClassVarDec1left, _)) :: rest671)) => let val  result = 
MlyValue.ClassVarDecList (ClassVarDec::ClassVarDecList)
 in ( LrTable.NT 2, ( result, ClassVarDec1left, ClassVarDecList1right)
, rest671)
end
|  ( 2, ( rest671)) => let val  result = MlyValue.ClassVarDecList ([])
 in ( LrTable.NT 2, ( result, defaultPos, defaultPos), rest671)
end
|  ( 3, ( ( _, ( _, _, SemiColon1right)) :: ( _, ( 
MlyValue.VarNameList VarNameList, _, _)) :: ( _, ( MlyValue.Type Type,
 _, _)) :: ( _, ( MlyValue.Static Static, Static1left, _)) :: rest671)
) => let val  result = MlyValue.ClassVarDec (
(Static, Type, VarNameList))
 in ( LrTable.NT 1, ( result, Static1left, SemiColon1right), rest671)

end
|  ( 4, ( ( _, ( _, _, SemiColon1right)) :: ( _, ( 
MlyValue.VarNameList VarNameList, _, _)) :: ( _, ( MlyValue.Type Type,
 _, _)) :: ( _, ( MlyValue.Field Field, Field1left, _)) :: rest671))
 => let val  result = MlyValue.ClassVarDec ((Field, Type, VarNameList)
)
 in ( LrTable.NT 1, ( result, Field1left, SemiColon1right), rest671)

end
|  ( 5, ( ( _, ( MlyValue.Identifier Identifier, Identifier1left, 
Identifier1right)) :: rest671)) => let val  result = 
MlyValue.VarNameList ([Identifier])
 in ( LrTable.NT 11, ( result, Identifier1left, Identifier1right), 
rest671)
end
|  ( 6, ( ( _, ( MlyValue.VarNameList VarNameList, _, 
VarNameList1right)) :: _ :: ( _, ( MlyValue.Identifier Identifier, 
Identifier1left, _)) :: rest671)) => let val  result = 
MlyValue.VarNameList (Identifier::VarNameList)
 in ( LrTable.NT 11, ( result, Identifier1left, VarNameList1right), 
rest671)
end
|  ( 7, ( rest671)) => let val  result = MlyValue.VarNameList ([])
 in ( LrTable.NT 11, ( result, defaultPos, defaultPos), rest671)
end
|  ( 8, ( ( _, ( _, Int1left, Int1right)) :: rest671)) => let val  
result = MlyValue.Type ("int")
 in ( LrTable.NT 3, ( result, Int1left, Int1right), rest671)
end
|  ( 9, ( ( _, ( _, Char1left, Char1right)) :: rest671)) => let val  
result = MlyValue.Type ("char")
 in ( LrTable.NT 3, ( result, Char1left, Char1right), rest671)
end
|  ( 10, ( ( _, ( _, Boolean1left, Boolean1right)) :: rest671)) => let
 val  result = MlyValue.Type ("bool")
 in ( LrTable.NT 3, ( result, Boolean1left, Boolean1right), rest671)

end
|  ( 11, ( ( _, ( _, Void1left, Void1right)) :: rest671)) => let val  
result = MlyValue.Type ("void")
 in ( LrTable.NT 3, ( result, Void1left, Void1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.Identifier Identifier, Identifier1left, 
Identifier1right)) :: rest671)) => let val  result = MlyValue.Type (
Identifier)
 in ( LrTable.NT 3, ( result, Identifier1left, Identifier1right), 
rest671)
end
|  ( 13, ( ( _, ( MlyValue.SubroutineDecList SubroutineDecList, _, 
SubroutineDecList1right)) :: ( _, ( MlyValue.SubroutineDec 
SubroutineDec, SubroutineDec1left, _)) :: rest671)) => let val  result
 = MlyValue.SubroutineDecList (SubroutineDec::SubroutineDecList)
 in ( LrTable.NT 5, ( result, SubroutineDec1left, 
SubroutineDecList1right), rest671)
end
|  ( 14, ( rest671)) => let val  result = MlyValue.SubroutineDecList (
[])
 in ( LrTable.NT 5, ( result, defaultPos, defaultPos), rest671)
end
|  ( 15, ( ( _, ( _, _, RCurly1right)) :: ( _, ( MlyValue.Statements 
Statements, _, _)) :: ( _, ( MlyValue.VarDecList VarDecList, _, _)) ::
 _ :: _ :: ( _, ( MlyValue.ParameterList ParameterList, _, _)) :: _ ::
 ( _, ( MlyValue.Identifier Identifier, _, _)) :: ( _, ( MlyValue.Type
 Type, _, _)) :: ( _, ( _, Constructor1left, _)) :: rest671)) => let
 val  result = MlyValue.SubroutineDec (
constructor'(Type, Identifier, ParameterList, VarDecList, Statements))
 in ( LrTable.NT 4, ( result, Constructor1left, RCurly1right), rest671
)
end
|  ( 16, ( ( _, ( _, _, RCurly1right)) :: ( _, ( MlyValue.Statements 
Statements, _, _)) :: ( _, ( MlyValue.VarDecList VarDecList, _, _)) ::
 _ :: _ :: ( _, ( MlyValue.ParameterList ParameterList, _, _)) :: _ ::
 ( _, ( MlyValue.Identifier Identifier, _, _)) :: ( _, ( MlyValue.Type
 Type, _, _)) :: ( _, ( _, Function1left, _)) :: rest671)) => let val 
 result = MlyValue.SubroutineDec (
function'(Type, Identifier, ParameterList, VarDecList, Statements))
 in ( LrTable.NT 4, ( result, Function1left, RCurly1right), rest671)

end
|  ( 17, ( ( _, ( _, _, RCurly1right)) :: ( _, ( MlyValue.Statements 
Statements, _, _)) :: ( _, ( MlyValue.VarDecList VarDecList, _, _)) ::
 _ :: _ :: ( _, ( MlyValue.ParameterList ParameterList, _, _)) :: _ ::
 ( _, ( MlyValue.Identifier Identifier, _, _)) :: ( _, ( MlyValue.Type
 Type, _, _)) :: ( _, ( _, Method1left, _)) :: rest671)) => let val  
result = MlyValue.SubroutineDec (
method'(Type, Identifier, ParameterList, VarDecList, Statements))
 in ( LrTable.NT 4, ( result, Method1left, RCurly1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.Parameter Parameter, Parameter1left, 
Parameter1right)) :: rest671)) => let val  result = 
MlyValue.ParameterList ([Parameter])
 in ( LrTable.NT 6, ( result, Parameter1left, Parameter1right), 
rest671)
end
|  ( 19, ( ( _, ( MlyValue.ParameterList ParameterList, _, 
ParameterList1right)) :: _ :: ( _, ( MlyValue.Parameter Parameter, 
Parameter1left, _)) :: rest671)) => let val  result = 
MlyValue.ParameterList (Parameter::ParameterList)
 in ( LrTable.NT 6, ( result, Parameter1left, ParameterList1right), 
rest671)
end
|  ( 20, ( rest671)) => let val  result = MlyValue.ParameterList ([])
 in ( LrTable.NT 6, ( result, defaultPos, defaultPos), rest671)
end
|  ( 21, ( ( _, ( MlyValue.Identifier Identifier, _, Identifier1right)
) :: ( _, ( MlyValue.Type Type, Type1left, _)) :: rest671)) => let
 val  result = MlyValue.Parameter ((Type, Identifier))
 in ( LrTable.NT 7, ( result, Type1left, Identifier1right), rest671)

end
|  ( 22, ( ( _, ( MlyValue.VarDecList VarDecList, _, VarDecList1right)
) :: ( _, ( MlyValue.VarDec VarDec, VarDec1left, _)) :: rest671)) =>
 let val  result = MlyValue.VarDecList (VarDec::VarDecList)
 in ( LrTable.NT 9, ( result, VarDec1left, VarDecList1right), rest671)

end
|  ( 23, ( rest671)) => let val  result = MlyValue.VarDecList ([])
 in ( LrTable.NT 9, ( result, defaultPos, defaultPos), rest671)
end
|  ( 24, ( ( _, ( _, _, SemiColon1right)) :: ( _, ( 
MlyValue.IdentifierList IdentifierList, _, _)) :: ( _, ( MlyValue.Type
 Type, _, _)) :: ( _, ( _, Var1left, _)) :: rest671)) => let val  
result = MlyValue.VarDec ((Type, IdentifierList))
 in ( LrTable.NT 10, ( result, Var1left, SemiColon1right), rest671)

end
|  ( 25, ( ( _, ( MlyValue.Identifier Identifier, Identifier1left, 
Identifier1right)) :: rest671)) => let val  result = 
MlyValue.IdentifierList ([Identifier])
 in ( LrTable.NT 12, ( result, Identifier1left, Identifier1right), 
rest671)
end
|  ( 26, ( ( _, ( MlyValue.IdentifierList IdentifierList, _, 
IdentifierList1right)) :: _ :: ( _, ( MlyValue.Identifier Identifier, 
Identifier1left, _)) :: rest671)) => let val  result = 
MlyValue.IdentifierList (Identifier::IdentifierList)
 in ( LrTable.NT 12, ( result, Identifier1left, IdentifierList1right),
 rest671)
end
|  ( 27, ( rest671)) => let val  result = MlyValue.IdentifierList ([])
 in ( LrTable.NT 12, ( result, defaultPos, defaultPos), rest671)
end
|  ( 28, ( ( _, ( MlyValue.Statements Statements, _, Statements1right)
) :: ( _, ( MlyValue.Statement Statement, Statement1left, _)) :: 
rest671)) => let val  result = MlyValue.Statements (
Statement::Statements)
 in ( LrTable.NT 13, ( result, Statement1left, Statements1right), 
rest671)
end
|  ( 29, ( ( _, ( MlyValue.Statement Statement, Statement1left, 
Statement1right)) :: rest671)) => let val  result = 
MlyValue.Statements ([Statement])
 in ( LrTable.NT 13, ( result, Statement1left, Statement1right), 
rest671)
end
|  ( 30, ( ( _, ( MlyValue.LetStatement LetStatement, 
LetStatement1left, LetStatement1right)) :: rest671)) => let val  
result = MlyValue.Statement (letstatement'(LetStatement))
 in ( LrTable.NT 14, ( result, LetStatement1left, LetStatement1right),
 rest671)
end
|  ( 31, ( ( _, ( MlyValue.IfStatement IfStatement, IfStatement1left, 
IfStatement1right)) :: rest671)) => let val  result = 
MlyValue.Statement (ifstatement'(IfStatement))
 in ( LrTable.NT 14, ( result, IfStatement1left, IfStatement1right), 
rest671)
end
|  ( 32, ( ( _, ( MlyValue.WhileStatement WhileStatement, 
WhileStatement1left, WhileStatement1right)) :: rest671)) => let val  
result = MlyValue.Statement (whilestatement'(WhileStatement))
 in ( LrTable.NT 14, ( result, WhileStatement1left, 
WhileStatement1right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.DoStatement DoStatement, DoStatement1left, 
DoStatement1right)) :: rest671)) => let val  result = 
MlyValue.Statement (dostatement'(DoStatement))
 in ( LrTable.NT 14, ( result, DoStatement1left, DoStatement1right), 
rest671)
end
|  ( 34, ( ( _, ( MlyValue.ReturnStatement ReturnStatement, 
ReturnStatement1left, ReturnStatement1right)) :: rest671)) => let val 
 result = MlyValue.Statement (returnstatement'(ReturnStatement))
 in ( LrTable.NT 14, ( result, ReturnStatement1left, 
ReturnStatement1right), rest671)
end
|  ( 35, ( ( _, ( _, _, SemiColon1right)) :: ( _, ( 
MlyValue.Expression Expression, _, _)) :: _ :: ( _, ( 
MlyValue.Identifier Identifier, _, _)) :: ( _, ( _, Let1left, _)) :: 
rest671)) => let val  result = MlyValue.LetStatement (
letstate'(Identifier, Expression))
 in ( LrTable.NT 15, ( result, Let1left, SemiColon1right), rest671)

end
|  ( 36, ( ( _, ( _, _, SemiColon1right)) :: ( _, ( 
MlyValue.Expression Expression2, _, _)) :: _ :: _ :: ( _, ( 
MlyValue.Expression Expression1, _, _)) :: _ :: ( _, ( 
MlyValue.Identifier Identifier, _, _)) :: ( _, ( _, Let1left, _)) :: 
rest671)) => let val  result = MlyValue.LetStatement (
letexpr'(Identifier, Expression1, Expression2))
 in ( LrTable.NT 15, ( result, Let1left, SemiColon1right), rest671)

end
|  ( 37, ( ( _, ( _, _, RCurly1right)) :: ( _, ( MlyValue.Statements 
Statements, _, _)) :: _ :: _ :: ( _, ( MlyValue.Expression Expression,
 _, _)) :: _ :: ( _, ( _, If1left, _)) :: rest671)) => let val  result
 = MlyValue.IfStatement (if'(Expression, Statements))
 in ( LrTable.NT 16, ( result, If1left, RCurly1right), rest671)
end
|  ( 38, ( ( _, ( _, _, RCurly2right)) :: ( _, ( MlyValue.Statements 
Statements2, _, _)) :: _ :: _ :: _ :: ( _, ( MlyValue.Statements 
Statements1, _, _)) :: _ :: _ :: ( _, ( MlyValue.Expression Expression
, _, _)) :: _ :: ( _, ( _, If1left, _)) :: rest671)) => let val  
result = MlyValue.IfStatement (
ifelse'(Expression, Statements1, Statements2))
 in ( LrTable.NT 16, ( result, If1left, RCurly2right), rest671)
end
|  ( 39, ( ( _, ( _, _, RCurly1right)) :: ( _, ( MlyValue.Statements 
Statements, _, _)) :: _ :: _ :: ( _, ( MlyValue.Expression Expression,
 _, _)) :: _ :: ( _, ( _, While1left, _)) :: rest671)) => let val  
result = MlyValue.WhileStatement (while'(Expression, Statements))
 in ( LrTable.NT 17, ( result, While1left, RCurly1right), rest671)
end
|  ( 40, ( ( _, ( _, _, SemiColon1right)) :: ( _, ( 
MlyValue.SubroutineCall SubroutineCall, _, _)) :: ( _, ( _, Do1left, _
)) :: rest671)) => let val  result = MlyValue.DoStatement (
do'(SubroutineCall))
 in ( LrTable.NT 18, ( result, Do1left, SemiColon1right), rest671)
end
|  ( 41, ( ( _, ( _, _, SemiColon1right)) :: ( _, ( _, Return1left, _)
) :: rest671)) => let val  result = MlyValue.ReturnStatement (
returnvoid')
 in ( LrTable.NT 19, ( result, Return1left, SemiColon1right), rest671)

end
|  ( 42, ( ( _, ( _, _, SemiColon1right)) :: ( _, ( 
MlyValue.Expression Expression, _, _)) :: ( _, ( _, Return1left, _))
 :: rest671)) => let val  result = MlyValue.ReturnStatement (
return'(Expression))
 in ( LrTable.NT 19, ( result, Return1left, SemiColon1right), rest671)

end
|  ( 43, ( ( _, ( MlyValue.Term Term, Term1left, Term1right)) :: 
rest671)) => let val  result = MlyValue.Expression (Term)
 in ( LrTable.NT 20, ( result, Term1left, Term1right), rest671)
end
|  ( 44, ( ( _, ( MlyValue.Expression Expression, _, Expression1right)
) :: _ :: ( _, ( MlyValue.Term Term, Term1left, _)) :: rest671)) =>
 let val  result = MlyValue.Expression (add'(Term,Expression))
 in ( LrTable.NT 20, ( result, Term1left, Expression1right), rest671)

end
|  ( 45, ( ( _, ( MlyValue.Expression Expression, _, Expression1right)
) :: _ :: ( _, ( MlyValue.Term Term, Term1left, _)) :: rest671)) =>
 let val  result = MlyValue.Expression (sub'(Term,Expression))
 in ( LrTable.NT 20, ( result, Term1left, Expression1right), rest671)

end
|  ( 46, ( ( _, ( MlyValue.Expression Expression, _, Expression1right)
) :: _ :: ( _, ( MlyValue.Term Term, Term1left, _)) :: rest671)) =>
 let val  result = MlyValue.Expression (mul'(Term,Expression))
 in ( LrTable.NT 20, ( result, Term1left, Expression1right), rest671)

end
|  ( 47, ( ( _, ( MlyValue.Expression Expression, _, Expression1right)
) :: _ :: ( _, ( MlyValue.Term Term, Term1left, _)) :: rest671)) =>
 let val  result = MlyValue.Expression (div'(Term,Expression))
 in ( LrTable.NT 20, ( result, Term1left, Expression1right), rest671)

end
|  ( 48, ( ( _, ( MlyValue.Expression Expression, _, Expression1right)
) :: _ :: ( _, ( MlyValue.Term Term, Term1left, _)) :: rest671)) =>
 let val  result = MlyValue.Expression (and'(Term,Expression))
 in ( LrTable.NT 20, ( result, Term1left, Expression1right), rest671)

end
|  ( 49, ( ( _, ( MlyValue.Expression Expression, _, Expression1right)
) :: _ :: ( _, ( MlyValue.Term Term, Term1left, _)) :: rest671)) =>
 let val  result = MlyValue.Expression (or'(Term,Expression))
 in ( LrTable.NT 20, ( result, Term1left, Expression1right), rest671)

end
|  ( 50, ( ( _, ( MlyValue.Expression Expression, _, Expression1right)
) :: _ :: ( _, ( MlyValue.Term Term, Term1left, _)) :: rest671)) =>
 let val  result = MlyValue.Expression (lt'(Term,Expression))
 in ( LrTable.NT 20, ( result, Term1left, Expression1right), rest671)

end
|  ( 51, ( ( _, ( MlyValue.Expression Expression, _, Expression1right)
) :: _ :: ( _, ( MlyValue.Term Term, Term1left, _)) :: rest671)) =>
 let val  result = MlyValue.Expression (gt'(Term,Expression))
 in ( LrTable.NT 20, ( result, Term1left, Expression1right), rest671)

end
|  ( 52, ( ( _, ( MlyValue.Expression Expression, _, Expression1right)
) :: _ :: ( _, ( MlyValue.Term Term, Term1left, _)) :: rest671)) =>
 let val  result = MlyValue.Expression (equal'(Term,Expression))
 in ( LrTable.NT 20, ( result, Term1left, Expression1right), rest671)

end
|  ( 53, ( ( _, ( MlyValue.Term Term, _, Term1right)) :: ( _, ( _, 
Minus1left, _)) :: rest671)) => let val  result = MlyValue.Expression
 (negate'(Term))
 in ( LrTable.NT 20, ( result, Minus1left, Term1right), rest671)
end
|  ( 54, ( ( _, ( MlyValue.Term Term, _, Term1right)) :: ( _, ( _, 
Tilde1left, _)) :: rest671)) => let val  result = MlyValue.Expression
 (not'(Term))
 in ( LrTable.NT 20, ( result, Tilde1left, Term1right), rest671)
end
|  ( 55, ( ( _, ( MlyValue.IntConstant IntConstant, IntConstant1left, 
IntConstant1right)) :: rest671)) => let val  result = MlyValue.Term (
integer'(IntConstant))
 in ( LrTable.NT 21, ( result, IntConstant1left, IntConstant1right), 
rest671)
end
|  ( 56, ( ( _, ( MlyValue.StringConstant StringConstant, 
StringConstant1left, StringConstant1right)) :: rest671)) => let val  
result = MlyValue.Term (string'(StringConstant))
 in ( LrTable.NT 21, ( result, StringConstant1left, 
StringConstant1right), rest671)
end
|  ( 57, ( ( _, ( MlyValue.KeywordConstant KeywordConstant, 
KeywordConstant1left, KeywordConstant1right)) :: rest671)) => let val 
 result = MlyValue.Term (KeywordConstant)
 in ( LrTable.NT 21, ( result, KeywordConstant1left, 
KeywordConstant1right), rest671)
end
|  ( 58, ( ( _, ( _, _, RSquare1right)) :: ( _, ( MlyValue.Expression 
Expression, _, _)) :: _ :: ( _, ( MlyValue.Identifier Identifier, 
Identifier1left, _)) :: rest671)) => let val  result = MlyValue.Term (
idarray'(Identifier,Expression))
 in ( LrTable.NT 21, ( result, Identifier1left, RSquare1right), 
rest671)
end
|  ( 59, ( ( _, ( MlyValue.SubroutineCall SubroutineCall, 
SubroutineCall1left, SubroutineCall1right)) :: rest671)) => let val  
result = MlyValue.Term (subroutine'(SubroutineCall))
 in ( LrTable.NT 21, ( result, SubroutineCall1left, 
SubroutineCall1right), rest671)
end
|  ( 60, ( ( _, ( _, _, RParen1right)) :: ( _, ( MlyValue.Expression 
Expression, _, _)) :: ( _, ( _, LParen1left, _)) :: rest671)) => let
 val  result = MlyValue.Term (parenexpr'(Expression))
 in ( LrTable.NT 21, ( result, LParen1left, RParen1right), rest671)

end
|  ( 61, ( ( _, ( MlyValue.Identifier Identifier, Identifier1left, 
Identifier1right)) :: rest671)) => let val  result = MlyValue.Term (
id'(Identifier))
 in ( LrTable.NT 21, ( result, Identifier1left, Identifier1right), 
rest671)
end
|  ( 62, ( ( _, ( _, _, RParen1right)) :: ( _, ( 
MlyValue.ExpressionList ExpressionList, _, _)) :: _ :: ( _, ( 
MlyValue.Identifier Identifier, Identifier1left, _)) :: rest671)) =>
 let val  result = MlyValue.SubroutineCall (
subcall1'(Identifier, ExpressionList))
 in ( LrTable.NT 22, ( result, Identifier1left, RParen1right), rest671
)
end
|  ( 63, ( ( _, ( _, _, RParen1right)) :: ( _, ( 
MlyValue.ExpressionList ExpressionList, _, _)) :: _ :: ( _, ( 
MlyValue.Identifier Identifier2, _, _)) :: _ :: ( _, ( 
MlyValue.Identifier Identifier1, Identifier1left, _)) :: rest671)) =>
 let val  result = MlyValue.SubroutineCall (
subcall2'(Identifier1, Identifier2, ExpressionList))
 in ( LrTable.NT 22, ( result, Identifier1left, RParen1right), rest671
)
end
|  ( 64, ( ( _, ( MlyValue.Expression Expression, Expression1left, 
Expression1right)) :: rest671)) => let val  result = 
MlyValue.ExpressionList ([Expression])
 in ( LrTable.NT 23, ( result, Expression1left, Expression1right), 
rest671)
end
|  ( 65, ( ( _, ( MlyValue.ExpressionList ExpressionList, _, 
ExpressionList1right)) :: _ :: ( _, ( MlyValue.Expression Expression, 
Expression1left, _)) :: rest671)) => let val  result = 
MlyValue.ExpressionList (Expression::ExpressionList)
 in ( LrTable.NT 23, ( result, Expression1left, ExpressionList1right),
 rest671)
end
|  ( 66, ( rest671)) => let val  result = MlyValue.ExpressionList ([])
 in ( LrTable.NT 23, ( result, defaultPos, defaultPos), rest671)
end
|  ( 67, ( ( _, ( _, True1left, True1right)) :: rest671)) => let val  
result = MlyValue.KeywordConstant (true')
 in ( LrTable.NT 24, ( result, True1left, True1right), rest671)
end
|  ( 68, ( ( _, ( _, False1left, False1right)) :: rest671)) => let
 val  result = MlyValue.KeywordConstant (false')
 in ( LrTable.NT 24, ( result, False1left, False1right), rest671)
end
|  ( 69, ( ( _, ( _, Null1left, Null1right)) :: rest671)) => let val  
result = MlyValue.KeywordConstant (null')
 in ( LrTable.NT 24, ( result, Null1left, Null1right), rest671)
end
|  ( 70, ( ( _, ( _, This1left, This1right)) :: rest671)) => let val  
result = MlyValue.KeywordConstant (this')
 in ( LrTable.NT 24, ( result, This1left, This1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.ClassDec x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a 
end
end
structure Tokens : calc_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun Class (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.Class i,p1,p2))
fun Constructor (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.Constructor i,p1,p2))
fun Function (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.Function i,p1,p2))
fun Method (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.Method i,p1,p2))
fun Field (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.Field i,p1,p2))
fun Static (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.Static i,p1,p2))
fun Var (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.Var i,p1,p2))
fun Int (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.Int i,p1,p2))
fun Char (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.Char i,p1,p2))
fun Boolean (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.Boolean i,p1,p2))
fun Void (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.Void i,p1,p2))
fun True (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.True i,p1,p2))
fun False (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.False i,p1,p2))
fun Null (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.Null i,p1,p2))
fun This (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.This i,p1,p2))
fun Let (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.Let i,p1,p2))
fun Do (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.Do i,p1,p2))
fun If (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.If i,p1,p2))
fun Else (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.Else i,p1,p2))
fun While (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.While i,p1,p2))
fun Return (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.Return i,p1,p2))
fun IntConstant (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.IntConstant i,p1,p2))
fun StringConstant (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.StringConstant i,p1,p2))
fun Identifier (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.Identifier i,p1,p2))
fun LCurly (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun RCurly (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun LSquare (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun RSquare (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun Period (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun Comma (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun SemiColon (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun Ampersand (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun Pipe (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun LParen (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun RParen (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun Equals (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun Plus (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun Minus (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun Times (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun Div (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun LessThan (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun GreaterThan (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
fun Tilde (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID,p1,p2))
end
end
