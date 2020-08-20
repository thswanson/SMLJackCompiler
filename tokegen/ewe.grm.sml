functor eweLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : ewe_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(* ewe.grm - parser spec *)

open eweAS;


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\001\000\039\000\000\000\
\\001\000\002\000\018\000\000\000\
\\001\000\002\000\022\000\000\000\
\\001\000\002\000\090\000\000\000\
\\001\000\005\000\068\000\000\000\
\\001\000\006\000\024\000\000\000\
\\001\000\006\000\025\000\000\000\
\\001\000\006\000\026\000\000\000\
\\001\000\006\000\027\000\000\000\
\\001\000\007\000\067\000\000\000\
\\001\000\007\000\069\000\000\000\
\\001\000\007\000\070\000\000\000\
\\001\000\007\000\086\000\000\000\
\\001\000\008\000\054\000\009\000\053\000\010\000\052\000\011\000\051\000\
\\012\000\050\000\013\000\049\000\000\000\
\\001\000\014\000\062\000\000\000\
\\001\000\014\000\065\000\000\000\
\\001\000\014\000\083\000\000\000\
\\001\000\019\000\023\000\000\000\
\\001\000\019\000\055\000\000\000\
\\001\000\019\000\063\000\000\000\
\\001\000\019\000\079\000\000\000\
\\001\000\020\000\066\000\000\000\
\\001\000\020\000\085\000\000\000\
\\001\000\020\000\089\000\000\000\
\\001\000\020\000\093\000\000\000\
\\001\000\021\000\029\000\022\000\028\000\000\000\
\\001\000\021\000\038\000\022\000\032\000\029\000\037\000\033\000\036\000\
\\034\000\035\000\000\000\
\\001\000\021\000\043\000\000\000\
\\001\000\021\000\043\000\022\000\032\000\033\000\031\000\000\000\
\\001\000\021\000\077\000\000\000\
\\001\000\021\000\080\000\000\000\
\\001\000\021\000\084\000\000\000\
\\001\000\021\000\088\000\000\000\
\\001\000\021\000\092\000\022\000\091\000\000\000\
\\001\000\022\000\017\000\023\000\016\000\025\000\015\000\026\000\014\000\
\\027\000\013\000\028\000\012\000\030\000\011\000\031\000\010\000\
\\032\000\009\000\033\000\008\000\034\000\007\000\000\000\
\\001\000\022\000\032\000\033\000\031\000\000\000\
\\001\000\022\000\040\000\000\000\
\\001\000\024\000\082\000\000\000\
\\001\000\025\000\087\000\000\000\
\\001\000\033\000\064\000\000\000\
\\097\000\000\000\
\\098\000\022\000\017\000\023\000\016\000\025\000\015\000\026\000\014\000\
\\027\000\013\000\028\000\012\000\030\000\011\000\031\000\010\000\
\\032\000\009\000\033\000\008\000\034\000\007\000\000\000\
\\099\000\000\000\
\\100\000\000\000\
\\101\000\000\000\
\\102\000\000\000\
\\103\000\000\000\
\\104\000\000\000\
\\105\000\000\000\
\\106\000\014\000\061\000\015\000\060\000\016\000\059\000\017\000\058\000\
\\018\000\057\000\000\000\
\\107\000\000\000\
\\108\000\000\000\
\\109\000\000\000\
\\110\000\000\000\
\\111\000\000\000\
\\112\000\000\000\
\\113\000\000\000\
\\114\000\000\000\
\\115\000\000\000\
\\116\000\000\000\
\\117\000\000\000\
\\118\000\000\000\
\\119\000\000\000\
\\120\000\000\000\
\\121\000\000\000\
\\122\000\000\000\
\\123\000\000\000\
\\124\000\003\000\021\000\000\000\
\\125\000\000\000\
\\126\000\000\000\
\\127\000\000\000\
\\127\000\004\000\033\000\000\000\
\\128\000\000\000\
\\129\000\000\000\
\\130\000\000\000\
\\131\000\000\000\
\\132\000\000\000\
\\133\000\000\000\
\"
val actionRowNumbers =
"\035\000\002\000\045\000\042\000\
\\068\000\003\000\018\000\067\000\
\\066\000\006\000\007\000\008\000\
\\009\000\026\000\036\000\072\000\
\\027\000\043\000\001\000\037\000\
\\036\000\029\000\036\000\036\000\
\\036\000\036\000\063\000\062\000\
\\014\000\019\000\071\000\035\000\
\\050\000\015\000\020\000\047\000\
\\046\000\041\000\040\000\049\000\
\\016\000\022\000\010\000\005\000\
\\011\000\012\000\036\000\078\000\
\\077\000\076\000\075\000\074\000\
\\073\000\028\000\044\000\036\000\
\\036\000\036\000\036\000\036\000\
\\030\000\029\000\021\000\031\000\
\\070\000\061\000\036\000\059\000\
\\058\000\038\000\055\000\054\000\
\\053\000\052\000\051\000\048\000\
\\017\000\032\000\023\000\013\000\
\\039\000\033\000\024\000\004\000\
\\060\000\034\000\025\000\068\000\
\\036\000\065\000\064\000\056\000\
\\069\000\057\000\000\000"
val gotoT =
"\
\\001\000\094\000\002\000\004\000\004\000\003\000\005\000\002\000\
\\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\002\000\017\000\004\000\003\000\005\000\002\000\006\000\001\000\000\000\
\\003\000\018\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\028\000\000\000\
\\000\000\
\\006\000\032\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\039\000\000\000\
\\006\000\040\000\000\000\
\\006\000\042\000\000\000\
\\006\000\043\000\000\000\
\\006\000\044\000\000\000\
\\006\000\045\000\000\000\
\\000\000\
\\000\000\
\\007\000\046\000\000\000\
\\000\000\
\\000\000\
\\004\000\054\000\005\000\002\000\006\000\001\000\000\000\
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
\\006\000\069\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\070\000\000\000\
\\006\000\071\000\000\000\
\\006\000\072\000\000\000\
\\006\000\073\000\000\000\
\\006\000\074\000\000\000\
\\000\000\
\\006\000\076\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\079\000\000\000\
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
\\003\000\092\000\000\000\
\\006\000\093\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 95
val numrules = 37
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
datatype svalue = VOID | ntVOID of unit | String of  (string)
 | Identifier of  (string) | Integer of  (int) | Condition of  (Cond)
 | MemRef of  (MemoryRef) | Instr of  (Instruction)
 | InstrLab of  (Instruction) | Equates of  (Instruction list)
 | Exec of  (Instruction list) | Prog of  (Instruction list)
end
type svalue = MlyValue.svalue
type result = Instruction list
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
  | (T 1) => "Assign"
  | (T 2) => "Equate"
  | (T 3) => "Colon"
  | (T 4) => "Comma"
  | (T 5) => "LeftParen"
  | (T 6) => "RightParen"
  | (T 7) => "GreaterEqual"
  | (T 8) => "Greater"
  | (T 9) => "LessEqual"
  | (T 10) => "Less"
  | (T 11) => "Equal"
  | (T 12) => "NotEqual"
  | (T 13) => "Plus"
  | (T 14) => "Sub"
  | (T 15) => "Mul"
  | (T 16) => "Divide"
  | (T 17) => "Modulo"
  | (T 18) => "LeftBracket"
  | (T 19) => "RightBracket"
  | (T 20) => "Integer"
  | (T 21) => "Identifier"
  | (T 22) => "If"
  | (T 23) => "Then"
  | (T 24) => "Goto"
  | (T 25) => "ReadInt"
  | (T 26) => "WriteInt"
  | (T 27) => "ReadStr"
  | (T 28) => "String"
  | (T 29) => "WriteStr"
  | (T 30) => "Halt"
  | (T 31) => "Break"
  | (T 32) => "M"
  | (T 33) => "PC"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 27) $$ (T 26)
 $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 19) $$ (T 18) $$ (T 17)
 $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10)
 $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 
2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( _, _, EOF1right)) :: ( _, ( MlyValue.Equates Equates
, _, _)) :: ( _, ( MlyValue.Exec Exec, Exec1left, _)) :: rest671)) =>
 let val  result = MlyValue.Prog (Exec@Equates)
 in ( LrTable.NT 0, ( result, Exec1left, EOF1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.InstrLab InstrLab, InstrLab1left, 
InstrLab1right)) :: rest671)) => let val  result = MlyValue.Exec (
[InstrLab])
 in ( LrTable.NT 1, ( result, InstrLab1left, InstrLab1right), rest671)

end
|  ( 2, ( ( _, ( MlyValue.Exec Exec, _, Exec1right)) :: ( _, ( 
MlyValue.InstrLab InstrLab, InstrLab1left, _)) :: rest671)) => let
 val  result = MlyValue.Exec ([InstrLab]@Exec)
 in ( LrTable.NT 1, ( result, InstrLab1left, Exec1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.InstrLab InstrLab, _, InstrLab1right)) :: _
 :: ( _, ( MlyValue.Identifier Identifier, Identifier1left, _)) :: 
rest671)) => let val  result = MlyValue.InstrLab (
label'(Identifier,InstrLab))
 in ( LrTable.NT 3, ( result, Identifier1left, InstrLab1right), 
rest671)
end
|  ( 4, ( ( _, ( MlyValue.Instr Instr, Instr1left, Instr1right)) :: 
rest671)) => let val  result = MlyValue.InstrLab (Instr)
 in ( LrTable.NT 3, ( result, Instr1left, Instr1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.Integer Integer, _, Integer1right)) :: _ :: 
( _, ( MlyValue.MemRef MemRef, MemRef1left, _)) :: rest671)) => let
 val  result = MlyValue.Instr (store'(MemRef,Integer))
 in ( LrTable.NT 4, ( result, MemRef1left, Integer1right), rest671)

end
|  ( 6, ( ( _, ( MlyValue.String String, _, String1right)) :: _ :: ( _
, ( MlyValue.MemRef MemRef, MemRef1left, _)) :: rest671)) => let val  
result = MlyValue.Instr (storeString'(MemRef,String))
 in ( LrTable.NT 4, ( result, MemRef1left, String1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.Integer Integer, _, Integer1right)) :: _ ::
 _ :: _ :: ( _, ( MlyValue.MemRef MemRef1, MemRef1left, _)) :: rest671
)) => let val  result = MlyValue.Instr (storePC'(MemRef1,Integer))
 in ( LrTable.NT 4, ( result, MemRef1left, Integer1right), rest671)

end
|  ( 8, ( ( _, ( MlyValue.MemRef MemRef, _, MemRef1right)) :: _ :: ( _
, ( _, PC1left, _)) :: rest671)) => let val  result = MlyValue.Instr (
restorePC'(MemRef))
 in ( LrTable.NT 4, ( result, PC1left, MemRef1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.MemRef MemRef2, _, MemRef2right)) :: _ :: (
 _, ( MlyValue.MemRef MemRef1, MemRef1left, _)) :: rest671)) => let
 val  result = MlyValue.Instr (move'(MemRef1,MemRef2))
 in ( LrTable.NT 4, ( result, MemRef1left, MemRef2right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.MemRef MemRef3, _, MemRef3right)) :: _ :: (
 _, ( MlyValue.MemRef MemRef2, _, _)) :: _ :: ( _, ( MlyValue.MemRef 
MemRef1, MemRef1left, _)) :: rest671)) => let val  result = 
MlyValue.Instr (add'(MemRef1,MemRef2,MemRef3))
 in ( LrTable.NT 4, ( result, MemRef1left, MemRef3right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.MemRef MemRef3, _, MemRef3right)) :: _ :: (
 _, ( MlyValue.MemRef MemRef2, _, _)) :: _ :: ( _, ( MlyValue.MemRef 
MemRef1, MemRef1left, _)) :: rest671)) => let val  result = 
MlyValue.Instr (sub'(MemRef1,MemRef2,MemRef3))
 in ( LrTable.NT 4, ( result, MemRef1left, MemRef3right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.MemRef MemRef3, _, MemRef3right)) :: _ :: (
 _, ( MlyValue.MemRef MemRef2, _, _)) :: _ :: ( _, ( MlyValue.MemRef 
MemRef1, MemRef1left, _)) :: rest671)) => let val  result = 
MlyValue.Instr (mul'(MemRef1,MemRef2,MemRef3))
 in ( LrTable.NT 4, ( result, MemRef1left, MemRef3right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.MemRef MemRef3, _, MemRef3right)) :: _ :: (
 _, ( MlyValue.MemRef MemRef2, _, _)) :: _ :: ( _, ( MlyValue.MemRef 
MemRef1, MemRef1left, _)) :: rest671)) => let val  result = 
MlyValue.Instr (div'(MemRef1,MemRef2,MemRef3))
 in ( LrTable.NT 4, ( result, MemRef1left, MemRef3right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.MemRef MemRef3, _, MemRef3right)) :: _ :: (
 _, ( MlyValue.MemRef MemRef2, _, _)) :: _ :: ( _, ( MlyValue.MemRef 
MemRef1, MemRef1left, _)) :: rest671)) => let val  result = 
MlyValue.Instr (mod'(MemRef1,MemRef2,MemRef3))
 in ( LrTable.NT 4, ( result, MemRef1left, MemRef3right), rest671)
end
|  ( 15, ( ( _, ( _, _, RightBracket1right)) :: ( _, ( 
MlyValue.Integer Integer, _, _)) :: _ :: ( _, ( MlyValue.MemRef 
MemRef2, _, _)) :: _ :: _ :: _ :: ( _, ( MlyValue.MemRef MemRef1, 
MemRef1left, _)) :: rest671)) => let val  result = MlyValue.Instr (
indRef'(MemRef1,MemRef2,Integer))
 in ( LrTable.NT 4, ( result, MemRef1left, RightBracket1right), 
rest671)
end
|  ( 16, ( ( _, ( MlyValue.MemRef MemRef2, _, MemRef2right)) :: _ :: _
 :: ( _, ( MlyValue.Integer Integer, _, _)) :: _ :: ( _, ( 
MlyValue.MemRef MemRef1, _, _)) :: _ :: ( _, ( _, M1left, _)) :: 
rest671)) => let val  result = MlyValue.Instr (
indStore'(MemRef1,Integer,MemRef2))
 in ( LrTable.NT 4, ( result, M1left, MemRef2right), rest671)
end
|  ( 17, ( ( _, ( _, _, RightParen1right)) :: ( _, ( MlyValue.MemRef 
MemRef, _, _)) :: _ :: ( _, ( _, ReadInt1left, _)) :: rest671)) => let
 val  result = MlyValue.Instr (readInt'(MemRef))
 in ( LrTable.NT 4, ( result, ReadInt1left, RightParen1right), rest671
)
end
|  ( 18, ( ( _, ( _, _, RightParen1right)) :: ( _, ( MlyValue.MemRef 
MemRef, _, _)) :: _ :: ( _, ( _, WriteInt1left, _)) :: rest671)) =>
 let val  result = MlyValue.Instr (writeInt'(MemRef))
 in ( LrTable.NT 4, ( result, WriteInt1left, RightParen1right), 
rest671)
end
|  ( 19, ( ( _, ( _, _, RightParen1right)) :: ( _, ( MlyValue.MemRef 
MemRef2, _, _)) :: _ :: ( _, ( MlyValue.MemRef MemRef1, _, _)) :: _ ::
 ( _, ( _, ReadStr1left, _)) :: rest671)) => let val  result = 
MlyValue.Instr (readStr'(MemRef1,MemRef2))
 in ( LrTable.NT 4, ( result, ReadStr1left, RightParen1right), rest671
)
end
|  ( 20, ( ( _, ( _, _, RightParen1right)) :: ( _, ( MlyValue.MemRef 
MemRef, _, _)) :: _ :: ( _, ( _, WriteStr1left, _)) :: rest671)) =>
 let val  result = MlyValue.Instr (writeStr'(MemRef))
 in ( LrTable.NT 4, ( result, WriteStr1left, RightParen1right), 
rest671)
end
|  ( 21, ( ( _, ( MlyValue.Integer Integer, _, Integer1right)) :: ( _,
 ( _, Goto1left, _)) :: rest671)) => let val  result = MlyValue.Instr
 (goto'(Integer))
 in ( LrTable.NT 4, ( result, Goto1left, Integer1right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.Identifier Identifier, _, Identifier1right)
) :: ( _, ( _, Goto1left, _)) :: rest671)) => let val  result = 
MlyValue.Instr (gotolab'(Identifier))
 in ( LrTable.NT 4, ( result, Goto1left, Identifier1right), rest671)

end
|  ( 23, ( ( _, ( MlyValue.Integer Integer, _, Integer1right)) :: _ ::
 _ :: ( _, ( MlyValue.MemRef MemRef2, _, _)) :: ( _, ( 
MlyValue.Condition Condition, _, _)) :: ( _, ( MlyValue.MemRef MemRef1
, _, _)) :: ( _, ( _, If1left, _)) :: rest671)) => let val  result = 
MlyValue.Instr (if'(Condition,MemRef1,MemRef2,Integer))
 in ( LrTable.NT 4, ( result, If1left, Integer1right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.Identifier Identifier, _, Identifier1right)
) :: _ :: _ :: ( _, ( MlyValue.MemRef MemRef2, _, _)) :: ( _, ( 
MlyValue.Condition Condition, _, _)) :: ( _, ( MlyValue.MemRef MemRef1
, _, _)) :: ( _, ( _, If1left, _)) :: rest671)) => let val  result = 
MlyValue.Instr (iflab'(Condition,MemRef1,MemRef2,Identifier))
 in ( LrTable.NT 4, ( result, If1left, Identifier1right), rest671)
end
|  ( 25, ( ( _, ( _, Halt1left, Halt1right)) :: rest671)) => let val  
result = MlyValue.Instr (halt')
 in ( LrTable.NT 4, ( result, Halt1left, Halt1right), rest671)
end
|  ( 26, ( ( _, ( _, Break1left, Break1right)) :: rest671)) => let
 val  result = MlyValue.Instr (break')
 in ( LrTable.NT 4, ( result, Break1left, Break1right), rest671)
end
|  ( 27, ( rest671)) => let val  result = MlyValue.Equates ([])
 in ( LrTable.NT 2, ( result, defaultPos, defaultPos), rest671)
end
|  ( 28, ( ( _, ( MlyValue.Equates Equates, _, Equates1right)) :: _ ::
 ( _, ( MlyValue.Integer Integer, _, _)) :: _ :: _ :: ( _, ( 
MlyValue.Identifier Identifier, _, _)) :: ( _, ( _, Equate1left, _))
 :: rest671)) => let val  result = MlyValue.Equates (
[memdef'(Identifier,Integer)]@Equates)
 in ( LrTable.NT 2, ( result, Equate1left, Equates1right), rest671)

end
|  ( 29, ( ( _, ( _, _, RightBracket1right)) :: ( _, ( 
MlyValue.Integer Integer, _, _)) :: _ :: ( _, ( _, M1left, _)) :: 
rest671)) => let val  result = MlyValue.MemRef (memref'(Integer))
 in ( LrTable.NT 5, ( result, M1left, RightBracket1right), rest671)

end
|  ( 30, ( ( _, ( MlyValue.Identifier Identifier, Identifier1left, 
Identifier1right)) :: rest671)) => let val  result = MlyValue.MemRef (
memrefId'(Identifier))
 in ( LrTable.NT 5, ( result, Identifier1left, Identifier1right), 
rest671)
end
|  ( 31, ( ( _, ( _, GreaterEqual1left, GreaterEqual1right)) :: 
rest671)) => let val  result = MlyValue.Condition (greaterEq')
 in ( LrTable.NT 6, ( result, GreaterEqual1left, GreaterEqual1right), 
rest671)
end
|  ( 32, ( ( _, ( _, Greater1left, Greater1right)) :: rest671)) => let
 val  result = MlyValue.Condition (greater')
 in ( LrTable.NT 6, ( result, Greater1left, Greater1right), rest671)

end
|  ( 33, ( ( _, ( _, LessEqual1left, LessEqual1right)) :: rest671)) =>
 let val  result = MlyValue.Condition (lessEq')
 in ( LrTable.NT 6, ( result, LessEqual1left, LessEqual1right), 
rest671)
end
|  ( 34, ( ( _, ( _, Less1left, Less1right)) :: rest671)) => let val  
result = MlyValue.Condition (less')
 in ( LrTable.NT 6, ( result, Less1left, Less1right), rest671)
end
|  ( 35, ( ( _, ( _, Equal1left, Equal1right)) :: rest671)) => let
 val  result = MlyValue.Condition (eq')
 in ( LrTable.NT 6, ( result, Equal1left, Equal1right), rest671)
end
|  ( 36, ( ( _, ( _, NotEqual1left, NotEqual1right)) :: rest671)) =>
 let val  result = MlyValue.Condition (neq')
 in ( LrTable.NT 6, ( result, NotEqual1left, NotEqual1right), rest671)

end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.Prog x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a 
end
end
structure Tokens : ewe_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun Assign (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun Equate (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun Colon (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun Comma (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun LeftParen (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun RightParen (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun GreaterEqual (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun Greater (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun LessEqual (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun Less (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun Equal (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun NotEqual (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun Plus (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun Sub (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun Mul (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun Divide (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun Modulo (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun LeftBracket (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun RightBracket (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun Integer (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.Integer i,p1,p2))
fun Identifier (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.Identifier i,p1,p2))
fun If (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun Then (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun Goto (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun ReadInt (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun WriteInt (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun ReadStr (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun String (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.String i,p1,p2))
fun WriteStr (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun Halt (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun Break (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun M (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun PC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
end
end
