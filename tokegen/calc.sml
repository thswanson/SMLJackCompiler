(*Tyler Swanson, worked close to Kyle*)
(*Help recieved from: Jonathan, Geggory, Josiah*)
(*Last implementation recieved help from: Zach, Kyle, Greg, Jonathan, and Adam*)

structure calc =
struct
open RegisterAllocation;
open calcAS;

     structure calcLrVals = calcLrValsFun(structure Token = LrParser.Token)

     structure calcLex = calcLexFun(structure Tokens = calcLrVals.Tokens)

     structure calcParser = Join(structure Lex= calcLex
                                structure LrParser = LrParser
                                structure ParserData = calcLrVals.ParserData)

     val input_line =
       fn f =>
          let val sOption = TextIO.inputLine f
          in
            if isSome(sOption) then
               Option.valOf(sOption)
            else
               ""
          end

     val calcparse =
         fn filename =>
           let val instrm = TextIO.openIn filename
               val lexer = calcParser.makeLexer(fn i => input_line instrm)
               val _ = calcLex.UserDeclarations.pos := 1
               val error = fn (e,i:int,_) =>
                               TextIO.output(TextIO.stdOut," line " ^ (Int.toString i) ^ ", Error: " ^ e ^ "\n")
           in
                calcParser.parse(30,lexer,error,()) before TextIO.closeIn instrm
           end

     (* These functions are needed for if-then-else expressions and functions *)
     val label = ref 0;
     val whilelabel = ref 0;

     fun nextLabel() =
         let val lab = !label
         in
           label := !label + 1;
           Int.toString(lab)
         end

     fun nextWhileLabel() =
         let val whilelab = !whilelabel
         in
            whilelabel := !whilelabel + 1;
            Int.toString(whilelab)
         end
    fun resetLabels() =
          (label := 0)

    fun resetWhileLabels() =
          (whilelabel := 0)


     val relOpOpposites = [("=","<>"),("<>","="),("<=",">"),(">=","<"),("<",">="),(">","<=")];

     exception notLocated;

     fun opposite(relOp) =
       let fun mappedVal x nil = raise notLocated
             | mappedVal (x:string) ((y,z)::rest) = if x = y then z else mappedVal x rest
       in
         mappedVal relOp relOpOpposites
       end

     (* These functions are needed for function and constant bindings *)

     fun forloop (0, f, x) = 0
       | forloop (y, f, x) = (f x; forloop(y-1, f, x));


     exception unboundId;

     (*datatype Typeold = function' of string
                   | constant' of string;*)

     (*fun boundTo(name,[]) =
         let val idname = (case name of
                              function'(s) => s
                            | constant'(s) => s)
         in
           TextIO.output(TextIO.stdOut, "Unbound identifier "^idname^" referenced or type error!\n");
           raise unboundId
         end

       | boundTo(name,(n,ol,depth)::t) = if name=n then ol else boundTo(name,t);

     fun depthOf(name,[]) =
         let val idname = (case name of
                              function'(s) => s
                            | constant'(s) => s)
         in
           TextIO.output(TextIO.stdOut, "Unbound identifier "^idname^" referenced or type error!\n");
           raise unboundId
         end

       | depthOf(name,(n,ol,depth)::t) = if name=n then depth else depthOf(name,t);*)

     val frameSize = 88;

     (* This is the code generation for the compiler *)

     exception Unimplemented;

     fun genvardec(name::names,types,offset,segment) =
          (TextIO.output(TextIO.stdOut, ""^ name ^ " at offset " ^ Int.toString(offset) ^  "\n\n");
          (name,types,offset,segment)::genvardec(names,types,offset+1,segment))

       | genvardec([],types,offset,segment) =
            ([])

     fun generatevd((types,namelist)::vdl,offset,segment) =
           (TextIO.output(TextIO.stdOut, "namelist at length " ^ Int.toString(length(namelist) + offset) ^ "\n\n");
           (genvardec(namelist,types,offset,segment)@generatevd(vdl,length(namelist) + offset,segment)))

       | generatevd([],offset,segment) =
           ( TextIO.output(TextIO.stdOut, "I hit the null case");
             [])

     fun generatepb((types,name)::pl,offset,segment) =
         ((name,types,offset,segment)::generatepb(pl,offset+1,segment))

      | generatepb([],offset,segment) =
          ([])

     fun generatecvd(name::names,types,offset,segment) =
          (TextIO.output(TextIO.stdOut, ""^ name ^ " at offset " ^ Int.toString(offset) ^  "\n\n");
            (name,types,offset,segment)::generatecvd(names,types,offset+1,segment))
       | generatecvd([],types,offset,segment) =
          ([])
     fun generatestaticvd(name::names, types, offset,segment) =
         (TextIO.output(TextIO.stdOut, ""^ name ^ " at offset " ^ Int.toString(offset) ^  "\n\n");
          (name,types,offset,segment)::generatestaticvd(names,types,offset+1,segment))
        | generatestaticvd([],types,offset,segment) =
          ([])

     fun generatecvdl((segment,types,names)::cvdl,offset,staticoffset) =
          (TextIO.output(TextIO.stdOut, "names at length " ^ Int.toString(length(names)) ^ "\n segment is "^segment^"\n");
          if segment = "field" then (generatecvd(names,types,offset,segment)@generatecvdl(cvdl,length(names) + offset, staticoffset))
          else (generatestaticvd(names,types,offset,segment)@generatecvdl(cvdl,offset,staticoffset+1))
          ) (*generatestaticvd(names,types,staticoffset,segment)@generatecvdl(cvdl,offset,staticoffset+1))*)

        | generatecvdl([],offset,staticoffset) =
          (TextIO.output(TextIO.stdOut, "null case hit \n");
            [])

      fun tuplePatternName((x,yy,y,z)) =
          ((x))

      fun tupleOffset((x,yy,y,z)) =
          ((y))

      fun tupleSegment((x,yy,y,z)) =
          ((z))

      fun tupleType((x,yy,y,z)) =
          ((yy))

     fun findname(var::varlist,identifier) =
          (
          if identifier = tuplePatternName(var) then
          (TextIO.output(TextIO.stdOut, "we found " ^ tuplePatternName(var) ^ " yay \n");
          (var))
          else findname(varlist,identifier))

     | findname([],identifier) =
          (("","",3,""))(*this should never get called*)
      (*)[(tuple1), (tuple2), (tuple3)]*)
      fun countField((x,yy,y,z)::varlist, number) =
          (if z = "field" then (countField(varlist,number + 1))
          else (countField(varlist, number)))
        | countField([],number) =
            (number)

      fun printbindings(bound::bindings) =
          (TextIO.output(TextIO.stdOut, "binding " ^ tuplePatternName(bound) ^ " is in the binding \n");
          printbindings(bindings))

        | printbindings([]) =
              (TextIO.output(TextIO.stdOut, "Printing bindings hit null \n");
              ())


    (*208-209 ch 10*)
    fun codegen(class'(s, cvdl, sublist),outFile,bindings,offset,name) =
            let val cvdbindings = generatecvdl(cvdl,0,0)
             val printer = printbindings(cvdbindings)
            in

                codegenlist(sublist,outFile,cvdbindings,offset,s)
            end

            (*TextIO.output(TextIO.stdOut, "<class>\n" );
            TextIO.output(TextIO.stdOut, "<keyword> class </keyword>\n");
            codegen(id'(s),outFile,bindings,offset,name);
            TextIO.output(TextIO.stdOut, "<symbol> { </symbol>\n" );
            TextIO.output(TextIO.stdOut, "<classvardec>\n");
            codegenlist(cvdl,outFile,bindings,offset,name);
            TextIO.output(TextIO.stdOut, "</classvardec>\n");
            codegenlist(sublist,outFile,bindings,offset,s);
            TextIO.output(TextIO.stdOut, "<symbol> } </symbol>\n" );
            TextIO.output(TextIO.stdOut, "</class>\n")*)
            (*let val classvars = generatevd(cvdl,0,"global")
            in
              (*codegen()*)
            end*)

    (*) | codegen(staticvar'(t, vnl),outFile,bindings,offset,name) =
            (TextIO.output(TextIO.stdOut, "<keyword> static </keyword>\n");
            codegenstringlist(vnl,outFile,bindings,offset,name))

     | codegen(fieldvar'(t, vnl),outFile,bindings,offset,name) =
            (TextIO.output(TextIO.stdOut, "<keyword> field </keyword>\n");
            codegenstringlist(vnl,outFile,bindings,offset,name))*)

     (*)| codegen(int',outFile,bindings,offset,name) =
            (TextIO.output(TextIO.stdOut, "<keyword> int </keyword>\n"))

     | codegen(char',outFile,bindings,offset,name) =
            (TextIO.output(TextIO.stdOut, "<keyword> char </keyword>\n"))

     | codegen(bool',outFile,bindings,offset,name) =
            (TextIO.output(TextIO.stdOut, "<keyword> bool </keyword>\n"))

     | codegen(void',outFile,bindings,offset,name) =
           (TextIO.output(TextIO.stdOut, "<keyword> void </keyword>\n"))*)

     | codegen(id'(id),outFile,bindings,offset,name) =
          let val bounded = findname(bindings,id);
          in
            if tupleSegment(bounded) = "field" then
               (TextIO.output(outFile, "push this "^ Int.toString(tupleOffset(bounded))^"\n"))
            else (
               TextIO.output(outFile, "push "^tupleSegment(bounded)^ " " ^ Int.toString(tupleOffset(bounded))^"\n"))
           end


     | codegen(constructor'(t,s,pl,vdl,states),outFile,bindings,offset,name) =
          let val parambindings = generatepb(pl,0,"argument");
              val varbindings = generatevd(vdl,0,"local")
              val number = countField(bindings,0);
          in
              TextIO.output(outFile, "function "^name^"."^s^" "^Int.toString(length(varbindings))^"\n");
              TextIO.output(outFile, "push constant "^ Int.toString(number)^ "\n");

              TextIO.output(outFile, "call Memory.alloc 1\n");
              TextIO.output(outFile, "pop pointer 0 \n");
              codegenlist(states,outFile,bindings@varbindings@parambindings,offset,name);
              resetLabels();
              resetWhileLabels()
          end
            (*TextIO.output(TextIO.stdOut, "<subroutinedec>\n");
            TextIO.output(TextIO.stdOut, "<keyword> constructor </keyword>\n");
            codegen(t,outFile,bindings,offset,name);
            codegen(id'(s),outFile,bindings,offset,name);
            TextIO.output(TextIO.stdOut, "<symbol> ( </symbol>\n");
            TextIO.output(TextIO.stdOut, "<parameterlist>\n");
            codegenlist(pl,outFile,bindings,offset,name);
            TextIO.output(TextIO.stdOut, "</parameterlist>\n");
            TextIO.output(TextIO.stdOut, "<symbol> ) </symbol>\n");
            codegen(sb,outFile,bindings,offset,name);
            TextIO.output(TextIO.stdOut, "</subroutinedec>\n");
            TextIO.output(outFile, ""))*)
            (*let
              val varbindings = generatevdl(cdl,0,"");
              val pbindings = generatepb*)


     | codegen(function'(t,s,pl,vdl,states),outFile,bindings,offset,name) =
            let (*val parameterbindings = genpb(pl,0);*)
                val varbindings = generatevd(vdl,0,"local");
                val pbindings = generatepb(pl,0,"argument")
            in
            (*TextIO.output(TextIO.stdOut, "<subroutinedec>\n");
            TextIO.output(TextIO.stdOut, "<keyword> function </keyword>\n");
            codegen(t,outFile,bindings,offset,name);
            codegen(id'(s),outFile,bindings,offset,name);
            TextIO.output(TextIO.stdOut, "<symbol> ( </symbol>\n");
            TextIO.output(TextIO.stdOut, "<parameterlist>\n");
            codegenlist(pl,outFile,bindings,offset,name);
            TextIO.output(TextIO.stdOut, "</parameterlist>\n");
            TextIO.output(TextIO.stdOut, "<symbol> ) </symbol>\n");
            codegen(sb,outFile,bindings,offset,name);
            TextIO.output(TextIO.stdOut, "</subroutinedec>\n");*)
              TextIO.output(outFile, "function "^name^"."^s^" "^Int.toString(length(varbindings))^"\n");
              codegenlist(states,outFile,bindings@varbindings@pbindings,offset,name);
              resetLabels();
              resetWhileLabels()
            end

     | codegen(method'(t,s,pl,vdl,states),outFile,bindings,offset,name) =
             let val parambindings = generatepb(pl,1,"argument");
                 val varbindings = generatevd(vdl,0,"local")
             in
                 TextIO.output(outFile, "function "^name^"."^s^" "^Int.toString(length(varbindings))^"\n");
                 TextIO.output(outFile, "push argument 0 \n");
                 TextIO.output(outFile, "pop pointer 0 \n");
                 codegenlist(states,outFile,bindings@varbindings@parambindings,offset,name);
                 resetLabels();
                 resetWhileLabels()
             end
            (*)(TextIO.output(TextIO.stdOut, "<subroutinedec>\n");
            TextIO.output(TextIO.stdOut, "<keyword> method </keyword>\n");
            codegen(t,outFile,bindings,offset,name);
            codegen(id'(s),outFile,bindings,offset,name);
            TextIO.output(TextIO.stdOut, "<symbol> ( </symbol>\n");
            TextIO.output(TextIO.stdOut, "<parameterlist>\n");
            codegenlist(pl,outFile,bindings,offset,name);
            TextIO.output(TextIO.stdOut, "</parameterlist>\n");
            TextIO.output(TextIO.stdOut, "<symbol> ) </symbol>\n");
            codegen(sb,outFile,bindings,offset,name);
            TextIO.output(TextIO.stdOut, "</subroutinedec>\n"))*)
            (*push argument 0
            pop argument 0*)

     | codegen(letstatement'(state),outFile,bindings,offset,name) =
            (TextIO.output(TextIO.stdOut, "<letstatement>\n");
            codegen(state,outFile,bindings,offset,name);
            TextIO.output(TextIO.stdOut, "</letstatement>\n"))

     | codegen(ifstatement'(state),outFile,bindings,offset,name) =
            (TextIO.output(TextIO.stdOut, "<ifstatement>\n");
            codegen(state,outFile,bindings,offset,name);
            TextIO.output(TextIO.stdOut, "</ifstatement>\n"))

     | codegen(whilestatement'(state),outFile,bindings,offset,name) =
            (TextIO.output(TextIO.stdOut, "<whilestatement>\n");
            codegen(state,outFile,bindings,offset,name);
            TextIO.output(TextIO.stdOut, "</whilestatement>\n"))

     | codegen(dostatement'(state),outFile,bindings,offset,name) =
            (TextIO.output(TextIO.stdOut, "<dostatement>\n");
            codegen(state,outFile,bindings,offset,name);
            TextIO.output(TextIO.stdOut, "</dostatement>\n"))

     | codegen(returnstatement'(state),outFile,bindings,offset,name) =
            (TextIO.output(TextIO.stdOut, "<returnstatement>\n");
            codegen(state,outFile,bindings,offset,name);
            TextIO.output(TextIO.stdOut, "</returnstatement>\n"))

     | codegen(letstate'(id,expr),outFile,bindings,offset,name) =
            (*TextIO.output(TextIO.stdOut, "<keyword>let<keyword>\n");
            codegen(id'(id),outFile,bindings,offset,name);
            TextIO.output(TextIO.stdOut, "<symbol> = </symbol>\n");
            TextIO.output(TextIO.stdOut, "<expression>\n");
            codegen(expr,outFile,bindings,offset,name);
            TextIO.output(TextIO.stdOut, "</expression>\n");
            TextIO.output(TextIO.stdOut, "<symbol> ; </symbol>\n")*)
            let val bounded = findname(bindings,id);
            in
              if tupleSegment(bounded) = "field" then
                (codegen(expr,outFile,bindings,offset,name);
                TextIO.output(outFile, "pop this "^Int.toString(tupleOffset(bounded))^"\n"))
              else (
                if tupleSegment(bounded) = "static" then
                (codegen(expr,outFile,bindings,offset,name);
                TextIO.output(outFile, "pop static "^Int.toString(tupleOffset(bounded))^"\n"))
                else (
                codegen(expr,outFile,bindings,offset,name);

                TextIO.output(outFile, "pop " ^tupleSegment(bounded)^ " " ^ Int.toString(tupleOffset(bounded))^"\n")))
                (*pop and push pointer *)

            end

      | codegen(letexpr'(id,expr1,expr2),outFile,bindings,offset,name) =
             (*TextIO.output(TextIO.stdOut, "<keyword>let<keyword>\n");
             codegen(id'(id),outFile,bindings,offset,name);
             TextIO.output(TextIO.stdOut, "<symbol> [ </symbol>\n");
             TextIO.output(TextIO.stdOut, "<expression>\n");
             codegen(expr1,outFile,bindings,offset,name);
             TextIO.output(TextIO.stdOut, "</expression>\n");
             TextIO.output(TextIO.stdOut, "<symbol> ] </symbol>\n");
             TextIO.output(TextIO.stdOut, "<symbol> = </symbol>\n");
             TextIO.output(TextIO.stdOut, "<expression>\n");
             codegen(expr2,outFile,bindings,offset,name);
             TextIO.output(TextIO.stdOut, "</expression>\n");
             TextIO.output(TextIO.stdOut, "<symbol> ; </symbol>\n")*)
             let val bounded = findname(bindings,id);
             in
               if tupleSegment(bounded) = "field" then
                 (TextIO.output(outFile, "push "^tupleSegment(bounded)^ " " ^ Int.toString(tupleOffset(bounded))^"\n");
                 codegen(expr1,outFile,bindings,offset,name);
                 codegen(expr2,outFile,bindings,offset,name);
                 TextIO.output(outFile, "pop this "^Int.toString(tupleOffset(bounded))^"\n"))
               else (
                 if tupleSegment(bounded) = "static" then
                 (codegen(expr1,outFile,bindings,offset,name);
                 codegen(expr2,outFile,bindings,offset,name);
                 TextIO.output(outFile, "pop static "^Int.toString(tupleOffset(bounded))^"\n"))
                 else (
                 codegen(expr1,outFile,bindings,offset,name);
                 TextIO.output(outFile, "push "^tupleSegment(bounded)^ " " ^ Int.toString(tupleOffset(bounded))^"\n");
                 TextIO.output(outFile, "add \n");
                 codegen(expr2,outFile,bindings,offset,name);
                 TextIO.output(outFile, "pop temp 0 \npop pointer 1 \npush temp 0 \npop that 0\n")
                 )
                 )
             end

       | codegen(if'(expr,state),outFile,bindings,offset,name) =
              (*TextIO.output(TextIO.stdOut, "<keyword>if<keyword>\n");
              TextIO.output(TextIO.stdOut, "<symbol> ( </symbol\n");
              TextIO.output(TextIO.stdOut, "<expression>\n");
              codegen(expr,outFile,bindings,offset,name);
              TextIO.output(TextIO.stdOut, "</expression>\n");
              TextIO.output(TextIO.stdOut, "<symbol> ) </symbol\n");
              TextIO.output(TextIO.stdOut, "<symbol> { </symbol\n");
              codegenlist(state,outFile,bindings,offset,name);
              TextIO.output(TextIO.stdOut, "<symbol> } </symbol\n")*)
              let val label = nextLabel()
              in
                codegen(expr,outFile,bindings,offset,name);
                TextIO.output(outFile, "if-goto IF_TRUE"^label^"\n");
                TextIO.output(outFile, "goto IF_FALSE"^label^"\n");
                TextIO.output(outFile, "label IF_TRUE"^label^"\n");
                codegenlist(state,outFile,bindings,offset,name);
                TextIO.output(outFile, "label IF_FALSE"^label^"\n")
              end

       | codegen(ifelse'(expr,state1,state2),outFile,bindings,offset,name) =
              (*TextIO.output(TextIO.stdOut, "<keyword>if<keyword>\n");
              TextIO.output(TextIO.stdOut, "<symbol> ( </symbol\n");
              TextIO.output(TextIO.stdOut, "<expression>\n");
              codegen(expr,outFile,bindings,offset,name);
              TextIO.output(TextIO.stdOut, "</expression>\n");
              TextIO.output(TextIO.stdOut, "<symbol> ) </symbol\n");
              TextIO.output(TextIO.stdOut, "<symbol> { </symbol\n");
              codegenlist(state1,outFile,bindings,offset,name);
              TextIO.output(TextIO.stdOut, "<symbol> } </symbol\n");
              TextIO.output(TextIO.stdOut, "<keyword> else </keyword>\n");
              TextIO.output(TextIO.stdOut, "<symbol> { </symbol\n");
              codegenlist(state2,outFile,bindings,offset,name);
              TextIO.output(TextIO.stdOut, "<symbol> } </symbol\n")*)
              let val label = nextLabel()
              in
                codegen(expr,outFile,bindings,offset,name);
                TextIO.output(outFile, "if-goto IF_TRUE"^label^"\n");
                TextIO.output(outFile, "goto IF_FALSE"^label^"\n");
                TextIO.output(outFile, "label IF_TRUE"^label^"\n");
                codegenlist(state1,outFile,bindings,offset,name);
                TextIO.output(outFile, "goto IF_END"^label^"\n");
                TextIO.output(outFile, "label IF_FALSE"^label^"\n");
                codegenlist(state2,outFile,bindings,offset+1,name);
                TextIO.output(outFile, "label IF_END"^label^"\n")
             end

         | codegen(while'(expr,states),outFile,bindings,offset,name) =
              (*TextIO.output(TextIO.stdOut, "<keyword> while </keyword>\n");
              TextIO.output(TextIO.stdOut, "<symbol> ( </symbol\n");
              TextIO.output(TextIO.stdOut, "<expression>\n");
              codegen(expr,outFile,bindings,offset,name);
              TextIO.output(TextIO.stdOut, "</expression>\n");
              TextIO.output(TextIO.stdOut, "<symbol> ) </symbol\n");
              TextIO.output(TextIO.stdOut, "<symbol> { </symbol\n");
              codegenlist(states,outFile,bindings,offset,name);
              TextIO.output(TextIO.stdOut, "<symbol> } </symbol\n")*)
              let val whilelabel = nextWhileLabel()
              in
                TextIO.output(outFile, "label WHILE_EXP"^whilelabel^"\n");
                codegen(expr,outFile,bindings,offset,name);
                TextIO.output(outFile, "not\n");
                TextIO.output(outFile, "if-goto WHILE_END"^whilelabel^"\n");
                codegenlist(states,outFile,bindings,offset,name);
                TextIO.output(outFile, "goto WHILE_EXP"^whilelabel^"\n");
                TextIO.output(outFile, "label WHILE_END"^whilelabel^"\n")
              end

         | codegen(do'(sc),outFile,bindings,offset,name) =
              ((*TextIO.output(TextIO.stdOut, "<keyword> do </keyword>\n");*)
              codegen(sc,outFile,bindings,offset,name);
              TextIO.output(outFile, "pop temp 0 \n"))
              (*TextIO.output(TextIO.stdOut, "<symbol> ; </symbol\n")*)
              (**)

         | codegen(returnvoid',outFile,bindings,offset,name) =
              (*TextIO.output(TextIO.stdOut, "<keyword> return </keyword>\n");*)
              (*TextIO.output(TextIO.stdOut, "<symbol> ; </symbol\n")*)
              (TextIO.output(outFile, "push constant 0 \n");
              TextIO.output(outFile, "return\n"))

         | codegen(return'(expr),outFile,bindings,offset,name) =
              ((*TextIO.output(TextIO.stdOut, "<keyword> return </keyword>\n");
              TextIO.output(TextIO.stdOut, "<expression>\n");
              codegen(expr,outFile,bindings,offset,name);
              TextIO.output(TextIO.stdOut, "</expression>\n");
              TextIO.output(TextIO.stdOut, "<symbol> ; </symbol\n")*)
              codegen(expr, outFile,bindings,offset,name);
              TextIO.output(outFile, "return \n"))

         | codegen(integer'(num),outFile,bindings,offset,name) =
              (*TextIO.output(TextIO.stdOut, "<integerconstant> "^Int.toString(num)^" </integerconstant>\n")*)
              (TextIO.output(outFile, "push constant "^Int.toString(num)^"\n"))

         | codegen(string'(s),outFile,bindings,offset,name) =
              let val stringexploded = String.explode(s);
                  val firstElements = List.take(stringexploded, (length(stringexploded)-1) );
                  val completedList = List.drop(firstElements, 1);
              in
              TextIO.output(outFile, "push constant " ^ Int.toString(length(completedList))^"\n");
              TextIO.output(outFile, "call String.new 1 \n");
                let fun getOrd([]) = ()
                | getOrd(str::strtail) =
                    (TextIO.output(outFile, "push constant "^ Int.toString(Char.ord(str))^"\n");
                    TextIO.output(outFile, "call String.appendChar 2 \n");
                    getOrd(strtail))
                in
                  getOrd(completedList)
                end
              end

         | codegen(idarray'(id, expr),outFile,bindings,offset,name) =
              (*codegen(id'(id),outFile,bindings,offset,name);
              TextIO.output(TextIO.stdOut, "<symbol> [ </symbol\n");
              codegen(expr,outFile,bindings,offset,name);
              TextIO.output(TextIO.stdOut, "<symbol> ] </symbol\n")*)
              let val binderz = findname(bindings,id)
              in
                  codegen(expr,outFile,bindings,offset,name);
                  TextIO.output(outFile, "push " ^ tupleSegment(binderz) ^ " " ^ Int.toString(tupleOffset(binderz))^"\n");
                  TextIO.output(outFile, "add \n");
                  TextIO.output(outFile, "pop pointer 1 \npush that 0\n")

                  (*push id on *)
                  (*might need add in here*)
                  (*pop push pointer *)
              end



         | codegen(subroutine'(src),outFile,bindings,offset,name) =
              (codegen(src,outFile,bindings,offset,name))

         | codegen(parenexpr'(expr),outFile,bindings,offset,name) =
              (TextIO.output(TextIO.stdOut, "<symbol> ( </symbol>\n");
              codegen(expr,outFile,bindings,offset,name);
             TextIO.output(TextIO.stdOut, "<symbol> ) </symbol>\n"))

         | codegen(subcall1'(id, exprlist),outFile,bindings,offset,name) =
              (*TextIO.output(TextIO.stdOut, "<subroutinecall>\n");
              codegen(id'(id),outFile,bindings,offset,name);
              TextIO.output(TextIO.stdOut, "<symbol> ( </symbol>\n");
              codegenlist(exprlist,outFile,bindings,offset,name);
              TextIO.output(TextIO.stdOut, "<symbol> ) </symbol>\n");
              TextIO.output(TextIO.stdOut, "</subroutinecall>\n")*)
              (*object is the first argument *)

              (TextIO.output(outFile, "push pointer 0\n");
              codegenlist(exprlist,outFile,bindings,offset,name);
              TextIO.output(outFile, "call "^name^ "."^id^ " " ^ Int.toString(length(exprlist) + 1) ^ "\n"))


          | codegen(subcall2'(id1,id2,exprlist),outFile,bindings,offset,name) =
               (*(TextIO.output(TextIO.stdOut, "<subroutinecall>\n");
               codegen(id'(id1),outFile,bindings,offset,name);
               TextIO.output(TextIO.stdOut, "<symbol>.</symbol>\n");
               codegen(id'(id2),outFile,bindings,offset,name);
               TextIO.output(TextIO.stdOut, "<symbol> ( </symbol>\n");
               codegenlist(exprlist,outFile,bindings,offset,name);
               TextIO.output(TextIO.stdOut, "<symbol> ) </symbol>\n");
               TextIO.output(TextIO.stdOut, "</subroutinecall>\n"))*)
               let val binderz = findname(bindings,id1)
               in

               if tupleType(binderz) = "" then
                 (codegenlist(exprlist,outFile,bindings,offset,name);
                 TextIO.output(outFile, "call ");
                 TextIO.output(outFile, id1^"."^id2);
                 TextIO.output(outFile, " "^Int.toString(length(exprlist))^" \n"))
               else(
                 if tupleSegment(binderz) = "field" then
                 (TextIO.output(outFile, "push this " ^Int.toString(tupleOffset(binderz))^"\n");
                 codegenlist(exprlist,outFile,bindings,offset,name);
                 TextIO.output(outFile, "call ");
                 TextIO.output(outFile, tupleType(binderz)^"."^id2);
                 TextIO.output(outFile, " "^Int.toString(length(exprlist) + 1)^" \n"))

                 else (
                   (TextIO.output(outFile, "push " ^tupleSegment(binderz)^" "^Int.toString(tupleOffset(binderz))^"\n");
                   codegenlist(exprlist,outFile,bindings,offset,name);
                   TextIO.output(outFile, "call ");
                   TextIO.output(outFile, tupleType(binderz)^"."^id2);
                   TextIO.output(outFile, " "^Int.toString(length(exprlist))^" \n"))
                   )
                   )
               end

          | codegen(true',outFile,bindings,offset,name) =
               (TextIO.output(outFile, "push constant 0 \n");
               TextIO.output(outFile,"not \n"))

          | codegen(false',outFile,bindings,offset,name) =
                (TextIO.output(outFile, "push constant 0 \n"))

          | codegen(null',outFile,bindings,offset,name) =
                (TextIO.output(outFile, "push constant 0 \n"))

          | codegen(this',outFile,bindings,offset,name) =
                (TextIO.output(outFile, "push pointer 0 \n"))

          | codegen(add'(expr1, expr2),outFile,bindings,offset,name) =
                (codegen(expr1,outFile,bindings,offset,name);
                TextIO.output(TextIO.stdOut, "<symbol> + </symbol>\n");
                codegen(expr2,outFile,bindings,offset,name);
                TextIO.output(outFile, "add\n"))

          | codegen(sub'(expr1, expr2),outFile,bindings,offset,name) =
                (codegen(expr1,outFile,bindings,offset,name);
                TextIO.output(TextIO.stdOut, "<symbol> - </symbol>\n");
                codegen(expr2,outFile,bindings,offset,name);
                TextIO.output(outFile, "sub\n"))
                (*(TextIO.output(outFile, "push sub")\n*)

          | codegen(mul'(expr1, expr2),outFile,bindings,offset,name) =
                (codegen(expr1,outFile,bindings,offset,name);
                TextIO.output(TextIO.stdOut, "<symbol> * </symbol>\n");
                codegen(expr2,outFile,bindings,offset,name);
                TextIO.output(outFile, "call Math.multiply 2 \n"))

           | codegen(div'(expr1, expr2),outFile,bindings,offset,name) =
                 (codegen(expr1,outFile,bindings,offset,name);
                 TextIO.output(TextIO.stdOut, "<symbol> / </symbol>\n");
                 codegen(expr2,outFile,bindings,offset,name);
                 TextIO.output(outFile, "call Math.divide 2 \n"))
                (*(TextIO.output(outFile, "call Math.divide number ")\n*)

           | codegen(and'(expr1, expr2),outFile,bindings,offset,name) =
                (codegen(expr1,outFile,bindings,offset,name);
                codegen(expr2,outFile,bindings,offset,name);
                TextIO.output(outFile, "and \n"))

           | codegen(or'(expr1, expr2),outFile,bindings,offset,name) =
               (codegen(expr1,outFile,bindings,offset,name);
               codegen(expr2,outFile,bindings,offset,name);
               TextIO.output(outFile, "or \n"))

           | codegen(lt'(expr1, expr2),outFile,bindings,offset,name) =
                 (codegen(expr1,outFile,bindings,offset,name);
                 codegen(expr2,outFile,bindings,offset,name);
                 TextIO.output(outFile, "lt \n"))

           | codegen(gt'(expr1, expr2),outFile,bindings,offset,name) =
                (codegen(expr1,outFile,bindings,offset,name);
                codegen(expr2,outFile,bindings,offset,name);
                TextIO.output(outFile, "gt \n")
                )

           | codegen(equal'(expr1, expr2),outFile,bindings,offset,name) =
                (codegen(expr1,outFile,bindings,offset,name);
                codegen(expr2,outFile,bindings,offset,name);
                TextIO.output(outFile, "eq\n"))

           | codegen(negate'(expr),outFile,bindings,offset,name) =
                ((*TextIO.output(TextIO.stdOut, "<symbol> - </symbol>\n");
                codegen(expr,outFile,bindings,offset,name)*)
                codegen(expr,outFile,bindings,offset,name);
                TextIO.output(outFile, "neg \n")
                )

           | codegen(not'(expr),outFile,bindings,offset,name) =
                (codegen(expr,outFile,bindings,offset,name);
                  TextIO.output(outFile, "not \n")
                )
(*----------------------------------------------------------------------------------------------*)
          (*need base case on code genlist*)
          and codegenlist(ast::astl,outFile,bindings,offset,name) =
                  (codegen(ast, outFile,bindings,offset,name);
                  (*TextIO.output(TextIO.stdOut, "<symbol> , </symbol>\n");*)
                  codegenlist(astl,outFile,bindings,offset,name))

            | codegenlist([],outFile,bindings,offset,name) =
                  ()

          and  codegenstringlist(str::strl,outFile,bindings,offset,name) =
                  (codegen(id'(str),outFile,bindings,offset,name);
                  codegenstringlist(strl,outFile,bindings,offset,name))

            | codegenstringlist([],outFile,bindings,offset,name) =
                  ()

          (*fun generatevd(types,vd::vdl,offset,segment) =
                ((types,vd,offset,segment)::generatevd(types,vdl,offset+1,segment))
                (*pass back list of bindings*)
                (*symbol table*)
            | generatevd(types,vd::[],offset,segment) =
                ([(types,vd,offset,segment)])

            | generatevd(types,[],offset,segment) =
                ([])*)

          (*fun generatepb(types,p::pl,offset) = ]
              ()*)


             (*)| codegenlist(varnamelist'(),outFile,bindings,offset,name) =
                  ([])

             | codegenlist(varnamelist'(vnl),outFile,bindings,offset,name) =
                   (codegen(vnl, outFile, bindings,offset,name))

             | codegenlist(varnamelist'(vnl::vnll),outFile,bindings,offset,name) =
                   (codegen(vnl, outFile,bindings,offset,name);
                   TextIo.output(TextIO.stdOut, "<symbol> , <symbol>");
                   codegenlist(vnnl,outFile,bindings,offset,name))

             | codegenlist(subroutinedeclist'(),outFile,bindings,offset,name) =
                   ([])

             | codegenlist(subroutinedeclist'(sd),outFile,bindings,offset,name) =
                 (codegen(sd, outFile, bindings,offset,name))

             | codegenlist(subroutinedeclist'(sd::sdl),outFile,bindings,offset,name) =
                 (codegen(sd, outFile,bindings,offset,name);
                 codegenlist(sdl,outFile,bindings,offset,name))

             | codegenlist(parameterlist'(),outFile,bindings,offset,name) =
                   ([])

             | codegenlist(parameterlist'(p),outFile,bindings,offset,name) =
                 (codegen(p, outFile, bindings,offset,name))

             | codegenlist(parameterlist'(p::pl),outFile,bindings,offset,name) =
                 (codegen(p, outFile,bindings,offset,name);
                 TextIo.output(TextIO.stdOut, "<symbol> , <symbol>");
                 codegenlist(pl,outFile,bindings,offset,name))

             | codegenlist(vardeclist'(),outFile,bindings,offset,name) =
                   ([])

             | codegenlist(vardeclist'(vd),outFile,bindings,offset,name) =
                 (codegen(vd, outFile, bindings,offset,name))

             | codegenlist(vardeclist'(vd::vdl),outFile,bindings,offset,name) =
                 (codegen(vd, outFile,bindings,offset,name);
                 codegenlist(vdl,outFile,bindings,offset,name))

             | codegenlist(identifierlist'(),outFile,bindings,offset,name) =
                   ([])

             | codegenlist(identifierlist'(id),outFile,bindings,offset,name) =
                 (codegen(id, outFile, bindings,offset,name))

             | codegenlist(identifierlist'(id::idl),outFile,bindings,offset,name) =
                 (codegen(id, outFile,bindings,offset,name);
                 TextIo.output(TextIO.stdOut, "<symbol> , <symbol>");
                 codegenlist(idl,outFile,bindings,offset,name))

             | codegenlist(statementlist'(),outFile,bindings,offset,name) =
                   ([])

             | codegenlist(statementlist'(st),outFile,bindings,offset,name) =
                 (codegen(st, outFile, bindings,offset,name))

             | codegenlist(statementlist'(st::stl),outFile,bindings,offset,name) =
                 (codegen(st, outFile,bindings,offset,name);
                 codegenlist(stl,outFile,bindings,offset,name))

             | codegenlist(expressionlist'(),outFile,bindings,offset,name) =
                   ([])

             | codegenlist(expressionlist'(expr),outFile,bindings,offset,name) =
                 (codegen(expr, outFile, bindings,offset,name))

             | codegenlist(expressionlist'(expr::exprl),outFile,bindings,offset,name) =
                 (codegen(expr, outFile,bindings,offset,name);
                 TextIo.output(TextIO.stdOut, "<symbol> , <symbol>");
                 codegenlist(exprl,outFile,bindings,offset,name))*)



     (*| codegen(add'(t1,t2),outFile,bindings,offset,depth) =
         let val _ = codegen(t1,outFile,bindings,offset,depth)
             val _ = codegen(t2,outFile,bindings,offset,depth)
             val reg2 = popReg()
             val reg1 = popReg()
         in
           TextIO.output(outFile,reg1 ^ ":="^reg1^"+"^reg2^"\n");
           delReg(reg2);
           pushReg(reg1)
         end

       | codegen(sub'(t1,t2),outFile,bindings,offset,depth) =
         let val _ = codegen(t1,outFile,bindings,offset,depth)
             val _ = codegen(t2,outFile,bindings,offset,depth)
             val reg2 = popReg()
             val reg1 = popReg()
         in
           TextIO.output(outFile,reg1 ^ ":="^reg1^"-"^reg2^"\n");
           delReg(reg2);
           pushReg(reg1)
         end

       | codegen(prod'(t1, t2),outFile,bindings,offset,depth) =
          let val _ = codegen(t1,outFile,bindings,offset,depth)
              val _ = codegen(t2,outFile,bindings,offset,depth)
              val reg2 = popReg()
              val reg1 = popReg()
          in
              TextIO.output(outFile,reg1 ^ ":="^reg1^"*"^reg2^"\n");
              delReg(reg2);
              pushReg(reg1)
          end

       | codegen(div'(t1, t2),outFile,bindings,offset,depth) =
          let val _ = codegen(t1,outFile,bindings,offset,depth)
              val _ = codegen(t2,outFile,bindings,offset,depth)
              val reg2 = popReg()
              val reg1 = popReg()
          in
              TextIO.output(outFile,reg1 ^ ":="^reg1^"/"^reg2^"\n");
              delReg(reg2);
              pushReg(reg1)
          end

       | codegen(store'(t),outFile,bindings,offset,depth) =
          let val _ = codegen(t,outFile,bindings,offset,depth)
              val reg1 = popReg()
          in
              TextIO.output(outFile,"MEM :="^reg1^"\n");
              pushReg(reg1)
          end

       | codegen(recall',outFile,bindings,offset,depth) =
          let val r = getReg()
          in
             TextIO.output(outFile, r ^ ":= MEM \n");
             pushReg(r)
          end

       | codegen(negate'(t),outFile,bindings,offset,depth) =
          let val _ = codegen(t,outFile,bindings,offset,depth)
              val reg1 = popReg()
          in
              TextIO.output(outFile, reg1^":="^reg1^"* negateVal\n");
              pushReg(reg1)
          end

         | codegen(integer'(i),outFile,bindings,offset,depth) =
           let val r = getReg()
           in
             TextIO.output(outFile, r ^ ":=" ^ Int.toString(i) ^ "\n");
             pushReg(r)
          end

        | codegen(valref'(s), outFile, bindings,offset,depth) =
            let val r = getReg()
                val bounded = boundTo(constant'(s),bindings)
                val depthiez = depthOf(constant'(s), bindings)
            in
              TextIO.output(outFile, r ^ ":= SP\n");
              forloop (depth - depthiez, TextIO.output, (outFile, r ^ " := M["^r^"+ 0]\n"));
              TextIO.output(outFile, r ^" := M["^r^"+ "^bounded^"]\n");
              pushReg(r)
            end

        | codegen(letval'(s, e1, e2), outFile, bindings, offset, depth) =
           let val _ = codegen(e1, outFile, bindings, offset, depth)
               val r = popReg()
           in
              TextIO.output(outFile, "M[SP+"^ Int.toString(offset) ^"]" ^ ":=" ^ r ^"\n");
              delReg(r);
              codegen(e2, outFile, (constant'(s), Int.toString(offset), depth)::bindings, offset + 1, depth)
           end

        | codegen(get',outFile,bindings,offset,depth) =
            let val r = getReg()
	          in
	             TextIO.output(outFile,"readInt("^r^")\n");
               pushReg(r)
	          end

        | codegen(ifElse'(expr1, RelOp, expr2, expr3, expr4),outFile, bindings, offset, depth) =
            let val  _ = codegen(expr1, outFile, bindings, offset, depth)
                val r1 = popReg()
                val  _ = codegen(expr2, outFile, bindings, offset, depth)
                val r2 = popReg()
                val label1 = nextLabel()
                val label2 = nextLabel()
            in
                TextIO.output(outFile, "if " ^ r1 ^ opposite(RelOp) ^ r2 ^ " then goto " ^ label1 ^ "\n");
                delReg(r2);
                delReg(r1);
                codegen(expr3, outFile, bindings, offset, depth);
                delReg(popReg());
                TextIO.output(outFile, "goto " ^ label2 ^ "\n");
                TextIO.output(outFile, label1 ^ ": \n");
                codegen(expr4, outFile, bindings, offset, depth);
                TextIO.output(outFile, label2 ^ ": \n")
            end

        | codegen(letfun'(id1, id2, expr1, expr2),outFile, bindings, offset, depth) =
            let
                val label0 = nextLabel()
                val label1 = nextLabel()
                val returnBind = (function'(id1), label1, depth)
                val constBind2 = (constant'(id2), Int.toString(11), depth + 1)
                val bounderz = returnBind::constBind2::bindings

            in
              TextIO.output(outFile, "goto "^label0^"\n");
              TextIO.output(outFile, ""^label1^":\n");
              TextIO.output(outFile, "M[SP+2] := PR0\n");
              TextIO.output(outFile, "M[SP+3] := PR1\n");
              TextIO.output(outFile, "M[SP+4] := PR2\n");
              TextIO.output(outFile, "M[SP+5] := PR3\n");
              TextIO.output(outFile, "M[SP+6] := PR4\n");
              TextIO.output(outFile, "M[SP+7] := PR5\n");
              TextIO.output(outFile, "M[SP+8] := PR6\n");
              TextIO.output(outFile, "M[SP+9] := PR7\n");
              TextIO.output(outFile, "M[SP+10] := PR8\n");
              TextIO.output(outFile, "M[SP+11] := PR9\n");
              codegen(expr1, outFile, bounderz, 12, depth+1);
              let
                val r = popReg();
              in
                TextIO.output(outFile, "PR9 :="^r^"\n");
                TextIO.output(outFile, "PR0 := M[SP+2]\n");
                TextIO.output(outFile, "PR1 := M[SP+3]\n");
                TextIO.output(outFile, "PR2 := M[SP+4]\n");
                TextIO.output(outFile, "PR3 := M[SP+5]\n");
                TextIO.output(outFile, "PR4 := M[SP+6]\n");
                TextIO.output(outFile, "PR5 := M[SP+7]\n");
                TextIO.output(outFile, "PR6 := M[SP+8]\n");
                TextIO.output(outFile, "PR7 := M[SP+9]\n");
                TextIO.output(outFile, "PR8 := M[SP+10]\n");
                TextIO.output(outFile, "SP:= M[SP +1]\n");
                TextIO.output(outFile, "PC := PR8\n");
                TextIO.output(outFile, label0^":\n");
                delReg(r);
                codegen(expr2, outFile, returnBind::bindings, offset, depth)

            end
          end


        | codegen(funref'(id, expr),outFile, bindings, offset, depth) =
            let
                val bounded = boundTo(function'(id), bindings)
                val depthiez = depthOf(function'(id), bindings)
            in

                codegen(expr, outFile, bindings, offset, depth);
                let
                   val r = popReg()
                in
                  TextIO.output(outFile, "PR8:= SP\n");
                  forloop (depth - depthiez, TextIO.output, (outFile, "PR8 := M[PR8 + 0]\n"));
                  TextIO.output(outFile, "M[SP+"^Int.toString(offset) ^"] := PR8\n");
                  TextIO.output(outFile, "M[SP +"^Int.toString(offset + 1)^" ] := SP\n");
                  TextIO.output(outFile, "PR9 :="^r^"\n");
                  TextIO.output(outFile, "PR8 := "^Int.toString(offset)^"\n");
                  TextIO.output(outFile, "SP := SP+PR8\n");
                  TextIO.output(outFile, "PR8 := PC + 1\n");
                  TextIO.output(outFile, "goto "^bounded^"\n");
                  TextIO.output(outFile, r^":= PR9\n");
                  pushReg(r)
              end
            end*)


   (*| codegen(_,outFile,bindings,offset,depth) =
         (TextIO.output(TextIO.stdOut, "Attempt to compile expression not currently supported!\n");
          raise Unimplemented)*)


     fun compile filename  =
         let val (ast, _) = calcparse filename
             val outFile = TextIO.openOut("Main.vm")
         in
          (*) TextIO.output(TextIO.stdOut, show(ast));
           TextIO.output(TextIO.stdOut, "\n");
           TextIO.output(outFile,"SP:=100\n");
           TextIO.output(outFile,"PR0 := 0\n");
           TextIO.output(outFile,"PR1 := 0\n");
           TextIO.output(outFile,"PR2 := 0\n");
           TextIO.output(outFile,"PR3 := 0\n");
           TextIO.output(outFile,"PR4 := 0\n");
           TextIO.output(outFile,"PR5 := 0\n");
           TextIO.output(outFile,"PR6 := 0\n");
           TextIO.output(outFile,"PR7 := 0\n");
           TextIO.output(outFile,"PR8 := 0\n");
           TextIO.output(outFile,"PR9 := 0\n");
           TextIO.output(outFile,"cr := 13\n");
           TextIO.output(outFile,"nl := 10\n");
           TextIO.output(outFile,"nullchar:=0\n");
           TextIO.output(outFile,"negateVal:=-1\n");*)
           let val s = codegen(ast,outFile,[],0,"")
               (*val reg1 = popReg()*)
           in
             (*TextIO.output(outFile,"writeInt("^reg1^")\nhalt\n\n");
             delReg(reg1);
             TextIO.output(outFile,"###### input function ######\n");
             TextIO.output(outFile,"input:  readInt(PR9)\t\t# read an integer into function result register\n");
             TextIO.output(outFile,"SP:=M[SP+1]\t\t# restore the stack pointer\n");
             TextIO.output(outFile,"PC:=PR8\t\t\t# return from whence you came\n");
             TextIO.output(outFile,"###### output function ######\n");
             TextIO.output(outFile,"output:  writeInt(PR9)\t\t# write the integer in function parameter register\n");
             TextIO.output(outFile,"writeStr(cr)\n");
             TextIO.output(outFile,"SP:=M[SP+1]\t\t# restore the stack pointer\n");
             TextIO.output(outFile,"PC:=PR8\t\t\t# return from whence you came\n");
             TextIO.output(outFile,"equ PR0 M[0]\n");
             TextIO.output(outFile,"equ PR1 M[1]\n");
             TextIO.output(outFile,"equ PR2 M[2]\n");
             TextIO.output(outFile,"equ PR3 M[3]\n");
             TextIO.output(outFile,"equ PR4 M[4]\n");
             TextIO.output(outFile,"equ PR5 M[5]\n");
             TextIO.output(outFile,"equ PR6 M[6]\n");
             TextIO.output(outFile,"equ PR7 M[7]\n");
             TextIO.output(outFile,"equ PR8 M[8]\n");
             TextIO.output(outFile,"equ PR9 M[9]\n");
             TextIO.output(outFile,"equ MEM M[12]\n");
             TextIO.output(outFile,"equ SP M[13]\n");
             TextIO.output(outFile,"equ cr M[14]\n");
             TextIO.output(outFile,"equ nl M[15]\n");
             TextIO.output(outFile,"equ nullchar M[16]\n");
             TextIO.output(outFile,"equ negateVal M[17]\n");*)
             printRegs(!regList,outFile);
             TextIO.closeOut(outFile)
           end
         end
         handle _ => (TextIO.output(TextIO.stdOut, "An error occurred while compiling!\n\n"));


     fun run(a,b::c) = (compile b; OS.Process.success)
       | run(a,b) = (TextIO.print("usage: sml @SMLload=calc\n");
                     OS.Process.success)
end
