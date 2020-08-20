structure RegisterAllocation =
    struct

    val regNum = ref 0;

    val regList = ref ([]:(int*int) list);

    val regStack = ref ([]:string list);

    exception emptyRegStack;

    fun pushReg r = 
        (regStack := (r::(!regStack)))

    fun popReg() =
        (if (!regStack = []) then 
          (TextIO.output(TextIO.stdErr,"Error:Popping an empty register stack!\n");
           raise emptyRegStack)
         else ();
         let val x as (r::rl) = !regStack
         in
             regStack:=rl;
             r
         end)

    fun createReg(l) =
        let val reg = !regNum
        in
            regNum := !regNum+1;
            ("R"^Int.toString(reg),(reg,~1)::l)
        end

    exception unknownReg;

    exception unFreedReg;

        fun unFreedCount([])=0
      | unFreedCount((x,y)::t) = if y = ~1 then 1+unFreedCount(t) else unFreedCount(t)

    fun freeReg(r, []) =  (TextIO.output(TextIO.stdErr, "Freeing non-existant register "^r^"!\n");
                    raise unknownReg)
      | freeReg(r, (regNum,num)::t) =
                if r = "R"^Int.toString(regNum) then (regNum,unFreedCount(t))::t
                else if num = ~1 then
                        (TextIO.output(TextIO.stdErr, "Illegally freeing register "^r^" before R"^Int.toString(regNum)^"\n");
                         raise unFreedReg)
                    else (regNum,num)::freeReg(r,t)

    fun getReg() = 
        let val result as (r,rl) = createReg(!regList)
        in
            regList:=rl;
            r
        end

    fun delReg(r) =
        let val rl = freeReg(r,!regList)
        in
            regList:=rl;
            ()
        end

    fun offsetRegs([],offset) = []
      | offsetRegs((regNum,color)::t,offset) =
            if color <> ~1
            then (regNum,color+offset)::(offsetRegs(t,offset))
            else (regNum,~1)::(offsetRegs(t,offset))

    fun concatRegs(g1, g2) = offsetRegs(g1,unFreedCount g2)@g2

    exception tooComplex

    fun pprintRegs([]) = TextIO.output(TextIO.stdOut,"\n")
      | pprintRegs((x,y)::t) = (TextIO.output(TextIO.stdOut,"("^Int.toString(x)^","^Int.toString(y)^")  ");
                                pprintRegs(t))

    fun printRegs([],fileStream) = ()
      | printRegs((sr,pr)::t,fileStream) = 
        (printRegs(t,fileStream); 
             if (pr=10) then
             (TextIO.output(TextIO.stdErr, "Expression too complex! Max number of physical registers has been exceeded!\n");
              raise tooComplex) 
         else if (pr = ~1) then
             (TextIO.output(TextIO.stdErr, "unFreed Register R"^Int.toString(sr)^" in target program!\n");
              raise unFreedReg)
              else
                  TextIO.output(fileStream, "equ R"^Int.toString(sr)^" M["^Int.toString(pr)^"]\n"));

end
