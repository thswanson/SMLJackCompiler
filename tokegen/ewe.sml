structure ewe =
struct
    open eweAS;
	
    structure eweLrVals = 
	eweLrValsFun(structure Token = LrParser.Token);
    structure eweLex = 
	eweLexFun(structure Tokens = eweLrVals.Tokens);
    structure eweParser = 
	Join(structure Lex= eweLex
	     structure LrParser = LrParser
	     structure ParserData = eweLrVals.ParserData);
	          
    exception parseError;

    val input_line = 
     fn f => 
        let val sOption = TextIO.inputLine f
        in
           if isSome(sOption) then
               Option.valOf(sOption)
           else
               ""
        end 

    val eweparse = 
     fn filename =>
	    let val instrm = TextIO.openIn filename
	        val lexer = eweParser.makeLexer(fn i => input_line instrm)
	        val _ = eweLex.UserDeclarations.pos := 1
	        val error = fn (e,i:int,_) => 
                           (TextIO.output(TextIO.stdOut,filename ^ "," ^
						                                " line " ^ (Int.toString i) ^ ", "^ e ^ "\n");
                            raise parseError)
	    in 
            eweParser.parse(0,lexer,error,()) before TextIO.closeIn instrm
	    end;
	    
    exception IllegalMemRef of (Int.int * Int.int) list;
    
    exception IllegalLabelRef;

    exception IllegalInstruction;

    exception IllegalType;

    exception ReadError;
    
    fun enumerate([],n) = []
      | enumerate(h::t,n) = (n,h)::enumerate(t,n+1);

    fun fetch(m,[],mem,pc) = 
	    (TextIO.output(TextIO.stdOut,
		               "Illegal Memory Reference of Location "^Int.toString(m)^
		               " at Instruction "^Int.toString(pc)^"!\n");
	     raise IllegalMemRef(mem))
      | fetch(m,(n,h)::t,mem,pc) = if (m=n) then h else fetch(m,t,mem,pc);
        
    fun intToString(i) = if (i<0) then "-"^Int.toString(~1*i)
			             else Int.toString(i);
		
    fun store([],v,l) = [(l,v)]
      | store((l1:int,x)::t,v,l2) = if (l1=l2) then (l1,v)::t else (l1,x)::store(t,v,l2);

    fun fetchString(m,[],mem,pc) = 
	    (TextIO.output(TextIO.stdOut,
		               "Illegal Memory Reference of Location "^Int.toString(m)^
		               " at Instruction "^Int.toString(pc)^"!\n");
	     raise IllegalMemRef(mem))
      | fetchString(m,(n,h)::t,mem,pc) = 
        if (m=n) then
            if h=0 then "" else String.implode([chr(h)]) ^ fetchString(m+1,mem,mem,pc)
        else
            fetchString(m,t,mem,pc);
        
    fun storeList(m,[],l) = m
      | storeList(m,h::t,l) = storeList(store(m,h,l),t,l+1)

    fun merge(cmp,[],y) = y
      | merge(cmp,x,[]) = x
      | merge(cmp,a::x,b::y) =
        if cmp(a,b) then a::merge(cmp,x,b::y)
        else b::merge(cmp,a::x,y);

    fun split ([]) = ([],[]) 
      | split (a::[]) = ([a],[])
      | split (a::b::y) = 
        let val (l1,l2) = split(y)
        in 
            (a::l1,b::l2)
        end       
       
    fun mergeSort(cmp,[]) = [] 
      | mergeSort(cmp,[a]) = [a]
      | mergeSort(cmp,x) = 
        let val (l1,l2) = split(x)
        in
            merge(cmp,mergeSort(cmp,l1),mergeSort(cmp,l2))
        end

    fun printMem([]) = TextIO.output(TextIO.stdOut,"\n")
      | printMem((l,v)::t) = 
        let val asciival = if v >=32 andalso v <= 126 then String.implode([chr(v)]) else "." 
            val firstPart = intToString(l)^": "^intToString(v)
            val fPLen = String.size(firstPart)
        in
	        TextIO.output(TextIO.stdOut,firstPart^String.substring("               ",0,15-fPLen)^asciival^"\n");
	        printMem(t)
        end

    fun printSortedMem(mem) = 
        let val sortedMem = mergeSort(fn ((a1,v1),(a2,v2)) => a1 < a2, mem)
        in
            printMem(sortedMem)
        end

    fun printProg([]) = TextIO.output(TextIO.stdOut,"\n")

      | printProg((i,store'(memref'(l),v))::t) = 
	    (TextIO.output(TextIO.stdOut,intToString(i)^
		                             ": M["^intToString(l)^"]:="^intToString(v)^"\n");
	     printProg(t))
	    
      | printProg((i,storeString'(memref'(l),v))::t) = 
	    (TextIO.output(TextIO.stdOut,intToString(i)^
		                             ": M["^intToString(l)^"]:="^v^"\n");
	     printProg(t))
	    
      | printProg((i,storePC'(memref'(l),off))::t) = 
	    (TextIO.output(TextIO.stdOut,intToString(i)^
		                             ": M["^intToString(l)^"]:=PC+"^intToString(off)^"\n");
	     printProg(t))
	    
      | printProg((i,restorePC'(memref'(l)))::t) = 
	    (TextIO.output(TextIO.stdOut,intToString(i)^": PC:=M["^intToString(l)^"]\n");
	     printProg(t))
	    
      | printProg((i,move'(memref'(l),memref'(r1)))::t) = 
	    (TextIO.output(TextIO.stdOut,intToString(i)^": M["^intToString(l)^"]:="^
		                             "M["^intToString(r1)^"]\n");
	     printProg(t))
        
      | printProg((i,add'(memref'(l),memref'(r1),memref'(r2)))::t) = 
	    (TextIO.output(TextIO.stdOut,intToString(i)^": M["^intToString(l)^"]:="^
		                             "M["^intToString(r1)^"]+M["^
				                     intToString(r2)^"]\n");
	     printProg(t))
        
      | printProg((i,sub'(memref'(l),memref'(r1),memref'(r2)))::t) = 
	    (TextIO.output(TextIO.stdOut,intToString(i)^": M["^intToString(l)^"]:="^
		                             "M["^intToString(r1)^"]-M["^
				                     intToString(r2)^"]\n");
	     printProg(t))
        
      | printProg((i,mul'(memref'(l),memref'(r1),memref'(r2)))::t) = 
	    (TextIO.output(TextIO.stdOut,intToString(i)^": M["^intToString(l)^"]:="^
		                             "M["^intToString(r1)^"]*M["^
				                     intToString(r2)^"]\n");
	     printProg(t))
        
      | printProg((i,div'(memref'(l),memref'(r1),memref'(r2)))::t) = 
	    (TextIO.output(TextIO.stdOut,intToString(i)^": M["^intToString(l)^"]:="^
		                             "M["^intToString(r1)^"]/M["^
				                     intToString(r2)^"]\n");
	     printProg(t))
        
      | printProg((i,mod'(memref'(l),memref'(r1),memref'(r2)))::t) = 
	    (TextIO.output(TextIO.stdOut,intToString(i)^": M["^intToString(l)^"]:="^
		                             "M["^intToString(r1)^"]%M["^
				                     intToString(r2)^"]\n");
	     printProg(t))
        
      | printProg((i,indRef'(memref'(l),memref'(ir),offset))::t) = 
	    (TextIO.output(TextIO.stdOut,intToString(i)^": M["^intToString(l)^"]:="^
		                             "M[M["^intToString(ir)^"]+"^Int.toString(offset)^"]\n");
	     printProg(t))
	    
      | printProg((i,indStore'(memref'(il),offset,memref'(r)))::t) = 
	    (TextIO.output(TextIO.stdOut,intToString(i)^": M[M["^intToString(il)^"]+"^Int.toString(offset)^"]:="^
		                             "M["^intToString(r)^"]\n");
	     printProg(t))
	    
      | printProg((i,readInt'(memref'(l)))::t) = 
	    (TextIO.output(TextIO.stdOut,intToString(i)^": readInt(M["^intToString(l)^"])\n");
	     printProg(t))
	    
      | printProg((i,writeInt'(memref'(l)))::t) = 
	    (TextIO.output(TextIO.stdOut,intToString(i)^": writeInt(M["^intToString(l)^"])\n");
	     printProg(t))
	    
      | printProg((i,readStr'(memref'(l),memref'(m)))::t) = 
	    (TextIO.output(TextIO.stdOut,intToString(i)^": readStr(M["^intToString(l)^"],M["^intToString(m)^"])\n");
	     printProg(t))

      | printProg((i,writeStr'(memref'(l)))::t) = 
	    (TextIO.output(TextIO.stdOut,intToString(i)^": writeStr(M["^intToString(l)^"])\n");
	     printProg(t))

      | printProg((i,goto'(l))::t) = 
	    (TextIO.output(TextIO.stdOut,intToString(i)^": goto "^intToString(l)^"\n");
	     printProg(t))
	    
      | printProg((i,if'(greaterEq',memref'(ml),memref'(ml2),il))::t) = 
	    (TextIO.output(TextIO.stdOut,intToString(i)^
		                             ": if M["^intToString(ml)^"]>=M["^intToString(ml2)^"] then goto "^
		                             intToString(il)^"\n");
	     printProg(t))
	    
      | printProg((i,if'(greater',memref'(ml),memref'(ml2),il))::t) = 
	    (TextIO.output(TextIO.stdOut,intToString(i)^
		                             ": if M["^intToString(ml)^"]>M["^intToString(ml2)^"] then goto "^
		                             intToString(il)^"\n");
	     printProg(t))
	    
      | printProg((i,if'(lessEq',memref'(ml),memref'(ml2),il))::t) = 
	    (TextIO.output(TextIO.stdOut,intToString(i)^
		                             ": if M["^intToString(ml)^"]<=M["^intToString(ml2)^"] then goto "^
		                             intToString(il)^"\n");
	     printProg(t))
	    
      | printProg((i,if'(less',memref'(ml),memref'(ml2),il))::t) = 
	    (TextIO.output(TextIO.stdOut,intToString(i)^
		                             ": if M["^intToString(ml)^"]<M["^intToString(ml2)^"] then goto "^
		                             intToString(il)^"\n");
	     printProg(t))
	    
      | printProg((i,if'(eq',memref'(ml),memref'(ml2),il))::t) = 
	    (TextIO.output(TextIO.stdOut,intToString(i)^
		                             ": if M["^intToString(ml)^"]=M["^intToString(ml2)^"] then goto "^
		                             intToString(il)^"\n");
	     printProg(t))
	    
      | printProg((i,if'(neq',memref'(ml),memref'(ml2),il))::t) = 
	    (TextIO.output(TextIO.stdOut,intToString(i)^
		                             ": if M["^intToString(ml)^"]<>M["^intToString(ml2)^"] then goto "^
		                             intToString(il)^"\n");
	     printProg(t))
	    
      | printProg((i,halt')::t) = 
	    (TextIO.output(TextIO.stdOut,intToString(i)^": halt\n");
	     printProg(t))
        
      | printProg((i,break')::t) = 
	    (TextIO.output(TextIO.stdOut,intToString(i)^": break\n");
	     printProg(t))
        
      | printProg((i,_)::t) = 
	    (TextIO.output(TextIO.stdOut,"Illegal instruction at line "^Int.toString(i)^"\n");
	     raise IllegalInstruction);
	    
    fun findLabel(m,[]) = 
	    (TextIO.output(TextIO.stdOut,
		               "Undefined Label "^m^" referenced\n");
	     raise IllegalLabelRef)
      | findLabel(m,(n,h)::t) = if (m=n) then h else findLabel(m,t);
        
    fun getLabels([]) = []

      | getLabels((i,label'(label,instruction))::t) = 
	    [(label,i)]@getLabels([(i,instruction)])@getLabels(t)
	    
      | getLabels((i,store'(l,v))::t) = 
	    getLabels(t)
	    
      | getLabels((i,storeString'(l,v))::t) = 
	    getLabels(t)
	    
      | getLabels((i,storePC'(l,off))::t) = 
	    getLabels(t)
	    
      | getLabels((i,restorePC'(l))::t) = 
	    getLabels(t)
	    
      | getLabels((i,move'(l,r1))::t) = 
	    getLabels(t)
        
      | getLabels((i,add'(l,r1,r2))::t) = 
	    getLabels(t)
        
      | getLabels((i,sub'(l,r1,r2))::t) = 
	    getLabels(t)
        
      | getLabels((i,mul'(l,r1,r2))::t) = 
	    getLabels(t)
        
      | getLabels((i,div'(l,r1,r2))::t) = 
	    getLabels(t)
        
      | getLabels((i,mod'(l,r1,r2))::t) = 
	    getLabels(t)
        
      | getLabels((i,indRef'(l,ir,offset))::t) = 
	    getLabels(t)
	    
      | getLabels((i,indStore'(il,offset,r))::t) = 
	    getLabels(t)
	    
      | getLabels((i,readInt'(l))::t) = 
	    getLabels(t)
	    
      | getLabels((i,writeInt'(l))::t) = 
	    getLabels(t)
	    
      | getLabels((i,readStr'(l,m))::t) = 
	    getLabels(t)
	    
      | getLabels((i,writeStr'(l))::t) = 
	    getLabels(t)
	    
      | getLabels((i,goto'(l))::t) = 
	    getLabels(t)
	    
      | getLabels((i,if'(cond,ml,ml2,il))::t) = 
	    getLabels(t)
	    
      | getLabels((i,gotolab'(l))::t) = 
	    getLabels(t)
	    
      | getLabels((i,iflab'(cond,ml,ml2,il))::t) = 
	    getLabels(t)
	    
      | getLabels((i,halt')::t) = 
	    getLabels(t)
        
      | getLabels((i,break')::t) = 
	    getLabels(t)
        
      | getLabels((i,memdef'(label,v))::t) =
	    (label,v)::getLabels(t);
	    
    fun rML(memref'(i),lab) = memref'(i)
      | rML(memrefId'(name),[]) = (TextIO.output(TextIO.stdOut, 
                                                 "Error: Undefined Memory Reference "^name^"!\n");
                                   raise IllegalMemRef([]))
                                         
      | rML(memrefId'(name),(label,i)::t) = 
	    if name=label then memref'(i)
	    else rML(memrefId'(name),t);
	    
    fun removeLabels([],lab) = []
                               
      | removeLabels((i,label'(label,instruction))::t,lab) = 
	    removeLabels([(i,instruction)],lab)@removeLabels(t,lab)
	    
      | removeLabels((i,store'(l,v))::t,lab) = 
	    (i,store'(rML(l,lab),v))::removeLabels(t,lab)
	    
      | removeLabels((i,storeString'(l,v))::t,lab) = 
	    (i,storeString'(rML(l,lab),v))::removeLabels(t,lab)
	    
      | removeLabels((i,storePC'(l,off))::t,lab) = 
	    (i,storePC'(rML(l,lab),off))::removeLabels(t,lab)
	    
      | removeLabels((i,restorePC'(l))::t,lab) = 
	    (i,restorePC'(rML(l,lab)))::removeLabels(t,lab)
	    
      | removeLabels((i,move'(l,r1))::t,lab) = 
	    (i,move'(rML(l,lab),rML(r1,lab)))::removeLabels(t,lab)
        
      | removeLabels((i,add'(l,r1,r2))::t,lab) = 
	    (i,add'(rML(l,lab),rML(r1,lab),rML(r2,lab)))::removeLabels(t,lab)
        
      | removeLabels((i,sub'(l,r1,r2))::t,lab) = 
	    (i,sub'(rML(l,lab),rML(r1,lab),rML(r2,lab)))::removeLabels(t,lab)
        
      | removeLabels((i,mul'(l,r1,r2))::t,lab) = 
	    (i,mul'(rML(l,lab),rML(r1,lab),rML(r2,lab)))::removeLabels(t,lab)
        
      | removeLabels((i,div'(l,r1,r2))::t,lab) = 
	    (i,div'(rML(l,lab),rML(r1,lab),rML(r2,lab)))::removeLabels(t,lab)
        
      | removeLabels((i,mod'(l,r1,r2))::t,lab) = 
	    (i,mod'(rML(l,lab),rML(r1,lab),rML(r2,lab)))::removeLabels(t,lab)
        
      | removeLabels((i,indRef'(l,ir,offset))::t,lab) = 
	    (i,indRef'(rML(l,lab),rML(ir,lab),offset))::removeLabels(t,lab)
	    
      | removeLabels((i,indStore'(il,offset,r))::t,lab) = 
	    (i,indStore'(rML(il,lab),offset,rML(r,lab)))::removeLabels(t,lab)
	    
      | removeLabels((i,readInt'(l))::t,lab) = 
	    (i,readInt'(rML(l,lab)))::removeLabels(t,lab)
	    
      | removeLabels((i,writeInt'(l))::t,lab) = 
	    (i,writeInt'(rML(l,lab)))::removeLabels(t,lab)
	    
      | removeLabels((i,readStr'(l,m))::t,lab) = 
	    (i,readStr'(rML(l,lab),rML(m,lab)))::removeLabels(t,lab)
	    
      | removeLabels((i,writeStr'(l))::t,lab) = 
	    (i,writeStr'(rML(l,lab)))::removeLabels(t,lab)
	    
      | removeLabels((i,goto'(l))::t,lab) = 
	    (i,goto'(l))::removeLabels(t,lab)
	    
      | removeLabels((i,if'(cond,ml,ml2,il))::t,lab) = 
	    (i,if'(cond,rML(ml,lab),rML(ml2,lab),il))::removeLabels(t,lab)
	    
      | removeLabels((i,gotolab'(l))::t,lab) = 
	    (i,goto'(findLabel(l,lab)))::removeLabels(t,lab)
	    
      | removeLabels((i,iflab'(cond,ml,ml2,il))::t,lab) = 
	    (i,if'(cond,rML(ml,lab),rML(ml2,lab),findLabel(il,lab)))::removeLabels(t,lab)
	    
      | removeLabels((i,memdef'(name,v))::t,lab) = 
	    removeLabels(t,lab)
	    
      | removeLabels((i,halt')::t,lab) = 
	    (i,halt')::removeLabels(t,lab)
        
      | removeLabels((i,break')::t,lab) = 
	    (i,break')::removeLabels(t,lab);

    fun execute(store'(memref'(l),v),p,pc,mem) = 
	    execute(fetch(pc+1,p,mem,pc),p,pc+1,store(mem,v,l))
	
      | execute(storeString'(memref'(l),v),p,pc,mem) = 
         let val strLen = String.size(v)-2
             val strimmed = String.substring(v,1,strLen)^String.implode([chr(0)])
             val iLst = List.map ord (String.explode(strimmed))
         in
	         execute(fetch(pc+1,p,mem,pc),p,pc+1,storeList(mem,iLst,l))
         end
	
      | execute(storePC'(memref'(l),off),p,pc,mem) = 
	    execute(fetch(pc+1,p,mem,pc),p,pc+1,store(mem,pc+1+off,l))
	
      | execute(restorePC'(memref'(l)),p,pc,mem) = 
	    let val newPC = fetch(l,mem,mem,pc) 
	    in
	        execute(fetch(newPC,p,mem,pc),p,newPC,mem)
	    end
	
      | execute(move'(memref'(l),memref'(r1)),p,pc,mem) = 
	    execute(fetch(pc+1,p,mem,pc),p,pc+1,store(mem,fetch(r1,mem,mem,pc),l))
	    
      | execute(add'(memref'(l),memref'(r1),memref'(r2)),p,pc,mem) = 
	    execute(fetch(pc+1,p,mem,pc),p,pc+1,store(mem,fetch(r1,mem,mem,pc)+fetch(r2,mem,mem,pc),l))
	  
      | execute(sub'(memref'(l),memref'(r1),memref'(r2)),p,pc,mem) = 
	    execute(fetch(pc+1,p,mem,pc),p,pc+1,store(mem,fetch(r1,mem,mem,pc)-fetch(r2,mem,mem,pc),l))
	    
      | execute(mul'(memref'(l),memref'(r1),memref'(r2)),p,pc,mem) = 
	    execute(fetch(pc+1,p,mem,pc),p,pc+1,store(mem,fetch(r1,mem,mem,pc)*fetch(r2,mem,mem,pc),l))
	    
      | execute(div'(memref'(l),memref'(r1),memref'(r2)),p,pc,mem) = 
	    execute(fetch(pc+1,p,mem,pc),p,pc+1,store(mem,fetch(r1,mem,mem,pc) div fetch(r2,mem,mem,pc),l))
	    
      | execute(mod'(memref'(l),memref'(r1),memref'(r2)),p,pc,mem) = 
	    execute(fetch(pc+1,p,mem,pc),p,pc+1,store(mem,fetch(r1,mem,mem,pc) mod fetch(r2,mem,mem,pc),l))
	    
      | execute(indRef'(memref'(l),memref'(ir),offset),p,pc,mem) = 
	    execute(fetch(pc+1,p,mem,pc),p,pc+1,store(mem,fetch(fetch(ir,mem,mem,pc)+offset,mem,mem,pc),l))
	    
      | execute(indStore'(memref'(il),offset,memref'(r)),p,pc,mem) = 
	    execute(fetch(pc+1,p,mem,pc),p,pc+1,store(mem,fetch(r,mem,mem,pc),fetch(il,mem,mem,pc)+offset))
	    
      | execute(readInt'(memref'(l)),p,pc,mem) = 
	    (TextIO.output(TextIO.stdOut,"? ");
	     TextIO.flushOut(TextIO.stdOut);
         let val sOption = Int.fromString(TextIO.input(TextIO.stdIn))
         in
             if Option.isSome(sOption) then
	             execute(fetch(pc+1,p,mem,pc),p,pc+1,
		                 store(mem,Option.valOf(sOption),l))
             else
                 raise ReadError
         end)
    
		
      | execute(writeInt'(memref'(l)),p,pc,mem) = 
	    (TextIO.output(TextIO.stdOut,intToString(fetch(l,mem,mem,pc)));
	     execute(fetch(pc+1,p,mem,pc),p,pc+1,mem))
	    
      | execute(readStr'(memref'(l),memref'(m)),p,pc,mem) = 
	    (TextIO.output(TextIO.stdOut,"? ");
	     TextIO.flushOut(TextIO.stdOut);
         let val sOption = TextIO.inputLine(TextIO.stdIn)
             val s = Option.valOf(sOption)
             val maxLen = fetch(m,mem,mem,pc)
             val readLen = Int.min(maxLen-1,String.size(s)-1)
             val strimmed = String.substring(s,0,readLen)^String.implode([chr(0)])
             val iLst = List.map ord (String.explode(strimmed))
         in
	         execute(fetch(pc+1,p,mem,pc),p,pc+1,storeList(mem,iLst,l))
         end)

      | execute(writeStr'(memref'(l)),p,pc,mem) = 
        let val s = fetchString(l,mem,mem,pc)
        in
            TextIO.output(TextIO.stdOut,s);
	        execute(fetch(pc+1,p,mem,pc),p,pc+1,mem)
        end
	    
      | execute(goto'(l),p,pc,mem) = 
	    execute(fetch(l,p,mem,pc),p,l,mem)
	    
      | execute(if'(greaterEq',memref'(ml),memref'(ml2),il),p,pc,mem) = 
	    if (fetch(ml,mem,mem,pc) >= fetch(ml2,mem,mem,pc)) then
	        execute(fetch(il,p,mem,pc),p,il,mem)
	    else
	        execute(fetch(pc+1,p,mem,pc),p,pc+1,mem)
            
      | execute(if'(greater',memref'(ml),memref'(ml2),il),p,pc,mem) = 
	    if (fetch(ml,mem,mem,pc) > fetch(ml2,mem,mem,pc)) then
	        execute(fetch(il,p,mem,pc),p,il,mem)
	    else
	        execute(fetch(pc+1,p,mem,pc),p,pc+1,mem)
            
      | execute(if'(lessEq',memref'(ml),memref'(ml2),il),p,pc,mem) = 
	    if (fetch(ml,mem,mem,pc) <= fetch(ml2,mem,mem,pc)) then
	        execute(fetch(il,p,mem,pc),p,il,mem)
	    else
	        execute(fetch(pc+1,p,mem,pc),p,pc+1,mem)
            
      | execute(if'(less',memref'(ml),memref'(ml2),il),p,pc,mem) = 
	    if (fetch(ml,mem,mem,pc) < fetch(ml2,mem,mem,pc)) then
	        execute(fetch(il,p,mem,pc),p,il,mem)
	    else
	        execute(fetch(pc+1,p,mem,pc),p,pc+1,mem)
            
      | execute(if'(eq',memref'(ml),memref'(ml2),il),p,pc,mem) = 
	    if (fetch(ml,mem,mem,pc) = fetch(ml2,mem,mem,pc)) then
	        execute(fetch(il,p,mem,pc),p,il,mem)
	    else
	        execute(fetch(pc+1,p,mem,pc),p,pc+1,mem)
            
      | execute(if'(neq',memref'(ml),memref'(ml2),il),p,pc,mem) = 
	    if (fetch(ml,mem,mem,pc) <> fetch(ml2,mem,mem,pc)) then
	        execute(fetch(il,p,mem,pc),p,il,mem)
	    else
	        execute(fetch(pc+1,p,mem,pc),p,pc+1,mem)
            
      | execute(break',p,pc,mem) = 
	    (TextIO.output(TextIO.stdOut,"Data Memory\n");
	     printSortedMem mem;
	     TextIO.output(TextIO.stdOut,
		               "Break point encountered at program address " ^ Int.toString(pc) ^ 
		               "!\nPress enter to continue.\n");
	     TextIO.input(TextIO.stdIn);
	     execute(fetch(pc+1,p,mem,pc),p,pc+1,mem))
	    

      | execute(halt',p,pc,mem) = mem
                                  
      | execute(_,p,pc,mem) = raise IllegalLabelRef;
	    
    fun interpret filename = 
	    (let val (prog, _) = eweparse filename 
	     in
	         let val executableProg = removeLabels(enumerate(prog,0),getLabels(enumerate(prog,0)))
	         in
                 TextIO.output(TextIO.stdOut,"\nProgram Memory\n");
		         printProg(executableProg);
		         TextIO.output(TextIO.stdOut,"Running Program\n");
		         let val mem = execute(fetch(0,executableProg,[],0),executableProg,0,[])
		                 handle IllegalMemRef(m) => m
		         in 
		             TextIO.output(TextIO.stdOut,"\n\nData Memory\n");
		             printSortedMem(mem)
		         end 
	         end
	     end
         handle IllegalMemRef(a) =>
                TextIO.output(TextIO.stdOut,"Program Terminating!\n")
		 handle IllegalLabelRef => 
		        TextIO.output(TextIO.stdOut,"Error: Illegal Label Reference in Program!\n")
	     handle a => TextIO.output(TextIO.stdOut,"Unknown Error: Program Terminating!\n"))
        handle a => TextIO.output(TextIO.stdOut,"Execution Aborted due to a "^exnName(a)^"!\n")
    
	                
    fun run(a,b::c) = (interpret b;
		               OS.Process.success)
      | run(a,b) = (TextIO.print("usage: sml @SMLload=ewe filename\n");
		            OS.Process.success);
end;


