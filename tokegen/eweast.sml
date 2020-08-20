structure eweAS = 
struct

datatype
    Instruction = store' of MemoryRef * int
                | storeString' of MemoryRef * string
                | storePC' of MemoryRef * int
                | restorePC' of MemoryRef
                | label' of string * Instruction
                | move' of MemoryRef * MemoryRef
		        | add' of MemoryRef * MemoryRef * MemoryRef
		        | sub' of MemoryRef * MemoryRef * MemoryRef
		        | mul' of MemoryRef * MemoryRef * MemoryRef
		        | div' of MemoryRef * MemoryRef * MemoryRef
		        | mod' of MemoryRef * MemoryRef * MemoryRef
		        | indRef' of MemoryRef * MemoryRef * int
		        | indStore' of MemoryRef * int * MemoryRef
		        | readInt' of MemoryRef
		        | writeInt' of MemoryRef
		        | readStr' of MemoryRef * MemoryRef
		        | writeStr' of MemoryRef
		        | goto' of int
		        | gotolab' of string
		        | if' of Cond * MemoryRef * MemoryRef * int
		        | iflab' of Cond * MemoryRef * MemoryRef * string
		        | memdef' of string * int
		        | halt'
              | break'
and
    MemoryRef = memref' of int
              | memrefId' of string
and
    Cond = greaterEq' | greater' | lessEq' | less' | eq' | neq'

and
    ValType = int' of int | str' of string;
    
		  
    

    
end;


