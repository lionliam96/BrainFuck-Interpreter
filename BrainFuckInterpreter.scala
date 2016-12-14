

object BrainFuckInterpreter {

	val MemorySize = 30000
	
	// brainfuck class represents brainfuck program:
	class BrainFuck (code: String) {
		var memory: Array[Int] = new Array[Int](MemorySize)
		val bfCode: String = code
		var codeLocation: Int = 0
		var pointer: Int = 0			
	}

	// keep pointer within location constraints:
	def wrapPointer (location: Int) = {
		if (location < 0) (MemorySize-1)
		else if (location.equals(MemorySize)) (0)
		else (location)
	}

	// get next instruction relative to current location:
	def getInstruction (bf: BrainFuck, n: Int) = {
		bf.codeLocation = bf.codeLocation + n
		(bf)
	}
	def nextInstruction (bf: BrainFuck) = getInstruction(bf, +1)
	def lastInstruction (bf: BrainFuck) = getInstruction(bf, -1)

	// modify memory at current pointer and goto next instruction:
	def modifyMemoryAtPointer (bf: BrainFuck, op: Int) = {
		var mem = bf.memory
		mem(bf.pointer) = (mem(bf.pointer)+op)
		bf.memory = mem
		nextInstruction(bf)
	}

	// modify pointer and goto next instruction:
	def modifyMemoryPointer (bf: BrainFuck, op: Int) = {
		bf.pointer = wrapPointer(bf.pointer+op)
		nextInstruction(bf)
	}

	def findLoopMatch (bf: BrainFuck, ch1: Char, ch2: Char, jump: (BrainFuck => BrainFuck)) = {
	  def recFindLoopMatch (bf: BrainFuck, nests: Int) : BrainFuck = {
		  val curInst = bf.bfCode.charAt(bf.codeLocation)
		  if (curInst.equals(ch1)&&nests.equals(1)) (bf)
		  else {
	      curInst match {
				  case `ch1` => recFindLoopMatch(jump(bf),(nests-1))
				  case `ch2` => recFindLoopMatch(jump(bf),(nests+1))
				  case _ => recFindLoopMatch(jump(bf),(nests)) 
			  }
		  }
	  }
		recFindLoopMatch(bf, 0)
	}

	// jump past current loop:
	def endLoop (bf: BrainFuck) = findLoopMatch(bf, ']', '[', nextInstruction)
	def startLoop (bf: BrainFuck) = findLoopMatch(bf, '[', ']', lastInstruction)

	// build map of brainfuck instructions:
	var brainFuckInstructions = {
		 Map(
		   '>' -> ((bf: BrainFuck) => modifyMemoryPointer(bf, (+1))),
		   '<' -> ((bf: BrainFuck) => modifyMemoryPointer(bf, (-1))),
			 '+' -> ((bf: BrainFuck) => modifyMemoryAtPointer(bf, (+1))),
			 '-' -> ((bf: BrainFuck) => modifyMemoryAtPointer(bf, (-1))),
			 '[' -> ((bf: BrainFuck) => if (bf.memory(bf.pointer).equals(0)) (endLoop(bf)) else (nextInstruction(bf))),
			 ']' -> ((bf: BrainFuck) => if (bf.memory(bf.pointer) != 0) (startLoop(bf)) else (nextInstruction(bf))),
			 '.' -> ((bf: BrainFuck) => { 
			   println(bf.memory(bf.pointer).toString())
			   (nextInstruction(bf))}),
			 ',' -> ((bf: BrainFuck) => { 
			   bf.memory(bf.pointer) = Console.readInt()
			   (nextInstruction(bf))}))
	} 

	// execute brainfuck program:
	def processInstructions (bf: BrainFuck) : Unit = {
		// check that there is remaining code:
		if (bf.codeLocation < bf.bfCode.length) {
			// look up instruction in table:
			if (brainFuckInstructions.contains(bf.bfCode.charAt(bf.codeLocation))) {
				// if instruction is valid, execute instruction:
				val inst = brainFuckInstructions(bf.bfCode.charAt(bf.codeLocation))
				processInstructions(inst(bf))
			}
			else {
				processInstructions(nextInstruction(bf))
			}
		}
	}

	def executeBrainFuckProgram (code: String) = {
		val bf = new BrainFuck(code)
		processInstructions(bf)
	}

	def main(args:Array[String]) : Unit = {
		val bfCode = "+++++++++++>+>>>>++++++++++++++++++++++++++++++++++++++++++++>++++++++++++++++++++++++++++++++<<<<<<[>[>>>>>>+>+<<<<<<<-]>>>>>>>[<<<<<<<+>>>>>>>-]<[>++++++++++[-<-[>>+>+<<<-]>>>[<<<+>>>-]+<[>[-]<[-]]>[<<[>>>+<<<-]>>[-]]<<]>>>[>>+>+<<<-]>>>[<<<+>>>-]+<[>[-]<[-]]>[<<+>>[-]]<<<<<<<]>>>>>[++++++++++++++++++++++++++++++++++++++++++++++++.[-]]++++++++++<[->-<]>++++++++++++++++++++++++++++++++++++++++++++++++.[-]<<<<<<<<<<<<[>>>+>+<<<<-]>>>>[<<<<+>>>>-]<-[>>.>.<<<[-]]<<[>>+>+<<<-]>>>[<<<+>>>-]<<[<+>-]>[<+>-]<<<-]"
		executeBrainFuckProgram(bfCode)
	}
}