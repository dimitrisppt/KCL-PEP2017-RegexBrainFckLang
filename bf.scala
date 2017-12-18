// Part 2 about an Interpreter for the Brainf*** language
//========================================================

object CW8b {

type Mem = Map[Int, Int]

// (2a) Complete the functions for safely reading
// and writing brainf*** memory. Safely read should
// Return the value stored in the Map for a given memory
// pointer, if it exists; otherwise it Returns 0. The
// writing function generates a new Map with the
// same data, except at the given memory pointer the
// value v is stored.


def sread(mem: Mem, mp: Int) : Int = {
    val memory_pointer = mem.getOrElse(mp, 0)
    if (memory_pointer == 0) {
        0
    } else {
        memory_pointer
    }
}

def write(mem: Mem, mp: Int, v: Int) : Mem = {
    val map_of_data = mem + (mp -> v)
    map_of_data
}


// (2b) Implement the two jumping instructions in the
// brainf*** language. In jumpRight, given a program and
// a program counter move the counter to the right
// until the command after the *matching* ]-command. Similarly,
// jumpLeft implements the move to the left to just after
// the *matching* [-command. The levels are used to find the
// *matching* bracket.

def jumpRight(prog: String, pc: Int, level: Int) : Int = Try(prog.charAt(pc)).getOrElse(Nil) match {
    case ']' => if (level == 0) pc + 1
                else jumpRight(prog, pc + 1, level - 1)
    case '[' => jumpRight(prog, pc + 1, level + 1)
    case Nil => pc
    case _ => jumpRight(prog, pc + 1, level)
}

def jumpLeft(prog: String, pc: Int, level: Int) : Int = Try(prog.charAt(pc)).getOrElse(Nil) match {
    case '[' => if (level == 0) pc + 1
                else jumpLeft(prog, pc - 1, level - 1)
    case ']' => jumpLeft(prog, pc - 1, level + 1)
    case Nil => pc
    case _ => jumpLeft(prog, pc - 1, level)
}



// (2c) Complete the run function that interprets (runs) a brainf***
// program: the arguments are a program, a program counter,
// a memory counter and a brainf*** memory. It Returns the
// memory at the stage when the execution of the brainf*** program
// finishes. The interpretation finishes once the program counter
// pc is pointing to something outside the program string.
// If the pc points to a character inside the program, the pc,
// memory pointer and memory need to be updated according to
// rules of the brainf*** language. Then, recursively, the run
// function continues with the command at the new program
// counter.
//
// Implement the start function that calls run with the program
// counter and memory counter set to 0.

//def run(prog: String, pc: Int, mp: Int, mem: Mem) : Mem = ...

//def start(prog: String, mem: Mem) = ...






// some sample bf programs collected from the Internet
//==================================================


/*
// first some contrived (small) programs

// clears the 0-cell
start("[-]", Map(0 -> 100))

// copies content of the 0-cell to 1-cell
start("[->+<]", Map(0 -> 10))

// copies content of the 0-cell to 2-cell and 4-cell
start("[>>+>>+<<<<-]", Map(0 -> 42))

start("+++[>+++++<-]", Map(0 -> 10))


// prints out numbers 0 to 9
start("""+++++[->++++++++++<]>--<+++[->>++++++++++<<]>>++<<----------[+>.>.<+<]""", Map())


// some more "useful" programs

// hello world program 1
start("""++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++
       ..+++.>>.<-.<.+++.------.--------.>>+.>++.""", Map())

// hello world program 2
start("""++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>+
      +.<<+++++++++++++++.>.+++.------.--------.>+.>.""", Map())


// draws the Sierpinski triangle
start("""++++++++[>+>++++<<-]>++>>+<[-[>>+<<-]+>>]>+[-<<<[
      ->[+[-]+>++>>>-<<]<[<]>>++++++[<<+++++>>-]+<<++.[-]<<
      ]>.>+[>>]>+]""", Map())

//Fibonacci numbers below 100
start("""+++++++++++
      >+>>>>++++++++++++++++++++++++++++++++++++++++++++
      >++++++++++++++++++++++++++++++++<<<<<<[>[>>>>>>+>
      +<<<<<<<-]>>>>>>>[<<<<<<<+>>>>>>>-]<[>++++++++++[-
      <-[>>+>+<<<-]>>>[<<<+>>>-]+<[>[-]<[-]]>[<<[>>>+<<<
      -]>>[-]]<<]>>>[>>+>+<<<-]>>>[<<<+>>>-]+<[>[-]<[-]]
      >[<<+>>[-]]<<<<<<<]>>>>>[+++++++++++++++++++++++++
      +++++++++++++++++++++++.[-]]++++++++++<[->-<]>++++
      ++++++++++++++++++++++++++++++++++++++++++++.[-]<<
      <<<<<<<<<<[>>>+>+<<<<-]>>>>[<<<<+>>>>-]<-[>>.>.<<<
      [-]]<<[>>+>+<<<-]>>>[<<<+>>>-]<<[<+>-]>[<+>-]<<<-]""", Map())


//outputs the square numbers up to 10000
start("""++++[>+++++<-]>[<+++++>-]+<+[
    >[>+>+<<-]++>>[<<+>>-]>>>[-]++>[-]+
    >>>+[[-]++++++>>>]<<<[[<++++++++<++>>-]+<.<[>----<-]<]
    <<[>>>>>[>>>[-]+++++++++<[>-<-]+++++++++>[-[<->-]+[<<<]]<[>+<-]>]<<-]<<-]""", Map())


//Collatz numbers (need to be typed in)
start(""">,[[----------[
      >>>[>>>>]+[[-]+<[->>>>++>>>>+[>>>>]++[->+<<<<<]]<<<]
      ++++++[>------<-]>--[>>[->>>>]+>+[<<<<]>-],<]>]>>>++>+>>[
      <<[>>>>[-]+++++++++<[>-<-]+++++++++>[-[<->-]+[<<<<]]<[>+<-]>]
      >[>[>>>>]+[[-]<[+[->>>>]>+<]>[<+>[<<<<]]+<<<<]>>>[->>>>]+>+[<<<<]]
      >[[>+>>[<<<<+>>>>-]>]<<<<[-]>[-<<<<]]>>>>>>>
      ]>>+[[-]++++++>>>>]<<<<[[<++++++++>-]<.[-]<[-]<[-]<]<,]""", Map())


// infinite Collatz (never stops)
start(""">>+>+<[[->>[>>]>>>[>>]+[<<]<<<[<<]>[>[>>]>>+>[>>]<+<[<<]<<<[<
      <]>-]>[>>]>>[<<<<[<<]>+>[>>]>>-]<<<<[<<]+>>]<<[+++++[>+++++++
      +<-]>.<++++++[>--------<-]+<<]>>[>>]+[>>>>[<<+>+>-]<-[>+<-]+<
      [<<->>-[<<+>>[-]]]>>>[<<<+<<+>>>>>-]<<<[>>>+<<<-]<<[[-]>+>>->
      [<+<[<<+>>-]<[>+<-]<[>+<-]>>>>-]<[>+<-]+<[->[>>]<<[->[<+++>-[
      <+++>-[<+++>-[<[-]++>>[-]+>+<<-[<+++>-[<+++>-[<[-]+>>>+<<-[<+
      ++>-[<+++>-]]]]]]]]]<[>+<-]+<<]>>>+<[->[<+>-[<+>-[<+>-[<+>-[<
      +>-[<+>-[<+>-[<+>-[<+>-[<[-]>>[-]+>+<<-[<+>-]]]]]]]]]]]<[>+<-
      ]+>>]<<[<<]>]<[->>[->+>]<[-[<+>-[<->>+<-[<+>-[<->>+<-[<+>-[<-
      >>+<-[<+>-[<->>+<-[<+>-[<->>+<-[<+>-[<->>+<-[<+>-[<->>+<-[<+>
      -[<->>+<-[<+>-[<->>+<-[<+>-]]]]]]]]]]]]]]]]]]]>[<+>-]<+<[<+++
      +++++++>-]<]>>[<+>->>]<<[>+>+<<-]>[<+>-]+>[<->[-]]<[-<<-]<<[<
      <]]++++++[>+++++++<-]>++.------------.[-]>[>>]<<[+++++[>+++++
      +++<-]>.<++++++[>--------<-]+<<]+<]>[<+>-]<]>>>[>>]<<[>[-]<-<
      <]++++++++++.[-]<<<[<<]>>>+<[->[<+>-[<+>-[<+>-[<+>-[<+>-[<+>-
      [<+>-[<+>-[<+>-[<[-]>>[-]+>+<<-]]]]]]]]]]<[>+<-]+>>]<<[<<]>>]""", Map())


*/

}
