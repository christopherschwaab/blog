I wrote this many years ago but for whatever reason I didn't publish it. My old
C++ article on generating switch statements still seems reasonably popular so
perhaps someone will find use in this (for reference when I wrote this I was
working off the ARMv5 manual from arm
http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0100i/index.html
and the 946E-S info from
http://infocenter.arm.com/help/topic/com.arm.doc.ddi0201d/DDI0201D_arm946es_r1p1_trm.pdf).
I also realized wordpress mangled this post so I've also brushed it up in
markdown and converted the post to literate C++ ([grab the code
here](https://github.com/christopherschwaab/blog/tree/master/generating-a-threaded-arm-interpreter-with-templates-in-c++/))
extractable with sed via.

    $ sed -n '/~~~~~{.cpp/,/~~~~~/p' post.md |grep -v '~~~~~' > post.cpp

Recently I started getting into efficient interpreters and binary
translation/JIT compilation. Although I've long been interested in the more
abstract corners of PL and have written a few native compilers I've never
bothered to learn about current day optimization. Given that I always learn more
when I have something to hack on I decided to implement an ARM interpreter, but
first...

I figure a good starting point is compilation of papers so I'll share a few I
liked. Again, I'm not in a position to make suggestions, but I thoroughly
enjoyed "An Architectural Trail to Threaded-Code Systems"
@an-architectural-trail-to-threaded-code-systems by Kogge. After that take a
look at Zaleski's work on context threading and mixed mode execution
@context-threading-a-flexible-and-efficient-dispatch-technique-for-virtual-machine-interpreters
@mixed-mode-execution-with-context-threading . For low level architectural
areas "QEMU, a Fast and Portable Dynamic Translator"
@qemu-a-fast-and-portable-dynamic-translator and "Virtualization Without Direct
Execution or Jitting: Designing a Portable Virtual Machine Infrastructure"
@virtualization-without-direct-execution-or-jitting-designing-a-portable-virtual-machine-infrastructure
offer great ideas.

Direct threading is a simple technique whereby instructions are directly
"threaded" together using indirect jumps rather than say, a loop with a switch
at the top. I find it easier to understand with some example code so I've
implemented a simple stack machine—winning no awards for cleanliness here

~~~~{.cpp}
#include <stdlib.h>
#include <stdio.h>

int main() {
        // We're using the GNU C, labels-as-values extension here.
        void *prog[] = {&&PUSH,(void*)6,
                        &&PUSH,(void*)7,
                        &&MUL,&&PRINT,&&HALT};
        void **vPC = prog;
        void *stack[4], **sp = stack;

        goto **vPC++;
        PUSH:  *sp++ = *vPC++; goto **vPC++;
        MUL:   *(sp-1) = (void*)(*(long*)(sp-1) * *(long*)(sp-2));
               --sp;
               goto **vPC++;;
        PRINT: printf("%li\n", *(long*)sp--); goto **vPC++;;
        HALT:  exit(0);

        return 0;
}
~~~~
We have a virtual program counter called `vPC` that points to the next
instruction to interpret within the array `prog`. In turn, `prog` stores the
sequence of instructions and their operands. This is a stack machine so we have
a stack, `stack` and a stack pointer `sp`. The big thing to notice here is that
we have no loop fetching, decoding, and dispatching instructions. Instead, each
instruction body finishes execution by jumping to the address of the next
instruction tracked by `vPC`.

How does this translate to ARM instructions? Clearly because they won't match
up with label addresses like our tiny stack machine above, decode must involve
some kind of lookup, preferably into a jump table.

For those unfamiliar every[^1] ARM instruction is 32 bits wide: how do they
map? Every instruction can be conditionally executed (a la setcc) and this
condition code is stored in the high nibble, the next nibble serves primarily
to identify instructions, we then tend to find opcode selection, and the low
20 bits are mostly reserved for operands.  This is of course a rough
explanation so I've diagrammed the multiply and data processing instruction
families
<table style="border-style:none;padding:0;">
<tbody>
<tr style="background-color:#ffffff;border-style:none;font-size:8px;text-align:right;margin:0;padding:0;">
<td style="border-style:none;" colspan="2">28</td>
<td style="border-style:none;" colspan="4">24</td>
<td style="border-style:none;" colspan="4">20</td>
<td style="border-style:none;">16</td>
<td style="border-style:none;">12</td>
<td style="border-style:none;">8</td>
<td style="border-style:none;">7</td>
<td style="border-style:none;" colspan="3">4</td>
<td style="border-style:none;">0</td>
</tr>
<tr style="border-style:none;background-color:#fcfcfc;text-align:center;">
<td style="background-color:#ffffff;border-style:none;font-size:10px;">multiply</td>
<td style="border:1px solid #ccc;">Cond</td>
<td style="border:1px solid #ccc;">0</td>
<td style="border:1px solid #ccc;">0</td>
<td style="border:1px solid #ccc;">0</td>
<td style="border:1px solid #ccc;">0</td>
<td style="border:1px solid #ccc;">0</td>
<td style="border:1px solid #ccc;">0</td>
<td style="border:1px solid #ccc;">A</td>
<td style="border:1px solid #ccc;">S</td>
<td style="border:1px solid #ccc;">Rd</td>
<td style="border:1px solid #ccc;">Rn</td>
<td style="border:1px solid #ccc;">Rs</td>
<td style="border:1px solid #ccc;">1</td>
<td style="border:1px solid #ccc;">0</td>
<td style="border:1px solid #ccc;">0</td>
<td style="border:1px solid #ccc;">1</td>
<td style="border:1px solid #ccc;">Rm</td>
</tr>
<tr style="border-style:none;background-color:#fcfcfc;text-align:center;">
<td style="background-color:#ffffff;border-style:none;font-size:10px;">dataproc</td>
<td style="border:1px solid #ccc;">Cond</td>
<td style="border:1px solid #ccc;">0</td>
<td style="border:1px solid #ccc;">0</td>
<td style="border:1px solid #ccc;">I</td>
<td style="border:1px solid #ccc;" colspan="4">Opcode</td>
<td style="border:1px solid #ccc;">S</td>
<td style="border:1px solid #ccc;">Rn</td>
<td style="border:1px solid #ccc;">Rd</td>
<td style="border:1px solid #ccc;" colspan="6">Operand2</td>
</tr>
</tbody>
</table>
How can we make this into a jump table? While it's not hard to spot ambiguity
in bits 27-20 (consider all zeros in both instructions), the 12 bits 27-20
and 7-4 turn out to be enough to determine every operation. What then, will a
sketched implementation of an ARM interpreter look like?

~~~~~{.cpp}
static int decode(int i)
{ return i >> 16 & 0xff0 | i >> 4 & 0xf; }
~~~~~
~~~~{.cpp}
int main() {
    int32_t prog[] = { i1, i2, ..., iHalt };
    int32_t *vPC = prog;
    void *jmptbl[4096] = { /* instruction addresses */ };

    goto *jmptbl[decode(*vPC++)];
    /* instruction bodies */
}
~~~~
Okay, but 4,000 entries? How can we exploit the clustering inherent in
instruction families while retaining the possibility of static
specialization?

Realistically we only have one choice: templates—we'll classify instructions
using the same bit patterns we would at runtime and then use the preprocessor
to enumerate all 4,096 possibilities. Before proceeding notice that actually
bits 7-4 of Operand2 and "1001" appear to collide in the above diagrams. If
you take a look at the ARM documentation you'll find that when bit "I" is 0,
bit 7 must be 0 too (see A5.1.1 in DDI 0100I), providing resolution; however
this must be made explicit to avoid template ambiguity. Luckily we can
leverage SFINAE to express this fact, yielding the following template
signature

~~~~~{.cpp}
#include <stdlib.h>
#include <iostream>
#include <boost/preprocessor/repeat.hpp>
#include <boost/utility/enable_if.hpp>

struct undefined_instruction {
        static void exec() {
                std::cout << "undefined instruction" << std::endl;
                // raise processor exception
        }
};

template <int B27,int B26,int B25,int B24,int B23,int B22,int B21,int B20,
          int B7,int B6,int B5,int B4, typename Enable = void>
// Anything not explicitly implemented is an undefined instruction
struct instruction_pattern : undefined_instruction {};
~~~~~
Now we're ready for a few example instructions (in DDI 0100I see A3.16.2 for
multiply, and A3.4.1 for data processing instruction families)

~~~~~{.cpp}
// Multiply
template <int B23,int B22,int B21,int B20>
struct instruction_pattern<0,0,0,0,B23,B22,B21,B20,1,0,0,1> {
        static void exec() {
                std::cout << "mul" << std::endl;
        }
};

// Data Processing
template <int B24,int B23,int B22,int B21>
struct dataproc : undefined_instruction {};

template <>
struct dataproc<0,0,0,0> {
      static void exec() {
              std::cout << "and" << std::endl;
      }
};

template <int I,int B24,int B23,int B22,int B21,int S,int B7,int B6,int B5,int B4>
struct instruction_pattern<0,0,I,B24,B23,B22,B21,S,B7,B6,B5,B4,
                           typename boost::disable_if_c<I == 0 && B7 != 0>::type>
  : dataproc<B24,B23,B22,B21> {};
/* more definitions... */
~~~~~

While closer to a solution we still have the problem that a straight line enumeration of integers 1,2,3,... must be passed into our `instruction_pattern`s as sequences of 12 bits. No tricks involved here, we first use a template, `to_bits`, to convert a value to a sequence of `n` bits and then we use a second template, `with_bin`, to apply a third template, `f`, to this bit sequence (we avoid variadic templates at the minor cost of generality here)

~~~~~{.cpp}
template <int NumberOfBitsInOutputStream, int N>
struct to_bits {
    static const int value = N % 2;
    typedef to_bits<NumberOfBitsInOutputStream - 1, N/2> s;
};
template <int N> struct to_bits<0,N> { static const int value = N % 2; };

template <int m,
          template <int,int,int,int,int,int,int,int,int,int,int,int,class> class f>
struct with_bin {
    typedef to_bits<12,m> bits;
    typedef f<bits::s::s::s::s::s::s::s::s::s::s::s::value,
              bits::s::s::s::s::s::s::s::s::s::s::value,
              bits::s::s::s::s::s::s::s::s::s::value,
              bits::s::s::s::s::s::s::s::s::value,
              bits::s::s::s::s::s::s::s::value,
              bits::s::s::s::s::s::s::value,
              bits::s::s::s::s::s::value,
              bits::s::s::s::s::value,
              bits::s::s::s::value,
              bits::s::s::value,
              bits::s::value,
              bits::value,
              void
             > type;
}; 
~~~~~
This implementation, similar to the threaded interpreter we started with will
use label addresses as entries in the jump table so we have to generate (a)
an address for each call to exec() with an associated indirect jump to the
next instruction, and (b) the actual jump table. The boost preprocessor
library is used for this step and because loops are limited to 255 iterations
we have to make a recursive call rather than the preferable `BOOST_PP_REPEAT(4096, OP_ENTRY, 0)`

~~~~~{.cpp}
#define STEP() goto *jmptbl[decode(*vPC++)]

// This is part for (a)
#define OP_ENTRY(z, m, n) \
  op_##n##_##m: with_bin<m+n*32, instruction_pattern>::type::exec();\
  STEP();
#define OP_ENTRY_R(z, n, __) BOOST_PP_REPEAT(32,  OP_ENTRY,   n)
#define EMIT_OPS()           BOOST_PP_REPEAT(128, OP_ENTRY_R, 0)

// This is part for (b)
#define JMP_ENTRY(z, m, n)    &&op_##n##_##m,
#define JMP_ENTRY_R(z, n, __) BOOST_PP_REPEAT(32,  JMP_ENTRY,   n)
#define EMIT_JMPTBL()         BOOST_PP_REPEAT(128, JMP_ENTRY_R, 0)
~~~~~
Cool, our implementation is in a testable state, what does main look like?

~~~~~{.cpp}
// We're implementing halt here as supervisor-call-0
template <int B23,int B22,int B21,int B20, int B7,int B6,int B5,int B4>
struct instruction_pattern<1,1,1,1,B23,B22,B21,B20,B7,B6,B5,B4> {
        static void exec() {
                std::cout << "halting";
                exit(EXIT_SUCCESS);
        }
};

int main() {
    //                 mul r0,r1,r2, and r0,r1,r0, svc 0
    uint32_t prog[] = { 0xe0000291,   0xe0010000,   0xef000000 };
    uint32_t *vPC = prog;
    void *jmptbl[4096] = { EMIT_JMPTBL() };

    STEP();
    EMIT_OPS();
}
~~~~~
You might want to grab some coffee while this is compiling...

    $ g++ -O2 -Wa,-ahl=main.s -o main main.cpp
    
    $ ./main
    mul
    and
    halting

While this technique works and lends itself well to further optimization via
context threading (also with gcc forced inline/noinline is easy to control)
I'm curious to see how it actually performs on real, multi-megabyte binaries.
We should also question whether things have actually been simplified, I
personally expect to have a stronger position on this once I make it more of
a simulator and less a (bad) disassembler. Furthermore not only is the
compile time bad—gcc hits 40s (actually if you compile the code posted
without implementing any other opcodes it takes 3s) on my i5-450m and clang++
3.0 lost my interest 20 minutes in—but the code produced is gigantic. A
simpler mechanism relying more on runtime disambiguation will presumably have
a smaller footprint—although I could be wrong here too—and I'm curious how
this will interact with cache. While I maintain skepticism I plan on using
vtune to try out dispatch techniques and continue to play around with
compile-time/runtime decode so hopefully I'll have interesting results for
discussion soon.

[^1]: Yeah, thumb.
