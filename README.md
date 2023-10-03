# Haskell-RM

By MMZK1526

A CLI that evaluates [**Register Machines**](#Register-Machine) efficiently in Haskell. It also provides an library defining and simulating Register Machines that can be embedded in Haskell code.

Web APP:
1. [Entrypoint](https://mmzk1526.github.io/rm_front_end/)
2. [Front-end Repo](https://github.com/MMZK1526/rm_front_end)
3. [Back-end Repo](https://github.com/MMZK1526/ktor-rm)

If you haven't heard of Register Machines, see [Introduction](#Introduction) for a brief summary.  

For an example of using the CLI to read RM code from a file and evaluate it with given arguments, go to [Example](#Example).

For the full CLI documentation, go to [CLI](#CLI).

## Register Machine

### Introduction

A Register Machine is a simple system involving a finite number of registers (each can hold a natural number), a finite number of lines, and only three instructions (increment, decrement and halt).  

An increment instruction takes a register and a line. It increments a the register and jumps to the given line.  

A decrement instruction takes a register and two lines (say `m` and `n`). If the register is positive, it decrements the value and jumps to line `m`. Otherwise it jump to line `n` (without changing the register, which is still 0).  

A halt instruction terminates the machine. If we jump to a line that does not exist, it is treated as a halt instruction as well.

Consider the following example:

```RM
L0: R1- 1 2
L1: R0+ 0
L2: R2- 3 4
L3: R0+ 2
L4: HALT
```

Assume `R0 = 0`, `R1 = 1` and `R2 = 2`. We start from line 0; which decrements R1 and goes to line 1; which increments R0 and goes back to line 0; which goes to line 2 since `R1 = 0`; which decrements R2 and goes to line 3; which increments R0 and goes to line 2; which decrements R2 and goes to line 3; which increments R0 and goes to line 3; which goes to line 4 since `R2 = 0`; which halts with `R0 = 3`, `R1 = 0` and `R2 = 0`.

If we treat R0 as the result and the other registers as the input, then a Register Machine that has registers from R0 to R{n} is a partial function from N^n to N (it is partial because the machine may not terminate, thus not providing any result). In our previous example, the function is `f(R1, R2) = R1 + R2`.  

Despite its first appearance, Register Machines are actually very powerful: the system is Turing-complete. This means they are capable of basically whatever modern computers can do.

### Performance

As one may imagine, Register Machines are in general very inefficient since it can increment or decrement at most one register at a time. For example, the [adder machine](Examples/add.rm) which computes `f(x, y) = x + y` takes `2(x + y) + 3` steps, and the [multiplier machine](Examples/mult.rm) which computes `f(x, y) = xy` takes `5xy + 3x + 2` steps. If we take the input size as the number of digits the inputs have, then these two "trivial" functions both have exponential time complexity.

As a result, a naïve RM simulation is pretty useless except for extremely small inputs. In my implementation, the simulator analyses the control flow of the machine, detecting execution cycles, and execute the iterations in one go. For example, the adder machine consists of a R0-R1 cycle and a R0-R2 cycle where the contents of both inputs "flow" into R0. With my optimisation, each cycle only consists of one step during the simulation so that the execution has *de juro* constant-time.

This optimisation also makes simulating the [Universal Register Machine](#Universal-Register-Machine) possible.

If an infinite loop is detected, the simulator would end immediately and report the machine is never going to terminate. The examples [loop](Examples/loop.rm) and [cycle](Examples/cycle.rm) demonstrate this behaviour. Of course, it is not able to detect all infinite loops since the Halting Problem is undecidable (see [Computability](#Computability)).

### Gödelisation

Intriguingly, there is a ONE TO ONE correspondence between natural number and Register Machines (**Gödelisation**). In other words, any natural number uniquely represents a Register Machine and *vice versa*.  

The foundation of Gödelisation lies in the following functions: let `p(x, y) = 2^x + (2y + 1)` and `p'(x, y) = p(x, y) - 1`, it can be proven that the former is a bijection between pairs of natural number to positive number and the latter a bijection to natural number:

p'|p|(x, y)
----|---|----
0|1|(0, 0)
1|2|(1, 0)
2|3|(0, 1)
3|4|(2, 0)
4|5|(0, 2)
5|6|(1, 1)
6|7|(0, 3)
7|8|(3, 0)
8|9|(0, 4)

With `p`, we can recursively define a function, `s`, that is a bijection between finite lists of natural number and singular natural number: `s([]) = 0; s(x : xs) = p(x, s(xs))`:

s|xs|s|xs
----|---|----|---
0|[]|10|[1, 1]
1|[0]|11|[0, 0, 1]
2|[1]|12|[2, 0]
3|[0, 0]|13|[0, 1, 0]
4|[2]|14|[1, 0, 0]
5|[0, 1]|15|[0, 0, 0, 0]
6|[1, 0]|16|[4]
7|[0, 0, 0]|17|[0, 3]
8|[3]|18|[1, 2]
9|[0, 2]|19|[0, 0, 2]

There is a trick to "decode" a number to a list of numbers, namely expressing the number in binary form and count number of zeros between ones from right to left. For example, 998 in binary is 1111100110, and if we count from the rightmost digit, it starts with 1 zero before reaching a one, then 0 zeros due to the consecutive ones, then 2 zeros, and so on. The result is then [1, 0, 2, 0, 0, 0, 0].

With the functions `p` and `p'`, we can then encode each line of a Register Machine. If the instruction is `HALT`, encode it with 0; if it is an increment, then it has a register number `r` with a line number `l`, and we encode it with `p(2r, l)`; if it is a decrement, then it has a register number `r` with two line numbers `l1` and `l2`, and we encode it with `p(2r + 1, p'(l1, l2))`.

Finally, once we encode each line of a Register Machine into a number, we can then encode the list of number into a single number by `s`.

One can verify that the "adder" machine in [Introduction](#Introduction) has a Gödel number of `s([152, 1, 4576, 5, 0])`, a number larger than 10^1426.

If we convert a natual number to a Register Machine, then most likely it will contain instruction that makes no sense, for example jumping to a non-existing line. This does not cause any problem, however, since we treat bad line number as Halt instructions.

In [Convert.hs](Convert.hs), there are several utility functions that can convert between `Line`s, `RMCode`s, lists, pairs, and natural numbers. Moreover, the CLI also provides encoding/decoding intructions, see [Usage](#Usage).

### Universal Register Machine

One of the consequences of being a Turing-complete model is that we can use a certain Register Machine to simulate all other Register Machines. In other words, we can build a single **Universal Register Machine (URM)** that can do the job of all possible register machines (which is why it is braned "Universal"). To use it, we provide a Register Machine as the first input and a list of numbers the second, then the URM can simulate running the input machine with the input list of arguments.

With [Gödelisation](#gödelisation), we can encode each Register Machine into a number. Moreover, we can also encode a list of arguments into a single number as well. Therefore, we can formalise the Universal Register Machine as following:

The Universal Register Machine , `U`, takes two inputs in R1 and R2. It decodes R1 into the corresponding Register Machine `M` and R2 into a list of arguments for that machine. It then simulate running `M` with the list of arguments, and put the result in `R0`.

Let us take the following machine `M` as an example:

```RM
R1- 1 2
R0+ 0
```

It represents the function `f(x) = x`, thus if we run it with a single argument 10, then the result should also be 10. The Gödel number of this machine is 28544953854119197621165719388989902727654932480, and the encoding of the singleton list is `s([10]) = 1024`. Therefore, if we run the URM with R1 = 28544953854119197621165719388989902727654932480 and R2 = 1024, it should simulate running `M` on the single input 10, which results in 10.

There are many ways to implement a URM, and our approach will be based on three "components", or **Gadgets**.

Firstly, we need the copy machine `C`, which copies R1 into R0 without changing R1 itself and leaves all other registers intact. This is implemented as follow:

```RM
L0: R1- 1 3
L1: R0+ 2
L2: R2+ 0
L3: R2- L4 H
L4: R1+ L3
```

Basically, whenever the machine decrements R1, it increments both R0 and R2. When R1 is emptied, it then "pour" R2 back to R1, effectively copying R1 to R0.

There are two subtleties. Firstly, we can easily change R1 and R0 to any two registers and conduct the same process of copying; if R2 is involved, we just need to change the "scratch register" to something else. Thus, we write `C(m, n)` as the machine that copies R{m} to R{n}. In particular, our original `C` is `C(1, 0)`.

Secondly, the `H` label represents a line number that does not exist, thus effectively halts the machine, but we can change it to another valid line number `l`, then the effect is "copy R1 to R0 and go to line `l`". In this way, we can treat this copy machine as a sub-routine that can be wired into larger machines.

A complete URM implementation is provided [here](Examples/urm.rm), and despite it takes absurdly many steps to carry out any useful simulation, this program makes such simulation possible, as demonstrated [here](#simulation).

### Computability

As mentioned earlier, a Register Machine with registers R0 to Rn can be treated as a partial function from N^n to N. In fact, the machine does not have to use exactly `n + 1` registers. If it has less, then the rest of the inputs has no effect on the output; if it has more, the surplus registers are initialised to 0 and can be used as "scrap registers" during the computation. For example, the [collatz program](Examples/collatz.rm) takes one input (R1), but also uses a temporary register (R2) that is initialised to 0.

For simplicity, let us consider the simplist case of `n = 1`. From now on, we use the term "function" for "partial functions from N to N".

It is easy to see that there are infinitely many functions, but we are interested to see which of these functions are "computable", in other words, their results can be calculated via a finite step of instructions.

Of course, such a definition is quite imprecise, as we have not yet defined what does "one instruction" mean. Since we are dealing with Register Machines, we can define **computable** more strictly as if the function can be implemented by a register machine. It is [equivalent](https://en.wikipedia.org/wiki/Church%E2%80%93Turing_thesis) to other well-established definition of "computable".

On first glance, we may believe that all functions are computable. This is, however, not the case. Thanks to [Gödelisation](#Gödelisation), we can prove so via Cantor's diagonal argument:

We can list Register Machines by their corresponding Gödel number, *i.e.* `RM0`, `R ` *etc.* These machines all have corresponding functions that they implement, *i.e.* `f0`, `f1` *etc.*:

Gödel |Machine|Function
-|-|-
0|RM0|f0
1|RM1|f1
2|RM2|f2
...|...|...

Assume all functions are computable, then all functions must appear in the (countably infinite) table above. However, we can define a function `f*` that does not appear in the table, thus leading to a contradiction. For any natural number n, we define `f*(n) = 0` if `f{n}(n)` is not defined, and leave `f*(n)` undefined if `f{n}(n)` is defined.

For example, `RM0` contains no instruction, thus it does not change the input at all and `f0(0) = 0`. Similarly, `RM1` only contains one Halt instruction, thus `f1(1) = 0` as well (because the input is in R1 but the output is read from R0). However, `RM2` contains one line of `0: R0+ 0`, thus it will never terminate. By our definition of `F`, we would have `f*(0)`, `f*(1)` undefined and `f*(2) = 0`.

One can also verify that `f*` is undefined for inputs 3, 4, 5 and 7, `f*(6) = 0`, `f*(8) = 0`, and so on. Notably, `RM8` is the first Register Machine in the table that actually do something to R0 (it always returns 1), and `RM1090519040` is the first I could find that actually has different outcomes based on the input (*please try find me a smaller one*).

By construction, `f*` and `f{n}` differs on their behaviours on input `n`, hence `f*` is not in the table, contradiction.

Although almost all functions are not computable, it is not trivial to come up with an example of incomputable functions (our `f*` defined above is one of them). In the following sections, we will briefly discuss three (or more?) other such examples.

### Busy Beaver, Wheezy Weaver & The Halting Problem

For a Register Machine with `n` lines, it is natural to ask, what is the largest number it can produce?

Strictly speaking, we start all registers from zero, and we want the result of R0 to be as large as possible. Such a machine is known as a **Busy Beaver**, and the objective is to find the best Busy Beaver with n lines for any natural number `n`. Define `b(n)` to be the maximum output of a Busy Beaver with `n` lines, clearly `b` is a function.

In general, it is very hard to find `b(n)` except for the very first couple of `n` values. `b(0) = 0` because there are no instructions to start with. `b(1) = 1`, which is realised by `L0: R0+ 1`. Note that `L0: R0+ 0` is not a Busy Beaver since it never terminates. Similarly, `b(n) = n` for `n` up to 4 (though it is less trivial to prove so). However, `b(5) = 6` as provided by the following Busy Beaver, while the proof is more complicated:

```RM
L0: R1+ 1
L1: R0+ 2
L2: R0+ 3
L3: R0+ 4
L4: R1- 1 5
```

I was not able to determine `b(6)`, but it is at least 9 from the following Busy Beaver:

```RM
L0: R1+ 1
L1: R1+ 2
L2: R0+ 3
L3: R0+ 4
L4: R0+ 5
L5: R1- 2 6
```

Despite its fascinating definition, the function `b` is not computable, therefore we can never find an algorithm - no matter how inefficient - to determine `b(n)` for any `n`. Its proof again uses *reductio ad absurdum*.

Firstly, we can design a Busy Beaver with `n + 2` lines that produces `2n` similar to the construction of the `b(5)` machine above. Let us take any total computable function `f`. Then it must have a corresponding Register Machine `R` with `l` lines. We can construct another Register Machine `R'` as following.

1. Utilise its first `l + 7 = (l + 5) + 2` lines to put the number `2(l + 5)` into R0 via the Busy Beaver decribed on the paragraph above.

2. Use two lines to move R0 to R1. This is easy to construct.

3. Use `l` lines to compute `f(2(l + 5)) = f(2l + 10)` in R0 via `R`.

4. Use one line to increment R0 and terminates.

By construction, `R'` is a Busy Beaver with `2l + 10` lines that produces the number `f(2l + 10) + 1`.

Assume that `b` is computable, and that it takes `l` lines to implement it as a Register Machine. According to the argument above, there exists a Busy Beaver with `2b + 10` lines that produces `b(2l + 10) + 1`. However, by definition, `b(2l + 10)` is the maximum value that a Busy Beaver with `2b + 10` lines may produce, which leads to a contradiction.

Before introducing the famous Halting Problem, let us look at a variation of Busy Beaver: we still initialise all registers with 0, but instead of trying to produce the largest number possible, this time we attempt to make our machine run with as many steps as possible. We call such machines **Wheezy Weavers**, and define `w(n)` as the most steps a n-line Wheezy Weaver can produce.

For first few values of `n`, we have `w(0) = 1` (starting at line 0, but it does not exist, thus treated as a `HALT`), `w(1) = 2`, `w(2) = 3`, `w(3) = 6` and `w(4) = 11`. The last two are demonstrated with the following machines:

```RM
L0: R1+ L1
L1: R0+ L2
L2: R1- L1 L3
```

```RM
L0: R1+ L1
L1: R0+ L2
L2: R1- L1 L3
L3: R0- L2 L4
```

Like `b`, `w` is also not computable, and the proof is similar. Firstly, notice that if `f(x) = y`, then the Register Machine corresponding to `f` (if exists) must take at leat `y` steps to calculate the result, because the result is in R0 which starts with 0, but each step increment R0 by at most 1. In particular, we have `w(n) >= b(n)` for all `n`.

Let us refer back to the machine that has n + 2 lines and produces 2n. It takes 2n + 4 steps to execute: each line except the first is reached twice plus the final implicit Halt. Now assume `w` is computable by a machine `R` with `l` lines, using the same construction for `R'` in the Busy Beaver proof, we end up with a machine with `2l + 10` lines and `2(l + 7) + 2(2l + 10) + X + 2` steps of execution, where `X` is the number of steps for `R` with input 2l + 10. Therefore the number of steps for `R'` is at least `X + 1 >= w(2l + 10) + 1`. However, `w(2l + 10)` is by definition the upper bound of number of steps `R'` could take, contradiction.

It is time to discuss the famous **Halting Problem**. Simply speaking, we would like to know if a Register Machine would terminate under a given input. While we can easily "eye-ball" simple machines to know if it halts, there is no general algorithm that can determine ternimation for any Register Machines. In other words, the Halting Problem is undecidable.

We can define a function `h` that takes the list encoding of the Gödel number of a machine as well as a series of arguments, returning 1 if the input machine terminates with the input arguments, otherwise 0. Clearly, `h` is a total function.

The most iconic proof for the incomputability of `h` introduces a wrapper around it and produces a contradiction. Here, however, we will prove that `h`, if computable, can be used to build a Register Machine for the Busy Beaver function `b`.

For any natural number `n`, we can develop a Register Machine that generates the Gödel number of all `n`-line Register Machine such that the register indices are between 0 and `n - 1` while the line numbers are between 0 and `n`. It is clear that all Register Machines with `n` lines are equivalent to one of these, because such machines can declare at most `n` registers, and all line numbers not between 0 and `n - 1` have the same behaviour as line `n` (namely implicit Halt).

We can imagine that such a machine is enormous, but it is feasible: one approach is to iterate through all numbers between 0 and `M`, then decode it piece by piece and check if the registers and line numbers are within the range, where `M` is the Gödel number of the machine with `n` identical lines of `R{n-1}- n n`.

With this machine and the [URM](#Universal-Register-Machine), one can simulate the execution of the generated machines one by one, and return the largest R0 value in each simulation. There is one caveat, however, as the simulation would last forever if the corresponding machine does not terminate on input 0. But if we have a machine that solves the Halting Problem, we can use it to check for termination and ignore those that does not finish. In this way, we are effectively building a machine corresponding to `b`. Therefore, the Halting Problem function `h` is not computable.

The [collatz program](Examples/collatz.rm) is a living example that witnesses the story of the undecidable Halting Problem. People believe that this machine terminates for all input, but it remains a conjecture. More information on it can be found [here](#Example).

We will end this long section with a final remark on Wheezy Weavers. If a `n`-line Register Machine executes with no less steps than `w(n)`, clearly it is not going to terminate. This not only gives us another proof on the incomputability of `w`, but also a stronger result: if a function `f` is greater than `w`, namely `f(n) >= w(n)` for all sufficiently large `n`, then `f` is not computable. If not, we can run the machine for `f` first before simulating the exeuction with an extended URM that also keeps track of the number of steps. If the number is greater than `f(n)`, we can immediately determine that the machine does not terminate, which is impossible.

## CLI

### Installation

If you are using Mac OS (both Intel chip and M1) or Ubuntu, you can download the appropriate executable in [Executables](Executables). The executable works on at least macOS Catalina, but **they currently do not represent the latest version since I'm too lazy to compile them on those target machines again**.
Otherwise, you need to compile the CLI, which requires [ghc and cabal](https://www.haskell.org/cabal/).
Once the Haskell environment is set up, make sure the packages `containers` and `parsec` are installed by running the following in the terminal:

```bash
  cabal update
  cabal install --lib containers
  cabal install --lib parsec
```

Then the CLI can be compiled by running `ghc -O3 Main -o mmzkrm -package parsec` from the root directory. It is going to generate an executable called `mmzkrm`.

I am aware that it is not ideal when the project itself is not managed by `cabal`. This is because when I started with the project a couple of years ago, my Haskell ability was quite limited (it is certainly still so now!) and I had no idea how to setup a `cabal` project properly.

### Example

To follow this example, make sure that `mmzkrm` is installed (see [Installation](#Installation)). We assume that the executable has been moved to the system path, if you haven't done so, replace all occurrences of `mmzkrm` by the path to the executable.

The [collatz program](Examples/collatz.rm) is a RM source file that takes one input at R1 and calculates the length of the [Collatz Sequence](https://en.wikipedia.org/wiki/Collatz_conjecture) starting at the input up to the first 1. Each line consists of a label and an instruction which specifies the register, the operation on it (+/-), and the label(s) of the next instruction. See [Syntax](#Syntax) for more information on the syntax of RM source files.

Run `mmzkrm Examples/collatz.rm 65537` from the root directory to evaluate the length of the Collatz Sequence starting from 65537. The result is as following:

```bash
Execution finished after 6186333 steps.
Register values: 
  R0: 100
  R1: 0
  R2: 0
```

Note that it takes more than six million steps to carry out the computation, thus it would be quite slow if naïvely implemented. However, this implementation finishes executing instantly (see [Performance](#Performance)).

We may also use the `-s` option to show the calculation step by step:

```bash
> mmzkrm Examples/collatz.rm 65537 -s
Step 1: 
PC: 0
R0: 0
R1: 65537
R2: 0

Step 2: 
PC: 1
R0: 0
R1: 65536
R2: 0

Step 3: 
PC: 2
R0: 0
R1: 65535
R2: 0

Step 4: 
PC: 3
R0: 0
R1: 65536
R2: 0

Step 5: 
PC: 4
R0: 0
R1: 65537
R2: 0

Step 6: 
PC: 5
R0: 0
R1: 65536
R2: 0

Step 7: 
PC: 6
R0: 0
R1: 65535
R2: 0

Step 8: 
PC: 4
R0: 0
R1: 65535
R2: 1

Step 9: 
PC: 5
R0: 0
R1: 65534
R2: 1

Step 10: 
PC: 6
R0: 0
R1: 65533
R2: 1

Step 11: 
PC: 4
R0: 0
R1: 65533
R2: 2

Step 12: 
PC: 5
R0: 0
R1: 65532
R2: 2

Step 13: 
PC: 6
R0: 0
R1: 65531
R2: 2

Step 14: 
PC: 4
R0: 0
R1: 65531
R2: 3

Step 15: 
PC: 5
R0: 0
R1: 65530
R2: 3

Step 16: 
PC: 6
R0: 0
R1: 65529
R2: 3

Step 17: 
PC: 4
R0: 0
R1: 65529
R2: 4

Step 18: 
PC: 5
R0: 0
R1: 65528
R2: 4

Step 19: 
PC: 6
R0: 0
R1: 65527
R2: 4

Step 20: 
PC: 4
R0: 0
R1: 65527
R2: 5


```

By default, it shows 20 steps at a time. We can either press enter to show the next 20 steps, or enter `quit` to jump to the final result. The  of steps per output is configuable, see [Usage](#Usage) for the details (as well as more options).

Note that when the show-step option is enabled, the simulator does not conduct the check on infinite loops.

### Simulation

In the "Examples" folder, we provided an implementation of the Universal Register Machine. Together with our optimised simulator, it is possible to actually use this machine and do some meaningful computation.

We start with the [adder machine](Examples/add.rm), which takes two arguments and computes the sum. Its Gödel number is insanely large:

```bash
> ./mmzkrm -e Examples/add.rm
Encode each line: 
  152
  1
  4576
  5
  0
Gödel number: 28738678589325274522871087916445314772282785417214007261208231566195210827554169179139861628470962486606050497613789319537928452929252379720353942732879195765819929905481411192367913165424870405419359236629869563732570138212258934001178960300833861066045972985771779328041376477915028074641341742845112134412602756554506347740596379378771007129255295254260103608448723007616643684771274023685388049424268528133557469406988031448736465728547080432626258098667281737687908406236777938345714191323532562315600695931011610336637664954962548791057158591283473247357043382114632403895483000116092134518686862114381002254085074428592896626333315826705672990675973186146242892276056650679867491893573492116998932363973633639087766413428595732271832274448318088427381471021785644268820953877626134901789547297676215103641013574133336623969712045882654242431727896093194490955173751425648027501827221333805372761355399652661789738833048217909906861836304228703616513335888646946990427220815196181512059681627100470476690600390128221682196050367210088188662695436612748023048427226148698151404842535621340206218031127722370326842030151162040144267846930945521052830263468727196374740202910097556253633281843875156784405312440341158883729372819819334058400746019743922715399937828161817022670556978440727174928369172154273083243857404267686720588699632647990306415126457193269688351591566128567521220764583547861786591559018936249608044544
```

Let's say we want to calculate 7 + 5. Fortunately, the encoding for `[7, 5]` is much more reasonable:

```bash
./mmzkrm -e 7 5
Encode from pair: 1408
Encode from list: 8320
```

Finally, we can start the URM simulation by providing the adder's Gödel number as well as the list encoding:

```bash
> ./mmzkrm Examples/urm.rm 28738678589325274522871087916445314772282785417214007261208231566195210827554169179139861628470962486606050497613789319537928452929252379720353942732879195765819929905481411192367913165424870405419359236629869563732570138212258934001178960300833861066045972985771779328041376477915028074641341742845112134412602756554506347740596379378771007129255295254260103608448723007616643684771274023685388049424268528133557469406988031448736465728547080432626258098667281737687908406236777938345714191323532562315600695931011610336637664954962548791057158591283473247357043382114632403895483000116092134518686862114381002254085074428592896626333315826705672990675973186146242892276056650679867491893573492116998932363973633639087766413428595732271832274448318088427381471021785644268820953877626134901789547297676215103641013574133336623969712045882654242431727896093194490955173751425648027501827221333805372761355399652661789738833048217909906861836304228703616513335888646946990427220815196181512059681627100470476690600390128221682196050367210088188662695436612748023048427226148698151404842535621340206218031127722370326842030151162040144267846930945521052830263468727196374740202910097556253633281843875156784405312440341158883729372819819334058400746019743922715399937828161817022670556978440727174928369172154273083243857404267686720588699632647990306415126457193269688351591566128567521220764583547861786591559018936249608044544 8320
Execution finished after 9311331862941388945410232484928281986219622475030095804432976937938965657666556558426993014837468904160272424047445885153472394730573007673586584294480163859726242121881749782143973974107142451052299598023245559817156182864571474096562830441048492239657820919383773714798848975249535274922913512926607846134872978465641995285088418319780979688681016151453541978592570771795770175460492864968554211550463373150458636866199860575761757213267031983551327773910660384291864404081365898183148908747473530093987045566892763224912390278023923261570440551639991197228684928020770901964921430319470402251413708508376866920355620357367602396599855770988441408934402146353264857654166800049168695632480725959373556844106284573617954331663725429837586380200826409774198356800817002581473405300508189566765650011805940358483045350280138756266585559084787641074734378146074134682371187289411907835091275073909211704089385649264906341952508609600157071634876573285537897307898235131973107508970016023404637422893568314296297813049628167583783311057123274063679000308912668413978989748948297496821467761318756112070597562544383076909270796889300780124762473988001944015866341172705770776205779535318720048189161378000936360237755569892334032314573893211387997388229552659797680197628982159264689090401956958812592469676321020663677801994658250827740020411219041643359828484866341089457871021325056106781585378736424436111018192046542030299171355 steps.
Register values: 
  R0: 12
  R1: 28738678589325274522871087916445314772282785417214007261208231566195210827554169179139861628470962486606050497613789319537928452929252379720353942732879195765819929905481411192367913165424870405419359236629869563732570138212258934001178960300833861066045972985771779328041376477915028074641341742845112134412602756554506347740596379378771007129255295254260103608448723007616643684771274023685388049424268528133557469406988031448736465728547080432626258098667281737687908406236777938345714191323532562315600695931011610336637664954962548791057158591283473247357043382114632403895483000116092134518686862114381002254085074428592896626333315826705672990675973186146242892276056650679867491893573492116998932363973633639087766413428595732271832274448318088427381471021785644268820953877626134901789547297676215103641013574133336623969712045882654242431727896093194490955173751425648027501827221333805372761355399652661789738833048217909906861836304228703616513335888646946990427220815196181512059681627100470476690600390128221682196050367210088188662695436612748023048427226148698151404842535621340206218031127722370326842030151162040144267846930945521052830263468727196374740202910097556253633281843875156784405312440341158883729372819819334058400746019743922715399937828161817022670556978440727174928369172154273083243857404267686720588699632647990306415126457193269688351591566128567521220764583547861786591559018936249608044544
  R2: 3
  R3: 0
  R4: 0
  R5: 0
  R6: 0
  R7: 0
  R8: 0
  R9: 0
```

It takes a few second to run on my computer, but is nevertheless feasible and terminates with the correct R0 value (7 + 5 = 12).

One can even take another leap forward and attempt the same input on the [multiplier machine](Examples/mult.rm). On my computer, this would take a couple of minutes. If we increase the inputs, the encoding would grow exponentially and the execution time would also blow up. On the other hand, if we try to simulate complex machines, their Gödel numbers would become unreasonably large. For example, the [collatz program](Examples/collatz.rm) in the previous section has a Gödel number with a size comparable to my RAM when written out. Therefore, simulating any more complex computations would be quite impractical.

### Syntax

In a RM source file, each line consists of an optional label, an instruction, and an optional comment.

Labels are in the form of `AlphanumericLabel:`. They must start with a letter and be globally unique. If a line does not have a label, it can only be referred by its line number (starting from 0).

Instruction has three types as expected:

1. Halt: In the form of `H`, `HALT` or `ARRÊT`.
2. Increment: In the form of `Rn+ lineNumOrLabel` where `Rn` is the register index and `lineNumOrLabel` can either be a label or a line number. For example, `R2+ 3` increments R2 and jumps to line 3.
3. Decrement: In the form of `Rn- lineNumOrLabel lineNumOrLabel` where `Rn` and `lineNumOrLabel` are defined same as above. For example, `R0- 1 end` decrements R0 and jump to line 1 if R0 is positive, otherwise jump to the line that begins with the label `end`.

Note that `R`, `+` and `-` can be ommited. For example, `R0- 1 end` is equivalent to `0 1 end`.

If a label never is never defined, it is treated as a Halt instruction.

Comments starts with "#" and continues until the end of the line. Note that they must follow a valid instruction, in other words, comments are not allowed on their own lines. The only exception is that comments are allowed to appear on their own lines at the very beginning of the file.

The [Examples](Examples) folder contains more examples that demonstrates the RM syntax.

### Usage

#### RM Simulation

`mmzkrm {<options>} <src_file.rm> {<arguments>}`

#### Gödel Number Decoding

`mmzkrm -d <gödel_number>`

#### Gödel Number Encoding

`mmzkrm -e (<src_file.rm> | {<argument>})`

#### Arguments

* `<src_file.rm>`: The path to source file containing a register machine.
* `{<argument>}`: A list of positive integers assigned to the registers, starting from R1; R0 is set to 0. Unspecified arguments default to 0.

#### Options

  Short | Long | Description
  -|-|-
  `-i` |`--initial`|          Starts the arguments from R0.
  `-s[20]`|  `--step[=20]` | Show the configuration after each step of evaluation. `--step=x` shows x steps at a time. Enter `quit` to jump to the result.
  `-d`   |   `--decode`  | Decode the following Gödel number.
  `-e`   |   `--encode`  | Encode the input, which could be a list of numbers separated by spaces, a pair of numbers, or the the path to a source file. By default, if the resultant number is too large, it will not be shown.
  `-f`   |   `--force`  | Used with the `--encode` option. Show the result regardless of its size. Note that this may cause the program to stall indefinitely if the number is too large.

If an option is provided more than once, its first occurrence is picked.

### Caveat

In Gödel Number conversion, the line numbers in a Register Machine can be arbitrarily large integers, but the register indices are always bounded by the size of the Haskell `Int` type. This usually will not cause any problem because during the conversion, the register number appears at the exponent and it cannot grow beyond the size limit for `Int`.  

In Register Machine simulation, the values in each register is unbounded, but the line number and the register indices are always bounded by the the size of the Haskell `Int` type. In other words, if the machine attempts to jump to a line number greater than `2 ^ 64`, the program may exhibit weird behaviours.

Again in the simulation, the simulator cannot handle a Machine that is completely empty; it will report an index out of bound error. This is purely an implementation caveat since the empty Machine is perfectly valid and is equivalent to a Machine that halts immediately.
