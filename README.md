# Haskell-RM

By MMZK1526 *a.k.a.* Yìtáng Chén

A CLI that evaluates [**Register Machines**](#Register-Machine) efficiently in `Haskell`. It also provides an library defining and simulating Register Machines that can be embedded in Haskell code.

If you haven't heard of Register Machines, see [Introduction](#Introduction) for a brief summary.  

For an example of using the CLI to read RM code from a file and evaluate it with given arguments, go to [Example](#Example).

For the full CLI documentation, go to [CLI](#CLI).

For the full API documentation, go to [Documentation](#Documentation).  

## Register Machine

### Introduction

A Register Machine is a simple system involving a finite number of registers (each can hold a natural number), a finite number of lines, and only three instructions (increment, decrement and halt).  

An increment instruction takes a register and a line. It increments a the register and jumps to the given line.  

A decrement instruction takes a register and two line s (say `m` and `n`). If the register is positive, it decrements the value and jumps to line `m`. Otherwise it jump to line `n` (without changing the register, which is still 0).  

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

This optimisation also makes simulating the [Universal Register Machine](#URM) possible.

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

Finally, once we encode each line of a Register Machine into a , we can then encode the list of number into a single number by `s`.

One can verify that the "adder" machine in [Introduction](#Introduction) has a Gödel number of `s([152, 1, 4576, 5, 0])`, a number larger than 10^1426.

If we convert a natual number to a Register Machine, then most likely it will contain instruction that makes no sense, for example jumping to a non-existing line . This does not cause any problem, however, since we treat bad line number as Halt instructions.

In [Convert.hs](Convert.hs), there are several utility functions that can convert between `Line`s, `RMCode`s, lists, pairs, and natural numbers. The documentation can be viewed [here](#Convert).

### URM

TODO

### Computability

As mentioned earlier, a Register Machine with registers R0 to Rn can be treated as a partial function from N^n to N. In fact, the machine does not have to use exactly `n + 1` registers. If it has less, then the rest of the inputs has no effect on the output; if it has more, the surplus registers are initialised to 0 and can be used as "scrap registers" during the computation. For example, the [collatz program](Examples/collatz.rm) takes one input (R1), but also uses a temporary register (R2) that is initialised to 0.

For simplicity, let us consider the simplist case of `n = 1`. From now on, we use the term "function" for "partial functions from N to N".

It is easy to see that there are infinitely many functions, but we are interested to see which of these functions are "computable", in other words, their results can be calculated via a finite step of instructions.

Of course, such a definition is quite imprecise, as we have not yet defined what does "one instruction" mean. Since we are dealing with Register Machines, we can define **computable** more strictly as if the function can be implemented by a register machine. It is [equivalent](https://en.wikipedia.org/wiki/Church%E2%80%93Turing_thesis) to other well-established definition of "computable".

On first glance, we may believe that all functions are computable. This is, however, not the case. Thanks to [Gödelisation](#Gödelisation), we can prove so via Cantor's diagonal argument:

We can list Register Machines by their corresponding Gödel , *i.e.* `RM0`, `RM1` *etc.* These machines all have corresponding functions that they implement, *i.e.* `f0`, `f1` *etc.*:

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

With this machine and the [URM](#URM), one can simulate the execution of the generated machines one by one, and return the largest R0 value in each simulation. There is one caveat, however, as the simulation would last forever if the corresponding machine does not terminate on input 0. But if we have a machine that solves the Halting Problem, we can use it to check for termination and ignore those that does not finish. In this way, we are effectively building a machine corresponding to `b`. Therefore, the Halting Problem function `h` is not computable.

The [collatz program](Examples/collatz.rm) is a living example that witnesses the story of the undecidable Halting Problem. People believe that this machine terminates for all input, but it remains a conjecture. More information on it can be found [here](#Example).

We will end this long section with a final remark on Wheezy Weavers. If a `n`-line Register Machine executes with no less steps than `w(n)`, clearly it is not going to terminate. This not only gives us another proof on the incomputability of `w`, but also a stronger result: if a function `f` is greater than `w`, namely `f(n) >= w(n)` for all sufficiently large `n`, then `f` is not computable. If not, we can run the machine for `f` first before simulating the exeuction with an extended URM that also keeps track of the number of steps. If the number is greater than `f(n)`, we can immediately determine that the machine does not terminate, which is impossible.

## CLI

### Installation

If you are using Mac OS with Intel chips (*i.e.* not the newer M1 chips), you can download the appropriate executable in [Executables](Executables). The executable works on at least macOS Catalina.
Otherwise, you need to compile the CLI, which requires [ghc and cabal](https://www.haskell.org/cabal/).
Once the Haskell environment is set up, make sure the packages `containers` and `parsec` are installed by running the following in the terminal:

```bash
  cabal update
  cabal install --lib containers
  cabal install --lib parsec
```

Then the CLI can be compiled by running `ghc -O3 Main -o mmzkrm` from the root directory. It is going to generate an executable called `mmzkrm`.

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

### Syntax

In a RM source file, each line consists of an optional label, an instruction, and an optional comment.

Labels are in the form of `AlphanumericLabel:`. They must start with a letter and be globally unique. If a line does not have a label, it can only be referred by its line number (starting from 0).

Instruction has three types as expected:

1. Halt: In the form of `H`, `HALT` or `ARRÊT`.
2. Increment: In the form of `Rn+ lineNumOrLabel` where `Rn` is the register index and `lineNumOrLabel` can either be a label or a line number. For example, `R2+ 3` increments R2 and jumps to line 3.
3. Decrement: In the form of `Rn- lineNumOrLabel lineNumOrLabel` where `Rn` and `lineNumOrLabel` are defined same as above. For example, `R0- 1 end` decrements R0 and jump to line 1 if R0 is positive, otherwise jump to the line that begins with the label `end`.

Note that `R`, `+` and `-` can be ommited. For example, `R0- 1 end` is equivalent to `0 1 end`.

Comments starts with "#" and continues until the end of the line. Note that they must follow a valid instruction, in other words, comments are not allowed on their own lines.

The [Examples](Examples) folder contains more examples that demonstrates the RM syntax.

### Usage

CLI Usage: `mmzkrm {<options>} <src_file.rm> {<arguments>}`

Arguments:
  A list of non-negative positive integers assigned to the registers, starting from R1; R0 is set to 0.

Options:
  Short | Long | Description
  -|-|-
  `-i` |`--initial`|          Starts the arguments from R0.
  `-s[20]`|  `--step[=20]` | Show the configuration after each step of evaluation. `--step=x` shows x steps at a time. Enter `quit` to jump to the result.

## Documentation

### Convert

* `decodeLine :: Integer -> Line`:  
  * Decodes an `Integer` into a `Line`.  
  * *e.g.* `decodeLine 24 = R1- 1 0` since 35 = 2 ^ **3** *(2* **1** + 1), 3 = 2 ***1** + 1 and 1 + 1 = 2 = (2 ^ **1*** (1 * **0** + 1)).  

* `decodeList :: Integer -> [Integer]`:  
  * Decodes an `Integer` into a list of `Integer`s.  
  * *e.g.* `decodeLine 42 = [1, 1, 1]` since 42 = 2 ^ **1** *(2* 2 ^ **1** *(2* (2 ^ **1** *(2* **0** + 1)) + 1) + 1).  

* `decodePair :: (Integral a, Integral b) => b -> (a, a)`:  
  * Decodes an `Integral` (*e.g* an `Integer`) into a pair of `Integrals`.  
  * *e.g.* `decodePair 100 = (2, 12)` (using `Integer`s) since 100 = 2 ^ **2** *(2* **12** + 1).  

* `decodeRM :: Integer -> RMCode`:  
  * Decodes an `Integer` into a Register Machine `RMCode`.  
  * *e.g.*  

  ```RM
  decodeRM 3072 = 
  L0: R0- 0 1
  L1: HALT
  ```

* `encodeLine :: Line -> Integer`:  
  * Encodes a `Line` into an `Integer`.  
  * *e.g.* `encodeLine l = 24` if `l = R1- 1 0`.  

* `encodeList :: [Integer] -> Integer`:  
  * Decodes a list of `Integer`s into an `Integer`.  
  * *e.g.* `decodeLine [1, 1, 1] = 42`.  

* `encodePair :: (Integral a, Integral b) => a -> a -> b`:  
  * Decodes a pair of `Integrals` into an `Integral`.  
  * *e.g.* `decodePair (2, 12) = 100` (using `Integer`s).  

* `encodeRM :: RMCode -> Integer`:  
  * Decodes a Register Machine `RMCode` into an `Integer`.  
  * *e.g.* `decodeRM rm = 3072` if  

  ```RM
  rm =
  L0: R0- 0 1
  L1: HALT
  ```  

  * Note that since the Gödel number of Register Machines "grows" very quickly, it is usually pointless trying to compute the entire number for machines of interests. An alternative is to view the list representation of a machine via `toList`.  

* `fromList :: LineLike l => [l] -> RMCode`:  
  * Decodes a list of `Linelike`s (namely `Line` or `Integer`) into a Register Machine `RMCode`.  
  * *e.g.*  

  ```RM
  fromList [1024, 0] = 
  L0: R0- 0 1
  L1: HALT
  ```

* `toList :: LineLike l => RMCode -> [l]`:  
  * Encodes a Register Machine `RMCode` into a list of `Linelike`s (namely `Line` or `Integer`).  
  * *e.g.* `toList rm :: [Integer] = [1024, 0]` if  

  ```RM
  rm =
  L0: R0- 0 1
  L1: HALT
  ```  

TODO: Other documentation
