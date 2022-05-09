# Haskell-RM

A CLI that evaluates Register Machines efficiently in `Haskell`. It also provides an library defining and simulating Register Machines that can be embedded in Haskell code.

If you haven't heard of Register Machines, see [Introduction](#Introduction) for a brief summary.  

For an example of using the CLI to read RM code from a file and evaluate it with given arguments, go to [Example](#Example).

For the full CLI documentation, go to [CLI](#CLI).

For the full API documentation, go to [Documentation](#Documentation).  

## Register Machine

### Introduction

A Register Machine is a simple system involving a finite number of registers (each holding a natural number), a finite number of lines, and only three operations (increment, decrement and halt).  

An increment operation takes a register and a line number. It increments a the register and jumps to the given line.  

A decrement operation takes a register and two line numbers (say `m` and `n`). If the register is positive, it decrements the value and jumps to line `m`. Otherwise it jump to line `n` (without changing the register, which is still 0).  

A halt operation terminates the machine.  

Consider the following example:

```RM
0: R1- 1 2
1: R0+ 0
2: R2- 3 4
3: R0+ 2
4: HALT
```

Assume `R0 = 0`, `R1 = 1` and `R2 = 2`. We start from line `0`; which decrements `R1` and goes to line `1`; which increments `R0` and goes back to line `0`; which goes to line `2` since `R1 = 0`; which decrements `R2` and goes to line `3`; which increments `R0` and goes to line `2`; which decrements `R2` and goes to line `3`; which increments `R0` and goes to line `2`; which goes to line `4` since `R2 = 0`; which halts with `R0 = 3`, `R1 = 0` and `R2 = 0`.

If we treat `R0` as the result and the other registers as the input, then a Register Machine that has registers from `R0` to `Rn` is a partial function from $\mathbb N^n$ to $\mathbb N$. In our previous example, the function is `f(R1, R2) = R1 + R2`.  

Despite its first appearance, Register Machines are actually very powerful: the system is Turing-complete. This means they are capable of basically whatever modern computers can do.

### Gödelisation

Intriguingly, there is a **ONE TO ONE** correspondence between natural numbers and Register Machines (Gödelisation). In other words, any natural number uniquely represents a Register Machine and *vice versa*.  

The foundation of Gödelisation lies in the following functions: let `F(x, y) = 2^x + (2y + 1)` and `f(x, y) = F(x, y) - 1`, it can be proven that the former is a bijection between pairs of natual numbers to positive numbers and the latter a bijection to natural numbers:

f|F|(x, y)
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

With `F`, we can recursively define a function, `L`, that is a bijection between finite lists of natural numbers and singular natural numbers: `L([]) = 0; L(x : xs) = F(x, L(xs))`:

L|xs|L|xs
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

There is a trick to "decode" a number to a list of numbers, namely expressing the number in binary form and count of number of zeros between ones from right to left. For example, 998 in binary is 1111100110, and if we count from the rightmost digit, it starts with 1 zero before reaching a one, then 0 zeros due to the consecutive ones, then 2 zeros, and so on. The result is then [1, 0, 2, 0, 0, 0, 0].

With the functions `F` and `f`, we can then encode each line of a Register Machine. If the instruction is `HALT`, encode it with 0; if it is an increment, then it has a register number `r` with a line number `l`, and we encode it with `F(2r, l)`; if it is a decrement, then it has a register number `r` with two line numbers `l1` and `l2`, and we encode it with `F(2r + 1, f(l1, l2))`.

Finally, once we encode each line of a Register Machine into a number, we can then encode the list of numbers into a single number by `L`.

One can verify that the "adder" machine in [Introduction](#Introduction) has a Gödel number of `L([152, 1, 4576, 5, 0])`, a number larger than 10^1426.

In [Convert.hs](Convert.hs), there are several utility functions that can convert between `Line`s, `RMCode`s, lists, pairs, and natural numbers. The documentation can be viewed [here](#Convert).

### Computability

TODO

### Performance

TODO

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

To follow this example, make sure that `mmzkrm` is installed (see [installation](#Installation)). We assume that the executable has been moved to the system path, if you haven't done so, replace all occurrences of `mmzkrm` by the path to the executable.

The [collatz program](Examples/collatz.rm) is a RM source file that takes one input at R1 and calculates the length of the [Collatz Sequence](https://en.wikipedia.org/wiki/Collatz_conjecture) starting at the input up to the first 1. Each line consists of a label and an instruction which specifies the register, the operation on it (+/-), and the label(s) of the next instruction. See [syntax](#Syntax) for more information on the syntax of RM source files.

Run `mmzkrm Examples/collatz.rm 65537` from the root directory to evaluate the length of the Collatz Sequence starting from 65537. The result is as following:

```bash
Execution finished after 6186333 steps.
Register values: 
  R0: 100
  R1: 0
  R2: 0
```

Note that it takes more than six million steps to carry out the computation, thus it would be quite slow if naïvely implemented. However, this implementation finishes executing instantly.

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

By default, it shows 20 steps at a time. We can either press enter to show the next 20 steps, or enter `quit` to jump to the final result. The number of steps per output is configuable, see [options](#Options) for the details (as well as more options).

### Syntax

TODO

### Options

TODO

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
  0: R0- 0 1
  1: HALT
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
  0: R0- 0 1
  1: HALT
  ```  

  * Note that since the Gödel Number of Register Machines "grows" very quickly, it is usually pointless trying to compute the entire number for machines of interests. An alternative is to view the list representation of a machine via `toList`.  

* `fromList :: LineLike l => [l] -> RMCode`:  
  * Decodes a list of `Linelike`s (namely `Line` or `Integer`) into a Register Machine `RMCode`.  
  * *e.g.*  

  ```RM
  fromList [1024, 0] = 
  0: R0- 0 1
  1: HALT
  ```

* `toList :: LineLike l => RMCode -> [l]`:  
  * Encodes a Register Machine `RMCode` into a list of `Linelike`s (namely `Line` or `Integer`).  
  * *e.g.* `toList rm :: [Integer] = [1024, 0]` if  

  ```RM
  rm =
  0: R0- 0 1
  1: HALT
  ```  

TODO: Evaluator example & documentation
