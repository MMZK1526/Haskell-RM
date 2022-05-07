# Haskell-RM

A CLI that evaluates register machines efficiently in `Haskell`. Also provide an library defining and simulating register machines that can be embedded in Haskell code.

The following is an example of using the CLI to read a Register Machine code from a file and evaluate it with given arguments. If you haven't heard of register machines, see [Introduction](###Introduction) for a brief summary.  

TODO

For the full CLI documentation, go to [MMZKRM](##MMZKRM).

For the full API documentation, go to [Documentation](##Documentation).  

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

Despite its first appearance, register machines are actually very powerful: the system is Turing-complete. This means they are capable of basically whatever modern computers can do.

### Gödelisation

Intriguingly, there is a **ONE TO ONE** correspondence between natural numbers and register machines. In other words, any natural number uniquely represents a Register Machine and *vice versa*.  

TODO

In [Convert.hs](Convert.hs), there are several utility functions that can convert between `Line`s, `RMCode`s, lists, pairs, and natural numbers. The documentation can be viewed [here](##Convert).

### Performance

TODO

## CLI

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
