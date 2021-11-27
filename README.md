Author: MMZK1526 *et ut* Yitang Chen

# Haskell-RM
Evaluate register machines efficiently in `Haskell`.  

The following is an example of using this package to read a register machine code from a file and evaluate it with given arguments. If you haven't heard of register machines, see [Introduction](Introduction) for a brief summary.  

TODO

For full documentation, go to [Documentation](#Documentation).  

# Register Machine
## Introduction
A register machine is a simple system involving a finite number of registers (each holding a natural number), a finite number of lines, and only three operations (increment, decrement and halt).  

An increment operation takes a register and a line number. It increments a the register and jumps to the given line.  

A decrement operation takes a register and two line numbers (say `m` and `n`). If the register is positive, it decrements the value and jumps to line `m`. Otherwise it jump to line `n` (without changing the register, which is still 0).  

A halt operation terminates the machine.  

Consider the following example:

```
0: R1- 1 2
1: R0+ 0
2: R2- 3 4
3: R0+ 2
4: HALT
```

Assume `R0 = 0`, `R1 = 1` and `R2 = 2`. We start from line `0`; which decrements `R1` and goes to line `1`; which increments `R0` and goes back to line `0`; which goes to line `2` since `R1 = 0`; which decrements `R2` and goes to line `3`; which increments `R0` and goes to line `2`; which decrements `R2` and goes to line `3`; which increments `R0` and goes to line `2`; which goes to line `4` since `R2 = 0`; which halts with `R0 = 3`, `R1 = 0` and `R2 = 0`.

If we treat `R0` as the result and the other registers as the input, then a register machine that has registers from `R0` to `Rn` is a partial function from $\mathbb N^n$ to $\mathbb N$. In our previous example, the function is `f(R1, R2) = R1 + R2`.  

Despite its first appearance, register machines are actually very powerful: the system is Turing-complete. This means they are capable of basically whatever modern computers can do.

## Efficiency
TODO

# Documentation
TODO
