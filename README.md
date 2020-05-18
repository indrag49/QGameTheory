# QGameTheory
---
Title: 'QGameTheory: Quantum Game Theory Simulator'

Author: "Indranil Ghosh"

Description: General purpose toolbox for simulating quantum versions of game theoretic models. Quantum versions of models that have been handeled are: Penny Flip Game, Prisoner's Dilemma, Two Person Duel, Battle of the Sexes, Newcomb's Paradox, Hawk and Dove Game and Monty Hall Problem.

---

## Installation

The development version of the package can be installed from the github repository:

```{r}
install.packages("devtools")
devtools::install_github("indrag49/QGameTheory")
```

## Dependencies

*QGameTheory* depends on three more packages:

```{r}
library(dplyr)
library(RColorBrewer)
library(R.utils)
```
## Global Variables

The global variables that are required for the quantum game theoretic models, are built by initializing:

```{r}
init()
```
All the global parameters/variables are made visible by:

```{r}
ls()
```
The simulator has access to maximum six qubits for quantum computations. Qubits |1>, |0110> and |111110> can be simulated as:

```{r}
Q1
Q0110
Q111110
```
This code chunk when run, produces:

```{r}
> Q1
     [,1]
[1,]    0
[2,]    1
> Q0110
      [,1]
 [1,]    0
 [2,]    0
 [3,]    0
 [4,]    0
 [5,]    0
 [6,]    0
 [7,]    1
 [8,]    0
 [9,]    0
[10,]    0
[11,]    0
[12,]    0
[13,]    0
[14,]    0
[15,]    0
[16,]    0
> Q111110
      [,1]
 [1,]    0
 [2,]    0
 [3,]    0
 [4,]    0
 [5,]    0
 [6,]    0
 [7,]    0
 [8,]    0
 [9,]    0
[10,]    0
[11,]    0
[12,]    0
[13,]    0
[14,]    0
[15,]    0
[16,]    0
[17,]    0
[18,]    0
[19,]    0
[20,]    0
[21,]    0
[22,]    0
[23,]    0
[24,]    0
[25,]    0
[26,]    0
[27,]    0
[28,]    0
[29,]    0
[30,]    0
[31,]    0
[32,]    0
[33,]    0
[34,]    0
[35,]    0
[36,]    0
[37,]    0
[38,]    0
[39,]    0
[40,]    0
[41,]    0
[42,]    0
[43,]    0
[44,]    0
[45,]    0
[46,]    0
[47,]    0
[48,]    0
[49,]    0
[50,]    0
[51,]    0
[52,]    0
[53,]    0
[54,]    0
[55,]    0
[56,]    0
[57,]    0
[58,]    0
[59,]    0
[60,]    0
[61,]    0
[62,]    0
[63,]    1
[64,]    0
```
The identity matrix:

```{r}
> I2
     [,1] [,2]
[1,]    1    0
[2,]    0    1
```
The Pauli-X, Pauli-Y and the Pauli-Z matrix:

```{r}
> sigmaX(I2)
     [,1] [,2]
[1,]    0    1
[2,]    1    0
> sigmaY(I2)
     [,1] [,2]
[1,] 0+0i 0-1i
[2,] 0+1i 0+0i
> sigmaZ(I2)
     [,1] [,2]
[1,]    1    0
[2,]    0   -1
```

The Hadamard Gate:

```{r}
> Hadamard(I2)
          [,1]       [,2]
[1,] 0.7071068  0.7071068
[2,] 0.7071068 -0.7071068
```
The application of Pauli-X gate on |0> and on |1>, i.e, the spin flip operations on qubits, can be simulated in the following way:

```{r}
> sigmaX(Q0)
     [,1]
[1,]    0
[2,]    1
> sigmaX(Q1)
     [,1]
[1,]    1
[2,]    0
```
There are other important quantum gates like: CNOT, Fredkin, Toffoli, T, Phase, Rx, etc.

```{r}
> CNOT(I4)
     [,1] [,2] [,3] [,4]
[1,]    1    0    0    0
[2,]    0    1    0    0
[3,]    0    0    0    1
[4,]    0    0    1    0
> CNOT(Q11)
     [,1]
[1,]    0
[2,]    0
[3,]    1
[4,]    0
> Fredkin(Q110)
     [,1]
[1,]    0
[2,]    0
[3,]    0
[4,]    0
[5,]    0
[6,]    1
[7,]    0
[8,]    0
> Toffoli(Q010)
     [,1]
[1,]    0
[2,]    0
[3,]    1
[4,]    0
[5,]    0
[6,]    0
[7,]    0
[8,]    0
> T(Q_minus)
                [,1]
[1,]  0.7071068+0.0i
[2,] -0.5000000-0.5i
> Phase(I2)
     [,1] [,2]
[1,] 1+0i 0+0i
[2,] 0+0i 0+1i
> Phase(Q_plus)
                     [,1]
[1,] 0.7071068+0.0000000i
[2,] 0.0000000+0.7071068i
> Rx(Q1, pi/3)
               [,1]
[1,] 0.8660254-0.5i
[2,] 1.0000000+0.0i
```
One can prepare on of the 4 Bell states by using:

```{r}
> Bell(Q0, Q1)
          [,1]
[1,] 0.0000000
[2,] 0.7071068
[3,] 0.7071068
[4,] 0.0000000
> Bell(Q1, Q1)
           [,1]
[1,]  0.0000000
[2,]  0.7071068
[3,] -0.7071068
[4,]  0.0000000
```
Finally for preparing and measuring an arbitrary quantum state,

```{r}
> sigma_x <- sigmaX(I2)
> U <- (kronecker(I2, I2)+1i*kronecker(sigma_x, sigma_x))/sqrt(2)
> Psi <- U %*% Q00
> Psi
                     [,1]
[1,] 0.7071068+0.0000000i
[2,] 0.0000000+0.0000000i
[3,] 0.0000000+0.0000000i
[4,] 0.0000000+0.7071068i
> QMeasure(Psi)
```

<img src="man/figures/1.png" alt=""/>

## Game Theory Concepts
