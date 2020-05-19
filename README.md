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

The **Quantum Fourier Transform** for a given state |y> is simulated by:

```{r}
> QFT(4)
              [,1]
[1,]  0.3535534+0i
[2,] -0.3535534+0i
[3,]  0.3535534-0i
[4,] -0.3535534+0i
[5,]  0.3535534-0i
[6,] -0.3535534+0i
[7,]  0.3535534-0i
[8,] -0.3535534+0i
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

The **Iterated Deletion of Strictly Dominated Strategies** algorithm is simulated by taking in the payoff matrices of two players from a two-person game:

```{r}
> P1 <- matrix(c(8, 0, 3, 3, 2, 4, 2, 1, 3), ncol=3, byrow=TRUE)
> P2 <- matrix(c(6, 9, 8, 2, 1, 3, 8, 5, 1), ncol=3, byrow=TRUE)
> IDSDS(P1, P2)
[[1]]
     [,1]
[1,]    4

[[2]]
     [,1]
[1,]    3
```

The **NASH** equilibrium of the payoff matrix of a two person game is computed similary in the following way:

```{r}
> NASH(P1, P2)
Joining, by = c("V1", "V2")
  V1 V2
1  2  3
```
This generates the indices of the cell corresponding to the NASH equilibrium.

## Quantum Game Theory Models

### Quantum Penny Flip

For the game tree mentioned below:

<img src="man/figures/2.png" alt=""/>

The simulation codes go in the following way:

```{r}
> psi <- (u+d)/sqrt(2)
> S1 <- sigmaX(I2)
> S2 <- I2
> H <- Hadamard(I2)
> SA <- list(S1, S2)
> SB <- list(H)
> QPennyFlip(psi, SA,SB)
```
It produces the plot:

<img src="man/figures/3.png" alt=""/>

### Quantum Prisoner's Dilemma

One instance of the Quantum Prisoner's Dilemma game can be simulated first by providing the strategies played by both Alice and Bob along with the payoffs *w, x, y, z* available to them corresponding to their choices. The payoffs follow, *z>w>x>y*. 

```{r}
> QPD(Hadamard(I2), sigmaZ(I2), 3, 1, 0, 5)
[1] 1.5 4.0
```

It also generates the plot:

<img src="man/figures/4.png" alt=""/>

The payoff matrix of the **Quantum Prisoner's Dilemma** for both the players can be constructed:

```{r}
> moves <- list(I2, sigmaX(I2), Hadamard(I2), sigmaZ(I2))
> PayoffMatrix_QPD(moves, 3, 1, 0, 5)
[[1]]
     [,1] [,2] [,3] [,4]
[1,]    3    0 0.50  1.0
[2,]    5    1 0.50  0.0
[3,]    3    3 2.25  1.5
[4,]    1    5 4.00  3.0

[[2]]
     [,1] [,2] [,3] [,4]
[1,]  3.0  5.0 3.00    1
[2,]  0.0  1.0 3.00    5
[3,]  0.5  0.5 2.25    4
[4,]  1.0  0.0 1.50    3
```

The above code also generates all the sixteen possible combinations of plots. Analysing, it is noticed that the quntum version helps us escape the so called dilemma in the classical Prisoner's dilemma game. One next uses the **IDSDS** algorithm to find the **strictly dominant strategy equilibrium**. The **NASH** equilibrium is also calculated by the above codes. It can be seen that both of them give the same result and the equilibrium is Pareto Optimum too.

### Quantum Two Person Duel

Simulation is carried out to calculate the expected payoffs to Alice and Bob for the following three cases:

1) The game is continued for 'n' rounds and none of the players shoots at the air.

2) The game is continued for 2 rounds and Alice shoots at the air in her second round.

3) The game is continued for 2 rounds and Bob shoots at the air in her second round.

```{r}
> Qs <- (Q0+Q1)/sqrt(2)
> Psi <- kronecker(Qs, Qs)
> QDuels_Alice_payoffs(Psi, 5, 0.666666, 0.5, 0, 0, 0.2, 0.7)
[1] 0.2087876 0.3281732 0.4894636
> QDuels_Bob_payoffs(Psi, 5, 0.666666, 0.5, 0, 0, 0.2, 0.7)
[1] 0.7912124 0.6718268 0.5105364
```

Four plotting functions are available to simulate the corresponding results:

1) For plotting Alice's and Bob's expected payoffs as functions of 'alpha1' and 'alpha2':

```{r}
QDuelsPlot1(Psi, 5, 0.66666, 0.5, 0.2, 0.7)
```

<p float="left">
  <img src="man/figures/5.png" width="100" />
  <img src="man/figures/6.png" width="100" /> 
</p>
