# QGameTheory
---
title: 'QGameTheory: Quantum Game Theory Simulator'

author: "Indranil Ghosh"

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



