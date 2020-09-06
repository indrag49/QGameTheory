---
title: 'QGameTheory: A Quantum Game Theory Simulator'
tags:
  - R Package
  - Quantum Computing
  - Game Theory
  - Econophysics
authors:
  - name: Indranil Ghosh
    orcid: 0000-0002-7964-1307
    affiliation: "1, 2" 
affiliations:
 - name: Department of Physics, Jadavpur University
   index: 1
 - name: Research Lead, Sirpi Products and Services Pvt. Ltd.
   index: 2
date: 06 September 2020
bibliography: paper.bib
---

# Summary

Quantum game theory [@grabbe2005introduction] has become an enlivening field of study that makes use of quantum manipulations to model the interplay between participating agents/players. These players as a result, apply quantum strategies instead of the classical versions, as studied in classical game theory. One of the earliest studies on consideration of game theory models from the viewpoint of quantum algorithms [@nielsen2002quantum] was commenced by David A. Meyer [@meyer1999quantum], on a quantum version of the penny flip game. He showed that a player applying quantum strategies always beats a player applying classical ones. Quantum game theory has found intriguing applications in several fields like population biology and market economics. A study on quantum Evolutionary Stable Strategies (QESS) was carried out by A. Iqbal and A. H. Toor [@iqbal2001evolutionarily], applying quantum game theory concepts on the original work on ESS by J. Maynard Smith and G. R. Price [@smith1973logic]. Games of survival that are played at the molecular level can be modeled by QESS. Another application of quantum game theory was on market games carried out by E. W. Piotrowski and J. Sladkowski beginning with Quantum Market Games [@piotrowski2002quantum].

``QGameTheory`` is a contributed R package (available at CRAN repository [@QGameThe67:online]) that helps in simulating quantum versions of various game theory models. It is a general-purpose toolbox that works with a simple quantum computing framework, known as the quantum circuit model to perform various computations. The current version of the package makes use of a maximum of six qubits and a total number of seven game theory models. A lot of interdisciplinary attentions within physicists, computer scientists, and economists have generated from the application of quantum computation in game theory, which give rise to many interesting results, like the escape from the dilemma in the case of Quantum Prisoner's Dilemma, which otherwise persists in the classical case. The objective of this paper is to introduce the working of QGameTheory, which the user can use to analyze various quantum games, required for research with applications in market research or evolutionary biology.Quantum  versions of models that have been handled are: Penny Flip Game [@meyer1999quantum], Prisoner's Dilemma [@grabbe2005introduction], Two Person Duel [@flitney2004quantum], Battle of the Sexes [@nawaz2004dilemma], Hawk and Dove Game [@nawaz2010evolutionarily], Newcomb's Paradox [@piotrowski2003quantum] and Monty Hall Problem [@flitney2002quantum].

The package ``QGameTheory`` provides a researcher with the simulation tools to apply the concepts of quantum games in the above research domains. It can be used mostly by econophysicists and mathematicians working in these interdisciplinary fields.

# Acknowledgements

We acknowledge important inputs from Nathan Shammah and also support from Debasish Lohar during the genesis of this project

# References
