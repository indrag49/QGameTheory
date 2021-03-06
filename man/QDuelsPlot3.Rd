% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/QDuelsPlot3.R
\name{QDuelsPlot3}
\alias{QDuelsPlot3}
\title{Quantum Two Person Duel game}
\usage{
QDuelsPlot3(Psi, alpha1, alpha2)
}
\arguments{
\item{Psi}{a vector representing the initial quantum state}

\item{alpha1}{a number}

\item{alpha2}{a number}
}
\value{
No return value, plots the improvement in Alice's expected payoff as a function of \code{a} and \code{b}, if Alice chooses to fire at the air in her second shot, in a two round game.
}
\description{
This function helps us to plot the improvement in Alice's expected payoff as a function of \code{a} and \code{b}, if Alice chooses to fire at the air in her second shot, in a two round game. \code{Psi} is the initial state of the quantum game, \code{n} is the number of rounds, \code{a} is the probability of Alice missing the target, \code{b} is the probability of Bob missing the target, and
\code{alpha1, alpha2, beta1, beta2} are arbitrary phase factors that lie in -pi to pi that control the outcome of a poorly performing player.
}
\examples{
init()
Qs <- (Q$Q0+Q$Q1)/sqrt(2)
Psi <- kronecker(Q$Q1, Qs)
QDuelsPlot3(Psi, pi/3, pi/6)

}
\references{
\url{https://arxiv.org/pdf/quant-ph/0506219.pdf}\cr
\url{https://arxiv.org/pdf/quant-ph/0208069.pdf}\cr
\url{https://arxiv.org/pdf/quant-ph/0305058.pdf}\cr
}
