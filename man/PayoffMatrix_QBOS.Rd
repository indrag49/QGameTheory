% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PayoffMatrix_QBOS.R
\name{PayoffMatrix_QBOS}
\alias{PayoffMatrix_QBOS}
\title{Quantum Battle of the Sexes game: Payoff Matrix}
\usage{
PayoffMatrix_QBOS(moves, alpha, beta, gamma)
}
\arguments{
\item{moves}{a list of matrices}

\item{alpha}{a number}

\item{beta}{a number}

\item{gamma}{a number}
}
\value{
The payoff matrices for the two players as two elements of a list.
}
\description{
This function generates the payoff matrix for the Quantum Battle of Sexes game for all the four combinations of \code{p} and \code{q}. \code{moves} is a list of two possible strategies for each of the players and \code{alpha, beta, gamma} are the payoffs for the players corresponding to the choices available to them with the chain of inequalities, \code{alpha>beta>gamma}.
}
\examples{
init()
moves <- list(Q$I2, sigmaX(Q$I2))
PayoffMatrix_QBOS(moves, 5, 3, 1)

}
\references{
\url{https://arxiv.org/pdf/quant-ph/0506219.pdf}\cr
\url{https://arxiv.org/pdf/quant-ph/0208069.pdf}\cr
\url{https://arxiv.org/abs/quant-ph/0110096}\cr
}
