% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/QDuelsPlot1.R
\name{QDuelsPlot1}
\alias{QDuelsPlot1}
\title{Quantum Two Person Duel game}
\usage{
QDuelsPlot1(Psi, n, a, b, beta1, beta2)
}
\arguments{
\item{Psi}{a vector representing the initial quantum state}

\item{n}{an integer}

\item{a}{a number}

\item{b}{a number}

\item{beta1}{a number}

\item{beta2}{a number}
}
\value{
No return value, plots Alice's and Bob's expected payoffs as functions of \code{alpha1} and \code{alpha2}.
}
\description{
This function helps us to plot Alice's and Bob's expected payoffs as functions of \code{alpha1} and \code{alpha2}. \code{Psi} is the initial state of the quantum game, \code{n} is the number of rounds, \code{a} is the probability of Alice missing the target, \code{b} is the probability of Bob missing the target, and
\code{alpha1, alpha2, beta1, beta2} are arbitrary phase factors that lie in -pi to pi that control the outcome of a poorly performing player.
}
\examples{
init()
QDuelsPlot1(Q$Q10, 2, 0.66666, 0.5, 0.2, 0.8)

}
\references{
\url{https://arxiv.org/pdf/quant-ph/0506219.pdf}\cr
\url{https://arxiv.org/pdf/quant-ph/0208069.pdf}\cr
\url{https://arxiv.org/pdf/quant-ph/0305058.pdf}\cr
}
