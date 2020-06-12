##############################################################################
#                                                                            #
#                     QUANTUM TWO PERSON DUEL GAME                           #
#                                                                            #
##############################################################################

#' @title
#' Quantum Two Person Duel game
#'
#' @description
#' This function helps us to plot Alice's and Bob's expected payoffs as functions of the number of rounds \code{n} played in a repeated quantum duel. \code{Psi} is the initial state of the quantum game, \code{n} is the number of rounds, \code{a} is the probability of Alice missing the target, \code{b} is the probability of Bob missing the target, and
#' \code{alpha1, alpha2, beta1, beta2} are arbitrary phase factors that lie in -pi to pi that control the outcome of a poorly performing player.
#'
#' @param Psi a vector representing the initial quantum state
#' @param n an integer
#' @param a a number
#' @param b a number
#' @param alpha1 a number
#' @param alpha2 a number
#' @param beta1 a number
#' @param beta2 a number
#'
#' @usage
#' QDuelsPlot2(Psi, n, a, b, alpha1, alpha2, beta1, beta2)
#'
#' @return No return value, plots Alice's and Bob's expected payoffs as functions of the number of rounds \code{n} played in a repeated quantum duel.
#'
#' @references
#' \url{https://arxiv.org/pdf/quant-ph/0506219.pdf}\cr
#' \url{https://arxiv.org/pdf/quant-ph/0208069.pdf}\cr
#' \url{https://arxiv.org/pdf/quant-ph/0305058.pdf}\cr
#'
#'
#' @examples
#' init()
#' QDuelsPlot2(Q$Q01, 10, 0.66666, 0.5, -pi/2, pi/4, 0.6, 0.4)
#'
#' @export
#'

QDuelsPlot2 <- function(Psi, n, a, b, alpha1, alpha2, beta1, beta2){
  Psi <- as.vector(Psi)
  X <- seq(1, n)
  Y1 <- c()
  Y2 <- c()
  for(i in X){
    Y1 <- c(Y1, QDuels_Alice_payoffs(Psi, i, a, b, alpha1, alpha2, beta1, beta2)[[1]])
    Y2 <- c(Y2, QDuels_Bob_payoffs(Psi, i, a, b, alpha1, alpha2, beta1, beta2)[[1]])
  }
  plot(X, Y1, col='blue', main="<pi_Alice> vs rounds", type='l')
  plot(X, Y2, col='red', main="<pi_Bob> vs rounds", type='l')
}
