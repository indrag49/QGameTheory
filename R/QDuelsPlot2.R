##############################################################################
#                                                                            #
#                     QUANTUM TWO PERSON DUEL GAME                           #
#                                                                            #
##############################################################################

#' @title
#' Quantum Two Person Duel game
#'
#' @description
#' This function helps us to plot Alice's and Bob's expected payoffs as functions of the number of rounds 'n' played in a repeated quantum duel. Psi is the initial state of the quantum game, 'n' is the number of rounds, 'a' is the probability of Alice missing the target, 'b' is the probability of Bob missing the target, and
#' {'alpha1', 'alpha2', 'beta1', 'beta2'} are arbitrary phase factors that lie in -pi to pi that control the outcome of a poorly performing player.
#'
#' @params
#' Psi, n, a, b, alpha1, alpha2, beta1, beta2
#'
#' @usage
#' QDuelsPlot2(Psi, n, a, b, alpha1, alpha2, beta1, beta2)
#'
#' @keywords
#' Quantum Game Theory, Two Person Duel
#'
#' @references
#' \url{https://arxiv.org/pdf/quant-ph/0506219.pdf}\cr
#' \url{https://arxiv.org/pdf/quant-ph/0208069.pdf}\cr
#' \url{https://arxiv.org/pdf/quant-ph/0305058.pdf}\cr
#'
#'
#' @examples
#' initialize_()
#' QDuelsPlot2(Q01, 10, 0.66666, 0.5, -pi/2, pi/4, 0.6, 0.4)
#'
#' @export
#'

QDuelsPlot2 <- function(Psi, n, a, b, alpha1, alpha2, beta1, beta2){
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
