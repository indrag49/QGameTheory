##############################################################################
#                                                                            #
#                     QUANTUM TWO PERSON DUEL GAME                           #
#                                                                            #
##############################################################################

#' @title
#' Quantum Two Person Duel game
#'
#' @description
#' This function returns the expected payoff to Bob for three possible cases for the Quantum Duel game:
#' 1) The game is continued for 'n' rounds and none of the players shoots at the air.
#' 2) The game is continued for 2 rounds and Alice shoots at the air in her second round.
#' 3) The game is continued for 2 rounds and Bob shoots at the air in her second round.
#' Psi is the initial state of the quantum game, 'n' is the number of rounds, 'a' is the probability of Alice missing the target, 'b' is the probability of Bob missing the target, and
#' {'alpha1', 'alpha2', 'beta1', 'beta2'} are arbitrary phase factors that lie in -pi to pi that control the outcome of a poorly performing player
#'
#' @params
#' Psi, n, a, b, alpha1, alpha2, beta1, beta2
#'
#' @usage
#' QDuels_Bob_payoffs(Psi, n, a, b, alpha1, alpha2, beta1, beta2)
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
#' QDuels_Bob_payoffs(Q11, 5, 0.666666, 0.5, 0, 0, 0.2, 0.7)
#' Qs <- (Q0+Q1)/sqrt(2)
#' Psi <- kronecker(Qs, Qs)
#' QDuels_Bob_payoffs(Psi, 5, 0.666666, 0.5, 0, 0, 0.2, 0.7)
#'
#' @export
#'

QDuels_Bob_payoffs <- function(Psi, n, a, b, alpha1, alpha2, beta1, beta2){
  return(1 - QDuels_Alice_payoffs(Psi, n, a, b, alpha1, alpha2, beta1, beta2))
}
