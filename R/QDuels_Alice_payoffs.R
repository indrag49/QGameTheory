##############################################################################
#                                                                            #
#                     QUANTUM TWO PERSON DUEL GAME                           #
#                                                                            #
##############################################################################

#' @title
#' Quantum Two Person Duel game
#'
#' @description
#' This function returns the expected payoff to Alice for three possible cases for the Quantum Duel game:
#' 1) The game is continued for \code{n} rounds and none of the players shoots at the air.
#' 2) The game is continued for 2 rounds and Alice shoots at the air in her second round.
#' 3) The game is continued for 2 rounds and Bob shoots at the air in her second round.
#'
#' \code{Psi} is the initial state of the quantum game, \code{n} is the number of rounds, \code{a} is the probability of Alice missing the target, \code{b} is the probability of Bob missing the target, and \code{alpha1, alpha2, beta1, beta2} are arbitrary phase factors that lie in -pi to pi that control the outcome of a poorly performing player.
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
#' QDuels_Alice_payoffs(Psi, n, a, b, alpha1, alpha2, beta1, beta2)
#'
#' @return A list consisting of the payoff value to Alice depending on three situations of the quantum duel game: 1) The game is continued for \code{n} rounds and none of the players shoots at the air, 2) The game is continued for 2 rounds and Alice shoots at the air in her second round and 3) The game is continued for 2 rounds and Bob shoots at the air in her second round.
#'
#' @references
#' \url{https://arxiv.org/pdf/quant-ph/0506219.pdf}\cr
#' \url{https://arxiv.org/pdf/quant-ph/0208069.pdf}\cr
#' \url{https://arxiv.org/pdf/quant-ph/0305058.pdf}\cr
#'
#'
#' @examples
#' init()
#' QDuels_Alice_payoffs(Q$Q11, 5, 0.666666, 0.5, 0, 0, 0.2, 0.7)
#' Qs <- (Q$Q0+Q$Q1)/sqrt(2)
#' Psi <- kronecker(Qs, Qs)
#' QDuels_Alice_payoffs(Psi, 5, 0.666666, 0.5, 0, 0, 0.2, 0.7)
#'
#' @export
#'

QDuels_Alice_payoffs <- function(Psi, n, a, b, alpha1, alpha2, beta1, beta2){
  Q$Q0 <- as.vector(Q$Q0)
  Q$Q1 <- as.vector(Q$Q1)
  Q$Q00 <- as.vector(Q$Q00)
  Q$Q01 <- as.vector(Q$Q01)
  Q$Q10 <- as.vector(Q$Q10)
  Q$Q11 <- as.vector(Q$Q11)
  Psi <- as.vector(Psi)

  Ab <- (exp(-1i*alpha1)*cos(acos(sqrt(a)))*Q$Q11+1i*exp(1i*beta1)*sin(acos(sqrt(a)))*Q$Q10)%o%Q$Q11 + (exp(1i*alpha1)*cos(acos(sqrt(a)))*Q$Q10+1i*exp(-1i*beta1)*sin(acos(sqrt(a)))*Q$Q11)%o%Q$Q10 + Q$Q00%o%Q$Q00 + Q$Q01%o%Q$Q01
  Ba <- (exp(-1i*alpha2)*cos(acos(sqrt(b)))*Q$Q11+1i*exp(1i*beta2)*sin(acos(sqrt(b)))*Q$Q01)%o%Q$Q11 + (exp(1i*alpha2)*cos(acos(sqrt(b)))*Q$Q01+1i*exp(-1i*beta2)*sin(acos(sqrt(b)))*Q$Q11)%o%Q$Q01 + Q$Q00%o%Q$Q00 + Q$Q10%o%Q$Q10
  A <- (Ba %*% Ab)
  for(i in 1:(n-1)) A <- A %*% (Ba %*% Ab)
  Psif1 <- A %*% Psi

  B <- Ba %*% (Ba %*% Ab)
  Psif2 <- B %*% Psi

  C <- Ab %*% (Ba %*% Ab)
  Psif3 <- C %*% Psi
  return(c(abs(Conj(Q$Q10)%*%Psif1)**2+0.5*abs(Conj(Q$Q11)%*%Psif1)**2, abs(Conj(Q$Q10)%*%Psif2)**2+0.5*abs(Conj(Q$Q11)%*%Psif2)**2, abs(Conj(Q$Q10)%*%Psif3)**2+0.5*abs(Conj(Q$Q11)%*%Psif3)**2))
}
