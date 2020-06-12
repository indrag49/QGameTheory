##############################################################################
#                                                                            #
#                      QUANTUM PRISONER'S DILEMMA GAME                       #
#                                                                            #
##############################################################################

#' @title
#' Quantum Prisoner's Dilemma game
#'
#' @description
#' This function returns the expected payoffs to Alice and Bob, with the strategy moves by Alice and Bob as two of the inputs. \code{w, x, y, z} are the payoffs to the players corresponding to the choices available to them with the chain of inequalities, \code{z>w>x>y}.
#' This function also plots the probability distribution plots of the qubits for one of all the combinations of the strategies of the players.
#'
#' @param U_Alice a matrix lying in SU(2)
#' @param U_Bob a matrix lying in SU(2)
#' @param w a number
#' @param x a number
#' @param y a number
#' @param z a number
#'
#' @usage
#' QPD(U_Alice, U_Bob, w, x, y, z)
#'
#' @return A vector consisting of the expected payoffs to Alice and Bob as its elements according to the strategies played by Alice and Bob and also the payoff values.
#'
#' @references
#' \url{https://arxiv.org/pdf/quant-ph/0506219.pdf}\cr
#' \url{https://arxiv.org/pdf/quant-ph/0208069.pdf}\cr
#' \url{https://arxiv.org/pdf/quant-ph/0004076.pdf}\cr
#'
#'
#' @examples
#' init()
#' QPD(Hadamard(Q$I2), sigmaZ(Q$I2), 3, 1, 0, 5)
#'
#' @export
#'


QPD <- function(U_Alice, U_Bob, w, x, y, z) {
  sigma_x <- sigmaX(Q$I2)
  U <- (kronecker(Q$I2, Q$I2)+1i*kronecker(sigma_x, sigma_x))/sqrt(2)
  U_dag <- Conj(t(U))

  initial <- U %*% Q$Q00
  PsiS <- kronecker(U_Alice, U_Bob) %*% initial
  PsiF <- U_dag %*% PsiS

  QMeasure(PsiF)

  cpsif <- Conj(t(PsiF))
  pi_Alice_exp <- w*abs(cpsif %*% Q$Q00)**2+y*abs(cpsif %*% Q$Q01)**2+z*abs(cpsif %*% Q$Q10)**2+x*abs(cpsif %*% Q$Q11)**2
  pi_Bob_exp <- w*abs(cpsif %*% Q$Q00)**2+z*abs(cpsif %*% Q$Q01)**2+y*abs(cpsif %*% Q$Q10)**2+x*abs(cpsif %*% Q$Q11)**2
  return (c(pi_Alice_exp, pi_Bob_exp))
}
