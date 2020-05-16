##############################################################################
#                                                                            #
#                       QUANTUM NEWCOMB'S PARADOX                            #
#                                                                            #
##############################################################################

#' @title
#' Quantum Newcomb's Paradox
#'
#' @description
#' This function simulates the quantum version of the Newcomb's Paradox by taking in the choice of the qubit |0> or |1> by the supercomputer 'Omega' and the probability 'probability' with which Alice plays the spin flip operator. It returns the final state of the quantum game along with plotting the probability densities of the qubits of the final state after measurement.
#'
#' @params
#' Omega, probability
#'
#' @usage
#' QNewcomb(Omega, probability)
#'
#' @keywords
#' Quantum Game Theory, Newcomb's Paradox
#'
#' @references
#' \url{https://arxiv.org/pdf/quant-ph/0506219.pdf}\cr
#' \url{https://arxiv.org/pdf/quant-ph/0208069.pdf}\cr
#' \url{https://arxiv.org/pdf/quant-ph/0202074.pdf}\cr
#'
#' @examples
#' initialize_()
#' QNewcomb(Q0, 0)
#' QNewcomb(Q1, 0)
#' QNewcomb(Q1, 0.7)
#'
#' @export
#'


QNewcomb <- function(Omega, probability){
  if (Omega[[1]] == 1) Psi_in <- Q00
  else if (Omega[[2]] == 1) Psi_in <- Q11
  H <- Hadamard(I2)
  sigma_x <- sigmaX(I2)
  Psif <- kronecker(H, I2) %*% (probability*kronecker(sigma_x, I2) + (1-probability)*kronecker(I2, I2)) %*% kronecker(H, I2) %*% Psi_in
  QMeasure(Psif)
  retrun(Psif)
}
