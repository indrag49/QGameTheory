##############################################################################
#                                                                            #
#                       QUANTUM NEWCOMB'S PARADOX                            #
#                                                                            #
##############################################################################

#' @title
#' Quantum Newcomb's Paradox
#'
#' @description
#' This function simulates the quantum version of the Newcomb's Paradox by taking in the choice of the qubit |0> or |1> by the supercomputer \code{Omega} and the probability \code{'probability'} with which Alice plays the spin flip operator. It returns the final state of the quantum game along with plotting the probability densities of the qubits of the final state after measurement.
#'
#' @param Omega |0> or |1>
#' @param probability a real number between 0 and 1 including the end points
#'
#' @usage
#' QNewcomb(Omega, probability)
#'
#' @return The final state of the quantum game as a vector along with plotting the probability densities of the qubits of the final state after measurement.
#'
#' @references
#' \url{https://arxiv.org/pdf/quant-ph/0506219.pdf}\cr
#' \url{https://arxiv.org/pdf/quant-ph/0208069.pdf}\cr
#' \url{https://arxiv.org/pdf/quant-ph/0202074.pdf}\cr
#'
#' @examples
#' init()
#' QNewcomb(Q$Q0, 0)
#' QNewcomb(Q$Q1, 0)
#' QNewcomb(Q$Q1, 0.7)
#'
#' @export
#'


QNewcomb <- function(Omega, probability){
  if (Omega[[1]] == 1) Psi_in <- Q$Q00
  else if (Omega[[2]] == 1) Psi_in <- Q$Q11
  H <- Hadamard(Q$I2)
  sigma_x <- sigmaX(Q$I2)
  Psif <- kronecker(H, Q$I2) %*% (probability*kronecker(sigma_x, Q$I2) + (1-probability)*kronecker(Q$I2, Q$I2)) %*% kronecker(H, Q$I2) %*% Psi_in
  QMeasure(Psif)
  return(Psif)
}
