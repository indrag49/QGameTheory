##############################################################################
#                                                                            #
#                             PAULI MATRIX                                   #
#                                                                            #
##############################################################################

#' @title
#' Pauli-Y gate
#'
#' @description
#' This function operates the Pauli-X gate on a conformable input matrix.
#'
#' @param n a vector/matrix
#'
#' @usage
#' sigmaY(n)
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Quantum_logic_gate}\cr
#' \url{http://www2.optics.rochester.edu/~stroud/presentations/muthukrishnan991/LogicGates.pdf}\cr
#' \url{http://www.physics.udel.edu/~msafrono/650/Lecture%204%20-%205.pdf}\cr
#'
#' @examples
#' init()
#' sigmaY(I2)
#' sigmaY(Hadamard(I2))
#' sigmaY(Q0)
#'
#' @export
#'
sigmaY <- function(n){
  Y <- matrix(c(0, -1i, 1i, 0), ncol=2, byrow=TRUE)
  return (Y %*% n)
}
