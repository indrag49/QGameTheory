##############################################################################
#                                                                            #
#                             PAULI MATRIX                                   #
#                                                                            #
##############################################################################

#' @title
#' Pauli-Y gate
#'
#' @description
#' This function operates the Pauli-Y gate on a conformable input matrix or a vector.
#'
#' @param n a vector/matrix
#'
#' @usage
#' sigmaY(n)
#'
#' @return A matrix or a vector after performing the Pauli-Y gate operation on a conformable input matrix or a vector.
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Quantum_logic_gate}\cr
#' \url{http://www2.optics.rochester.edu/~stroud/presentations/muthukrishnan991/LogicGates.pdf}\cr
#' \url{http://www.physics.udel.edu/~msafrono/650/Lecture%204%20-%205.pdf}\cr
#'
#' @examples
#' init()
#' sigmaY(Q$I2)
#' sigmaY(Hadamard(Q$I2))
#' sigmaY(Q$Q0)
#'
#' @export
#'
sigmaY <- function(n){
  Y <- matrix(c(0, -1i, 1i, 0), ncol=2, byrow=TRUE)
  return (Y %*% n)
}
