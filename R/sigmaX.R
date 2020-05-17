##############################################################################
#                                                                            #
#                             PAULI MATRIX                                   #
#                                                                            #
##############################################################################

#' @title
#' Pauli-X gate
#'
#' @description
#' This function operates the Pauli-X gate on a conformable input matrix.
#'
#' @param n a vector/matrix
#'
#' @usage
#' sigmaX(n)
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Quantum_logic_gate}\cr
#' \url{http://www2.optics.rochester.edu/~stroud/presentations/muthukrishnan991/LogicGates.pdf}\cr
#' \url{http://www.physics.udel.edu/~msafrono/650/Lecture%204%20-%205.pdf}\cr
#'
#' @examples
#' init()
#' sigmaX(I2)
#' sigmaX(Hadamard(I2))
#' sigmaX(Q1)
#'
#' @export
#'

sigmaX <- function(n){
  X <- matrix(c(0, 1, 1, 0), ncol=2, byrow=TRUE)
  return(X %*% n)
}
