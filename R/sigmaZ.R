##############################################################################
#                                                                            #
#                             PAULI MATRIX                                   #
#                                                                            #
##############################################################################

#' @title
#' Pauli-Z gate
#'
#' @description
#' This function operates the Pauli-Z gate on a conformable input matrix.
#'
#' @param n a vector/matrix
#'
#' @usage
#' sigmaZ(n)
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Quantum_logic_gate}\cr
#' \url{http://www2.optics.rochester.edu/~stroud/presentations/muthukrishnan991/LogicGates.pdf}\cr
#' \url{http://www.physics.udel.edu/~msafrono/650/Lecture%204%20-%205.pdf}\cr
#'
#' @examples
#' init()
#' sigmaZ(I2)
#' sigmaZ(Hadamard(I2))
#' sigmaZ(Q0)
#'
#' @export
#'
sigmaZ <- function(n){
  Z <- matrix(c(1, 0, 0, -1), ncol=2, byrow=TRUE)
  return (Z %*% n)
}
