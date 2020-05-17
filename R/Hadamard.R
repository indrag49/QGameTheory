##############################################################################
#                                                                            #
#                       OPERATION OF HADAMARD GATE                           #
#                                                                            #
##############################################################################

#' @title
#' Hadamard Gate
#'
#' @description
#' This function operates the Hadamard gate on a conformable input matrix
#'
#' @param n A vector/matrix
#'
#' @usage
#' Hadamard(n)
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Quantum_logic_gate}\cr
#' \url{http://www2.optics.rochester.edu/~stroud/presentations/muthukrishnan991/LogicGates.pdf}\cr
#'
#' @examples
#' init()
#' Hadamard(Q0)
#' Hadamard(I2)
#' Hadamard(Hadamard(Q1))
#'
#' @export
#'

Hadamard <- function(n){
  H <- matrix(c(1, 1, 1, -1), ncol=2, byrow=TRUE)/sqrt(2)
  return (H %*% n)
}
