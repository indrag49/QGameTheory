##############################################################################
#                                                                            #
#                         OPERATION OF CNOT GATE                             #
#                                                                            #
##############################################################################

#' @title
#' CNOT gate
#'
#' @description
#' This function operates the CNOT gate on a conformable input matrix/vector
#'
#' @param n A vector/matrix
#'
#' @usage
#' CNOT(n)
#'
#' @return A matrix or a vector after performing the CNOT gate operation on a conformable input matrix or a vector.
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Quantum_logic_gate}\cr
#' \url{http://www2.optics.rochester.edu/~stroud/presentations/muthukrishnan991/LogicGates.pdf}\cr
#'
#' @examples
#' init()
#' CNOT(Q$I4)
#' CNOT(Q$Q11)
#'
#' @export
#'

CNOT <- function(n){
  x <- Q$I4
  t <- x[3,]
  x[3,] <- x[4,]
  x[4,] <- t
  return (x %*% n)
}
