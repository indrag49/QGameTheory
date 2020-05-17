##############################################################################
#                                                                            #
#                         OPERATION OF CNOT GATE                             #
#                                                                            #
##############################################################################

#' @title
#' CNOT gate
#'
#' @description
#' This function operates the CNOT gate on a conformable input matrix
#'
#' @param n A vector/matrix
#'
#' @usage
#' CNOT(n)
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Quantum_logic_gate}\cr
#' \url{http://www2.optics.rochester.edu/~stroud/presentations/muthukrishnan991/LogicGates.pdf}\cr
#'
#' @examples
#' init()
#' CNOT(I4)
#' CNOT(Q11)
#'
#' @export
#'

CNOT <- function(n){
  x <- I4
  t <- x[3,]
  x[3,] <- x[4,]
  x[4,] <- t
  return (x %*% n)
}
