##############################################################################
#                                                                            #
#                             SWAP GATE                                      #
#                                                                            #
##############################################################################

#' @title
#' SWAP gate
#'
#' @description
#' This function operates the SWAP gate on a conformable input matrix or a vector.
#'
#' @param n a vector/matrix
#'
#' @usage
#' SWAP(n)
#'
#' @return A matrix or a vector after performing the SWAP gate operation on a conformable input matrix or a vector.
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Quantum_logic_gate}\cr
#' \url{http://www2.optics.rochester.edu/~stroud/presentations/muthukrishnan991/LogicGates.pdf}\cr
#'
#' @examples
#' init()
#' SWAP(I4)
#' SWAP(Q10)
#'
#' @export
#'


SWAP <- function(n) {
  x <- I4
  t <- x[2,]
  x[2,] <- x[3,]
  x[3,] <- t
  return (x %*% n)
}
