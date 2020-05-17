##############################################################################
#                                                                            #
#                         TOFFOLI (CCNOT) GATE                               #
#                                                                            #
##############################################################################

#' @title
#' Toffoli gate
#'
#' @description
#' This function operates the Toffoli gate on a conformable input matrix.
#'
#' @param n a vector/matrix
#'
#' @usage
#' Toffoli(n)
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Quantum_logic_gate}\cr
#' \url{http://www2.optics.rochester.edu/~stroud/presentations/muthukrishnan991/LogicGates.pdf}\cr
#'
#' @examples
#' init()
#' Toffoli(I8)
#' Toffoli(Q010)
#'
#' @export
#'


Toffoli <- function(n) {
  x <- I8
  t <- x[7,]
  x[7,] <- x[8,]
  x[8,] <- t
  return (x %*% n)
}
