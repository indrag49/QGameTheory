##############################################################################
#                                                                            #
#                       OPERATION OF FREDKIN GATE                            #
#                                                                            #
##############################################################################

#' @title
#' Fredkin Gate
#'
#' @description
#' This function operates the Fredkin gate on a conformable input matrix/vector
#'
#' @param n A vector/matrix
#'
#' @usage
#' Fredkin(n)
#'
#' @return A matrix or a vector after performing the Fredkin gate operation on a conformable input matrix or a vector.
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Quantum_logic_gate}\cr
#' \url{http://www2.optics.rochester.edu/~stroud/presentations/muthukrishnan991/LogicGates.pdf}\cr
#'
#' @examples
#' init()
#' Fredkin(Q$I8)
#' Fredkin(Q$Q110)
#'
#' @export
#'

Fredkin <- function(n){
  x <- Q$I8
  t <- x[6,]
  x[6,] <- x[7,]
  x[7,] <- t
  return (x %*% n)
}
