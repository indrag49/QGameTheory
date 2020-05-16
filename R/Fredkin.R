##############################################################################
#                                                                            #
#                       OPERATION OF FREDKIN GATE                            #
#                                                                            #
##############################################################################

#' @title
#' Fredkin Gate
#'
#' @description
#' This function operates the Fredkin gate on a conformable input matrix
#'
#' @params
#' n
#'
#' @usage
#' Fredkin(n)
#'
#' @keywords
#' Quantum operations, quantum logic gates
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Quantum_logic_gate}\cr
#' \url{http://www2.optics.rochester.edu/~stroud/presentations/muthukrishnan991/LogicGates.pdf}\cr
#'
#' @examples
#' initialize_()
#' Fredkin(I8)
#' Fredkin(Q110)
#'
#' @export
#'

Fredkin <- function(n){
  x <- I8
  t <- x[6,]
  x[6,] <- x[7,]
  x[7,] <- t
  return (x %*% n)
}
