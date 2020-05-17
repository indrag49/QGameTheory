##############################################################################
#                                                                            #
#                       OPERATION OF PHASE GATE                              #
#                                                                            #
##############################################################################

#' @title
#' Phase Gate
#'
#' @description
#' This function operates the Phase gate on a conformable input matrix
#'
#' @param n a vector/matrix
#'
#' @usage
#' Phase(n)
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Quantum_logic_gate}\cr
#' \url{http://www2.optics.rochester.edu/~stroud/presentations/muthukrishnan991/LogicGates.pdf}\cr
#'
#' @examples
#' init()
#' Phase(I2)
#' Phase(Q_plus)
#'
#' @export
#'

Phase <- function(n) {
  P <- matrix(c(1, 0, 0, 1i), ncol=2, byrow=TRUE)
  return (P %*% n)
}
