##############################################################################
#                                                                            #
#                       OPERATION OF PHASE GATE                              #
#                                                                            #
##############################################################################

#' @title
#' Phase Gate
#'
#' @description
#' This function operates the Phase gate on a conformable input matrix/vector
#'
#' @param n a vector/matrix
#'
#' @usage
#' Phase(n)
#'
#' @return A matrix or a vector after performing the Phase gate operation on a conformable input matrix or a vector.
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Quantum_logic_gate}\cr
#' \url{http://www2.optics.rochester.edu/~stroud/presentations/muthukrishnan991/LogicGates.pdf}\cr
#'
#' @examples
#' init()
#' Phase(Q$I2)
#' Phase(Q$Q_plus)
#'
#' @export
#'

Phase <- function(n) {
  P <- matrix(c(1, 0, 0, 1i), ncol=2, byrow=TRUE)
  return (P %*% n)
}
