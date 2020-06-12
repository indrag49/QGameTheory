##############################################################################
#                                                                            #
#                                T GATE                                      #
#                                                                            #
##############################################################################

#' @title
#' T gate
#'
#' @description
#' This function operates the T gate on a conformable input matrix or a vector.
#'
#' @param n a vector/matrix
#'
#' @usage
#' T(n)
#'
#' @return A matrix or a vector after performing the T gate operation on a conformable input matrix or a vector.
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Quantum_logic_gate}\cr
#' \url{http://www2.optics.rochester.edu/~stroud/presentations/muthukrishnan991/LogicGates.pdf}\cr
#'
#' @examples
#' init()
#' T(Q$I2)
#' T(Q$Q_minus)
#'
#' @export
#'


T <- function(n){
  t <- matrix(c(1, 0, 0, exp(1i*pi/4)), ncol=2, byrow=TRUE)
  return (t %*% n)
}
