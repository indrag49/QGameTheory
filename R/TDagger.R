##############################################################################
#                                                                            #
#                       HERMITIAN TRANSPOSE OF THE T GATE                    #
#                                                                            #
##############################################################################

#' @title
#' Hermitian Transpose of the T gate
#'
#' @description
#' This function operates the hermitian transpose of the T gate on a conformable input matrix.
#'
#' @param n a vector/matrix
#'
#' @usage
#' TDagger(n)
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Quantum_logic_gate}\cr
#' \url{http://www2.optics.rochester.edu/~stroud/presentations/muthukrishnan991/LogicGates.pdf}\cr
#'
#' @examples
#' init()
#' TDagger(I2)
#' TDagger(Q_plus)
#'
#' @export
#'


TDagger <- function(n){
  t <- matrix(c(1, 0, 0, exp(-1i*pi/4)), ncol=2, byrow=TRUE)
  return (t %*% n)
}
