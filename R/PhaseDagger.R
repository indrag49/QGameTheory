##############################################################################
#                                                                            #
#          OPERATION OF THE HERMITIAN TRANSPOSE OF THE PHASE GATE            #
#                                                                            #
##############################################################################

#' @title
#' Hermitian Transpose of the Phase Gate
#'
#' @description
#' This function operates the hermitian transpose of the Phase gate on a conformable input matrix/vector
#'
#' @param n a vector/matrix
#'
#' @usage
#' PhaseDagger(n)
#'
#' @return A matrix or a vector after performing the operation of the hermitian transpose of the Phase gate on a conformable input matrix or a vector.
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Quantum_logic_gate}\cr
#' \url{http://www2.optics.rochester.edu/~stroud/presentations/muthukrishnan991/LogicGates.pdf}\cr
#'
#' @examples
#' init()
#' Conj(t(Phase(Q$I2)))==PhaseDagger(Q$I2)
#' PhaseDagger(Q$Q_plus)
#'
#' @export
#'


PhaseDagger <- function(n){
  P <- matrix(c(1, 0, 0, -1i), ncol=2, byrow=TRUE)
  return (P %*% n)
}
