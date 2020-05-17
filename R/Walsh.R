##############################################################################
#                                                                            #
#                             WALSH-HADAMARD OPERATION                       #
#                                                                            #
##############################################################################

#' @title
#' Walsh-Hadamard gate
#'
#' @description
#' This function operates the Walsh-Hadamard gate on a conformable input matrix.
#'
#' @param n a vector/matrix
#'
#' @usage
#' Walsh(n)
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Quantum_logic_gate}\cr
#' \url{http://www2.optics.rochester.edu/~stroud/presentations/muthukrishnan991/LogicGates.pdf}\cr
#' \url{https://arxiv.org/pdf/quant-ph/0506219.pdf}\cr
#' \url{https://en.wikipedia.org/wiki/Hadamard_transform}\cr
#'
#' @examples
#' init()
#' Walsh(I2)
#' Walsh(Q0)
#'
#' @export
#'


Walsh <- function(n){
  return (Hadamard(n))
}
