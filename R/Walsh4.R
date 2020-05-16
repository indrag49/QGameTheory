##############################################################################
#                                                                            #
#                             WALSH-HADAMARD OPERATION                       #
#                                                                            #
##############################################################################

#' @title
#' Walsh-Hadamard gate
#'
#' @description
#' This function operates the Walsh-4 gate on a conformable input matrix.
#'
#' @params
#' n
#'
#' @usage
#' Walsh4(n)
#'
#' @keywords
#' Quantum operations, quantum logic gates
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Quantum_logic_gate}\cr
#' \url{http://www2.optics.rochester.edu/~stroud/presentations/muthukrishnan991/LogicGates.pdf}\cr
#' \url{https://arxiv.org/pdf/quant-ph/0506219.pdf}\cr
#' \url{https://en.wikipedia.org/wiki/Hadamard_transform}\cr
#'
#' @examples
#' initialize_()
#' Walsh4(I4)
#' Walsh4(Q10)
#'
#' @export
#'


Walsh4 <- function(n){
  h <- Hadamard(I2)
  w <- kronecker(h, h)
  return (w %*% n)
}
