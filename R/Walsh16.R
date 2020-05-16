##############################################################################
#                                                                            #
#                             WALSH-HADAMARD OPERATION                       #
#                                                                            #
##############################################################################

#' @title
#' Walsh-Hadamard gate
#'
#' @description
#' This function operates the Walsh-16 gate on a conformable input matrix.
#'
#' @params
#' n
#'
#' @usage
#' Walsh16(n)
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
#' Walsh16(I16)
#' Walsh16(Q1001)
#'
#' @export
#'

Walsh16 <- function(n){
  h <- Hadamard(I2)
  w <- kronecker(kronecker(kronecker(h, h), h), h)
  return (w %*% n)
}
