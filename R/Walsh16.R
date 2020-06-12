##############################################################################
#                                                                            #
#                             WALSH-HADAMARD OPERATION                       #
#                                                                            #
##############################################################################

#' @title
#' Walsh-Hadamard gate
#'
#' @description
#' This function operates the Walsh-16 gate on a conformable input matrix or a vector.
#'
#' @param n a vector/matrix
#'
#' @usage
#' Walsh16(n)
#'
#' @return A matrix or a vector after performing the Walsh-16 gate operation on a conformable input matrix or a vector.
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Quantum_logic_gate}\cr
#' \url{http://www2.optics.rochester.edu/~stroud/presentations/muthukrishnan991/LogicGates.pdf}\cr
#' \url{https://arxiv.org/pdf/quant-ph/0506219.pdf}\cr
#' \url{https://en.wikipedia.org/wiki/Hadamard_transform}\cr
#'
#' @examples
#' init()
#' Walsh16(Q$I16)
#' Walsh16(Q$Q1001)
#'
#' @export
#'

Walsh16 <- function(n){
  h <- Hadamard(Q$I2)
  w <- kronecker(kronecker(kronecker(h, h), h), h)
  return (w %*% n)
}
