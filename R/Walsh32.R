##############################################################################
#                                                                            #
#                             WALSH-HADAMARD OPERATION                       #
#                                                                            #
##############################################################################

#' @title
#' Walsh-Hadamard gate
#'
#' @description
#' This function operates the Walsh-32 gate on a conformable input matrix or a vector.
#'
#' @param n a vector/matrix
#'
#' @usage
#' Walsh32(n)
#'
#' @return A matrix or a vector after performing the Walsh-32 gate operation on a conformable input matrix or a vector.
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Quantum_logic_gate}\cr
#' \url{http://www2.optics.rochester.edu/~stroud/presentations/muthukrishnan991/LogicGates.pdf}\cr
#' \url{https://arxiv.org/pdf/quant-ph/0506219.pdf}\cr
#' \url{https://en.wikipedia.org/wiki/Hadamard_transform}\cr
#'
#' @examples
#' init()
#' Walsh32(Q$I32)
#' Walsh32(Q$Q10011)
#'
#' @export
#'

Walsh32 <- function(n){
  h <- Hadamard(Q$I2)
  w <- kronecker(kronecker(kronecker(kronecker(h, h), h), h), h)
  return (w %*% n)
}
