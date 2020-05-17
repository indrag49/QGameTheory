##############################################################################
#                                                                            #
#                             WALSH-HADAMARD OPERATION                       #
#                                                                            #
##############################################################################

#' @title
#' Walsh-Hadamard gate
#'
#' @description
#' This function operates the Walsh-8 gate on a conformable input matrix.
#'
#' @param n a vector/matrix
#'
#' @usage
#' Walsh8(n)
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Quantum_logic_gate}\cr
#' \url{http://www2.optics.rochester.edu/~stroud/presentations/muthukrishnan991/LogicGates.pdf}\cr
#' \url{https://arxiv.org/pdf/quant-ph/0506219.pdf}\cr
#' \url{https://en.wikipedia.org/wiki/Hadamard_transform}\cr
#'
#' @examples
#' init()
#' Walsh8(I8)
#' Walsh8(Q000)
#'
#' @export
#'

Walsh8 <- function(n){
  h <- Hadamard(I2)
  w <- kronecker(kronecker(h, h), h)
  return (w %*% n)
}
