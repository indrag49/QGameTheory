##############################################################################
#                                                                            #
#                       OPERATION OF ISING GATE                              #
#                                                                            #
##############################################################################

#' @title
#' Ising Gate
#'
#' @description
#' This function operates the Ising gate on a conformable input matrix. A phase (phi) is also given as another input.
#'
#' @params
#' n, phi
#'
#' @usage
#' Ising(n, phi)
#'
#' @keywords
#' Quantum operations, quantum logic gates
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Quantum_logic_gate}\cr
#'
#' @examples
#' initialize_()
#' Ising(I4, pi/4)
#' Ising(Q11, pi/6)
#'
#' @export
#'

Hadamard <- function(n){
  H <- matrix(c(1, 1, 1, -1), ncol=2, byrow=TRUE)/sqrt(2)
  return (H %*% n)
}


Ising  <- function(n, phi){
  I <- matrix(c(1, 0, 0, exp(1i*(phi-pi/2)), 0, 1, -1i, 0, 0, -1i, 1, 0, exp(1i*(-phi-pi/2)), 0, 0, 1), ncol=4, byrow=TRUE)
  return (I %*% n)
}
