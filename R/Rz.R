##############################################################################
#                                                                            #
#                             ROTATION GATE                                  #
#                                                                            #
##############################################################################

#' @title
#' Rotation operation about z-axis of the Bloch sphere
#'
#' @description
#' This function operates the Rotation gate about the z-axis of the Bloch sphere by an angle \code{theta} on a conformable input matrix \code{n}.
#'
#' @param n a vector/matrix
#' @param theta an angle
#'
#' @usage
#' Rz(n, theta)
#'
#' @return A vector or a matrix after operating the Rotation gate about the z-axis of the Bloch sphere, by an angle \code{theta}, on a conformable input matrix or a vector \code{n}.
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Quantum_logic_gate}\cr
#' \url{http://www2.optics.rochester.edu/~stroud/presentations/muthukrishnan991/LogicGates.pdf}\cr
#' \url{http://www.physics.udel.edu/~msafrono/650/Lecture%204%20-%205.pdf}\cr
#'
#' @examples
#' init()
#' Rz(Q$Q1, pi)
#'
#' @export
#'

Rz <- function(n, theta){
  r <- matrix(c(exp(-1i*theta/2), 0, 0, exp(1i*theta/2)), ncol=2, byrow=TRUE)
  return (r %*% n)
}
