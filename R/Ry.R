##############################################################################
#                                                                            #
#                             ROTATION GATE                                  #
#                                                                            #
##############################################################################

#' @title
#' Rotation operation about y-axis of the Bloch sphere
#'
#' @description
#' This function operates the Rotation gate about the y-axis of the Bloch sphere by an angle \code{theta} on a conformable input matrix \code{n}.
#'
#' @param n a vector/matrix
#' @param theta an angle
#'
#' @usage
#' Ry(n, theta)
#'
#' @return A vector or a matrix after operating the Rotation gate about the y-axis of the Bloch sphere, by an angle \code{theta}, on a conformable input matrix or a vector \code{n}.
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Quantum_logic_gate}\cr
#' \url{http://www2.optics.rochester.edu/~stroud/presentations/muthukrishnan991/LogicGates.pdf}\cr
#' \url{http://www.physics.udel.edu/~msafrono/650/Lecture%204%20-%205.pdf}\cr
#'
#' @examples
#' init()
#' Ry(Q$Q1, pi/3)
#'
#' @export
#'
Ry <- function(n, theta){
  r <- matrix(c(cos(theta/2), -sin(theta/2), sin(theta/2), cos(theta/2)), ncol=2, byrow=TRUE)
  return (r %*% n)
}
