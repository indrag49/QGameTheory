##############################################################################
#                                                                            #
#                             ROTATION GATE                                  #
#                                                                            #
##############################################################################

#' @title
#' Rotation operation about z-axis of the Bloch sphere
#'
#' @description
#' This function operates the Rotation gate about the z-axis of the Bloch sphere by an angle 'theta' on a conformable input matrix 'n'.
#'
#' @param n a vector/matrix
#' @param theta an angle
#'
#' @usage
#' Rz(n, theta)
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Quantum_logic_gate}\cr
#' \url{http://www2.optics.rochester.edu/~stroud/presentations/muthukrishnan991/LogicGates.pdf}\cr
#' \url{http://www.physics.udel.edu/~msafrono/650/Lecture%204%20-%205.pdf}\cr
#'
#' @examples
#' init()
#' Rz(Q1, pi)
#'
#' @export
#'

Rz <- function(n, theta){
  return (exp(-1i*theta*sigmaZ(I2)/2))
}
