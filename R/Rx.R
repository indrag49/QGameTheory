##############################################################################
#                                                                            #
#                             ROTATION GATE                                  #
#                                                                            #
##############################################################################

#' @title
#' Rotation operation about x-axis of the Bloch sphere
#'
#' @description
#' This function operates the Rotation gate about the x-axis of the Bloch sphere by an angle 'theta' on a conformable input matrix 'n'.
#'
#' @params
#' n, theta
#'
#' @usage
#' Rx(n, theta)
#'
#' @keywords
#' Quantum operations, quantum logic gates
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Quantum_logic_gate}\cr
#' \url{http://www2.optics.rochester.edu/~stroud/presentations/muthukrishnan991/LogicGates.pdf}\cr
#' \url{http://www.physics.udel.edu/~msafrono/650/Lecture%204%20-%205.pdf}\cr
#'
#' @examples
#' initialize_()
#' Rx(Q0, pi/6)
#'
#' @export
#'

Rx <- function(n, theta){
  return (exp(-1i*theta*sigmaX(I2)/2) %*% n)
}
