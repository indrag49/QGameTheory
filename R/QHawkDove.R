##############################################################################
#                                                                            #
#                        QUANTUM HAWK AND DOVE GAME                          #
#                                                                            #
##############################################################################

#' @title
#' Quantum Hawk and Dove game
#'
#' @description
#' This function returns the expected payoffs to Alice and Bob with respect to the probabilities \code{p} and \code{q}. \code{p+q} should equal 1 and \code{moves} is a list of two possible strategies for each of the players and \code{v, j, D} are the value of resource, cost of injury and cost of displaying respectively.
#'
#' @param p a real number between 0 and 1 including the end points
#' @param q a real number between 0 and 1 including the end points
#' @param moves a list of matrics
#' @param v a number
#' @param j a number
#' @param D a number
#'
#' @usage
#' QHawkDove(p, q, moves, v, j, D)
#'
#' @return A vector consisting of the expected payoffs to Alice and Bob as its elements calculated according to the probabilities \code{p} and \code{q} provided as inputs.
#'
#' @references
#' \url{https://arxiv.org/pdf/quant-ph/0506219.pdf}\cr
#' \url{https://arxiv.org/pdf/quant-ph/0208069.pdf}\cr
#' \url{https://arxiv.org/pdf/quant-ph/0108075.pdf}\cr
#'
#'
#' @examples
#' init()
#' moves <- list(Q$I2, sigmaX(Q$I2))
#' QHawkDove(0, 1, moves, 50, -100, -10)
#' QHawkDove(0, 0, moves, 50, -100, -10)
#'
#' @export
#'

QHawkDove <- function(p, q, moves, v, j, D){
  a <- b <- d <- sqrt(5/16)
  c <- 1/4
  Psi_in <- a*Q$Q00 + b*Q$Q01 + c*Q$Q10 + d*Q$Q11
  density_in <- kronecker(Psi_in, t(Conj(Psi_in)))
  m1 <- moves[[1]]
  m2 <- moves[[2]]
  d1 <- p*q*kronecker(m1, m1) %*% density_in %*% kronecker(t(Conj(m1)), t(Conj(m1)))
  d2 <- p*(1-q)*kronecker(m1, m2) %*% density_in %*% kronecker(t(Conj(m1)), t(Conj(m2)))
  d3 <- q*(1-p)*kronecker(m2, m1) %*% density_in %*% kronecker(t(Conj(m2)), t(Conj(m1)))
  d4 <- (1-p)*(1-q)*kronecker(m2, m2) %*% density_in %*% kronecker(t(Conj(m2)), t(Conj(m2)))
  density_matrix <- d1+d2+d3+d4
  PA <- (v+j)/2*kronecker(Q$Q00, t(Conj(Q$Q00)))+(v/2+D)*kronecker(Q$Q11, t(Conj(Q$Q11)))+v*kronecker(Q$Q01, t(Conj(Q$Q01)))
  PB <- (v+j)/2*kronecker(Q$Q00, t(Conj(Q$Q00)))+(v/2+D)*kronecker(Q$Q11, t(Conj(Q$Q11)))+v*kronecker(Q$Q10, t(Conj(Q$Q10)))
  payoff_A <- sum(diag(PA %*% density_matrix))
  payoff_B <- sum(diag(PB %*% density_matrix))
  return (c(payoff_A, payoff_B))
}
