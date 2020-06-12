##############################################################################
#                                                                            #
#          QUANTUM BATTLE OF THE SEXES GAME                                  #
#                                                                            #
##############################################################################

#' @title
#' Quantum Battle of the Sexes game
#'
#' @description
#' This function returns the expected payoffs to Alice and Bob with respect to the probabilities \code{p} and \code{q}. \code{p+q} should equal 1 and \code{moves} is a list of two possible strategies for each of the players and \code{alpha, beta, gamma} are the payoffs for the players corresponding to the choices available to them with the chain of inequalities, \code{alpha>beta>gamma}.
#'
#' @param p a real number between 0 and 1 including the end points
#' @param q a real number between 0 and 1 including the end points
#' @param moves alist of matrices
#' @param alpha a number
#' @param beta a number
#' @param gamma a number
#'
#' @usage
#' QBOS(p, q, moves, alpha, beta, gamma)
#'
#' @return A vector consisting of the Payoffs to Alice and Bob as its two elements depending on the inputs.
#'
#' @references
#' \url{https://arxiv.org/pdf/quant-ph/0506219.pdf}\cr
#' \url{https://arxiv.org/pdf/quant-ph/0208069.pdf}\cr
#' \url{https://arxiv.org/abs/quant-ph/0110096}\cr
#'
#'
#' @examples
#' init()
#' moves <- list(Q$I2, sigmaX(Q$I2))
#' QBOS(0, 1, moves, 5, 3, 1)
#' QBOS(1, 1, moves, 5, 3, 1)
#' QBOS(0.5, 0.5, moves, 5, 3, 1)
#'
#' @export
#'

QBOS <- function(p, q, moves, alpha, beta, gamma){
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
  PA <- alpha*kronecker(Q$Q00, t(Conj(Q$Q00)))+beta*kronecker(Q$Q11, t(Conj(Q$Q11)))+gamma*(kronecker(Q$Q01, t(Conj(Q$Q01)))+kronecker(Q$Q10, t(Conj(Q$Q10))))
  PB <- beta*kronecker(Q$Q00, t(Conj(Q$Q00)))+alpha*kronecker(Q$Q11, t(Conj(Q$Q11)))+gamma*(kronecker(Q$Q01, t(Conj(Q$Q01)))+kronecker(Q$Q10, t(Conj(Q$Q10))))
  payoff_A <- sum(diag(PA %*% density_matrix))
  payoff_B <- sum(diag(PB %*% density_matrix))
  return (c(payoff_A, payoff_B))
}
