##############################################################################
#                                                                            #
#          PAYOFF MATRIX FOR THE QUANTUM BATTLE OF THE SEXES GAME            #
#                                                                            #
##############################################################################

#' @title
#' Quantum Battle of the Sexes game: Payoff Matrix
#'
#' @description
#' This function generates the payoff matrix for the Quantum Battle of Sexes game for all the four combinations of \code{p} and \code{q}. \code{moves} is a list of two possible strategies for each of the players and \code{alpha, beta, gamma} are the payoffs for the players corresponding to the choices available to them with the chain of inequalities, \code{alpha>beta>gamma}.
#'
#' @param moves a list of matrices
#' @param alpha a number
#' @param beta a number
#' @param gamma a number
#'
#' @usage
#' PayoffMatrix_QBOS(moves, alpha, beta, gamma)
#'
#' @return The payoff matrices for the two players as two elements of a list.
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
#' PayoffMatrix_QBOS(moves, 5, 3, 1)
#'
#' @export
#'

PayoffMatrix_QBOS <- function(moves, alpha, beta, gamma){
  Alice <- matrix(0, 2, 2)
  Bob <- matrix(0, 2, 2)
  for(i in 1:2){
    for (j in 1:2){
      X <- QBOS(i-1, j-1, moves, alpha, beta, gamma)
      Alice[i, j] <- X[[1]]
      Bob[i, j] <- X[[2]]
    }
  }
  return(list(Alice, Bob))
}
