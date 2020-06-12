##############################################################################
#                                                                            #
#          PAYOFF MATRIX FOR THE QUANTUM HAWK AND DOVE GAME                  #
#                                                                            #
##############################################################################

#' @title
#' Quantum Hawk and Dove game: Payoff Matrix
#'
#' @description
#' This function generates the payoff matrix for the Quantum Hawk and Dove game for all the four combinations of \code{p} and \code{q}. \code{moves} is a list of two possible strategies for each of the players and \code{v, j, D} are the value of resource, cost of injury and cost of displaying respectively.
#'
#' @param moves a list of matrices
#' @param v a number
#' @param j a number
#' @param D a number
#'
#' @usage
#' PayoffMatrix_QHawkDove(moves, v, j, D)
#'
#' @return The payoff matrices for the two players as two elements of a list.
#'
#' @references
#' \url{https://arxiv.org/pdf/quant-ph/0506219.pdf}\cr
#' \url{https://arxiv.org/pdf/quant-ph/0208069.pdf}\cr
#' \url{https://arxiv.org/pdf/quant-ph/0108075.pdf}\cr
#'
#' @examples
#' init()
#' moves <- list(Q$I2, sigmaX(Q$I2))
#' PayoffMatrix_QHawkDove(moves, 50, -100, -10)
#'
#' @export
#'

PayoffMatrix_QHawkDove <- function(moves, v, j, D){
  Alice <- matrix(0, 2, 2)
  Bob <- matrix(0, 2, 2)
  for(i in 1:2){
    for (j in 1:2){
      X <- QHawkDove(i-1, j-1, moves, v, j, D)
      Alice[i, j] <- X[[1]]
      Bob[i, j] <- X[[2]]
    }
  }
  return(list(Alice, Bob))
}
