##############################################################################
#                                                                            #
#           PAYOFF MATRIX FOR THE QUANTUM PRISONER'S DILEMMA GAME            #
#                                                                            #
##############################################################################

#' @title
#' Quantum Prisoner's Dilemma game: Payoff Matrix
#'
#' @description
#' This function generates the payoff matrix for the Quantum Prisoner's Dilemma game . \code{moves} is a list of the possible strategies for each of the players and \code{w, x, y, z} are the payoffs for the players corresponding to the choices available to them with the chain of inequalities, \code{z>w>x>y}.
#' This function also plots the probability distribution plots of the qubits for all the possible combinations of the strategies of the players.
#'
#' @param moves a list of matrices
#' @param w a number
#' @param x a number
#' @param y a number
#' @param z a number
#'
#' @usage
#' PayoffMatrix_QPD(moves, w, x, y, z)
#'
#' @return The payoff matrices for the two players as two elements of a list.
#'
#' @references
#' \url{https://arxiv.org/pdf/quant-ph/0506219.pdf}\cr
#' \url{https://arxiv.org/pdf/quant-ph/0208069.pdf}\cr
#' \url{https://arxiv.org/pdf/quant-ph/0004076.pdf}\cr
#'
#' @examples
#' init()
#' moves <- list(Q$I2, sigmaX(Q$I2), Hadamard(Q$I2), sigmaZ(Q$I2))
#' PayoffMatrix_QPD(moves, 3, 1, 0, 5)
#'
#' @export
#'


PayoffMatrix_QPD <- function(moves, w, x, y, z){
  n <- length(moves)
  Alice <- matrix(0, n, n)
  Bob <- matrix(0, n, n)
  for(i in 1:n){
    for (j in 1:n){
      X <- QPD(moves[[i]], moves[[j]], w, x, y, z)
      Alice[i, j] <- X[[1]]
      Bob[i, j] <- X[[2]]
    }
  }
  return(list(Alice, Bob))
}
