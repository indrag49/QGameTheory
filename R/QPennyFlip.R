##############################################################################
#                                                                            #
#                      QUANTUM PENNY FLIP GAME                               #
#                                                                            #
##############################################################################

#' @title
#' Quantum Penny Flip game
#'
#' @description
#' This function simulates the Quantum Penny Flip game by taking in the initial state of the game that is set by Alice and the strategies available to Alice and Bob. It returns the final state of the game along with the plot of the probability distribution of the qubits after measurement of the final state.
#'
#' @param initial_state a vector representing the initial quantum state
#' @param strategies_Alice a matrix lying in SU(2)
#' @param strategies_Bob a matrix lying in SU(2)
#'
#' @usage
#' QPennyFlip(initial_state, strategies_Alice, strategies_Bob)
#'
#' @return The final state of the game along with the plot of the probability distribution of the qubits after measurement of the final state by taking in the initial state of the game that is set by Alice and the strategies available to Alice and Bob as the inputs.
#'
#' @references
#' \url{https://arxiv.org/pdf/quant-ph/0506219.pdf}\cr
#' \url{https://arxiv.org/pdf/quant-ph/0208069.pdf}\cr
#' \url{https://arxiv.org/pdf/quant-ph/9804010.pdf}\cr
#'
#'
#' @examples
#' init()
#' psi <- (u+d)/sqrt(2)
#' S1 <- sigmaX(Q$I2)
#' S2 <- Q$I2
#' H <- Hadamard(Q$I2)
#' SA <- list(S1, S2)
#' SB <- list(H)
#' QPennyFlip(psi, SA,SB)
#'
#' @export
#'

QPennyFlip <- function(initial_state, strategies_Alice, strategies_Bob){

  l_Alice <- length(strategies_Alice)
  l_Bob <- length(strategies_Bob)

  # Alice prepares the initial state
  Alice <- initial_state
  state <- Alice

  # Bob plays his move:
  B <- sample(1:l_Bob, 1, replace=TRUE)
  Bob <- strategies_Bob[[B]]
  state <- Bob %*% state

  # Alice plays again:
  A <- sample(1:l_Alice, 1, replace=TRUE)
  Alice <- strategies_Alice[[A]]
  state <- Alice %*% state

  # Last move by Bob:
  B <- sample(1:l_Bob, 1, replace=TRUE)
  Bob <- strategies_Bob[[B]]
  state <- Bob %*% state

  QMeasure(state)
  return(state)
}
