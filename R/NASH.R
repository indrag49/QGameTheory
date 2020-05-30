##############################################################################
#                                                                            #
#                            NASH EQUILIBRIUM                                #
#                                                                            #
##############################################################################

#' @title
#' Nash Equilibrium
#'
#' @description
#' This function finds out the Nash equilibria of the 2-D payoff matrix for the players. The input parameters are equal dimensional payoff matrices for the first and the second players.
#'
#' @param P1 Payoff matrix to Alice
#' @param P2 Payoff matrix to Bob
#'
#' @usage
#' NASH(P1, P2)
#'
#' @return The cell positons of the Nash equilibrium/equilibria as a dataframe from the payoff matrices of the players.
#'
#' @references
#' \url{https://arxiv.org/abs/1512.06808}\cr
#' \url{https://en.wikipedia.org/wiki/Nash_equilibrium}\cr
#'
#' @examples
#' init()
#' Alice <- matrix(c(4, 3, 2, 4, 4, 2, 1, 0, 3, 5, 3, 5, 2, 3, 1, 3), ncol=4, byrow=TRUE)
#' Bob <- matrix(c(0, 2, 3, 8, 2, 1, 2, 2, 6, 5, 1, 0, 3, 2, 2, 3), ncol=4, byrow=TRUE)
#' NASH(Alice, Bob)
#'
#' @export
#'


NASH <- function(P1, P2){
  L1a <- c()
  L2a <- c()
  L1b <- c()
  L2b <- c()
  p1 <- dim(P1)[[2]]
  p2 <- dim(P1)[[1]]
  p3 <- dim(P2)[[1]]
  p4 <- dim(P2)[[2]]

  for (i in 1:p1){
    a <- P1[,i]
    M <- max(a)
    for (j in 1:p2){
      if (a[[j]]==M){
        L1a <- c(L1a, j)
        L1b <- c(L1b, i)
      }
    }
  }

  for (i in 1:p3){
    a <- P2[i, ]
    M <- max(a)
    for (j in 1:p4){
      if (a[[j]]==M){
        L2a <- c(L2a, i)
        L2b <- c(L2b, j)
      }
    }
  }
 L1 <- matrix(c(L1a, L1b), ncol=length(L1a), byrow=TRUE)
 L1 <- as.data.frame(t(L1))
 L2 <- matrix(c(L2a, L2b), ncol=length(L2a), byrow=TRUE)
 L2 <- as.data.frame(t(L2))
 return(inner_join(L1, L2))
}
