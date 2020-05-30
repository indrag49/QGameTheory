##############################################################################
#                                                                            #
#                       IDSDS ALGORITHM                                      #
#                                                                            #
##############################################################################

#' @title
#' Iterated Deletion of Strictly Dominated Strategies algorithm
#'
#' @description
#' This function applies the IDSDS algorithm to result in the equilibrium strategies based on the rationaility of the players. The input parameters are equal dimensional payoff matrices for the first and the second players.
#'
#' @param P1 Payoff matrix to Alice
#' @param P2 Payoff matrix to Bob
#'
#' @usage
#' IDSDS(P1, P2)
#'
#' @return A list consisting of the equilibrium strategies based on the rationality of the players by application of the IDSDS algorithm on \code{P1} and \code{P2}.
#'
#' @references
#' \url{https://arxiv.org/abs/1512.06808}\cr
#' \url{https://en.wikipedia.org/wiki/Strategic_dominance}\cr
#'
#' @examples
#' init()
#' Alice <- matrix(c(8, 0, 3, 3, 2, 4, 2, 1, 3), ncol=3, byrow=TRUE)
#' Bob <- matrix(c(6, 9, 8, 2, 1, 3, 8, 5, 1), ncol=3, byrow=TRUE)
#' IDSDS(Alice, Bob)
#'
#' @export
#'

IDSDS <- function(P1, P2){
  check1 <- 0
  check2 <- 0
  while(TRUE){
    for(i in 1:row_count(P1)){
      for(j in 1:row_count(P1)){
        if (is.null(dim(P1))) P1 <- t(P1)
        if (length(which(c(P1[i, ]<P1[j, ]))==TRUE) == col_count(P1)){
          P1 <- P1[-i, ]
          P2 <- P2[-i, ]
          check1 <- 1
          break
        }
      }
      if(check1==1) break
    }
    if(check1==0){
      for(i in 1:col_count(P2)){
        for(j in 1:col_count(P2)){
          if (is.null(dim(P2))) P2 <- t(P2)
          if (length(which(c(P2[,i]<P2[,j]))==TRUE) == row_count(P2)){
            P2 <- P2[, -i]
            P1 <- P1[, -i]
            check2 <- 1
            break
          }
        }
        if (check2==1) break
      }
    }
    if(check1==0 & check2==0) break
    check1 <- 0
    check2 <- 0
  }

  return (list(P1, P2))
}
