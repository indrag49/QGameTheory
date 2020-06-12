##############################################################################
#                                                                            #
#                       QUANTUM MONTY HALL PROBLEM                           #
#                                                                            #
##############################################################################

#' @title
#' Quantum Monty Hall Problem
#'
#' @description
#' This function simulates the quantum version of the Monty Hall problem, by taking in \code{Psi_in} as the initial quantum state of the game, \code{gamma} lying in 0 to pi/2, \code{Ahat} and \code{Bhat} as the choice operators in SU(3) for Alice and Bob respectively as the inputs. It returns the expected payoffs to Alice and Bob after the end of the game.
#'
#' @param Psi_in a vector representing the initial quantum state
#' @param gamma a number between 0 and pi/2 including the end points
#' @param Ahat a matrix lying in SU(3)
#' @param Bhat a matrix lying in SU(3)
#'
#' @usage
#' QMontyHall(Psi_in, gamma, Ahat, Bhat)
#'
#' @return A vector consisting of the expected payoffs to Alice and Bob as its elements depending on the input parameters.
#'
#' @references
#' \url{https://arxiv.org/pdf/quant-ph/0506219.pdf}\cr
#' \url{https://arxiv.org/pdf/quant-ph/0208069.pdf}\cr
#' \url{https://arxiv.org/pdf/quant-ph/0109035.pdf}\cr
#'
#' @examples
#' init()
#' Psi_in <- kronecker(Q$Qt0, (Q$Qt00+Q$Qt11+Q$Qt22)/sqrt(3))
#' QMontyHall(Psi_in, pi/4, Q$Identity3, Q$Hhat)
#'
#' @export
#'


QMontyHall <- function(Psi_in, gamma, Ahat, Bhat){
  O1 <- 0
  O2 <- 0

  for (i in 0:2){
    for (j in 0:2){
      for (k in 0:2){
        for(l in 0:2){
          n <- (i + l) %% 3
          a <- kronecker(kronecker(Q$Dict3[[n+1]], Q$Dict3[[j+1]]), Q$Dict3[[k+1]])
          b <- t(Conj(kronecker(kronecker(Q$Dict3[[l+1]], Q$Dict3[[j+1]]), Q$Dict3[[k+1]])))
          O1 <- O1 + abs(levi_civita(i, j, k))*kronecker(a, b)
        }
      }
    }
  }

  for (j in 0:2){
    for (l in 0:2){
      m <- (j+l+1)%%3
      a <- kronecker(kronecker(Q$Dict3[[m+1]], Q$Dict3[[j+1]]), Q$Dict3[[j+1]])
      b <- t(Conj(kronecker(kronecker(Q$Dict3[[l+1]], Q$Dict3[[j+1]]), Q$Dict3[[j+1]])))
      O2 <- O2 + kronecker(a, b)
    }
  }

  Ohat <- O1+O2

  S1 <- S2 <-0

  for (i in 0:2){
    for(k in 0:2){
      for (l in 0:2){
        for(l in 0:2){
          a <- kronecker(kronecker(Q$Dict3[[i+1]], Q$Dict3[[l+1]]), Q$Dict3[[k+1]])
          b <- t(Conj(kronecker(kronecker(Q$Dict3[[i+1]], Q$Dict3[[j+1]]), Q$Dict3[[k+1]])))
          S1 <- S1 + abs(levi_civita(i, j, l))*kronecker(a, b)
        }
      }
    }
  }

  for(i in 0:2){
    for(j in 0:2){
      a <- kronecker(kronecker(Q$Dict3[[i+1]], Q$Dict3[[i+1]]), Q$Dict3[[j+1]])
      b <- t(Conj(a))
      S2 <- S2 + kronecker(a, b)
    }
  }
  Shat <- S1+S2

  Nhat <- diag(3**3)
  Psi1 <- kronecker(Q$Identity3, kronecker(Bhat, Ahat)) %*% Psi_in
  Psi2 <- Ohat %*% Psi1
  Psif <- (Shat*cos(gamma) + Nhat*sin(gamma)) %*% Psi2

  Bob_expected_payoff <- 0
  for(i in 0:2){
    for(j in 0:2){
      a <- t(Conj(kronecker(kronecker(Q$Dict3[[i+1]], Q$Dict3[[j+1]]), Q$Dict3[[j+1]])))
      Bob_expected_payoff <- Bob_expected_payoff + abs(a %*% Psif)**2
    }
  }
  Alice_expected_payoff <- 1 - Bob_expected_payoff
  return(c(Alice_expected_payoff, Bob_expected_payoff))
}
