##############################################################################
#                                                                            #
#                       QUANTUM MONTY HALL PROBLEM                           #
#                                                                            #
##############################################################################

#' @title
#' Quantum Monty Hall Problem
#'
#' @description
#' This function simulates the quantum version of the Monty Hall problem, by taking in 'Psi_in' as the initial quantum state of the game, 'gamma' lying in [0, pi/2], Ahat and Bhat as the choice operators in SU(3) for Alice and Bob respectively as the inputs. It returns the expected payoffs to Alice and Bob after the end of the game.
#'
#' @params
#' Psi_in, gamma, Ahat, Bhat
#'
#' @usage
#' QNewcomb(Psi_in, gamma, Ahat, Bhat)
#'
#' @keywords
#' Quantum Game Theory, Monty Hall Problem
#'
#' @references
#' \url{https://arxiv.org/pdf/quant-ph/0506219.pdf}\cr
#' \url{https://arxiv.org/pdf/quant-ph/0208069.pdf}\cr
#' \url{https://arxiv.org/pdf/quant-ph/0109035.pdf}\cr
#'
#' @examples
#' initialize_()
#' Psi_in <- kronecker(Qt0, (Qt00+Qt11+Qt22)/sqrt(3))
#' QMontyHall(Psi_in, pi/4, Identity3, Hhat)
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
          a <- as.vector(t(as.vector(t(Dict3[[n+1]] %o% Dict3[[j+1]])) %o% Dict3[[k+1]]))
          b <- t(Conj(as.vector(t(as.vector(t(Dict3[[l+1]] %o% Dict3[[j+1]])) %o% Dict3[[k+1]]))))
          O1 <- O1 + abs(levi_civita(i, j, k))*kronecker(a, b)
        }
      }
    }
  }

  for (j in 0:2){
    for (l in 0:2){
      m <- (j+l+1)%%3
      a <- as.vector(t(as.vector(t(Dict3[[m+1]] %o% Dict3[[j+1]])) %o% Dict3[[j+1]]))
      b <- t(Conj(as.vector(t(as.vector(t(Dict3[[l+1]] %o% Dict3[[j+1]])) %o% Dict3[[j+1]]))))
      O2 <- O2 + kronecker(a, b)
    }
  }

  Ohat <- O1+O2

  S1 <- S2 <-0

  for (i in 0:2){
    for(k in 0:2){
      for (l in 0:2){
        for(l in 0:2){
          a <- as.vector(t(as.vector(t(Dict3[[i+1]] %o% Dict3[[l+1]])) %o% Dict3[[k+1]]))
          b <- t(Conj(as.vector(t(as.vector(t(Dict3[[i+1]] %o% Dict3[[j+1]])) %o% Dict3[[k+1]]))))
          S1 <- S1 + abs(levi_civita(i, j, l))*kronecker(a, b)
        }
      }
    }
  }

  for(i in 0:2){
    for(j in 0:2){
      a <- as.vector(t(as.vector(t(Dict3[[i+1]] %o% Dict3[[i+1]])) %o% Dict3[[j+1]]))
      b <- t(Conj(a))
      S2 <- S2 + kronecker(a, b)
    }
  }
  Shat <- S1+S2

  Nhat <- diag(3**3)
  Psi1 <- kronecker(Identity3, kronecker(Bhat, Ahat)) %*% Psi_in
  Psi2 <- Ohat %*% Psi1
  Psif <- (Shat*cos(gamma) + Nhat*sin(gamma)) %*% Psi2

  Bob_expected_payoff <- 0
  for(i in 0:2){
    for(j in 0:2){
      a <- t(Conj(as.vector(t(as.vector(t(Dict3[[i+1]] %o% Dict3[[j+1]])) %o% Dict3[[j+1]]))))
      Bob_expected_payoff <- Bob_expected_payoff + abs(a %*% Psif)**2
    }
  }
  Alice_expected_payoff <- 1 - Bob_expected_payoff
  return(c(Alice_expected_payoff, Bob_expected_payoff))
}
