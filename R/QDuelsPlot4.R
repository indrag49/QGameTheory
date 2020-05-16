##############################################################################
#                                                                            #
#                     QUANTUM TWO PERSON DUEL GAME                           #
#                                                                            #
##############################################################################

#' @title
#' Quantum Two Person Duel game
#'
#' @description
#' This function helps us to plot the improvement in Bob's expected payoff as a function of 'a' and 'b', if Bob chooses to fire at the air in her second shot, in a two round game. Psi is the initial state of the quantum game, 'n' is the number of rounds, 'a' is the probability of Alice missing the target, 'b' is the probability of Bob missing the target, and
#' {'alpha1', 'alpha2', 'beta1', 'beta2'} are arbitrary phase factors that lie in -pi to pi that control the outcome of a poorly performing player.
#'
#' @params
#' Psi, alpha1, alpha2
#'
#' @usage
#' QDuelsPlot4(Psi, alpha1, alpha2)
#'
#' @keywords
#' Quantum Game Theory, Two Person Duel
#'
#' @references
#' \url{https://arxiv.org/pdf/quant-ph/0506219.pdf}\cr
#' \url{https://arxiv.org/pdf/quant-ph/0208069.pdf}\cr
#' \url{https://arxiv.org/pdf/quant-ph/0305058.pdf}\cr
#'
#'
#' @examples
#' initialize_()
#' Qs <- (Q0+Q1)/sqrt(2)
#' Psi <- kronecker(Q1, Qs)
#' QDuelsPlot4(Psi, pi/3, pi/6)
#'
#' @export
#'

QDuelsPlot4 <- function(Psi, alpha1, alpha2){
  a <- seq(0, 1, length=20)
  b <- seq(0, 1, length=20)
  z <- matrix(data=NA, nrow=length(a), ncol=length(b))
  for(i in 1:length(a)){
    for (j in 1:length(b)){
      d <- QDuels_Alice_payoffs(Psi, 2, a[[i]], b[[j]], alpha1, alpha2, 0, 0)
      z[i, j] <- d[[3]] - (1-d[[1]])
    }
  }
  persp(a, b, z, xlab="a", ylab="b", zlab="<pi_Bob_diff>", theta=20, phi=50, r=2, shade=0.4, axes=TRUE, scale=TRUE, box=TRUE, nticks=5, ticktype = "detailed", col="green")
}
