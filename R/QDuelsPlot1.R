##############################################################################
#                                                                            #
#                     QUANTUM TWO PERSON DUEL GAME                           #
#                                                                            #
##############################################################################

#' @title
#' Quantum Two Person Duel game
#'
#' @description
#' This function helps us to plot Alice's and Bob's expected payoffs as functions of \code{alpha1} and \code{alpha2}. \code{Psi} is the initial state of the quantum game, \code{n} is the number of rounds, \code{a} is the probability of Alice missing the target, \code{b} is the probability of Bob missing the target, and
#' \code{alpha1, alpha2, beta1, beta2} are arbitrary phase factors that lie in -pi to pi that control the outcome of a poorly performing player.
#'
#' @param Psi a vector representing the initial quantum state
#' @param n an integer
#' @param a a number
#' @param b a number
#' @param beta1 a number
#' @param beta2 a number
#'
#' @usage
#' QDuelsPlot1(Psi, n, a, b, beta1, beta2)
#'
#' @return No return value, plots Alice's and Bob's expected payoffs as functions of \code{alpha1} and \code{alpha2}.
#'
#' @references
#' \url{https://arxiv.org/pdf/quant-ph/0506219.pdf}\cr
#' \url{https://arxiv.org/pdf/quant-ph/0208069.pdf}\cr
#' \url{https://arxiv.org/pdf/quant-ph/0305058.pdf}\cr
#'
#'
#' @examples
#' init()
#' QDuelsPlot1(Q$Q10, 2, 0.66666, 0.5, 0.2, 0.8)
#'
#' @export
#'

QDuelsPlot1 <- function(Psi, n, a, b, beta1, beta2){
  Psi <- as.vector(Psi)
  x <- y <- seq(-pi, pi, length=20)
  z1 <- z2 <- matrix(data=NA, nrow=length(x), ncol=length(y))
  for (i in 1:length(x)){
    for (j in 1:length(y)){
      z1[i, j] <- QDuels_Alice_payoffs(Psi, n, a, b, x[[i]], y[[j]], beta1, beta2)[[1]]
      z2[i, j] <- QDuels_Bob_payoffs(Psi, n, a, b, x[[i]], y[[j]], beta1, beta2)[[1]]
    }
  }
  persp(x, y, z1, xlab="alpha_1", ylab="alpha_2", zlab="<pi_Alice>", theta=20, phi=50, r=2, shade=0.4, axes=TRUE, scale=TRUE, box=TRUE, nticks=5, ticktype = "detailed", col="cyan")
  persp(x, y, z2, xlab="alpha_1", ylab="alpha_2", zlab="<pi_Bob>", theta=20, phi=50, r=2, shade=0.4, axes=TRUE, scale=TRUE, box=TRUE, nticks=5, ticktype = "detailed", col="red")
}
