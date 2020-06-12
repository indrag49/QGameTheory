##############################################################################
#                                                                            #
#                     QUANTUM FOURIER TRANSFORM                              #
#                                                                            #
##############################################################################

#' @title
#' Quantum Fourier Transform
#'
#' @description
#' This function performs Quantum Fourier Transform for a given state \code{|y>} from the computational basis to the Fourier basis.
#'
#' @param y an integer
#'
#' @usage
#' QFT(y)
#'
#' @return A vector representing the Quantum Fourier transformation of the state \code{|y>} from the computational basis to the Fourier basis.
#'
#' @references
#' \url{https://arxiv.org/pdf/quant-ph/0506219.pdf}\cr
#' \url{https://books.google.co.in/books?id=66TgFp2YqrAC&pg=PA25&redir_esc=y}\cr
#' \url{https://en.wikipedia.org/wiki/Quantum_Fourier_transform}\cr
#'
#'
#' @examples
#' init()
#' QFT(5)
#'
#' @export
#'

QFT <- function(y){
  Y <- intToBin(y)
  n <- nchar(Y)
  s <- 0
  for (x in 0:(2**n-1)){
    s <- s+exp(2*pi*1i*x*y/(2**n))*Q$Dict[[paste(strrep('0', n-nchar(intToBin(x))), intToBin(x), sep="")]]
  }
  s <- s/sqrt(2**n)
  return (s)
}
