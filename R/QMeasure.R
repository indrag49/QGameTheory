##############################################################################
#                                                                            #
#                     MEASUREMENT OF QUANTUM STATES                          #
#                                                                            #
##############################################################################

#' @title
#' Measurement
#'
#' @description
#' This function performs a projective measurement of a quantum state \code{n}, in the computational basis and plots the corresponding probability distributions of the qubits.
#'
#' @param n a vector representing a quantum state
#'
#' @usage
#' QMeasure(n)
#'
#' @return No return value, plots the probability distributions of the qubits after performing a projective measurement of a quantum state \code{n}.
#'
#' @references
#' \url{https://books.google.co.in/books?id=66TgFp2YqrAC&pg=PA25&redir_esc=y}\cr
#' \url{https://en.wikipedia.org/wiki/Measurement_in_quantum_mechanics}\cr
#'
#' @examples
#' init()
#' QMeasure(Q$Q10110)
#'
#' @export
#'

QMeasure <- function(n){
  l <- length(n)
  values <- c()
  for (i in 1:l){
    values <- c(values, abs(n[i])**2)
  }
  p <- t(as.data.frame(values))
  if (l==2) colnames(p) <- c("|0>", "|1>")
  else if (l==2**2) colnames(p) <- c("|00>", "|01>", "|10>", "|11>")
  else if (l==2**3) colnames(p) <- c("|000>","|001>","|010>","|011>","|100>","|101>","|110>","|111>")
  else if (l==2**4) colnames(p) <- c("|0000>","|0001>","|0010>","|0011>","|0100>","|0101>","|0110>","|0111>","|1000>","|1001>","|1010>","|1011>","|1100>","|1101>","|1110>","|1111>")
  else if (l==2**5) colnames(p) <- c("|00000>","|00001>","|00010>","|00011>","|00100>","|00101>","|00110>","|00111>","|01000>","|01001>","|01010>","|01011>","|01100>","|01101>","|01110>","|01111>","|10000>","|10001>","|10010>","|10011>","|10100>","|10101>","|10110>","|10111>","|11000>","|11001>","|11010>","|11011>","|11100>","|11101>","|11110>","|11111>")
  else colnames(p) <- c('|000000>', '|000001>', '|000010>', '|000011>', '|000100>', '|000101>', '|000110>', '|000111>', '|001000>', '|001001>', '|001010>', '|001011>', '|001100>', '|001101>', '|001110>', '|001111>', '|010000>', '|010001>', '|010010>', '|010011>', '|010100>', '|010101>', '|010110>', '|010111>', '|011000>', '|011001>', '|011010>', '|011011>', '|011100>', '|011101>', '|011110>', '|011111>', '|100000>', '|100001>', '|100010>', '|100011>', '|100100>', '|100101>', '|100110>', '|100111>', '|101000>', '|101001>', '|101010>', '|101011>', '|101100>', '|101101>', '|101110>', '|101111>', '|110000>', '|110001>', '|110010>', '|110011>', '|110100>', '|110101>', '|110110>', '|110111>', '|111000>', '|111001>', '|111010>', '|111011>', '|111100>', '|111101>', '|111110>', '|111111>')

  coul <- brewer.pal(8, "Set2")
  barplot(t(as.matrix(p)), beside=TRUE, legend.text = colnames(p), xlab='Qubits', ylab='Probabilities', ylim=0:1, main='Probability Distribution', col=coul)
}
