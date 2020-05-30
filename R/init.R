##############################################################################
#                                                                            #
#                         INITIALIZATION FUNCTION                            #
#                                                                            #
##############################################################################

#' @title
#' Initialization
#'
#' @description
#' Builds the parameters in the global environment after initialization
#'
#' @usage
#' init()
#'
#' @return No return value, generates the global variables/parameters.
#'
#'
#' @references
#' \url{https://arxiv.org/pdf/quant-ph/0512125.pdf}\cr
#' \url{https://arxiv.org/pdf/0910.4222.pdf}\cr
#' \url{https://arxiv.org/pdf/quant-ph/9703032.pdf}\cr
#' \url{https://en.wikipedia.org/wiki/Quantum_computing}\cr
#' \url{https://en.wikipedia.org/wiki/Qubit}\cr
#' \url{https://en.wikipedia.org/wiki/Qutrit}\cr
#' \url{https://en.wikipedia.org/wiki/Clebsch%E2%80%93Gordan_coefficients_for_SU(3)}\cr
#'
#' @examples
#' init()
#' Q110
#' Qt12
#' Q_minus
#' lambda4
#'
#' @import
#' dplyr
#' RColorBrewer
#' R.utils
#'
#' @importFrom
#' graphics
#' barplot
#' persp
#' plot
#'
#' @export
#'

init <- function(){

  Q0 = Q1 = Q00 = Q01 = Q10 = Q11 = Q_plus = Q_minus = Q_clock = Q_anticlock = NULL
  I2 = I4 = I8 = I16 = I32 = I64 = I128 = I256 = u = d = Qt0 = Qt1 = Qt2 = Qt00 = Qt01 = Qt02 = Qt10 = Qt11 = Qt12 = Qt20 = Qt21  = Qt22 = NULL
  lambda1 = lambda2 = lambda3 = lambda4 = lambda5 = lambda6 = lambda7 = lambda8 =  Identity3 = Ihat_3 = Ihat_plus = Ihat_minus = Vhat_plus = Vhat_minus = Uhat_plus = Uhat_minus = Yhat = Hhat = NULL
  Q000=Q001=Q010=Q011=Q100=Q101=Q110=Q111=NULL
  Q0000=Q0001=Q0010=Q0011=Q0100=Q0101=Q0110=Q0111=Q1000=Q1001=Q1010=Q1011=Q1100=Q1101=Q1110=Q1111=NULL
  Q00000=Q00001=Q00010=Q00011=Q00100=Q00101=Q00110=Q00111=Q01000=Q01001=Q01010=NULL
  Q01011=Q01100=Q01101=Q01110=Q01111=Q10000=Q10001=Q10010=Q10011=Q10100=Q10101=Q10110=Q10111=Q11000=Q11001=Q11010=Q11011=Q11100=Q11101=Q11110=NULL
  Q11111=Q000000=Q000001=Q000010=Q000011=Q000100=Q000101=Q000110=Q000111=NULL
  Q001000=Q001001=Q001010=Q001011=Q001100=Q001101=Q001110=Q001111=Q010000=NULL
  Q010001=Q010010=Q010011=Q010100=Q010101=Q010110=Q010111=Q011000=Q011001=NULL
  Q011010=Q011011=Q011100=Q011101=Q011110=Q011111=Q100000=Q100001=Q100010=NULL
  Q100011=Q100100=Q100101=Q100110=Q100111=Q101000=Q101001=Q101010=Q101011=NULL
  Q101100=Q101101=Q101110=Q101111=Q110000=Q110001=Q110010=Q110011=Q110100=NULL
  Q110101=Q110110=Q110111=Q111000=Q111001=Q111010=Q111011=Q111100=Q111101=NULL
  Q111110=Q111111=Dict=Dict3=NULL

  # Initialise the 1-Qubit system : {|0>, |1>}
  Q0 <- matrix(c(1, 0))
  Q0 <<- Q0
  Q1 <- matrix(c(0, 1))
  Q1 <<- Q1

  # Initialise the 2-Qubit system : {|00>, |01>, |10>, |11>}
  Q00 <- kronecker(Q0, Q0)
  Q00 <<- Q00
  Q01 <- kronecker(Q0, Q1)
  Q01 <<- Q01
  Q10 <- kronecker(Q1, Q0)
  Q10 <<- Q10
  Q11 <- kronecker(Q1, Q1)
  Q11 <<- Q11

  # Initialise the multi Qubit systems
  Q000 <- kronecker(Q00, Q0)
  Q000 <<- Q000
  Q001 <- kronecker(Q00, Q1)
  Q001 <<- Q001
  Q010 <- kronecker(Q01, Q0)
  Q010 <<- Q010
  Q011 <- kronecker(Q01, Q1)
  Q011 <<- Q011
  Q100 <- kronecker(Q10, Q0)
  Q100 <<- Q100
  Q101 <- kronecker(Q10, Q1)
  Q101 <<- Q101
  Q110 <- kronecker(Q11, Q0)
  Q110 <<- Q110
  Q111 <- kronecker(Q11, Q1)
  Q111 <<- Q111
  Q0000 <- kronecker(Q000, Q0)
  Q0000 <<- Q0000
  Q0001 <- kronecker(Q000, Q1)
  Q0001 <<- Q0001
  Q0010 <- kronecker(Q001, Q0)
  Q0010 <<- Q0010
  Q0011 <- kronecker(Q001, Q1)
  Q0011 <<- Q0011
  Q0100 <- kronecker(Q010, Q0)
  Q0100 <<- Q0100
  Q0101 <- kronecker(Q010, Q1)
  Q0101 <<- Q0101
  Q0110 <- kronecker(Q011, Q0)
  Q0110 <<- Q0110
  Q0111 <- kronecker(Q011, Q1)
  Q0111 <<- Q0111
  Q1000 <- kronecker(Q100, Q0)
  Q1000 <<- Q1000
  Q1001 <- kronecker(Q100, Q1)
  Q1001 <<- Q1001
  Q1010 <- kronecker(Q101, Q0)
  Q1010 <<- Q1010
  Q1011 <- kronecker(Q101, Q1)
  Q1011 <<- Q1011
  Q1100 <- kronecker(Q110, Q0)
  Q1100 <<- Q1100
  Q1101 <- kronecker(Q110, Q1)
  Q1101 <<- Q1101
  Q1110 <- kronecker(Q111, Q0)
  Q1110 <<- Q1110
  Q1111 <- kronecker(Q111, Q1)
  Q1111 <<- Q1111
  Q00000 <- kronecker(Q0000, Q0)
  Q00000 <<- Q00000
  Q00001 <- kronecker(Q0000, Q1)
  Q00001 <<- Q00001
  Q00010 <- kronecker(Q0001, Q0)
  Q00010 <<- Q00010
  Q00011 <- kronecker(Q0001, Q1)
  Q00011 <<- Q00011
  Q00100 <- kronecker(Q0010, Q0)
  Q00100 <<- Q00100
  Q00101 <- kronecker(Q0010, Q1)
  Q00101 <<- Q00101
  Q00110 <- kronecker(Q0011, Q0)
  Q00110 <<-Q00110
  Q00111 <- kronecker(Q0011, Q1)
  Q00111 <<- Q00111
  Q01000 <- kronecker(Q0100, Q0)
  Q01000 <<- Q01000
  Q01001 <- kronecker(Q0100, Q1)
  Q01001 <<- Q01001
  Q01010 <- kronecker(Q0101, Q0)
  Q01010 <<- Q01010
  Q01011 <- kronecker(Q0101, Q1)
  Q01011 <<- Q01011
  Q01100 <- kronecker(Q0110, Q0)
  Q01100 <<- Q01100
  Q01101 <- kronecker(Q0110, Q1)
  Q01101 <<- Q01101
  Q01110 <- kronecker(Q0111, Q0)
  Q01110 <<- Q01110
  Q01111 <- kronecker(Q0111, Q1)
  Q01111 <<- Q01111
  Q10000 <- kronecker(Q1000, Q0)
  Q10000 <<- Q10000
  Q10001 <- kronecker(Q1000, Q1)
  Q10001 <<- Q10001
  Q10010 <- kronecker(Q1001, Q0)
  Q10010 <<- Q10010
  Q10011 <- kronecker(Q1001, Q1)
  Q10011 <<- Q10011
  Q10100 <- kronecker(Q1010, Q0)
  Q10100 <<- Q10100
  Q10101 <- kronecker(Q1010, Q1)
  Q10101 <<- Q10101
  Q10110 <- kronecker(Q1011, Q0)
  Q10110 <<- Q10110
  Q10111 <- kronecker(Q1011, Q1)
  Q10111 <<- Q10111
  Q11000 <- kronecker(Q1100, Q0)
  Q11000 <<- Q11000
  Q11001 <- kronecker(Q1100, Q1)
  Q11001 <<- Q11001
  Q11010 <- kronecker(Q1101, Q0)
  Q11010 <<- Q11010
  Q11011 <- kronecker(Q1101, Q1)
  Q11011 <<- Q11011
  Q11100 <- kronecker(Q1110, Q0)
  Q11100 <<- Q11100
  Q11101 <- kronecker(Q1110, Q1)
  Q11101 <<- Q11101
  Q11110 <- kronecker(Q1111, Q0)
  Q11110 <<- Q11110
  Q11111 <- kronecker(Q1111, Q1)
  Q11111 <<- Q11111
  Q000000 <- kronecker(Q00000, Q0)
  Q000000 <<- Q000000
  Q000001 <- kronecker(Q00000,Q1)
  Q000001 <<- Q000001
  Q000010 <- kronecker(Q00001, Q0)
  Q000010 <<- Q000010
  Q000011 <- kronecker(Q00001, Q1)
  Q000011 <<- Q000011
  Q000100 <- kronecker(Q00010, Q0)
  Q000100 <<- Q000100
  Q000101 <- kronecker(Q00010, Q1)
  Q000101 <<- Q000101
  Q000110 <- kronecker(Q00011, Q0)
  Q000110 <<- Q000110
  Q000111 <- kronecker(Q00011, Q1)
  Q000111 <<- Q000111
  Q001000 <- kronecker(Q00100, Q0)
  Q001000 <<- Q001000
  Q001001 <- kronecker(Q00100, Q1)
  Q001001 <<- Q001001
  Q001010 <- kronecker(Q00101, Q0)
  Q001010 <<- Q001010
  Q001011 <- kronecker(Q00101, Q1)
  Q001011 <<- Q001011
  Q001100 <- kronecker(Q00110, Q0)
  Q001100 <<- Q001100
  Q001101 <- kronecker(Q00110, Q1)
  Q001101 <<- Q001101
  Q001110 <- kronecker(Q00111, Q0)
  Q001110 <<- Q001110
  Q001111 <- kronecker(Q00111, Q1)
  Q001111 <<- Q001111
  Q010000 <- kronecker(Q01000, Q0)
  Q010000 <<- Q010000
  Q010001 <- kronecker(Q01000, Q1)
  Q010001 <<- Q010001
  Q010010 <- kronecker(Q01001, Q0)
  Q010010 <<- Q010010
  Q010011 <- kronecker(Q01001, Q1)
  Q010011 <<- Q010011
  Q010100 <- kronecker(Q01010, Q0)
  Q010100 <<- Q010100
  Q010101 <- kronecker(Q01010, Q1)
  Q010101 <<- Q010101
  Q010110 <- kronecker(Q01011, Q0)
  Q010110 <<- Q010110
  Q010111 <- kronecker(Q01011, Q1)
  Q010111 <<- Q010111
  Q011000 <- kronecker(Q01100, Q0)
  Q011000 <<- Q011000
  Q011001 <- kronecker(Q01100, Q1)
  Q011001 <<- Q011001
  Q011010 <- kronecker(Q01101, Q0)
  Q011010 <<- Q011010
  Q011011 <- kronecker(Q01101, Q1)
  Q011011 <<- Q011011
  Q011100 <- kronecker(Q01110, Q0)
  Q011100 <<- Q011100
  Q011101 <- kronecker(Q01110, Q1)
  Q011101 <<- Q011101
  Q011110 <- kronecker(Q01111, Q0)
  Q011110 <<- Q011110
  Q011111 <- kronecker(Q01111, Q1)
  Q011111 <<- Q011111
  Q100000 <- kronecker(Q10000, Q0)
  Q100000 <<- Q100000
  Q100001 <- kronecker(Q10000, Q1)
  Q100001 <<- Q100001
  Q100010 <- kronecker(Q10001, Q0)
  Q100010 <<- Q100010
  Q100011 <- kronecker(Q10001, Q0)
  Q100011 <<- Q100011
  Q100100 <- kronecker(Q10010, Q0)
  Q100100 <<- Q100100
  Q100101 <- kronecker(Q10010, Q1)
  Q100101 <<- Q100101
  Q100110 <- kronecker(Q10011, Q0)
  Q100110 <<- Q100110
  Q100111 <- kronecker(Q10011, Q1)
  Q100111 <<- Q100111
  Q101000 <- kronecker(Q10100, Q0)
  Q101000 <<- Q101000
  Q101001 <- kronecker(Q10100, Q1)
  Q101001 <<- Q101001
  Q101010 <- kronecker(Q10101, Q0)
  Q101010 <<- Q101010
  Q101011 <- kronecker(Q10101, Q1)
  Q101011 <<- Q101011
  Q101100 <- kronecker(Q10110, Q0)
  Q101100 <<- Q101100
  Q101101 <- kronecker(Q10110, Q1)
  Q101101 <<- Q101101
  Q101110 <- kronecker(Q10111, Q0)
  Q101110 <<- Q101110
  Q101111 <- kronecker(Q10111, Q1)
  Q101111 <<- Q101111
  Q110000 <- kronecker(Q11000, Q0)
  Q110000 <<- Q110000
  Q110001 <- kronecker(Q11000, Q1)
  Q110001 <<- Q110001
  Q110010 <- kronecker(Q11001, Q0)
  Q110010 <<- Q110010
  Q110011 <- kronecker(Q11001, Q1)
  Q110011 <<- Q110011
  Q110100 <- kronecker(Q11010, Q0)
  Q110100 <<- Q110100
  Q110101 <- kronecker(Q11010, Q1)
  Q110101 <<- Q110101
  Q110110 <- kronecker(Q11011, Q0)
  Q110110 <<- Q110110
  Q110111 <- kronecker(Q11011, Q1)
  Q110111 <<- Q110111
  Q111000 <- kronecker(Q11100, Q0)
  Q111000 <<- Q111000
  Q111001 <- kronecker(Q11100, Q1)
  Q111001 <<- Q111001
  Q111010 <- kronecker(Q11101, Q0)
  Q111010 <<- Q111010
  Q111011 <- kronecker(Q11101, Q1)
  Q111011 <<- Q111011
  Q111100 <- kronecker(Q11110, Q0)
  Q111100 <<- Q111100
  Q111101 <- kronecker(Q11110, Q1)
  Q111101 <<- Q111101
  Q111110 <- kronecker(Q11111, Q0)
  Q111110 <<- Q111110
  Q111111 <- kronecker(Q11111, Q1)
  Q111111 <<- Q111111

  Dict <- list('0'=Q0, '1'=Q1, '00'=Q00, '01'=Q01, '10'=Q10, '11'=Q11, "000"=Q000, "001"=Q001, "010"=Q010, "011"=Q011, "100"=Q100, "101"=Q101, "110"=Q110, "111"=Q111,
               "0000"=Q0000, "0001"=Q0001, "0010"=Q0010, "0011"=Q0011, "0100"=Q0100, "0101"=Q0101, "0110"=Q0110,"0111"=Q0111,"1000"=Q1000,"1001"=Q1001,"1010"=Q1010,"1011"=Q1011,"1100"=Q1100, "1101"=Q1101, "1110"=Q1110,"1111"=Q1111, "00000"=Q00000,
               "00001"=Q00001,"00010"=Q00010,"00011"=Q00011,"00100"=Q00100,"00101"=Q00101,"00110"=Q00110,"00111"=Q00111,"01000"=Q01000,"01001"=Q01001,"01010"=Q01010,
               "01011"=Q01011,"01100"=Q01100,"01101"=Q01101,"01110"=Q01110,"01111"=Q01111,"10000"=Q10000,"10001"=Q10001,"10010"=Q10010,"10011"=Q10011,"10100"=Q10100, "10101"=Q10101, "10110"=Q10110, "10111"=Q10111, "11000"=Q11000,"11001"=Q11001,"11010"=Q11010,"11011"=Q11011,"11100"=Q11100,"11101"=Q11101,"11110"=Q11110,
               "11111"=Q11111,"000000"=Q000000,"000001"=Q000001,"000010"=Q000010,"000011"=Q000011,"000100"=Q000100, "000101"=Q000101,"000110"=Q000110,"000111"=Q000111,
               "001000"=Q001000, "001001"=Q001001,"001010"=Q001010,"001011"=Q001011,"001100"=Q001100,"001101"=Q001101,"001110"=Q001110,"001111"=Q001111,"010000"=Q010000,
               "010001"=Q010001,"010010"=Q010010,"010011"=Q010011,"010100"=Q010100,"010101"=Q010101,"010110"=Q010110,"010111"=Q010111,"011000"=Q011000,"011001"=Q011001,
               "011010"=Q011010,"011011"=Q011011,"011100"=Q011100,"011101"=Q011101,"011110"=Q011110,"011111"=Q011111,"100000"=Q100000,"100001"=Q100001,"100010"=Q100010,
               "100011"=Q100011,"100100"=Q100100,"100101"=Q100101,"100110"=Q100110, "100111"=Q100111,"101000"=Q101000,"101001"=Q101001,"101010"=Q101010,"101011"=Q101011,
               "101100"=Q101100,"101101"=Q101101,"101110"=Q101110,"101111"=Q101111,"110000"=Q110000,"110001"=Q110001,"110010"=Q110010,"110011"=Q110011,"110100"=Q110100,
               "110101"=Q110101,"110110"=Q110110,"110111"=Q110111,"111000"=Q111000,"111001"=Q111001,"111010"=Q111010,"111011"=Q111011,"111100"=Q111100,"111101"=Q111101,"111110"=Q111110,"111111"=Q111111)

  Dict <<- Dict

  # Diagonal Basis
  Q_plus <- matrix(c(1, 1))/sqrt(2)
  Q_plus <<- Q_plus
  Q_minus <- matrix(c(1, -1))/sqrt(2)
  Q_minus <<- Q_minus

  # Circular Basis
  Q_clock <- matrix(c(1, 1i))/sqrt(2)
  Q_clock <<- Q_clock
  Q_anticlock <- matrix(c(1, -1i))/sqrt(2)
  Q_anticlock <<- Q_anticlock

  # Identity matrices
  I2 <- diag(2)
  I2 <<- I2
  I4 <- diag(2**2)
  I4 <<- I4
  I8 <- diag(2**3)
  I8 <<- I8
  I16 <- diag(2**4)
  I16 <<- I16
  I32 <- diag(2**5)
  I32 <<- I32
  I64 <- diag(2**6)
  I64 <<- I64
  I128 <- diag(2**7)
  I128 <<- I128
  I256 <- diag(2**8)
  I256 <<- I256

  # up and down states
  u <- Q0
  u <<- u
  d <- Q1
  d <<- d

  # Qutrit systems
  Qt0 <- matrix(c(1, 0, 0))
  Qt0 <<- Qt0
  Qt1 <- matrix(c(0, 1, 0))
  Qt1 <<- Qt1
  Qt2 <- matrix(c(0, 0, 1))
  Qt2 <<- Qt2
  Qt00 <- kronecker(Qt0, Qt0)
  Qt00 <<- Qt00
  Qt01 <- kronecker(Qt0, Qt1)
  Qt01 <<- Qt01
  Qt02 <- kronecker(Qt0, Qt2)
  Qt02 <<- Qt02
  Qt10 <- kronecker(Qt1, Qt0)
  Qt10 <<- Qt10
  Qt11 <- kronecker(Qt1, Qt1)
  Qt11 <<- Qt11
  Qt12 <- kronecker(Qt1, Qt2)
  Qt12 <<- Qt12
  Qt20 <- kronecker(Qt2, Qt0)
  Qt20 <<- Qt20
  Qt21 <- kronecker(Qt2, Qt1)
  Qt21 <<- Qt21
  Qt22 <- kronecker(Qt2, Qt2)
  Qt22 <<- Qt22

  Dict3 <- list(Qt0, Qt1, Qt2)
  Dict3 <<- Dict3

  # SU(3) matrices

  # Gel Mann matrices
  lambda1 <- matrix(c(0, 1, 0, 1, 0, 0, 0, 0, 0), ncol=3, byrow=TRUE)
  lambda1 <<- lambda1
  lambda2 <- matrix(c(0, -1i, 0, 1i, 0, 0, 0, 0, 0), ncol=3, byrow=TRUE)
  lambda2 <<- lambda2
  lambda3 <- matrix(c(1, 0, 0, 0, -1, 0, 0, 0, 0), ncol=3, byrow=TRUE)
  lambda3 <<- lambda3
  lambda4 <- matrix(c(0, 0, 1, 0, 0, 0, 1, 0, 0), ncol=3, byrow=TRUE)
  lambda4 <<- lambda4
  lambda5 <- matrix(c(0, 0, -1i, 0, 0, 0, 1i, 0, 0), ncol=3, byrow=TRUE)
  lambda5 <<- lambda5
  lambda6 <- matrix(c(0, 0, 0, 0, 0, 1, 0, 1, 0), ncol=3, byrow=TRUE)
  lambda6 <<- lambda6
  lambda7 <- matrix(c(0, 0, 0, 0, 0, -1i, 0, 1i, 0), ncol=3, byrow=TRUE)
  lambda7 <<- lambda7
  lambda8 <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, -2), ncol=3, byrow=TRUE)/sqrt(3)
  lambda8 <<- lambda8

  # Identity operator
  Identity3 <- diag(3)
  Identity3 <<- Identity3

  # Cartan-Weyl basis of the Lie Algebra of SU(3)
  Ihat_plus <- (lambda1 + 1i*lambda2)/2
  Ihat_plus <<- Ihat_plus
  Ihat_minus <- (lambda1 - 1i*lambda2)/2
  Ihat_minus <<- Ihat_minus
  Ihat_3 <- lambda3/2
  Ihat_3 <<- Ihat_3
  Vhat_plus <- (lambda4 + 1i*lambda5)/2
  Vhat_plus <<- Vhat_plus
  Vhat_minus <- (lambda4 - 1i*lambda5)/2
  Vhat_minus <<- Vhat_minus
  Uhat_plus <- (lambda6 + 1i*lambda7)/2
  Uhat_plus <<- Uhat_plus
  Uhat_minus <- (lambda6 - 1i*lambda7)/2
  Uhat_minus <<- Uhat_minus
  Yhat <- lambda8/sqrt(3)
  Yhat <<- Yhat

  # miscellaneous
  Hhat <- matrix(c(1/sqrt(2), 0.5, 0.5, -0.5, (3 - 1i*sqrt(7))/(4*sqrt(2)), (1+1i*sqrt(7))/(4*sqrt(2)), -(1+1i*sqrt(7))/(4*sqrt(2)), (-3 + 1i*sqrt(7))/8, (5+1i*sqrt(7))/8), ncol=3, byrow = TRUE)
  Hhat <<- Hhat
  }
