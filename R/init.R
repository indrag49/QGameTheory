##############################################################################
#                                                                            #
#                         INITIALIZATION FUNCTION                            #
#                                                                            #
##############################################################################

#' @title
#' Initialization
#'
#' @description
#' Builds the parameters in the required environment after initialization
#'
#' @usage
#' init()
#'
#' @return No return value, generates the required variables/parameters.
#'
#'
#' @references
#' \url{https://arxiv.org/pdf/Quant-ph/0512125.pdf}\cr
#' \url{https://arxiv.org/pdf/0910.4222.pdf}\cr
#' \url{https://arxiv.org/pdf/Quant-ph/9703032.pdf}\cr
#' \url{https://en.wikipedia.org/wiki/Quantum_computing}\cr
#' \url{https://en.wikipedia.org/wiki/Qubit}\cr
#' \url{https://en.wikipedia.org/wiki/Qutrit}\cr
#' \url{https://en.wikipedia.org/wiki/Clebsch%E2%80%93Gordan_coefficients_for_SU(3)}\cr
#'
#' @examples
#' init()
#' Q$Q110
#' Q$Qt12
#' Q$Q_minus
#' Q$lambda4
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
  Q <<- new.env(parent=emptyenv())
  
  Q$Q0 = Q$Q1 = Q$Q00 = Q$Q01 = Q$Q10 = Q$Q11 = Q$Q_plus = Q$Q_minus = Q$Q_clock = Q$Q_anticlock = NULL
  Q$I2 = Q$I4 = Q$I8 = Q$I16 = Q$I32 = Q$I64 = Q$I128 = Q$I256 = u = d = Q$Qt0 = Q$Qt1 = Q$Qt2 = Q$Qt00 = Q$Qt01 = Q$Qt02 = Q$Qt10 = Q$Qt11 = Q$Qt12 = Q$Qt20 = Q$Qt21  = Q$Qt22 = NULL
  Q$lambda1 = Q$lambda2 = Q$lambda3 = Q$lambda4 = Q$lambda5 = Q$lambda6 = Q$lambda7 = Q$lambda8 =  Q$Identity3 = Q$Ihat_3 = Q$Ihat_plus = Q$Ihat_minus = Q$Vhat_plus = Q$Vhat_minus = Q$Uhat_plus = Q$Uhat_minus = Q$Yhat = Q$Hhat = NULL
  Q$Q000=Q$Q001=Q$Q010=Q$Q011=Q$Q100=Q$Q101=Q$Q110=Q$Q111=NULL
  Q$Q0000=Q$Q0001=Q$Q0010=Q$Q0011=Q$Q0100=Q$Q0101=Q$Q0110=Q$Q0111=Q$Q1000=Q$Q1001=Q$Q1010=Q$Q1011=Q$Q1100=Q$Q1101=Q$Q1110=Q$Q1111=NULL
  Q$Q00000=Q$Q00001=Q$Q00010=Q$Q00011=Q$Q00100=Q$Q00101=Q$Q00110=Q$Q00111=Q$Q01000=Q$Q01001=Q$Q01010=NULL
  Q$Q01011=Q$Q01100=Q$Q01101=Q$Q01110=Q$Q01111=Q$Q10000=Q$Q10001=Q$Q10010=Q$Q10011=Q$Q10100=Q$Q10101=Q$Q10110=Q$Q10111=Q$Q11000=Q$Q11001=Q$Q11010=Q$Q11011=Q$Q11100=Q$Q11101=Q$Q11110=NULL
  Q$Q11111=Q$Q000000=Q$Q000001=Q$Q000010=Q$Q000011=Q$Q000100=Q$Q000101=Q$Q000110=Q$Q000111=NULL
  Q$Q001000=Q$Q001001=Q$Q001010=Q$Q001011=Q$Q001100=Q$Q001101=Q$Q001110=Q$Q001111=Q$Q010000=NULL
  Q$Q010001=Q$Q010010=Q$Q010011=Q$Q010100=Q$Q010101=Q$Q010110=Q$Q010111=Q$Q011000=Q$Q011001=NULL
  Q$Q011010=Q$Q011011=Q$Q011100=Q$Q011101=Q$Q011110=Q$Q011111=Q$Q100000=Q$Q100001=Q$Q100010=NULL
  Q$Q100011=Q$Q100100=Q$Q100101=Q$Q100110=Q$Q100111=Q$Q101000=Q$Q101001=Q$Q101010=Q$Q101011=NULL
  Q$Q101100=Q$Q101101=Q$Q101110=Q$Q101111=Q$Q110000=Q$Q110001=Q$Q110010=Q$Q110011=Q$Q110100=NULL
  Q$Q110101=Q$Q110110=Q$Q110111=Q$Q111000=Q$Q111001=Q$Q111010=Q$Q111011=Q$Q111100=Q$Q111101=NULL
  Q$Q111110=Q$Q111111=Dict=Dict3=NULL
  
  # Initialise the 1-Qubit system : {|0>, |1>}
  Q$Q0 <- matrix(c(1, 0))
  Q$Q0 <<- Q$Q0
  Q$Q1 <- matrix(c(0, 1))
  Q$Q1 <<- Q$Q1
  
  # Initialise the 2-Qubit system : {|00>, |01>, |10>, |11>}
  Q$Q00 <- kronecker(Q$Q0, Q$Q0)
  Q$Q00 <<- Q$Q00
  Q$Q01 <- kronecker(Q$Q0, Q$Q1)
  Q$Q01 <<- Q$Q01
  Q$Q10 <- kronecker(Q$Q1, Q$Q0)
  Q$Q10 <<- Q$Q10
  Q$Q11 <- kronecker(Q$Q1, Q$Q1)
  Q$Q11 <<- Q$Q11
  
  # Initialise the multi Qubit systems
  Q$Q000 <- kronecker(Q$Q00, Q$Q0)
  Q$Q000 <<- Q$Q000
  Q$Q001 <- kronecker(Q$Q00, Q$Q1)
  Q$Q001 <<- Q$Q001
  Q$Q010 <- kronecker(Q$Q01, Q$Q0)
  Q$Q010 <<- Q$Q010
  Q$Q011 <- kronecker(Q$Q01, Q$Q1)
  Q$Q011 <<- Q$Q011
  Q$Q100 <- kronecker(Q$Q10, Q$Q0)
  Q$Q100 <<- Q$Q100
  Q$Q101 <- kronecker(Q$Q10, Q$Q1)
  Q$Q101 <<- Q$Q101
  Q$Q110 <- kronecker(Q$Q11, Q$Q0)
  Q$Q110 <<- Q$Q110
  Q$Q111 <- kronecker(Q$Q11, Q$Q1)
  Q$Q111 <<- Q$Q111
  Q$Q0000 <- kronecker(Q$Q000, Q$Q0)
  Q$Q0000 <<- Q$Q0000
  Q$Q0001 <- kronecker(Q$Q000, Q$Q1)
  Q$Q0001 <<- Q$Q0001
  Q$Q0010 <- kronecker(Q$Q001, Q$Q0)
  Q$Q0010 <<- Q$Q0010
  Q$Q0011 <- kronecker(Q$Q001, Q$Q1)
  Q$Q0011 <<- Q$Q0011
  Q$Q0100 <- kronecker(Q$Q010, Q$Q0)
  Q$Q0100 <<- Q$Q0100
  Q$Q0101 <- kronecker(Q$Q010, Q$Q1)
  Q$Q0101 <<- Q$Q0101
  Q$Q0110 <- kronecker(Q$Q011, Q$Q0)
  Q$Q0110 <<- Q$Q0110
  Q$Q0111 <- kronecker(Q$Q011, Q$Q1)
  Q$Q0111 <<- Q$Q0111
  Q$Q1000 <- kronecker(Q$Q100, Q$Q0)
  Q$Q1000 <<- Q$Q1000
  Q$Q1001 <- kronecker(Q$Q100, Q$Q1)
  Q$Q1001 <<- Q$Q1001
  Q$Q1010 <- kronecker(Q$Q101, Q$Q0)
  Q$Q1010 <<- Q$Q1010
  Q$Q1011 <- kronecker(Q$Q101, Q$Q1)
  Q$Q1011 <<- Q$Q1011
  Q$Q1100 <- kronecker(Q$Q110, Q$Q0)
  Q$Q1100 <<- Q$Q1100
  Q$Q1101 <- kronecker(Q$Q110, Q$Q1)
  Q$Q1101 <<- Q$Q1101
  Q$Q1110 <- kronecker(Q$Q111, Q$Q0)
  Q$Q1110 <<- Q$Q1110
  Q$Q1111 <- kronecker(Q$Q111, Q$Q1)
  Q$Q1111 <<- Q$Q1111
  Q$Q00000 <- kronecker(Q$Q0000, Q$Q0)
  Q$Q00000 <<- Q$Q00000
  Q$Q00001 <- kronecker(Q$Q0000, Q$Q1)
  Q$Q00001 <<- Q$Q00001
  Q$Q00010 <- kronecker(Q$Q0001, Q$Q0)
  Q$Q00010 <<- Q$Q00010
  Q$Q00011 <- kronecker(Q$Q0001, Q$Q1)
  Q$Q00011 <<- Q$Q00011
  Q$Q00100 <- kronecker(Q$Q0010, Q$Q0)
  Q$Q00100 <<- Q$Q00100
  Q$Q00101 <- kronecker(Q$Q0010, Q$Q1)
  Q$Q00101 <<- Q$Q00101
  Q$Q00110 <- kronecker(Q$Q0011, Q$Q0)
  Q$Q00110 <<-Q$Q00110
  Q$Q00111 <- kronecker(Q$Q0011, Q$Q1)
  Q$Q00111 <<- Q$Q00111
  Q$Q01000 <- kronecker(Q$Q0100, Q$Q0)
  Q$Q01000 <<- Q$Q01000
  Q$Q01001 <- kronecker(Q$Q0100, Q$Q1)
  Q$Q01001 <<- Q$Q01001
  Q$Q01010 <- kronecker(Q$Q0101, Q$Q0)
  Q$Q01010 <<- Q$Q01010
  Q$Q01011 <- kronecker(Q$Q0101, Q$Q1)
  Q$Q01011 <<- Q$Q01011
  Q$Q01100 <- kronecker(Q$Q0110, Q$Q0)
  Q$Q01100 <<- Q$Q01100
  Q$Q01101 <- kronecker(Q$Q0110, Q$Q1)
  Q$Q01101 <<- Q$Q01101
  Q$Q01110 <- kronecker(Q$Q0111, Q$Q0)
  Q$Q01110 <<- Q$Q01110
  Q$Q01111 <- kronecker(Q$Q0111, Q$Q1)
  Q$Q01111 <<- Q$Q01111
  Q$Q10000 <- kronecker(Q$Q1000, Q$Q0)
  Q$Q10000 <<- Q$Q10000
  Q$Q10001 <- kronecker(Q$Q1000, Q$Q1)
  Q$Q10001 <<- Q$Q10001
  Q$Q10010 <- kronecker(Q$Q1001, Q$Q0)
  Q$Q10010 <<- Q$Q10010
  Q$Q10011 <- kronecker(Q$Q1001, Q$Q1)
  Q$Q10011 <<- Q$Q10011
  Q$Q10100 <- kronecker(Q$Q1010, Q$Q0)
  Q$Q10100 <<- Q$Q10100
  Q$Q10101 <- kronecker(Q$Q1010, Q$Q1)
  Q$Q10101 <<- Q$Q10101
  Q$Q10110 <- kronecker(Q$Q1011, Q$Q0)
  Q$Q10110 <<- Q$Q10110
  Q$Q10111 <- kronecker(Q$Q1011, Q$Q1)
  Q$Q10111 <<- Q$Q10111
  Q$Q11000 <- kronecker(Q$Q1100, Q$Q0)
  Q$Q11000 <<- Q$Q11000
  Q$Q11001 <- kronecker(Q$Q1100, Q$Q1)
  Q$Q11001 <<- Q$Q11001
  Q$Q11010 <- kronecker(Q$Q1101, Q$Q0)
  Q$Q11010 <<- Q$Q11010
  Q$Q11011 <- kronecker(Q$Q1101, Q$Q1)
  Q$Q11011 <<- Q$Q11011
  Q$Q11100 <- kronecker(Q$Q1110, Q$Q0)
  Q$Q11100 <<- Q$Q11100
  Q$Q11101 <- kronecker(Q$Q1110, Q$Q1)
  Q$Q11101 <<- Q$Q11101
  Q$Q11110 <- kronecker(Q$Q1111, Q$Q0)
  Q$Q11110 <<- Q$Q11110
  Q$Q11111 <- kronecker(Q$Q1111, Q$Q1)
  Q$Q11111 <<- Q$Q11111
  Q$Q000000 <- kronecker(Q$Q00000, Q$Q0)
  Q$Q000000 <<- Q$Q000000
  Q$Q000001 <- kronecker(Q$Q00000,Q$Q1)
  Q$Q000001 <<- Q$Q000001
  Q$Q000010 <- kronecker(Q$Q00001, Q$Q0)
  Q$Q000010 <<- Q$Q000010
  Q$Q000011 <- kronecker(Q$Q00001, Q$Q1)
  Q$Q000011 <<- Q$Q000011
  Q$Q000100 <- kronecker(Q$Q00010, Q$Q0)
  Q$Q000100 <<- Q$Q000100
  Q$Q000101 <- kronecker(Q$Q00010, Q$Q1)
  Q$Q000101 <<- Q$Q000101
  Q$Q000110 <- kronecker(Q$Q00011, Q$Q0)
  Q$Q000110 <<- Q$Q000110
  Q$Q000111 <- kronecker(Q$Q00011, Q$Q1)
  Q$Q000111 <<- Q$Q000111
  Q$Q001000 <- kronecker(Q$Q00100, Q$Q0)
  Q$Q001000 <<- Q$Q001000
  Q$Q001001 <- kronecker(Q$Q00100, Q$Q1)
  Q$Q001001 <<- Q$Q001001
  Q$Q001010 <- kronecker(Q$Q00101, Q$Q0)
  Q$Q001010 <<- Q$Q001010
  Q$Q001011 <- kronecker(Q$Q00101, Q$Q1)
  Q$Q001011 <<- Q$Q001011
  Q$Q001100 <- kronecker(Q$Q00110, Q$Q0)
  Q$Q001100 <<- Q$Q001100
  Q$Q001101 <- kronecker(Q$Q00110, Q$Q1)
  Q$Q001101 <<- Q$Q001101
  Q$Q001110 <- kronecker(Q$Q00111, Q$Q0)
  Q$Q001110 <<- Q$Q001110
  Q$Q001111 <- kronecker(Q$Q00111, Q$Q1)
  Q$Q001111 <<- Q$Q001111
  Q$Q010000 <- kronecker(Q$Q01000, Q$Q0)
  Q$Q010000 <<- Q$Q010000
  Q$Q010001 <- kronecker(Q$Q01000, Q$Q1)
  Q$Q010001 <<- Q$Q010001
  Q$Q010010 <- kronecker(Q$Q01001, Q$Q0)
  Q$Q010010 <<- Q$Q010010
  Q$Q010011 <- kronecker(Q$Q01001, Q$Q1)
  Q$Q010011 <<- Q$Q010011
  Q$Q010100 <- kronecker(Q$Q01010, Q$Q0)
  Q$Q010100 <<- Q$Q010100
  Q$Q010101 <- kronecker(Q$Q01010, Q$Q1)
  Q$Q010101 <<- Q$Q010101
  Q$Q010110 <- kronecker(Q$Q01011, Q$Q0)
  Q$Q010110 <<- Q$Q010110
  Q$Q010111 <- kronecker(Q$Q01011, Q$Q1)
  Q$Q010111 <<- Q$Q010111
  Q$Q011000 <- kronecker(Q$Q01100, Q$Q0)
  Q$Q011000 <<- Q$Q011000
  Q$Q011001 <- kronecker(Q$Q01100, Q$Q1)
  Q$Q011001 <<- Q$Q011001
  Q$Q011010 <- kronecker(Q$Q01101, Q$Q0)
  Q$Q011010 <<- Q$Q011010
  Q$Q011011 <- kronecker(Q$Q01101, Q$Q1)
  Q$Q011011 <<- Q$Q011011
  Q$Q011100 <- kronecker(Q$Q01110, Q$Q0)
  Q$Q011100 <<- Q$Q011100
  Q$Q011101 <- kronecker(Q$Q01110, Q$Q1)
  Q$Q011101 <<- Q$Q011101
  Q$Q011110 <- kronecker(Q$Q01111, Q$Q0)
  Q$Q011110 <<- Q$Q011110
  Q$Q011111 <- kronecker(Q$Q01111, Q$Q1)
  Q$Q011111 <<- Q$Q011111
  Q$Q100000 <- kronecker(Q$Q10000, Q$Q0)
  Q$Q100000 <<- Q$Q100000
  Q$Q100001 <- kronecker(Q$Q10000, Q$Q1)
  Q$Q100001 <<- Q$Q100001
  Q$Q100010 <- kronecker(Q$Q10001, Q$Q0)
  Q$Q100010 <<- Q$Q100010
  Q$Q100011 <- kronecker(Q$Q10001, Q$Q0)
  Q$Q100011 <<- Q$Q100011
  Q$Q100100 <- kronecker(Q$Q10010, Q$Q0)
  Q$Q100100 <<- Q$Q100100
  Q$Q100101 <- kronecker(Q$Q10010, Q$Q1)
  Q$Q100101 <<- Q$Q100101
  Q$Q100110 <- kronecker(Q$Q10011, Q$Q0)
  Q$Q100110 <<- Q$Q100110
  Q$Q100111 <- kronecker(Q$Q10011, Q$Q1)
  Q$Q100111 <<- Q$Q100111
  Q$Q101000 <- kronecker(Q$Q10100, Q$Q0)
  Q$Q101000 <<- Q$Q101000
  Q$Q101001 <- kronecker(Q$Q10100, Q$Q1)
  Q$Q101001 <<- Q$Q101001
  Q$Q101010 <- kronecker(Q$Q10101, Q$Q0)
  Q$Q101010 <<- Q$Q101010
  Q$Q101011 <- kronecker(Q$Q10101, Q$Q1)
  Q$Q101011 <<- Q$Q101011
  Q$Q101100 <- kronecker(Q$Q10110, Q$Q0)
  Q$Q101100 <<- Q$Q101100
  Q$Q101101 <- kronecker(Q$Q10110, Q$Q1)
  Q$Q101101 <<- Q$Q101101
  Q$Q101110 <- kronecker(Q$Q10111, Q$Q0)
  Q$Q101110 <<- Q$Q101110
  Q$Q101111 <- kronecker(Q$Q10111, Q$Q1)
  Q$Q101111 <<- Q$Q101111
  Q$Q110000 <- kronecker(Q$Q11000, Q$Q0)
  Q$Q110000 <<- Q$Q110000
  Q$Q110001 <- kronecker(Q$Q11000, Q$Q1)
  Q$Q110001 <<- Q$Q110001
  Q$Q110010 <- kronecker(Q$Q11001, Q$Q0)
  Q$Q110010 <<- Q$Q110010
  Q$Q110011 <- kronecker(Q$Q11001, Q$Q1)
  Q$Q110011 <<- Q$Q110011
  Q$Q110100 <- kronecker(Q$Q11010, Q$Q0)
  Q$Q110100 <<- Q$Q110100
  Q$Q110101 <- kronecker(Q$Q11010, Q$Q1)
  Q$Q110101 <<- Q$Q110101
  Q$Q110110 <- kronecker(Q$Q11011, Q$Q0)
  Q$Q110110 <<- Q$Q110110
  Q$Q110111 <- kronecker(Q$Q11011, Q$Q1)
  Q$Q110111 <<- Q$Q110111
  Q$Q111000 <- kronecker(Q$Q11100, Q$Q0)
  Q$Q111000 <<- Q$Q111000
  Q$Q111001 <- kronecker(Q$Q11100, Q$Q1)
  Q$Q111001 <<- Q$Q111001
  Q$Q111010 <- kronecker(Q$Q11101, Q$Q0)
  Q$Q111010 <<- Q$Q111010
  Q$Q111011 <- kronecker(Q$Q11101, Q$Q1)
  Q$Q111011 <<- Q$Q111011
  Q$Q111100 <- kronecker(Q$Q11110, Q$Q0)
  Q$Q111100 <<- Q$Q111100
  Q$Q111101 <- kronecker(Q$Q11110, Q$Q1)
  Q$Q111101 <<- Q$Q111101
  Q$Q111110 <- kronecker(Q$Q11111, Q$Q0)
  Q$Q111110 <<- Q$Q111110
  Q$Q111111 <- kronecker(Q$Q11111, Q$Q1)
  Q$Q111111 <<- Q$Q111111
  
  Q$Dict <- list('0'=Q$Q0, '1'=Q$Q1, '00'=Q$Q00, '01'=Q$Q01, '10'=Q$Q10, '11'=Q$Q11, "000"=Q$Q000, "001"=Q$Q001, "010"=Q$Q010, "011"=Q$Q011, "100"=Q$Q100, "101"=Q$Q101, "110"=Q$Q110, "111"=Q$Q111,
               "0000"=Q$Q0000, "0001"=Q$Q0001, "0010"=Q$Q0010, "0011"=Q$Q0011, "0100"=Q$Q0100, "0101"=Q$Q0101, "0110"=Q$Q0110,"0111"=Q$Q0111,"1000"=Q$Q1000,"1001"=Q$Q1001,"1010"=Q$Q1010,"1011"=Q$Q1011,"1100"=Q$Q1100, "1101"=Q$Q1101, "1110"=Q$Q1110,"1111"=Q$Q1111, "00000"=Q$Q00000,
               "00001"=Q$Q00001,"00010"=Q$Q00010,"00011"=Q$Q00011,"00100"=Q$Q00100,"00101"=Q$Q00101,"00110"=Q$Q00110,"00111"=Q$Q00111,"01000"=Q$Q01000,"01001"=Q$Q01001,"01010"=Q$Q01010,
               "01011"=Q$Q01011,"01100"=Q$Q01100,"01101"=Q$Q01101,"01110"=Q$Q01110,"01111"=Q$Q01111,"10000"=Q$Q10000,"10001"=Q$Q10001,"10010"=Q$Q10010,"10011"=Q$Q10011,"10100"=Q$Q10100, "10101"=Q$Q10101, "10110"=Q$Q10110, "10111"=Q$Q10111, "11000"=Q$Q11000,"11001"=Q$Q11001,"11010"=Q$Q11010,"11011"=Q$Q11011,"11100"=Q$Q11100,"11101"=Q$Q11101,"11110"=Q$Q11110,
               "11111"=Q$Q11111,"000000"=Q$Q000000,"000001"=Q$Q000001,"000010"=Q$Q000010,"000011"=Q$Q000011,"000100"=Q$Q000100, "000101"=Q$Q000101,"000110"=Q$Q000110,"000111"=Q$Q000111,
               "001000"=Q$Q001000, "001001"=Q$Q001001,"001010"=Q$Q001010,"001011"=Q$Q001011,"001100"=Q$Q001100,"001101"=Q$Q001101,"001110"=Q$Q001110,"001111"=Q$Q001111,"010000"=Q$Q010000,
               "010001"=Q$Q010001,"010010"=Q$Q010010,"010011"=Q$Q010011,"010100"=Q$Q010100,"010101"=Q$Q010101,"010110"=Q$Q010110,"010111"=Q$Q010111,"011000"=Q$Q011000,"011001"=Q$Q011001,
               "011010"=Q$Q011010,"011011"=Q$Q011011,"011100"=Q$Q011100,"011101"=Q$Q011101,"011110"=Q$Q011110,"011111"=Q$Q011111,"100000"=Q$Q100000,"100001"=Q$Q100001,"100010"=Q$Q100010,
               "100011"=Q$Q100011,"100100"=Q$Q100100,"100101"=Q$Q100101,"100110"=Q$Q100110, "100111"=Q$Q100111,"101000"=Q$Q101000,"101001"=Q$Q101001,"101010"=Q$Q101010,"101011"=Q$Q101011,
               "101100"=Q$Q101100,"101101"=Q$Q101101,"101110"=Q$Q101110,"101111"=Q$Q101111,"110000"=Q$Q110000,"110001"=Q$Q110001,"110010"=Q$Q110010,"110011"=Q$Q110011,"110100"=Q$Q110100,
               "110101"=Q$Q110101,"110110"=Q$Q110110,"110111"=Q$Q110111,"111000"=Q$Q111000,"111001"=Q$Q111001,"111010"=Q$Q111010,"111011"=Q$Q111011,"111100"=Q$Q111100,"111101"=Q$Q111101,"111110"=Q$Q111110,"111111"=Q$Q111111)
  
  Q$Dict <<- Q$Dict
  
  # Diagonal Basis
  Q$Q_plus <- matrix(c(1, 1))/sqrt(2)
  Q$Q_plus <<- Q$Q_plus
  Q$Q_minus <- matrix(c(1, -1))/sqrt(2)
  Q$Q_minus <<- Q$Q_minus
  
  # Circular Basis
  Q$Q_clock <- matrix(c(1, 1i))/sqrt(2)
  Q$Q_clock <<- Q$Q_clock
  Q$Q_anticlock <- matrix(c(1, -1i))/sqrt(2)
  Q$Q_anticlock <<- Q$Q_anticlock
  
  # Q$Identity matrices
  Q$I2 <- diag(2)
  Q$I2 <<- Q$I2
  Q$I4 <- diag(2**2)
  Q$I4 <<- Q$I4
  Q$I8 <- diag(2**3)
  Q$I8 <<- Q$I8
  Q$I16 <- diag(2**4)
  Q$I16 <<- Q$I16
  Q$I32 <- diag(2**5)
  Q$I32 <<- Q$I32
  Q$I64 <- diag(2**6)
  Q$I64 <<- Q$I64
  Q$I128 <- diag(2**7)
  Q$I128 <<- Q$I128
  Q$I256 <- diag(2**8)
  Q$I256 <<- Q$I256
  
  # up and down states
  u <- Q$Q0
  u <<- u
  d <- Q$Q1
  d <<- d
  
  # Q$Qutrit systems
  Q$Qt0 <- matrix(c(1, 0, 0))
  Q$Qt0 <<- Q$Qt0
  Q$Qt1 <- matrix(c(0, 1, 0))
  Q$Qt1 <<- Q$Qt1
  Q$Qt2 <- matrix(c(0, 0, 1))
  Q$Qt2 <<- Q$Qt2
  Q$Qt00 <- kronecker(Q$Qt0, Q$Qt0)
  Q$Qt00 <<- Q$Qt00
  Q$Qt01 <- kronecker(Q$Qt0, Q$Qt1)
  Q$Qt01 <<- Q$Qt01
  Q$Qt02 <- kronecker(Q$Qt0, Q$Qt2)
  Q$Qt02 <<- Q$Qt02
  Q$Qt10 <- kronecker(Q$Qt1, Q$Qt0)
  Q$Qt10 <<- Q$Qt10
  Q$Qt11 <- kronecker(Q$Qt1, Q$Qt1)
  Q$Qt11 <<- Q$Qt11
  Q$Qt12 <- kronecker(Q$Qt1, Q$Qt2)
  Q$Qt12 <<- Q$Qt12
  Q$Qt20 <- kronecker(Q$Qt2, Q$Qt0)
  Q$Qt20 <<- Q$Qt20
  Q$Qt21 <- kronecker(Q$Qt2, Q$Qt1)
  Q$Qt21 <<- Q$Qt21
  Q$Qt22 <- kronecker(Q$Qt2, Q$Qt2)
  Q$Qt22 <<- Q$Qt22
  
  Q$Dict3 <- list(Q$Qt0, Q$Qt1, Q$Qt2)
  Q$Dict3 <<- Q$Dict3
  
  # SU(3) matrices
  
  # Gel Mann matrices
  Q$lambda1 <- matrix(c(0, 1, 0, 1, 0, 0, 0, 0, 0), ncol=3, byrow=TRUE)
  Q$lambda1 <<- Q$lambda1
  Q$lambda2 <- matrix(c(0, -1i, 0, 1i, 0, 0, 0, 0, 0), ncol=3, byrow=TRUE)
  Q$lambda2 <<- Q$lambda2
  Q$lambda3 <- matrix(c(1, 0, 0, 0, -1, 0, 0, 0, 0), ncol=3, byrow=TRUE)
  Q$lambda3 <<- Q$lambda3
  Q$lambda4 <- matrix(c(0, 0, 1, 0, 0, 0, 1, 0, 0), ncol=3, byrow=TRUE)
  Q$lambda4 <<- Q$lambda4
  Q$lambda5 <- matrix(c(0, 0, -1i, 0, 0, 0, 1i, 0, 0), ncol=3, byrow=TRUE)
  Q$lambda5 <<- Q$lambda5
  Q$lambda6 <- matrix(c(0, 0, 0, 0, 0, 1, 0, 1, 0), ncol=3, byrow=TRUE)
  Q$lambda6 <<- Q$lambda6
  Q$lambda7 <- matrix(c(0, 0, 0, 0, 0, -1i, 0, 1i, 0), ncol=3, byrow=TRUE)
  Q$lambda7 <<- Q$lambda7
  Q$lambda8 <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, -2), ncol=3, byrow=TRUE)/sqrt(3)
  Q$lambda8 <<- Q$lambda8
  
  # Q$Identity operator
  Q$Identity3 <- diag(3)
  Q$Identity3 <<- Q$Identity3
  
  # Cartan-Weyl basis of the Lie Algebra of SU(3)
  Q$Ihat_plus <- (Q$lambda1 + 1i*Q$lambda2)/2
  Q$Ihat_plus <<- Q$Ihat_plus
  Q$Ihat_minus <- (Q$lambda1 - 1i*Q$lambda2)/2
  Q$Ihat_minus <<- Q$Ihat_minus
  Q$Ihat_3 <- Q$lambda3/2
  Q$Ihat_3 <<- Q$Ihat_3
  Q$Vhat_plus <- (Q$lambda4 + 1i*Q$lambda5)/2
  Q$Vhat_plus <<- Q$Vhat_plus
  Q$Vhat_minus <- (Q$lambda4 - 1i*Q$lambda5)/2
  Q$Vhat_minus <<- Q$Vhat_minus
  Q$Uhat_plus <- (Q$lambda6 + 1i*Q$lambda7)/2
  Q$Uhat_plus <<- Q$Uhat_plus
  Q$Uhat_minus <- (Q$lambda6 - 1i*Q$lambda7)/2
  Q$Uhat_minus <<- Q$Uhat_minus
  Q$Yhat <- Q$lambda8/sqrt(3)
  Q$Yhat <<- Q$Yhat
  
  # miscellaneous
  Q$Hhat <- matrix(c(1/sqrt(2), 0.5, 0.5, -0.5, (3 - 1i*sqrt(7))/(4*sqrt(2)), (1+1i*sqrt(7))/(4*sqrt(2)), -(1+1i*sqrt(7))/(4*sqrt(2)), (-3 + 1i*sqrt(7))/8, (5+1i*sqrt(7))/8), ncol=3, byrow = TRUE)
  Q$Hhat <<- Q$Hhat
}
