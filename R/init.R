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
#' @keywords
#' Quantum Computing, Qubits, Qutrits, SU(2), SU(3), Basis States, superposition
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

  Dict=Dict3=NULL

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

  Dict <- list('0'=Q0, '1'=Q1, '00'=Q00, '01'=Q01, '10'=Q10, '11'=Q11)
  Dict <<- Dict

  # Initialise the multi Qubit systems
  L <- c()
  for(n in 3:8){
    L <- c(L, strsplit(intToBin(0:(2 ^ n - 1)), split = " "))
  }


  for(i in L){
    l <- nchar(i)
    s <- paste('Q', i, sep="")
    s1 <- substr(i, 1, l-1)
    s2 <- substr(i, l, l)
    r <- kronecker(Dict[[s1]], Dict[[s2]])
    assign(s, r, envir=.GlobalEnv)
    Dict[[substr(s, 2, nchar(s))]] <- r
  }
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
