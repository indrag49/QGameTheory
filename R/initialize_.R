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
#' @param
#'
#'
#' @usage
#' initialize_()
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
#' initialize_()
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
#' @export
#'

initialize_ <- function(){
  library(R.utils)
  library(dplyr)
  library(RColorBrewer)

  # Initialise the 1-Qubit system : {|0>, |1>}
  Q0 <<- c(1, 0)
  Q1 <<- c(0, 1)

  # Initialise the 2-Qubit system : {|00>, |01>, |10>, |11>}
  Q00 <<- as.vector(t(Q0 %o% Q0))
  Q01 <<- as.vector(t(Q0 %o% Q1))
  Q10 <<- as.vector(t(Q1 %o% Q0))
  Q11 <<- as.vector(t(Q1 %o% Q1))

  Dict <<- list('0'=Q0, '1'=Q1, '00'=Q00, '01'=Q01, '10'=Q10, '11'=Q11)

  # Initialise the multi Qubit systems
  L <<- c()
  for(n in 3:8){
    L <<- c(L, strsplit(intToBin(0:(2 ^ n - 1)), split = " "))
  }

  for(i in L){
    l <<- nchar(i)
    s <<- paste('Q', i, sep="")
    s1 <<- substr(i, 1, l-1)
    s2 <<- substr(i, l, l)
    r <- as.vector(t(Dict[[s1]] %o% Dict[[s2]]))
    assign(s, r, envir=globalenv())
    Dict[[substr(s, 2, nchar(s))]] <<- r
  }

  # Diagonal Basis
  Q_plus <<- c(1, 1)/sqrt(2)
  Q_minus <<- c(1, -1)/sqrt(2)

  # Circular Basis
  Q_clock <<- c(1, 1i)/sqrt(2)
  Q_anticlock <<- c(1, -1i)/sqrt(2)

  # Identity matrices
  I2 <<- diag(2)
  I4 <<- diag(2**2)
  I8 <<- diag(2**3)
  I16 <<- diag(2**4)
  I32 <<- diag(2**5)
  I64 <<- diag(2**6)
  I128 <<- diag(2**7)
  I256 <<- diag(2**8)

  # up and down states
  u <<- Q0
  d <<- Q1

  # Qutrit systems
  Qt0 <<- c(1, 0, 0)
  Qt1 <<- c(0, 1, 0)
  Qt2 <<- c(0, 0, 1)
  Qt00 <<- as.vector(t(Qt0 %o% Qt0))
  Qt01 <<- as.vector(t(Qt0 %o% Qt1))
  Qt02 <<- as.vector(t(Qt0 %o% Qt2))
  Qt10 <<- as.vector(t(Qt1 %o% Qt0))
  Qt11 <<- as.vector(t(Qt1 %o% Qt1))
  Qt12 <<- as.vector(t(Qt1 %o% Qt2))
  Qt20 <<- as.vector(t(Qt2 %o% Qt0))
  Qt21 <<- as.vector(t(Qt2 %o% Qt1))
  Qt22 <<- as.vector(t(Qt2 %o% Qt2))

  Dict3 <<- list(Qt0, Qt1, Qt2)

  # SU(3) matrices

  # Gel Mann matrices
  lambda1 <<- matrix(c(0, 1, 0, 1, 0, 0, 0, 0, 0), ncol=3, byrow=TRUE)
  lambda2 <<- matrix(c(0, -1i, 0, 1i, 0, 0, 0, 0, 0), ncol=3, byrow=TRUE)
  lambda3 <<- matrix(c(1, 0, 0, 0, -1, 0, 0, 0, 0), ncol=3, byrow=TRUE)
  lambda4 <<- matrix(c(0, 0, 1, 0, 0, 0, 1, 0, 0), ncol=3, byrow=TRUE)
  lambda5 <<- matrix(c(0, 0, -1i, 0, 0, 0, 1i, 0, 0), ncol=3, byrow=TRUE)
  lambda6 <<- matrix(c(0, 0, 0, 0, 0, 1, 0, 1, 0), ncol=3, byrow=TRUE)
  lambda7 <<- matrix(c(0, 0, 0, 0, 0, -1i, 0, 1i, 0), ncol=3, byrow=TRUE)
  lambda8 <<- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, -2), ncol=3, byrow=TRUE)/sqrt(3)

  # Identity operator
  Identity3 <<- diag(3)

  # Cartan-Weyl basis of the Lie Algebra of SU(3)
  Ihat_plus <<- (lambda1 + 1i*lambda2)/2
  Ihat_minus <<- (lambda1 - 1i*lambda2)/2
  Ihat_3 <<- lambda3/2
  Vhat_plus <<- (lambda4 + 1i*lambda5)/2
  Vhat_minus <<- (lambda4 - 1i*lambda5)/2
  Uhat_plus <<- (lambda6 + 1i*lambda7)/2
  Uhat_minus <<- (lambda6 - 1i*lambda7)/2
  Yhat <<- lambda8/sqrt(3)

  # miscellaneous
  Hhat <<- matrix(c(1/sqrt(2), 0.5, 0.5, -0.5, (3 - 1i*sqrt(7))/(4*sqrt(2)), (1+1i*sqrt(7))/(4*sqrt(2)), -(1+1i*sqrt(7))/(4*sqrt(2)), (-3 + 1i*sqrt(7))/8, (5+1i*sqrt(7))/8), ncol=3, byrow = TRUE)

  }
