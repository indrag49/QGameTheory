##############################################################################
#                                                                            #
#                         PREPARATION OF BELL STATES                         #
#                                                                            #
##############################################################################

#' @title
#' Bell States
#'
#' @description
#' The function builds one of the four Bell states, according to the input qubits
#'
#' @params
#' qubit1, qubit2
#'
#' @usage
#' Bell(qubit1, qubit2)
#'
#' @keywords
#' Quantum entanglement, Bell basis
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Bell_state}\cr
#' \url{https://books.google.co.in/books?id=66TgFp2YqrAC&pg=PA25&redir_esc=y}\cr
#'
#' @examples
#' initialize_()
#' Bell(0, 0)
#' Bell(0, 1)
#' Bell(1, 0)
#' Bell(1, 1)
#'
#' @export

Bell <- function(qubit1, qubit2){
  if(qubit1 == 0){
    h <- Hadamard(Q0)
  }
  else{
    h <- Hadamard(Q1)
  }
  if(qubit2 == 1){
    x <- as.vector(t(h %*% Q1))
  }
  else{
    x <- as.vector(t(h %*% Q0))
  }
  return (CNOT(x))
}
