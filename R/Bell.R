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
#' @param qubit1 1st input qubit
#' @param qubit2 2nd input qubit
#'
#' @usage
#' Bell(qubit1, qubit2)
#'
#' @return One of the Bell states as a vector depending on the input qubits.
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Bell_state}\cr
#' \url{https://books.google.co.in/books?id=66TgFp2YqrAC&pg=PA25&redir_esc=y}\cr
#'
#' @examples
#' init()
#' Bell(Q$Q0, Q$Q0)
#' Bell(Q$Q0, Q$Q1)
#' Bell(Q$Q1, Q$Q0)
#' Bell(Q$Q1, Q$Q1)
#'
#' @export

Bell <- function(qubit1, qubit2){
  if (length(qubit1)!=2 | length(qubit2)!=2) return ("Error")
  if(qubit1[[1]] == 1){
    h <- Hadamard(Q$Q0)
  }
  else{
    h <- Hadamard(Q$Q1)
  }
  if(qubit2[[1]] == 0){
    x <- kronecker(h, Q$Q1)
  }
  else{
    x <- kronecker(h, Q$Q0)
  }
  return (CNOT(x))
}
