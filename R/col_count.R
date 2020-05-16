##############################################################################
#                                                                            #
#                              COLUMN COUNTS                                 #
#                                                                            #
##############################################################################

#' @title
#' Number of columns of a vector/matrix
#'
#' @description
#' This function counts the number of columns of a vector or a matrix
#'
#' @params
#' M
#'
#' @usage
#' col_count(M)
#'
#' @keywords
#' Vector, Matrix
#'
#' @examples
#' initialize_()
#' col_count(Q11)
#' col_count(lambda4)
#' col_count(I2)
#'
#' @export
#'

col_count <- function(M){
  if (exists(deparse(substitute(M)), envir=.GlobalEnv)) return (NCOL(M))
  if (is.null(dim(M))){
    return(NCOL(t(M)))
  }
  return(NCOL(M))
}
