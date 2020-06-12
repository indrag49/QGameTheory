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
#' @param M A vector/matrix
#'
#' @usage
#' col_count(M)
#'
#' @return An integer that gives the number of columns in a vector or a matrix.
#'
#' @examples
#' init()
#' col_count(Q$Q11)
#' col_count(Q$lambda4)
#' col_count(Q$I2)
#'
#' @export
#'

col_count <- function(M){
  if (is.null(dim(M))){
    return(NCOL(t(M)))
  }
  return(NCOL(M))
}
