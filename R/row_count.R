##############################################################################
#                                                                            #
#                              ROW COUNTS                                    #
#                                                                            #
##############################################################################

#' @title
#' Number of rows of a vector/matrix
#'
#' @description
#' This function counts the number of rows of a vector or a matrix
#'
#' @param M A vector/matrix
#'
#' @return An integer that gives the number of rows in a vector or a matrix.
#'
#' @usage
#' row_count(M)
#'
#' @examples
#' init()
#' row_count(Q01)
#' row_count(lambda5)
#' row_count(Qt12)
#'
#' @export
#'

row_count <- function(M){
  if (exists(deparse(substitute(M)), envir=.GlobalEnv)) return (NROW(M))
  if (is.null(dim(M))){
    return(NROW(t(M)))
  }
  return(NROW(M))
}
