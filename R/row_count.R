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
#' @params
#' M
#'
#' @usage
#' row_count(M)
#'
#' @keywords
#' Vector, Matrix
#'
#' @examples
#' initialize_()
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
