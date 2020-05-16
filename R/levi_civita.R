##############################################################################
#                                                                            #
#                           LEVI - CIVITA SYMBOL                             #
#                                                                            #
##############################################################################

#' @title
#' Levi-Civita symbol
#'
#' @description
#' This function computes the Levi-Civita symbol depending on the permutations of the three inputs, lying in [0,2]
#'
#' @params
#' i, j, k
#'
#' @usage
#' levi_civita(i, j, k)
#'
#' @keywords
#' Linear algebra, tensor analysis
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Levi-Civita_symbol}\cr
#'
#' @examples
#' initialize_()
#' levi_civita(0, 2, 1)
#' levi_civita(1, 2, 0)
#' levi_civita(1, 2, 1)
#'
#' @export
#'

levi_civita <- function(i, j, k){
  if ((i==0 & j==1 & k==2) | (i==1 & j==2 & k==0) | (i==2 & j==0 & k==1)) return (1)
  else if ((i==2 & j==1 & k==0) | (i==0 & j==2 & k==1) | (i==1 & j==0 & k==2)) return (-1)
  else return(0)
}
