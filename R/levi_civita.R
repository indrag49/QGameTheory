##############################################################################
#                                                                            #
#                           LEVI - CIVITA SYMBOL                             #
#                                                                            #
##############################################################################

#' @title
#' Levi-Civita symbol
#'
#' @description
#' This function computes the Levi-Civita symbol depending on the permutations of the three inputs, lying in 0 to 2
#'
#' @param i an integer 0, 1 or 2
#' @param j an integer 0, 1 or 2
#' @param k an integer 0, 1 or 2
#'
#' @usage
#' levi_civita(i, j, k)
#'
#' @return 0, 1 or -1 after computing the Levi-Civita symbol depending on the permutations of the three inputs 0, 1 and 2
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Levi-Civita_symbol}\cr
#'
#' @examples
#' init()
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
