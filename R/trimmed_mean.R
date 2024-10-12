#' Calculate Trimmed Mean
#'
#' This function calculates the trimmed mean of a numeric vector by removing a specified number of smallest and largest values.
#'
#' @param x A numeric vector from which to calculate the trimmed mean.
#' @param s An integer specifying the number of smallest values to trim.
#' @param l An integer specifying the number of largest values to trim.
#'
#' @return The trimmed mean of the vector `x` after trimming `s` smallest and `l` largest values.
#' @examples
#' trimmed_mean(c(1, 2, 3, 4, 5, 6), s = 1, l = 1)  # Returns the mean of 2, 3, 4, 5
#' trimmed_mean(c(1,2), s=1, l=2)
#' trimmed_mean(c(1,2,3,4,5), s=1, l=2)
#' trimmed_mean(c(5,2,4,5,6,7), s=1, l=2)


trimmed_mean <- function (x, s, l){

  # check the size of x
  if (length(x)< s+l+1) {stop("The vector is too small")}else{
    #reorder what is inside x
    ordered_x <- order(x)
    trimmed_x <- ordered_x[(s+1) : (length(x)-l)]
    return(sum(trimmed_x)/length (trimmed_x))
  }
}


