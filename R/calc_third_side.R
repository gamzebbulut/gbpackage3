#' Calculate the Third Side of a Right Triangle
#'
#' This function calculates the length of the third side of a right triangle when two sides are provided.
#' Given two sides (either `a` and `b` for the legs, or one leg and the hypotenuse `c`), the function will compute the missing side.
#'
#' @param a Numeric. One of the sides of the right triangle. Set to `NA` if this is the side to be calculated.
#' @param b Numeric. Another side of the right triangle. Set to `NA` if this is the side to be calculated.
#' @param c Numeric. The hypotenuse of the right triangle. Set to `NA` if this is the side to be calculated.
#'
#' @return The length of the missing side (`a`, `b`, or `c`) of the right triangle.
#' @keywords geometry, right triangle, Pythagoras
#' @export
#' @examples
#' # Calculate the hypotenuse when the two legs are known
#' calc_third_side(a = 3, b = 4, c = NA)  # Returns 5
#'
#' # Calculate one leg when the other leg and the hypotenuse are known
#' calc_third_side(a = NA, b = 4, c = 5)  # Returns 3
#'
#' # You must specify two sides for the calculation
#' calc_third_side(a = 3, b = NA, c = NA)  # Error, as only one side is provided
#'
#'

# this is the third iteration checking which sides are given
calc_third_side <- function (a, b, c){

  #Check if input is numeric
  if ((is.character(a) == TRUE) | (is.character(b) == TRUE) | (is.character(c) == TRUE))
  {stop("Numbers only please!")}

  #Check if 3 sides are given
  if (!is.na(a) & !is.na(b) & !is.na(c)) {stop("You gave me 3 sides, I only need two.")}else

    # Check if only 1 side is given
    if (is.na(a) & is.na(b)) {stop("You need to input two sides")}else
      if (is.na(a) & is.na(c)) {stop("You need to input two sides")}else
        if (is.na(b) & is.na(c)) {stop("You need to input two sides")}else

          #Check which of a, b, c are given
          if (is.na(a)){ return (sqrt(c^2 - b^2))} else
            if (is.na(b)){ return (sqrt(c^2 - a^2))} else
              if (is.na(c)){ return (sqrt(a^2 + b^2))}
}
