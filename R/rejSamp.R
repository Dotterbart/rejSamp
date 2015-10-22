#' Generate random numbers by rejection sampling
#'
#' @param f [\code{function}]\cr
#'  Function to be interpreted as probability density function.
#' @param n  [\code{integer}]\cr
#'  Amount of random numbers to be generated. Default is 1.
#' @param min [\code{numeric}]\cr
#' @param max [\code{numeric}]\cr
#'  Interval the generated random numbers are from. Default is (0, 1).
#' @param g [\code{function}]\cr
#'  Probability density function of the instrumental distribution.
#' @param rg  [\code{function}]\cr
#'  Function that generates random numbers from the instrumental distribution.
#' @param g.fact [\code{numeric}]\cr
#'  Factor for \code{g}. Default is 1.
#' @return [\code{numeric}]\cr
#'  Random numbers from the function \code{f} interpreted as probality density function on the interval (\code{min}, \code{max}).
#' @details
#' If no instrumental distribution is specified, the uniform distribution is used.
#'
#' The call \code{g}(x) should produce the probability of the realisation x for the instrumental distribution \code{g}.
#'
#' The choice of \code{g.fact} should guarantee \code{f(x) < g.fact * g(x)}.
#' @examples
#' test <- function(x) x^2
#' rejSamp(f = test, n = 10, min = -1, max = 1)

rejSamp <- function(f, n = 1, min = 0, max = 1, g = NULL, rg = NULL, g.fact = 1) {
  numbers = c()
  g.factor = g.fact
  if (is.null(g)) {
    g = function(x) dunif(x, min, max)
    rg = function(x) runif(1, min, max)
    g.factor = (max - min) * optimize(f, interval = c(min, max), maximum = TRUE)$objective
  }
  for(i in 1:n) {
    j = TRUE
    while(j) {
      y = rg()
      z = runif(1)
      if (f(y) > g.factor * z * g(y)) {
        numbers[i] <- y
        j = FALSE
      }
    }
  }
  return(numbers)
}
