#' Generate random numbers by rejection sampling
#'
#' @param f [\code{function}]
#'  Function to be interpreted as probability density function.
#' @param n  [\code{integer(1)}]
#'  Amount of random numbers to be generated. Default is 1.
#' @param min [\code{numeric(1)}]
#' @param max [\code{numeric(1)}]
#'  Interval the generated random numbers are from. Default is (0, 1).
#' @param g [\code{function}]
#'  Probability density function of the instrumental distribution.
#' @param rg  [\code{function}]
#'  Function that generates random numbers from the instrumental distribution.
#' @param g.factor [\code{numeric(1)}]
#'  Factor for \code{g}. Default is 1.
#' @return
#'  Random numbers from the function \code{f} interpreted as probality density function on the interval (\code{min}, \code{max}).
#' @details
#' If no instrumental distribution is specified, the uniform distribution is used.
#'
#' The call \code{g}(x) should produce the probability of the realisation x for the instrumental distribution \code{g}.
#'
#' The choice of \code{g.factor} should guarantee \code{f(x) < g.factor * g(x)}.
#' @examples
#' test <- function(x) x^2
#' rejSamp(f = test, n = 10, min = -1, max = 1)

rejSamp <- function(f, n = 1, min = 0, max = 1, g = NULL, rg = NULL, g.factor = 1) {
  checkmate::assertFunction(f)
  checkmate::assertCount(n, positive = TRUE)
  if (!is.null(g)) {
    checkmate::assertFunction(g)
    checkmate::assertFunction(rg)
    checkmate::assertNumber(rg(), lower = min, upper = max)
  }
  checkmate::assertNumber(min, finite = TRUE)
  checkmate::assertNumber(max, finite = TRUE)
  checkmate::assertNumber(g.factor, lower = 1e-6, finite = TRUE)
  if (min > max) {
    temp = max
    max = min
    min = temp
    warning("min and max have been swapped")
  }
  numbers = c()
  if (is.null(g)) {
    g = function(x) dunif(x, min, max)
    rg = function(x) runif(1, min, max)
    g.factor = (max - min) * optimize(f, interval = c(min, max), maximum = TRUE)$objective
  }
  rej.helper = function() {
    j = TRUE
    while(j) {
      y = rg()
      z = runif(1)
      if (f(y) > g.factor * z * g(y)) {
        return(y)
      }
    }
  }
  return(replicate(n, rej.helper()))
}


