context("rejSamp")

funExp = function(fun, min, max) {
  div = integrate(fun, min, max)$value
  fun2 = function(x) x * fun(x) / div
  return(integrate(fun2, min, max)$value)
}
fun1 = function(x) x
fun2 = function(x) dunif(x, 1, 5)
fun3 = function() runif(1, 1, 5)
fun4 = function() runif(7, 1, 5)
xxx <- "Hello"

set.seed(9247)

test_that("rejSamp", {
  expect_true(is.numeric(rejSamp(fun1, 10, -5000, 5000)))
  expect_error(rejSamp(fun1, 0, -5000, 5000))
  expect_error(rejSamp(fun1, 2.5, -5000, 5000))
  expect_error(rejSamp(fun1, 10, -Inf, 5000))
  expect_error(rejSamp(xxx, 10, -5000, 5000))
  expect_warning(rejSamp(fun1, 10, 5000, -5000))
  expect_true(is.numeric(rejSamp(fun1, 10, 1, 5, fun2, fun3, 20)))
  expect_error(rejsamp(fun1, 10, 1, 5, fun2, fun3, 1e-7))
  expect_error(rejsamp(fun1, 10, 1, 5, xxx, fun3, 20))
  expect_error(rejsamp(fun1, 10, 1, 5, fun2, xxx, 20))
  expect_error(rejsamp(fun1, 10, 1, 5, fun2, fun4, 20))
  expect_that(mean(rejSamp(fun1, 10000, 1, 5)), is_less_than(1.03 * funExp(fun1, 1, 5)))
  expect_that(mean(rejSamp(fun1, 10000, 1, 5)), is_more_than(0.97 * funExp(fun1, 1, 5)))
})
