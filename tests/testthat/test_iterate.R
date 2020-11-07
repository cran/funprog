context("iterate")

test_that("stop_n alone", {

  expect_equal(
    iterate(3, function(x) x + 2, stop_n = 3),
    9
  )

  expect_equal( # (same with purrr syntax)
    iterate(3, ~ . + 2, stop_n = 3),
    9
  )

  expect_error(
    iterate(3, ~ . + 2, stop_n = "two")
  )

  expect_error(
    iterate(3, ~ . + 2, stop_n = -1)
  )

})

test_that("stop_fun alone", {

  expect_equal(
    iterate(3, function(x) x + 2, stop_fun = function(x) x > 20),
    21 # applied last time when 19, hence 21
  )

  expect_equal( # (same with purrr syntax)
    iterate(3, ~ . + 2, stop_fun = ~ . > 20),
    21
  )

})


test_that("prevents infinite loop", {

  expect_error(
    iterate(3, function(x) x + 2),
    "this will run forever"
  )

  expect_error(
    iterate(3, function(x) x + 2, stop_n = Inf),
    "this will run forever"
  )

})

test_that("stop_n and stop_fun together", {

  expect_equal(
    iterate(3, ~ . + 2, stop_n = 3, stop_fun = ~ . > 100),
    9
  )

  expect_equal(
    iterate(3, ~ . + 2, stop_n = 30, stop_fun = ~ . > 20),
    21
  )

})

test_that("accumulate", {

  syracuse_next <- function(x) if (x %% 2) 3 * x + 1 else x / 2

  expect_equal(
    iterate(
      10,
      syracuse_next,
      stop_fun = function(n) n == 1,
      accumulate = FALSE
    ),
    1
  )

  expect_equal(
    iterate(
      10,
      syracuse_next,
      stop_fun = function(n) n == 1,
      accumulate = TRUE # [+]
    ),
    list(10, 5, 16, 8, 4, 2, 1)
  )

})
