context("unique_by")

test_that("on vectors", {

  expect_equal(
    unique_by(-3:2, abs),
    c(-3, -2, -1, 0)
  )

  expect_equal(
    unique_by(-3:2, abs, first = FALSE),
    c(-3, 0, 1, 2)
  )

  expect_equal(
    unique_by(c(1, 2, 4, 5, 6), function(x) x %% 3),
    c(1, 2, 6)
  )

  expect_equal(
    unique_by(c(1, 2, 4, 5, 6), function(x) x %% 3, first = FALSE),
    c(4, 5, 6)
  )

})

test_that("on lists", {

  expect_equal(
    unique_by(list(1:2, 2:3, 2:4), length),
    list(1:2, 2:4)
  )

  expect_equal(
    unique_by(list(1:2, 2:3, 2:4), length, first = FALSE),
    list(2:3, 2:4)
  )

  expect_equal(
    unique_by(list(1:2, 2:3, 2:4), function(x) x[1]),
    list(1:2, 2:3)
  )

  expect_equal(
    unique_by(list(1:2, 2:3, 2:4), function(x) x[1], first = FALSE),
    list(1:2, 2:4)
  )

})

test_that("purrr syntax", {

  expect_equal(
    unique_by(c(1, 2, 4, 5, 6), ~ . %% 3),
    c(1, 2, 6)
  )

  expect_equal(
    unique_by(list(1:2, 2:3, 2:4), 1),
    list(1:2, 2:3)
  )

})
