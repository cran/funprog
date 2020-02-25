context("partition")

test_that("on vectors", {

  expect_equal(
    partition(c(2, 1, 3, 4, 1, 5), function(x) x < 3),
    list(c(2, 1, 1), c(3, 4, 5))
  )

})

test_that("on lists", {

  expect_equal(
    partition(list(1:3, NA, c(1, NA, 3)), anyNA),
    list(
      list(NA, c(1, NA, 3)),
      list(1:3)
    )
  )

})

test_that("purrr syntax", {

  expect_equal(
    partition(c(2, 1, 3, 4, 1, 5), ~ . < 3),
    list(c(2, 1, 1), c(3, 4, 5))
  )

})
