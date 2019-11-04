context("sort_by")

test_that("single function", {

  expect_equal(
    sort_by(-3:2, abs),
    c(0, -1, 1, -2, 2, -3)
  )

  expect_equal(
    sort_by(list(5:7, 0, 1:4), length),
    list(0, 5:7, 1:4)
  )

  expect_equal(
    sort_by(list(5:7, 0, 1:4), function(x) x[1]),
    list(0, 1:4, 5:7)
  )

  expect_equal(
    sort_by(
      list(iris[1:10, 1:2], iris[1:6, 1:3]),
      function(x) nrow(x) * ncol(x)
    ),
    list(iris[1:6, 1:3], iris[1:10, 1:2])
  )

})

test_that("several functions", {

  expect_equal(
    sort_by(-3:2, abs, function(x) -x),
    c(0, 1, -1, 2, -2, -3)
  )

  expect_equal(
    sort_by(list(3:4, 1:2, 5), length, sum),
    list(5, 1:2, 3:4)
  )

})

test_that("descending", {

  expect_equal(
    sort_by(-3:2, descending(abs)),
    c(-3, -2, 2, -1, 1, 0)
  )

  expect_equal(
    sort_by(list(1:2, 3:4, 5), length, descending(sum)),
    list(5, 3:4, 1:2)
  )

})

test_that("incorrect input", {

  expect_error(
    sort_by(list(3:4, 1:2, 5), unique),
    "must be atomic"
  )

  expect_error(
    sort_by(list(3:4, 1:2, 5), length, unique),
    "must be atomic"
  )

})
