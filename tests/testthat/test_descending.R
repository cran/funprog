context("descending")

test_that("laziness", {

  funs <- list(abs = abs, identity = identity)

  desc_funs <- vector("list", 0)
  for (f in names(funs)) {
    desc_funs[[f]] <- descending(funs[[f]])
  }

  expect_equal(
    order(desc_funs$identity(-2:1)),
    c(4, 3, 2, 1)
  )

  expect_equal(
    order(desc_funs$abs(-2:1)),
    c(1, 2, 4, 3)
  )

})

test_that("purrr syntax", {

  input <- list(c(a = 1, b = 2, c = 3), c(a = 10, b = 20))

  expect_equal(
    descending(~ abs(.))(-2:1),
    descending(abs)(-2:1)
  )

  expect_equal(
    descending(1)(input),
    descending(function(x) x[[1]])(input)
  )

})
