context("%on%")

test_that("laziness", {

  funs <- list(abs = abs, identity = identity)

  max_on <- vector("list", 0)
  for (f in names(funs)) {
    max_on[[f]] <- max %on% (funs[[f]])
  }

  expect_equal(
    max_on$identity(-2, 1),
    1
  )

  expect_equal(
    max_on$abs(-2, 1),
    2
  )

})

test_that("predicates with purrr syntax", {

  expect_true(
    (`==` %on% 1)(1, 1:2)
  )

  expect_true(
    (`==` %on% (~ .[1]))(1, 1:2)
  )

  expect_true(
    ((~ .x == .y) %on% 1)(1, 1:2)
  )

  expect_true(
    ((~ .x == .y) %on% (~ .[1]))(1, 1:2)
  )

})
