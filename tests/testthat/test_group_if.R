# add_val_to_groups -------------------------------------------------------

context(":::add_val_to_groups")

add_val_to_groups <- funprog:::add_val_to_groups

test_that("on atomic values", {

  expect_equal(
    add_val_to_groups(list(), 1, `==`),
    list(1)
  )

  expect_equal(
    add_val_to_groups(list(1), 1, `==`),
    list(c(1, 1))
  )

  expect_equal(
    add_val_to_groups(list(1), 2, `==`),
    list(1, 2)
  )

  expect_equal(
    add_val_to_groups(
      list(1, c(2,2)),
      2,
      `==`
    ),
    list(
      1,
      c(2, 2, 2)
    )
  )

  expect_equal(
    add_val_to_groups(
      list(1, c(2,2)),
      1,
      `==`
    ),
    list(
      1,
      c(2, 2),
      1
    )
  )

})

test_that("on list values", {

  expect_equal(
    add_val_to_groups(
      list(),
      list(1:3),
      identical
    ),
    list(
      list(1:3)
    )
  )

  expect_equal(
    add_val_to_groups(
      list(list(1:3, 1:3)),
      list(1:3),
      identical
    ),
    list(
      list(1:3, 1:3, 1:3)
    )
  )

  expect_equal(
    add_val_to_groups(
      list(list(1:3, 1:3)),
      list(4:5),
      identical
    ),
    list(
      list(1:3, 1:3),
      list(4:5)
    )
  )

  expect_equal(
    add_val_to_groups(
      list(
        list(1:3),
        list(4:5, 4:5)
      ),
      list(4:5),
      identical
    ),
    list(
      list(1:3),
      list(4:5, 4:5, 4:5)
    )
  )

  expect_equal(
    add_val_to_groups(
      list(
        list(1:3),
        list(4:5, 4:5)
      ),
      list(1:3),
      identical
    ),
    list(
      list(1:3),
      list(4:5, 4:5),
      list(1:3)
    )
  )

})


# group_if -----------------------------------------------------------------

context("group_if")

x <- c(3, 4, 2, 2, 1, 1, 1, 3)

test_that("group_eq", {

  expect_equal(
    group_eq(x),
    list(
      3,
      4,
      c(2, 2),
      c(1, 1, 1),
      3
    )
  )

  expect_equal(
    group_eq(c("a", letters[1:4], "d")),
    list(c("a", "a"), "b", "c", c("d", "d"))
  )

})

test_that("other predicates", {

  expect_equal(
    group_if(
      x,
      predicate = `<=`
    ),
    list(
      c(3, 4),
      c(2, 2),
      c(1, 1, 1, 3)
    )
  )

  expect_equal(
    group_if(
      x,
      predicate = `>=`
    ),
    list(
      3,
      c(4, 2, 2, 1, 1, 1),
      3
    )
  )

  expect_equal(
    group_if(
      x,
      predicate = function(x, y) abs(x - y) > 1
    ),
    list(
      3,
      c(4, 2),
      2,
      1,
      1,
      c(1, 3)
    )
  )

  expect_equal(
    group_if(
      letters[1:8],
      function(x, y) x %in% c("g", "a", "c")
    ),
    list(
      c("a", "b"),
      c("c", "d"),
      "e",
      "f",
      c("g", "h")
    )
  )

  expect_equal(
    group_if(
      letters[1:8],
      function(x, y) y %in% c("g", "a", "c")
    ),
    list(
      "a",
      c("b", "c"),
      "d", "e",
      c("f", "g"),
      "h"
    )
  )

})

test_that("NA", {

  expect_error(
    group_eq(c(NA, x, na.rm = FALSE)),
    "missing"
  )

  expect_equal(
    group_eq(c(NA, x), na.rm = TRUE),
    group_eq(x)
  )

})

test_that("flattened back", {

  expect_equal(
    unlist(group_eq(x)),
    x
  )

})

test_that("keep names", {

  names(x) <- c("A", NA, "B", NA, NA, NA, "C", NA)

  expect_equal(
    group_eq(x),
    list(
      c(A = 3),
      4,
      structure(c(2, 2), .Names = c("B", NA)),
      structure(c(1, 1, 1), .Names = c(NA, NA, "C")),
      3
    )
  )

})

test_that("on lists", {

  x <- list(1:3, 1:3, 3:5, 1, 2)

  expect_equal(
    group_eq(x),
    list(
      list(1:3, 1:3),
      list(3:5),
      list(1),
      list(2)
    )
  )

  expect_equal(
    group_if(
      x,
      function(x, y) length(x) == length(y) # `==` %on% length
    ),
    list(
      list(1:3, 1:3, 3:5),
      list(1, 2)
    )
  )

  expect_equal(
    group_if(
      x,
      function(x, y) x[1] < y[1]
    ),
    list(
      list(1:3),
      list(1:3, 3:5),
      list(1, 2)
    )
  )

  expect_equal(
    group_eq(
      list(NULL, NULL, 1:2)
    ),
    list(
      list(NULL, NULL),
      list(1:2)
    )
  )

})

test_that("on list of data.frames (list of lists)", {

  dfs <- list(
    data.frame(C = 1, D = 2, G = 1),
    data.frame(C = 3, D = 4, E = 5),
    data.frame(C = c(1, -1), D = 2, E = 3)
  )

  expect_equal(
    group_if(dfs, identical %on% names),
    list(
      list(
        data.frame(C = 1, D = 2, G = 1)
      ),
      list(
        data.frame(C = 3, D = 4, E = 5),
        data.frame(C = c(1, -1), D = 2, E = 3)
      )
    )
  )

})
