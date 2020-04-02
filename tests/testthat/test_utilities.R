test_that("duplicated on col_positions finds duplicates", {
  x <- matrix(c(1, 0, 1, 0), ncol = 2, byrow = TRUE)
  expect_equal(duplicated(col_positions(x)), c(FALSE, TRUE))
})


test_that("create_index creates a valid index", {
  dat <- list(A = c("a", "b"), B = c("c", "d"))

  expect_equal(create_index(list(A = "a", B = "d"), dat), "1-2")
  expect_equal(create_index(list(A = "a"), dat), "1-0")
  expect_equal(create_index(list(B = "c"), dat), "0-1")
  expect_equal(create_index(list(B = c("c", "d")), dat), "0-1_2")
  expect_equal(create_index(list(), dat), "0-0")

  expect_error(create_index(list(A = "a", B = c("e", "jf")), dat))
})

test_that("create_index creates a working index maker", {
  dat <- list(A = c("a", "b"), B = c("c", "d"))

  idx <- create_index_maker(dat)

  expect_equal(idx(A = "a", B = "d"), "1-2")
  expect_equal(idx(A = "a"), "1-0")
  expect_equal(idx(B = "c"), "0-1")
  expect_equal(idx(), "0-0")
})
