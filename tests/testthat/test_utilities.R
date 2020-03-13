test_that("duplicated on col_positions finds duplicates", {
  x <- matrix(c(1, 0, 1, 0), ncol = 2, byrow = TRUE)
  expect_equal(duplicated(col_positions(x)), c(FALSE, TRUE))
})
