# context("Matrix operations")

test_that("...", {
  library(Matrix)
  x <- Diagonal(2, c(1, 1))
  y <- Diagonal(3, c(1, 1, 1))
  z <- Diagonal(4, c(1, 1, 1, 1))


  ind_combn_count <- function(...){
    unlist(lapply(list(...), function(m) sum(Matrix::diag(m))))
  }

  three_matrix_tester <- function(x, y, z){
    vals <- ind_combn_count(x, y, z)
    c(1, sum(vals),
      sum(prod(vals[1:2]), prod(vals[c(1,3)]), prod(vals[2:3])),
      prod(vals)
    )
  }

  combn_count <- function(...){
    rle(sort(Matrix::rowSums(.allway(...))))
  }

  expect_equal(
    three_matrix_tester(x, y, z),
    combn_count(x, y, z)[["lengths"]]
  )



})
