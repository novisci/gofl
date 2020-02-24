# context("expression operations")

test_that("eval_expr correctly evals expression", {
  library(rlang)
  library(Matrix)
  x1 = Diagonal(2L, c(1, 1))
  dimnames(x1) <- list(NULL, c("x1", "x2"))
  y1 = Diagonal(2L, c(1, 1))
  dimnames(y1) <- list(NULL, c("y1", "y2"))
  z1 = Diagonal(3L, c(1, 1, 1))
  dimnames(z1) <- list(NULL, c("z1", "z2", "z3"))


  bin_rep <- function(x){
    apply(x, 1, as_bin)
  }

  dat <- list(x = x1, y = y1, z = z1)

  expect_equal(
    bin_rep(eval_expr(~ x*y, dat)),
    c(1, 2, 4, 8, 5, 9, 6, 10)
  )

  expect_equal(
    bin_rep(eval_expr(~ x:y, dat)),
    c(5, 9, 6, 10)
  )

  # TODO fill these in...
  # eval_expr(~ x*y*z, dat)
  # eval_expr(~ .zoom(x)*y, dat)
  # eval_expr(~ x*.zoom(y), dat)
  # eval_expr(~ x + y, dat)
  # eval_expr(~ .zoom(x) + y, dat)
  # eval_expr(~ x + .zoom(y, 1), dat)
  # eval_expr(~ x + .zoom(y, 2), dat)
  # eval_expr(~ .zoom(z, c(1, 2))*x, dat)
  # eval_expr(~ z*x, dat)
  # eval_expr(~ z:.zoom(y), dat)
  # eval_expr(~ x + .zoom(y), dat)
  #
  # eval_expr(~ x*y + .zoom(y):z, dat)
  # eval_expr(~ z*x + z*.zoom(y), dat)
  # eval_expr(~ z*(x + .zoom(y)), dat)
  # eval_expr(~ z*x + z*y, dat)
  # eval_expr(~ z*(x + y), dat)
  #
  #
  # t1[ , c("x1", "x2", "y1", "y2", "z1", "z2", "z3")]
  # t2[ , c("x1", "x2", "y1", "y2", "z1", "z2", "z3")]
  #
  # t3[ , c("x1", "x2", "y1", "y2", "z1", "z2", "z3")]
  # t4[ , c("x1", "x2", "y1", "y2", "z1", "z2", "z3")]
})





