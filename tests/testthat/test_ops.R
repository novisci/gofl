# context("Matrix operations")
x1 = Matrix::Diagonal(2L, c(1, 1))
dimnames(x1) <- list(NULL, c("x1", "x2"))
x1 <- methods::new("tagged", mat = x1, tags = gofl:::empty_tags(2))

y1 = Matrix::Diagonal(2L, c(1, 1))
dimnames(y1) <- list(NULL, c("y1", "y2"))
y1 <- methods::new("tagged", mat = y1, tags = gofl:::empty_tags(2))

z1 = Matrix::Diagonal(3L, c(1, 1, 1))
dimnames(z1) <- list(NULL, c("z1", "z2", "z3"))
z1 <- methods::new("tagged", mat = z1, tags = gofl:::empty_tags(3))

dat <- list(x = x1, y = y1, z = z1)

test_that("tagging works", {

  do_test <- function(x, comparator){
    expect_s4_class(x, "tagged")
    expect_equal(nrow(x@mat), length(x@tags))
    expect_equal(x@tags, comparator)
  }

  test1 <-
    gofl:::eval_expr( ~ x + tag(y*z, "yANDz"), dat, .f = identity)
  do_test(test1, c(list(NULL, NULL), replicate(11, "yANDz", simplify = FALSE)))

  test2 <-
    gofl:::eval_expr( ~ tag(x, "justx") + tag(y*z, "yANDz"),dat, .f = identity)
  do_test(test2, c(list("justx", "justx"), replicate(11, "yANDz", simplify = FALSE)))

  test3 <-
    gofl:::eval_expr( ~ tag(y:z, "yANDz"), dat, .f = identity)
  do_test(test3, c(replicate(6, "yANDz", simplify = FALSE)))

  test4 <-
    gofl:::eval_expr( ~ tag(x, "justx"):tag(y:z, "yANDz"), dat, .f = identity)
  do_test(test4, c(replicate(12, c("justx", "yANDz"), simplify = FALSE)))

  test5 <-
    gofl:::eval_expr( ~ tag(x, "justx")*tag(y:z, "yANDz"), dat, .f = identity)
  do_test(test5,
          c(replicate(2, c("justx"), simplify = FALSE),
            replicate(6, c("yANDz"), simplify = FALSE),
            replicate(12, c("justx", "yANDz"), simplify = FALSE)))

  test6 <-
    gofl:::eval_expr( ~ tag(x, "justx")*tag(y:.zoom(z, 1), "yANDz1"),
                      dat, .f = identity)
  do_test(test6, c(
    replicate(2, c("justx"), simplify = FALSE),
    replicate(2, c("yANDz1"), simplify = FALSE),
    replicate(4, c("justx", "yANDz1"), simplify = FALSE)))

  test7 <-
    gofl:::eval_expr( ~ tag(x, "justx")*tag(y:.zoom(z, 1:2), "yANDz1"),
                      dat, .f = identity)
  do_test(test7, c(
    replicate(2, c("justx"), simplify = FALSE),
    replicate(2, c("yANDz1"), simplify = FALSE),
    replicate(4, c("justx", "yANDz1"), simplify = FALSE)))
})


test_that("eval_expr correctly evals expression", {

  dat <- list(x = x1, y = y1, z = z1)

  expect_equal(
    bin_rep(eval_expr(~ x*y, dat, .f = strip_tagger)@mat),
    c(1, 2, 4, 8, 5, 9, 6, 10)
  )

  expect_equal(
    bin_rep(eval_expr(~ x:y, dat, .f = strip_tagger)@mat),
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

test_that("c on tagged works", {
  x <- overall(c("x", "y", "z"))
  y <- overall(c("x", "y", "z"))
  z <- new("tagged", mat = y@mat, tags = list(c("a", "b")))


  expect_true(nrow(c(x, y)@mat) == 2L)
  expect_true(length(c(x, y)@tags) == 2L)
  expect_true(nrow(c(x, z)@mat) == 2L)
  expect_true(length(c(x, z)@tags) == 2L)

})
