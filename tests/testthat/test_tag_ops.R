x1 = Matrix::Diagonal(2L, c(1, 1))
dimnames(x1) <- list(NULL, c("x1", "x2"))
y1 = Matrix::Diagonal(2L, c(1, 1))
dimnames(y1) <- list(NULL, c("y1", "y2"))
z1 = Matrix::Diagonal(3L, c(1, 1, 1))
dimnames(z1) <- list(NULL, c("z1", "z2", "z3"))
dat <- list(x = x1, y = y1, z = z1)

test_that("tagging works", {

  do_test <- function(x, comparator){
    expect_s4_class(x, "tagged")
    expect_equal(nrow(x@mat), length(x@tags))
    expect_equal(x@tags, comparator)
  }

  test1 <- gofl:::eval_expr( ~ x + tag(y*z, "yANDz"), dat, .f = gofl:::tagger)
  do_test(test1, c(list(NULL, NULL), replicate(11, "yANDz", simplify = FALSE)))

  test2 <- gofl:::eval_expr( ~ tag(x, "justx") + tag(y*z, "yANDz"), dat, .f = gofl:::tagger)
  do_test(test2, c(list("justx", "justx"), replicate(11, "yANDz", simplify = FALSE)))

  test3 <- gofl:::eval_expr( ~ tag(y:z, "yANDz"), dat, .f = gofl:::tagger)
  do_test(test3, c(replicate(6, "yANDz", simplify = FALSE)))

  test4 <- gofl:::eval_expr( ~ tag(x, "justx"):tag(y:z, "yANDz"), dat, .f = gofl:::tagger)
  do_test(test4, c(replicate(12, c("justx", "yANDz"), simplify = FALSE)))

  test5 <- gofl:::eval_expr( ~ tag(x, "justx")*tag(y:z, "yANDz"), dat, .f = gofl:::tagger)
  do_test(test5, c(replicate(20, c("justx", "yANDz"), simplify = FALSE)))


  test6 <- gofl:::eval_expr( ~ tag(x, "justx")*tag(y:.zoom(z, 1), "yANDz1"),
                               dat, .f = gofl:::tagger)
  do_test(test6, c(replicate(8, c("justx", "yANDz1"), simplify = FALSE)))
})
