
# data to check
# - make sure things evaluate
# - associativity

dat <-
expand.grid(
  op1  = c("+", ":", "*"),
  op2  = c("+", ":", "*"),
  # op3  = c("+", ":", "*"),
  x    = 1:3,
  y    = 1:3,
  z    = 1:3,
  tag  = 0:3,
  zoom = 0:3,
  stringsAsFactors = FALSE
)

dat <- dat[dat$x != dat$y & dat$x != dat$z & dat$y != dat$z, ]

library(dplyr)

dat <-
dat %>%
  rowwise() %>%
  # head() %>%
  mutate(
    expr_text = list(list(c("x", "y", "z")[c(x, y, z)])),
    expr_text = purrr::map2(
      .x = expr_text,
      .y = zoom,
      .f = ~  { if (.y > 0) { .x[.y] <- sprintf(".zoom(%s, levels = 1)", .x[.y])  } ; list(.x) }),
    expr_text = purrr::map2(
      .x = expr_text,
      .y = tag,
      .f = ~  { if (.y > 0) { .x[.y] <- sprintf("tag(%s, 'testTag')", .x[.y])  } ; list(.x) }),
    expr_text = purrr::pmap(
      .l = list(expr_text, op1, op2),
      .f = ~ sprintf("~ %s %s %s %s %s", ..1[1], ..2, ..1[2], ..3, ..1[3])
    ),
    expr = purrr::map(
      expr_text,
      rlang::parse_expr),
    x_size = ifelse(x == zoom, 1, 2),
    y_size = ifelse(y == zoom, 1, 3),
    z_size = ifelse(z == zoom, 1, 4)
  )

facts <- list(x = c("x1", "x2"),
              y =c("y1", "y2", "y3"),
              z = c("z1", "z2", "z3", "z4"))


test_that("groupings are created", {

  for(i in  seq_along(dat[["expr"]])){

    # create_grouping_matrix(~tag(z, "testTag"):y + x, data = facts)

    test <- gofl::create_groupings(  dat[["expr"]][[i]] , data = facts)

    should_be_tagged <- dat[["tag"]][[i]] > 0
    is_tagged <- length(purrr::compact(purrr::map(test, ~ .x$tag))) > 0


    fact_mats <- purrr::map(
      facts,
      ~   Matrix::Diagonal(length(.x), rep.int(1, length(.x)))
    )

    # This is almost circular testing because create_groupings
    # uses eval_expr with a different .f argument, but I'm going with this for
    # now due to the difficulty (or lack of my imagination) of coming up with
    # a better way to do this systematicatlly - B. Saul
    expected_size <- gofl:::eval_expr(dat[["expr"]][[i]],
                                      data = fact_mats,
                                      .f = gofl:::replace_by_size)


    message <- sprintf("%s failed", dat[["expr_text"]][[i]])

    expect_is(test, "list", label = message)
    expect_equal(should_be_tagged, is_tagged, label = message)
    expect_equal(expected_size, length(test), label = message)
  }
})


test_that("examples work", {
  # TODO
  # levels <- list(
  #   smithsFans = TRUE,
  #   swiftsFans = TRUE,
  #   age_cat   = c("young", "old")
  # )
  #
  # # levels_mats <- gofl:::as_matrices(levels)
  #
  # create_grouping_matrix(
  #   ~ swiftsFans*age_cat,
  #   data = levels)
  #
  # create_grouping_matrix(
  #   ~ tag(swiftsFans, "shaking")*age_cat,
  #   data = levels)
  #
  # create_grouping_matrix(
  #   ~ tag(swiftsFans*age_cat, "shaking"),
  #   data = levels)
  #
  # create_grouping_matrix(
  #   ~ smithsFans + tag(swiftsFans*age_cat, "shaking"),
  #   data = levels)
  #
  # create_grouping_matrix(
  #   ~ smithsFans + tag(swiftsFans, "shaking")*age_cat,
  #   data = levels)
  #
  # create_grouping_matrix(
  #   ~ tag(smithsFans, "depression") + tag(swiftsFans, "shaking")*age_cat,
  #   data = levels)
  #
  # create_grouping_matrix(
  #   ~ (tag(smithsFans, "depression") + tag(swiftsFans, "shaking")):age_cat,
  #   data = levels)
  #
  # create_grouping_matrix(
  #   ~ tag(smithsFans, "depression"):age_cat + tag(swiftsFans, "shaking"):age_cat,
  #   data = levels)
  #
  # create_grouping_matrix(
  #   ~ tag(smithsFans, "depression")*age_cat + tag(swiftsFans, "shaking"):age_cat,
  #   data = levels)
  #
  # create_grouping_matrix(
  #   ~ tag(smithsFans + smithsFans:age_cat, "depression") +
  #     tag(swiftsFans + swiftsFans:age_cat, c("depression", "shaking")),
  #   data = levels)
  #
  # create_groupings(
  #   ~ tag(smithsFans + smithsFans:age_cat, "depression") +
  #     tag(swiftsFans + swiftsFans:age_cat, c("depression", "shaking")),
  #   data = levels)
  #
  # create_grouping_matrix(
  #   ~ tag(
  #     (smithsFans + tag(swiftsFans, "shaking")):age_cat,
  #     "depression"),
  #   data = levels)


})
