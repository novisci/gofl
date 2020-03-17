
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

    test <-
      gofl::create_groupings( dat[["expr"]][[i]], data = facts)[["groupings"]]

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
  levels <- list(
    smithsFans = TRUE,
    swiftsFans = TRUE,
    age_cat   = c("young", "old")
  )

  # TODO: add these tests
#
#   create_grouping_matrix(
#     ~ swiftsFans + age_cat,
#     data = levels)
#
#   create_grouping_matrix(
#     ~ swiftsFans:age_cat,
#     data = levels)
#
#   create_grouping_matrix(
#     ~ smithsFans + swiftsFans:age_cat,
#     data = levels)
#
#   create_grouping_matrix(
#     ~ swiftsFans*age_cat,
#     data = levels)
#
#   create_grouping_matrix(
#     ~ swiftsFans + tag(swiftsFans:age_cat, "shaking"),
#     data = levels)
#   create_grouping_matrix(
#     ~ tag(swiftsFans:age_cat, "shaking"),
#     data = levels)
#
#   create_grouping_matrix(
#     ~ tag(swiftsFans, "shaking") + age_cat,
#     data = levels)
#
#   create_grouping_matrix(
#     ~ tag(swiftsFans, "shaking")*age_cat,
#     data = levels)
#
#   create_grouping_matrix(
#     ~ tag(swiftsFans*age_cat, "shaking"),
#     data = levels)
#
#   create_grouping_matrix(
#     ~ smithsFans + tag(swiftsFans*age_cat, "shaking"),
#     data = levels)
#
#   create_grouping_matrix(
#     ~ smithsFans + tag(swiftsFans, "shaking")*age_cat,
#     data = levels)
#
#   create_grouping_matrix(
#     ~ tag(smithsFans, "depression") + tag(swiftsFans, "shaking")*age_cat,
#     data = levels)
#
#   create_grouping_matrix(
#     ~ (tag(smithsFans, "depression") + tag(swiftsFans, "shaking")):age_cat,
#     data = levels)
#
#   create_grouping_matrix(
#     ~ tag(smithsFans, "depression"):age_cat + tag(swiftsFans, "shaking"):age_cat,
#     data = levels)
#
#   create_grouping_matrix(
#     ~ tag(smithsFans, "depression")*age_cat + tag(swiftsFans, "shaking"):age_cat,
#     data = levels)
#
#   create_grouping_matrix(
#     ~ tag(smithsFans + smithsFans:age_cat, "depression") +
#       tag(swiftsFans + swiftsFans:age_cat, c("depression", "shaking")),
#     data = levels)
#
#   create_groupings(
#     ~ tag(smithsFans + smithsFans:age_cat, "depression") +
#       tag(swiftsFans + swiftsFans:age_cat, c("depression", "shaking")),
#     data = levels)
#
#   create_grouping_matrix(
#     ~ tag(
#       (smithsFans + tag(swiftsFans, "shaking")):age_cat,
#       "depression"),
#     data = levels)


})

test_that("complicated example runs", {

  calendar_summary_plan <- ~

    # Within enrollment cohort
    tag(keep_1,
        c("enrollment_cohort", "enroll_on_index", "outcomes_enrollment")) +

    # within enrolled: (for weighting)
    #  - prevalent candidates
    #      - overall
    #      - by all age/gender crosses
    #  - incident candidates
    #      - overall
    #      - by all age/gender crosses
    tag(keep_2:(
      (prev_cand + inc_cand_1yr + inc_cand_all_past) +
        (prev_cand + inc_cand_1yr + inc_cand_all_past):(baseline_age_3cat*baseline_gender) ),
      c("hf_cohort", "enroll_on_index", "outcomes_hf")
    ) +

    # within enrolled365: (for figures)
    #  - prevalent candidates
    #      - overall
    #      - by all age/gender crosses
    #  - incident candidates
    #      - overall
    #      - by all age/gender crosses (with subgrouping by region, diabetes)
    tag(keep_3:(
      (prev_cand + inc_cand_1yr + inc_cand_all_past) +
        (prev_cand + inc_cand_1yr + inc_cand_all_past):
        ((baseline_age_3cat*baseline_gender) *
           (baseline_region +
              baseline_state +
              baseline_diabetes +
              baseline_renal +
              baseline_ascvd +
              baseline_copd +
              baseline_anemia))),
      c("hf_cohort", "enroll_365_lookback", "outcomes_hf")
    )  +
    # within enrolled365: (for table 1)
    #  - prevalent only
    #  - incident only
    tag(keep_3:(outcome_prv_hfdx + outcome_year_ind_hfdx),
        c("hf_cohort", "enroll_365_lookback", "covariates")) +
    #
    #  within enrolledAll: (for sensitivity analysis)
    tag(keep_4:(
      (prev_cand + inc_cand_1yr + inc_cand_all_past) +
        (prev_cand + inc_cand_1yr + inc_cand_all_past):(baseline_age_3cat*baseline_gender)),
      c("hf_cohort", "enroll_all_lookback", "outcomes_hf"))

  calendar_summary_dat <- list(
    keep_1     = TRUE,
    keep_2     = TRUE,
    unused     = TRUE,
    keep_3     = TRUE,
    keep_4     = TRUE,
    prev_cand  = TRUE,
    inc_cand_1yr      = TRUE,
    inc_cand_all_past = TRUE,
    outcome_prv_hfdx       = TRUE,
    outcome_year_ind_hfdx  = TRUE,
    baseline_age_3cat = c("18-54", "55-74", "75+"),
    baseline_gender   = c("M", "F"),
    baseline_region   = c("Northeast", "Midwest", "South", "West", "Unknown"),
    baseline_state    = c("CT", "ME", "MA", "NH", "RI", "VT", "NJ", "NY", "PA", "IL",
                          "IN", "MI", "OH", "WI", "IA", "KS", "MN", "MO", "NE", "ND",
                          "SD", "DE", "FL", "GA", "MD", "NC", "SC", "VA", "DC", "WV",
                          "AL", "KY", "MS", "TN", "AR", "LA", "OK", "TX", "AZ", "CO",
                          "ID", "MT", "NV", "NM", "UT", "WY", "AK", "CA", "HI", "OR",
                          "WA"),
    baseline_diabetes = c(TRUE, FALSE),
    baseline_renal    = c(TRUE, FALSE),
    baseline_ascvd    = c(TRUE, FALSE),
    baseline_copd     = c(TRUE, FALSE),
    baseline_anemia   = c(TRUE, FALSE)
  )

  test0 <- gofl::create_groupings(calendar_summary_plan, calendar_summary_dat)
  expect_is(test0, "list")
})
