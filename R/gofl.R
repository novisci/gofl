#' gofl
#'
#' A simple domain specific language for grouping, organizing, and filtering.

'_PACKAGE'


#' Creating a grouping matrix
#'
#' @param formula a RHS formula
#' @param data list where each element is a
#' @export

create_grouping_matrix <- function(formula, data){
  dat <- as_tmatrices(data)
  out <- eval_expr(formula, data = dat, .f = identity)

  # browser()
  # TODO: it would great to handle any duplicates within the operations rather
  #       than this stage
  duped <- duplicated(col_positions(out@mat))
  out@mat  <- reorder_columns_by_names(out@mat[!duped, ], names(data))
  out@tags <- out@tags[!duped]
  out
}

#' Create a list of filtration quosures
#'
#' Each element is a list of quosures to be used in a filter statement. The list
#' of quosures that can be used in a \code{\link[dplyr]{filter}} call, e.g.,
#' \code{filter(!!! flist)}.
#'
#' @inheritParams create_grouping_matrix
#' @importFrom methods is
#' @export

create_groupings <- function(formula, data){

  assertthat::assert_that(
    rlang::is_named(data),
    msg = "All the elements of data must be named."
  )

  data_names <- names(data)

  # keep only data elements that are used in the formula
  data <- data[unique(eval_expr(formula, data, get_symbol_text))]

  # sort the data according to the order of the incoming data
  data <- data[data_names[data_names %in% names(data)]]

  # make all the data values character for consistency
  # data <- purrr::map(data, as.character)


  grouping_mat <- create_grouping_matrix(formula, data)

  tags <- grouping_mat@tags
  grouping_mat <- grouping_mat@mat


  # TODO: there's a lot that could be cleaned up and refactor
  # make sure names are in order
  colnames <- purrr::flatten_chr(purrr::imap(data, ~ gen_names(.y, .x) ))
  grouping_mat <- grouping_mat[, colnames]

  # create intermediate quosures
  setnames <- rep(names(data), times = purrr::map_int(data, length))
  rhsquos  <- purrr::flatten(purrr::map(data, ~ purrr::map(.x, rlang::expr)))

  prepquos <- purrr::map2(setnames, rhsquos, ~ list(key = .x, value = .y))
  prepquos <- purrr::map(
    .x = prepquos,
    .f = ~ {
      list(key = .x$key,
           value = rlang::quo(!!rlang::sym(.x$key) == !!.x$value))
    }
  )

  # TODO: there's a bit of redundancy that could be cleaned up/functionized here
  # create final quosures
  gquos <- apply(grouping_mat, 1, function(x) prepquos[as.logical(x)])
  gquos <- purrr::map(gquos, collect_by_key)
  gquos <- purrr::map(gquos, ~ unname(purrr::map(.x, collapse_by_or)))

  # browser()
  # create grouping key/value
  colnames <- matrix(colnames, nrow = 1)
  gset <- purrr::map(
    .x = 1:nrow(grouping_mat),
    .f = ~ colnames[as.logical(grouping_mat[.x, ])])

  gset <- purrr::map(gset, split_names)
  gset <- purrr::map(
    .x = gset,
    .f = ~ purrr::map(.x, ~ list(key = .x[1], value = .x[2])))
  gset <- purrr::map(gset, collect_by_key)


  idxs <- purrr::map(gset, ~ create_index(.x, data))
  grps <- purrr::pmap(
    .l = list(idxs, gquos, gset, tags),
    .f = ~ list(i = ..1, q = ..2, g = ..3, tags = ..4))

  list(
    data      = data,
    groupings = grps
  )
}
