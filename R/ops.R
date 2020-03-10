
sep <- "____"

gen_names <- function(n, x){
  paste0(n, sep, x)
}

split_names <- function(x){
  strsplit(x, split = sep)
}

as_matrix <- function(n, x){
  out <- Matrix::Diagonal(length(x), rep.int(1L, length(x)))
  dimnames(out) <- list(NULL, gen_names(n, x))
  out
}

as_matrices <- function(l){
  purrr::imap(l, ~ as_matrix(.y, .x))
}

collect_by_key <- function(l){

  out <- list()
  for(i in seq_along(l)){
    out[[l[[i]]$key]] <- c(out[[l[[i]]$key]],  l[[i]]$value)
  }
  out
}

collapse_by_or <- function(l){
  Reduce(function(x, y) {  rlang::quo( !! x | !! y) }, l)
}

get_symbol_text <- function(x){
  if (rlang::is_symbol(x)){
    x <- rlang::expr_text(x)
  } else if(is_zoom(x)) {
    x <- rlang::expr_text(x[[2]])
  } else if(is.character(x)) {
    x <- NULL
  } else {
    x[[1]] <- c
  }
  x
}

#' Creating a grouping matrix
#'
#' @param formula a RHS formula
#' @param data list where each element is a
#' @export

create_grouping_matrix <- function(formula, data){
  dat <- as_matrices(data)
  eval_expr(formula, data = dat, .f = tagger)
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
  # update data based on names used in formula

  # browser()
  data <- data[unique(eval_expr(formula, data, get_symbol_text))]

  grouping_mat <- create_grouping_matrix(formula, data)

  if(methods::is(grouping_mat, "tagged")){
    tags <- grouping_mat@tags
    grouping_mat <- grouping_mat@mat
  } else {
    tags <- vector("list", length = nrow(grouping_mat))
    grouping_mat <- grouping_mat
  }


  # make sure names are in order
  colnames <- purrr::flatten_chr(purrr::imap(data, ~ gen_names(.y, .x) ))
  grouping_mat <- grouping_mat[, colnames]

  # create intermediate quosures
  setnames <- rep(names(data), times = purrr::map_int(data, length))
  rhsquos  <- purrr::flatten(purrr::map(data, ~ purrr::map(.x, rlang::quo)))

  prepquos <- purrr::map2(setnames, rhsquos, ~ list(key = .x, value = .y))
  prepquos <- purrr::map(
    .x = prepquos,
    .f = ~ list(key = .x$key, value = rlang::quo(!!rlang::sym(.x$key) == !!.x$value))
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
  # gset <- apply(grouping_mat, 1, function(x) colnames[as.logical(x)])
  # gset <- apply(gset, 2, split_names)
  gset <- purrr::map(
    .x = gset,
    .f = ~ purrr::map(.x, ~ list(key = .x[1], value = .x[2])))
  gset <- purrr::map(gset, collect_by_key)

  out <- purrr::pmap(
    .l = list(gquos, gset, tags),
    .f = ~ list(q = ..1, g = ..2, tags = ..3))

  out
}
