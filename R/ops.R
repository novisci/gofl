
gen_names <- function(n, x){
  paste0(n, "____", x)
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

#' Creating a grouping matrix
#'
#' @param formula a RHS formula
#' @param data list where each element is a
#' @export

create_grouping_matrix <- function(formula, data){
  dat <- as_matrices(data)

  eval_expr(formula, data = dat)
}

#' Create a list of filtration quosures
#'
#' Each element is a list of quosures to be used in a filter statement. The list
#' of quosures that can be used in a \code{\link[dplyr]{filter}} call, e.g.,
#' \code{filter(!!! flist)}.
#'
#' @inheritParams create_grouping_matrix
#' @export

create_grouping_quos <- function(formula, data){

  grouping_mat <- create_grouping_matrix(formula, data)

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

  # create final quosures
  gquos <- apply(grouping_mat, 1, function(x) prepquos[as.logical(x)])
  gquos <- purrr::map(gquos, collect_by_key)
  gquos <- purrr::map(gquos, ~ unname(purrr::map(.x, collapse_by_or)))
  gquos
}
