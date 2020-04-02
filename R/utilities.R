
as_bin <- function(x){
  sum(2^(0:(length(x)-1)) * x)
}

bin_rep <- function(x){
  apply(x, 1, as_bin)
}

#' For each row in a matrix, identify which columns have a
#' @param x a matrix of 0s and 1s
#' @return a list of \code{nrow(x)} where each element is an integer vector of
#'      column positions that have a 1 for each matrix row.
#' @keywords internal
col_positions <- function(x){
  out <- vector(mode = "list", length = nrow(x))
  for(i in seq_along(out)){
    out[[i]] <- which(as.logical(x[i, ]))
  }
  out
}

#' Collect items in a list by key
#'
#' @param l a list
#' @keywords internal

collect_by_key <- function(l){
  out <- list()
  for(i in seq_along(l)){
    out[[l[[i]]$key]] <- c(out[[l[[i]]$key]],  l[[i]]$value)
  }
  out
}

#' Combine list elements by or'ing them together
#' @param l
#' @keywords internal
collapse_by_or <- function(l){
  Reduce(function(x, y) {  rlang::quo( !! x | !! y) }, l)
}

#' TODO
#' @keywords internal
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


sep <- "____"

gen_names <- function(n, x){
  paste0(n, sep, x)
}

split_names <- function(x){
  strsplit(x, split = sep)
}

#' Reorder a matrix with column names by a character vector
#'
#' @param mat a \code{matrix} with \code{colnames} that may contain \code{sep}
#' @param names a \code{character} vector by which to reorder the columns.
#'      hese are the grouping keys of the columns (the string before \code{sep})
#' @return a \code{matrix} with the same data but reordered columns
#' @keywords internal
reorder_columns_by_names <- function(mat, names){
  cnames <- colnames(mat)
  vnames <- gsub(paste0(sep, ".*"), "", cnames)
  x <- rle(vnames)
  x <- x$lengths[match(names, x$values)]
  from_pos <- match(names, vnames)

  new_pos <- unlist(Map(function(x, y) { sort(x:(x+y-1)) },
                        from_pos, x))
  mat[ , new_pos]

}

#' Create an index for a grouping based on the variables available for the gofl
#'
#' @param grouping a named list of variable values that define a grouping. The
#'        names of this list should also be names in \code{data}
#' @param data a named list of all valid variable values that can be used to
#'        define a grouping. The names of this list are the variable names
#' @keywords internal

create_index <- function(grouping, data){

  grp_names <- names(grouping)
  dat_names <- names(data)

  # check for valid variables
  assertthat::assert_that(
    all(grp_names %in% dat_names),
    msg = sprintf("%s are not available variables by which to define a grouping",
                  paste(grp_names[!(grp_names %in% dat_names)], collapse = ","))
  )

  which_keys <- match(grp_names, dat_names)
  index_values <- data[which_keys]

  # check for valid variable levels
  for (i in seq_along(grouping)) {

    glev <- grouping[[i]]
    dlev <- index_values[[i]]
    assertthat::assert_that(
      all(glev %in% dlev),
      msg = sprintf("%s are not valid levels for variable %s.",
                    paste(glev[!(glev %in% dlev)], collapse = ", "),
                    names(index_values)[i])
    )
  }

  init <- rep.int(0, length(data))
  idx_vals <- purrr::map2(index_values, grouping, ~ match(.y, .x))

  # collapse claims where the value in an index position has > 1 value
  idx_vals <- purrr::map_chr(idx_vals, ~ paste(.x, collapse = "_"))
  init[which_keys] <- idx_vals
  paste(init, collapse = "-")
}

#'
#'
create_index_maker <- function(data){
  force(data)
  force(create_index)
  function(...){
    dots <- list(...)
    create_index(dots, data = data)
  }
}
