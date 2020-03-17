
as_bin <- function(x){
  sum(2^(0:(length(x)-1)) * x)
}

bin_rep <- function(x){
  apply(x, 1, as_bin)
}

col_positions <- function(x){
  out <- vector(mode = "list", length = nrow(x))
  for(i in seq_along(out)){
    out[[i]] <- which(as.logical(x[i, ]))
  }
  out
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


sep <- "____"

gen_names <- function(n, x){
  paste0(n, sep, x)
}

split_names <- function(x){
  strsplit(x, split = sep)
}

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

create_index <- function(grouping, data){
  which_keys <- match(names(grouping), names(data))
  index_values <- data[which_keys]
  init <- rep.int(0, length(data))
  idx_vals <- purrr::map2(index_values, grouping, ~ match(.y, .x))

  # collapse claims where the value in an index position has > 1 value
  idx_vals <- purrr::map_chr(idx_vals, ~ paste(.x, collapse = "_"))
  init[which_keys] <- idx_vals
  paste(init, collapse = "-")
}

