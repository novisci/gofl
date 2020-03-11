
as_bin <- function(x){
  sum(2^(0:(length(x)-1)) * x)
}

bin_rep <- function(x){
  apply(x, 1, as_bin)
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
