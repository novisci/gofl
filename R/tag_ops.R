
tag <- function(expr, label){ }

strip_tagger <- function(x){
  if (!is_tagger(x)){ return(x) }
  x <- match.call(tag, x)
  # remove call to tag() with its expr
  x[["expr"]]
}

get_label <- function(x){
  x <- match.call(tag, x)
  # remove call to tag() with its expr
  x[["label"]]
}

init_tags <- function(x, n){
  # browser()
  replicate(n, x, simplify = FALSE)
}

empty_tags <- function(n){

  vector("list", n)
}

cross_tags <- function(x, y){
  hold <- purrr::cross2(x, y)
  hold <- purrr::map(hold, ~ Reduce(c, .x))
  hold[c(seq.int(1, (length(x)*length(y)+((length(x) == 1))), by = 2L),
         seq.int(2, (length(x)*length(y)), by = 2L))]
}

left_tags <- function(x, y){
  init_tags(x[[1]], length(x) * length(y))
}

right_tags <- function(x, y){
  init_tags(y[[1]], length(x) * length(y))
}

cross_tags2 <- function(x, y){
  hold <- cross_tags(x, y)
  c(init_tags(hold[[1]], length(x) + length(y)), hold)
}

left_tags2 <- function(x, y){
  # hold <- left_tags(x, y)
  c(init_tags(x[[1]], length(x) + length(y)),
    init_tags(x[[1]], length(x) * length(y)))
}

right_tags2 <- function(x, y){
  c(init_tags(y[[1]], length(x) + length(y)),
    init_tags(y[[1]], length(x) * length(y)))
}

replace_by_size <- function(x){
  if (is_zoom(x)) {
    get_zoom_size(x)
  } else if (!rlang::is_symbol(x)){
    strip_tagger(x)
  } else {
    call("nrow", x)
  }
}

tagger <- function(x){
  if (is_tagger(x)){
    # browser()
    x[[1]] <- methods::new
    x[[4]] <- call("init_tags", x[[3]], call("nrow", x[[2]]))
    x[[3]] <- x[[2]]
    x[[2]] <- "tagged"
    names(x) <- c("", "Class", "mat", "tags")
  }
  x
}


