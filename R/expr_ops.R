
#' @importFrom rlang is_symbol
#' @noRd

is_leafish <- function(x){
  rlang::is_symbol(x) || is_zoom(x[[1]])
}

#' @importFrom rlang expr_name
#' @noRd
is_tagger <- function(x){
  !rlang::is_symbol(x) && rlang::expr_name(x[[1]]) == "tag" && length(x) == 3
}

is_zoom <- function(x){
  grepl("^\\.zoom", rlang::expr_name(x))
}

is_cross <- function(x){
  rlang::expr_name(x) == ":"
}

is_product <- function(x){
  rlang::expr_name(x) == "*"
}

is_sum <- function(x){
  rlang::expr_name(x) == "+"
}

examine_expr <- function(expr){

  # Exit if nullary, unary or zoom
  if(is_leafish(expr)){
    return(expr)
  } else if (is_product(expr[[1]])){
    expr[[1]] <- `%pw%`
  } else if (is_sum(expr[[1]])){
    expr[[1]] <- `%+%`
  } else if(is_cross(expr[[1]])){
    expr[[1]] <- `%cp%`
  }

  expr
}

traverse_expr <- function(expr, f){
  if (length(expr) == 1 || is_zoom(expr[[1]]) ){
    expr
  } else {
    expr[-1] <- lapply(expr[-1], function(x) examine_expr(f(traverse_expr(x, f))))
 }
  expr
}

#' @importFrom rlang eval_tidy is_bare_formula
#' @noRd

eval_expr <- function(expr, data, .f){
  stopifnot(rlang::is_expression(expr))
  rlang::eval_tidy(traverse_expr(expr, f = .f)[[2]], data = data)
}
