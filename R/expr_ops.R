
#' @importFrom rlang is_symbol
#' @noRd

is_leafish <- function(x){
  rlang::is_symbol(x) || is_zoom(x[[1]])
}

#' @importFrom rlang expr_name
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

# is_sum_of_products <- function(x){
#   if(!is_sum(x[[1]])){
#     return(FALSE)
#   }
#
#   if(is_product(x[[2]][[1]]) && is_product(x[[3]][[1]])){
#     el1  <- purrr::map_chr(purrr::keep(x[[2]][-1], is_rootish), expr_name)
#     el2  <- purrr::map_chr(purrr::keep(x[[3]][-1], is_rootish), expr_name)
#
#     any(el1 %in% el2)
#
#   } else {
#     FALSE
#   }
#
# }


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

traverse_expr <- function(expr){
  if (length(expr) == 1 || is_zoom(expr) ){
    expr
  } else {
    expr[-1] <- lapply(expr[-1], function(x) examine_expr(traverse_expr(x)))
 }
  expr
}

#' @importFrom rlang eval_tidy is_bare_formula
#' @noRd

eval_expr <- function(expr, data){
  stopifnot(rlang::is_bare_formula(expr))
  rlang::eval_tidy(traverse_expr(expr)[[2]], data = data)
}
