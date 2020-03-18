#' An internal representative of tagged matrices
#' @noRd
setClass(
  "tagged",
  slots = c(
    mat = "dMatrix",
    tags = "list"
  )
)

apply_op_tagged <- function(op, x, y){
  methods::new("tagged", mat = op(x@mat, y@mat), tags = op(x@tags, y@tags) )
}

setMethod("nrow", "tagged", function(x) nrow(x@mat))

#' Initialize a tagged object for category levels
#' @param n name of category
#' @param x vector of category levels
#' @noRd
as_tmatrix <- function(n, x){
  mat <- Matrix::Diagonal(length(x), rep.int(1L, length(x)))
  dimnames(mat) <- list(NULL, gen_names(n, x))
  methods::new("tagged", mat = mat, tags = empty_tags(length(x)))
}

as_tmatrices <- function(l){
  purrr::imap(l, ~ as_tmatrix(.y, .x))
}


## Matrix utilities ####

#' cbind
#' @noRd
`%||%` <- function(x, y){
  cbind(x, y)
}

#' pad 0 to the left
#' @noRd
`%p0l%` <- function(x, y){
  Matrix::sparseMatrix(i = {}, j = {},
                       dims = c(nrow(y), ncol(x)),
                       dimnames = list(NULL, dimnames(x)[[2]])) %||% y
}

#' pad 0 to the right
#' @noRd
`%p0r%` <- function(x, y){
  x %||% Matrix::sparseMatrix(i = {}, j = {},
                              dims = c(nrow(x), ncol(y)),
                              dimnames = list(NULL, dimnames(y)[[2]]))
}

#' cbind by name
#' @noRd
`%<n>%` <- function(x, y){

  xcol <- dimnames(x)[[2]]
  ycol <- dimnames(y)[[2]]
  inboth <- intersect(xcol, ycol)
  xonly  <- setdiff(xcol, inboth)
  yonly  <- setdiff(ycol, inboth)

  x <- x[ , c(xonly, inboth), drop = FALSE] %p0r% y[ , yonly, drop = FALSE]
  y <- x[ , xonly, drop = FALSE] %p0l% y[ , c(inboth, yonly), drop = FALSE]

  out  <- rbind(x, y)
  dimnames(out) <- list(NULL, c(setdiff(xcol, inboth),
                                inboth,
                                setdiff(ycol, inboth)))
  out
}

#' sum operators
#' @importFrom Matrix bdiag sparseMatrix kronecker
#' @noRd

setGeneric("%s%", function(x, y) standardGeneric("%s%"))

setMethod(
  "%s%",
  signature = c("dMatrix", "dMatrix"),
  function(x, y){
    xcol <- dimnames(x)[[2]]
    ycol <- dimnames(y)[[2]]
    if (any(xcol %in% ycol)){
      out <- x %<n>% y
      # 20200313 - if the matrix is deduped here then the tags and the matrix
      # gets out of sync. I'm pushing this further upstream to create_grouping_matrix
      # for now. However,
      # TODO: it would be great to avoid the deduping altogether.
      # out <- out[!duplicated(bin_rep(x)), ]
    } else {
      out <- Matrix::bdiag(x, y)
      dimnames(out) <- list(NULL, c(xcol, ycol))
    }
    out
  }
)

setMethod("%s%", signature = c("integer", "integer"), function(x, y){ x + y })
setMethod("%s%", signature = c("list", "list"), function(x, y){ c(x, y) } )
setMethod("%s%", signature = c("tagged", "tagged"),
  function(x, y){ apply_op_tagged(`%s%`, x, y) }
)

#' product operators
#' @noRd
setGeneric("%p%", function(x, y) standardGeneric("%p%"))

setMethod(
  "%p%",
  signature = c("dMatrix", "dMatrix"),
  function(x, y){
    out <- kronecker(x, rep.int(1, nrow(y))) %||% kronecker(rep.int(1, nrow(x)), y)
    dimnames(out) <- list(NULL, c(dimnames(x)[[2]], dimnames(y)[[2]]))
    out
  }
)

setMethod("%p%", signature = c("integer", "integer"), function(x, y){ x * y })

setMethod(
  "%p%",
  signature = c("tagged", "tagged"),
  function(x, y){ apply_op_tagged(`%p%`, x, y) }
)

setMethod(
  "%p%",
  signature = c("list", "list"),
  function(x, y){
    # browser()
    hold <- purrr::cross2(x, y)
    hold <- purrr::map(hold, ~ Reduce(c, .x))
    hold[c(seq.int(1, (length(x)*length(y)), by = 2L),
           seq.int(2, (length(x)*length(y)), by = 2L))]
  }
)

#' all marginal and pairwise combinations
#' @noRd
setGeneric("%ssp%", function(x, y) standardGeneric("%ssp%"))

setMethod(
  "%ssp%",
  signature = c("ANY", "ANY"),
  function(x, y){ (x %s% y) %s% (x %p% y) }
)

#------------------------------------------------------------------------------#
# Zoom functions ####

#' Subset to particular levels
#' @noRd
.zoom <- function(x, levels = 1){

  # browser()
  mat <- Matrix::sparseMatrix(i = rep.int(1, length(levels)),
                       j = levels,
                       dimnames = dimnames(x@mat),
                       x = 1L, dims = c(1L, ncol(x@mat)))
  methods::new("tagged", mat = mat, tags = empty_tags(nrow(mat)))
}

z <- .zoom

get_zoom_size <- function(expr){
  f <- match.call(.zoom, expr)
  length(f[["levels"]])
}

#------------------------------------------------------------------------------#
# Expression operations ####

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

is_product <- function(x){
  rlang::expr_name(x) == ":"
}

is_sumproduct <- function(x){
  rlang::expr_name(x) == "*"
}

is_sum <- function(x){
  rlang::expr_name(x) == "+"
}

examine_expr <- function(expr){
  # Exit if nullary, unary or zoom
  if(is_leafish(expr)){
    return(expr)
  } else if (is_sumproduct(expr[[1]])){
    expr[[1]] <- `%ssp%`
  } else if (is_sum(expr[[1]])){
    expr[[1]] <- `%s%`
  } else if(is_product(expr[[1]])){
    expr[[1]] <- `%p%`
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

#------------------------------------------------------------------------------#
# Tagging functions ####

tag <- function(x, label){
  methods::new(
    "tagged",
    mat  = x@mat,
    tags = purrr::map(x@tags,~  c(.x, label))
  )
}

strip_tagger <- function(expr){
  if (!is_tagger(expr)){ return(expr) }
  expr <- match.call(tag, expr)
  # remove call to tag() with its expr
  expr[["x"]]
}

get_label <- function(x){
  x <- match.call(tag, x)
  x[["label"]]
}

init_tags <- function(x, n){
  replicate(n, x, simplify = FALSE)
}

empty_tags <- function(n){
  vector("list", n)
}

#------------------------------------------------------------------------------#
# expression tree functions ####

replace_by_size <- function(x){
  if (is_zoom(x)) {
    get_zoom_size(x)
  } else if (!rlang::is_symbol(x)){
    strip_tagger(x)
  } else {
    call("nrow", x)
  }
}

