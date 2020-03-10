#' An internal representative of tagged matrices
#' @noRd
setClass(
  "tagged",
  slots = c(
    mat = "dMatrix",
    tags = "list"
  )
)

#' Internal matrix operators
#' @importFrom Matrix bdiag sparseMatrix kronecker
#' @noRd

setGeneric("%+%", function(x, y) standardGeneric("%+%"))

setMethod(
  "%+%",
  signature = c("dMatrix", "dMatrix"),
  function(x, y){

    # browser()
    xcol <- dimnames(x)[[2]]
    ycol <- dimnames(y)[[2]]
    if(any(xcol %in% ycol)){
      out <- x %<n>% y
      out <- out[!duplicated(bin_rep(x)), ]
    } else {
      out <- Matrix::bdiag(x, y)
      dimnames(out) <- list(NULL, c(xcol, ycol))
    }

    out
  }
)

setMethod(
  "%+%",
  signature = c("integer", "integer"),
  function(x, y){ x + y }
)

do_tagged_op <- function(mat_op, tag_op, xmat, ymat, xtags, ytags){
  # browser()
  methods::new(
    "tagged",
     mat  = mat_op(xmat, ymat),
     tags = tag_op(xtags, ytags))
}

setMethod(
  "%+%",
  signature = c("tagged", "tagged"),
  function(x, y){ do_tagged_op(`%+%`, c, x@mat, y@mat, x@tags, y@tags) }
)

setMethod(
  "%+%",
  signature = c("dMatrix", "tagged"),
  function(x, y){ do_tagged_op(`%+%`, c, x, y@mat, empty_tags(nrow(x)), y@tags) }
)

setMethod(
  "%+%",
  signature = c("tagged", "dMatrix"),
  function(x, y){ do_tagged_op(`%+%`, c, x@mat, y, x@tags, empty_tags(nrow(y))) }
)

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

#' pairwise combinations
#' @noRd
setGeneric("%cp%", function(x, y) standardGeneric("%cp%"))

setMethod(
  "%cp%",
  signature = c("dMatrix", "dMatrix"),
  function(x, y){
    out <- kronecker(x, rep.int(1, nrow(y))) %||% kronecker(rep.int(1, nrow(x)), y)
    dimnames(out) <- list(NULL, c(dimnames(x)[[2]], dimnames(y)[[2]]))
    out
  }
)

setMethod(
  "%cp%",
  signature = c("integer", "integer"),
  function(x, y){ x * y }
)


setMethod(
  "%cp%",
  signature = c("tagged", "tagged"),
  function(x, y){ do_tagged_op(`%cp%`, cross_tags, x@mat, y@mat, x@tags, y@tags) }
)

setMethod(
  "%cp%",
  signature = c("dMatrix", "tagged"),
  function(x, y){ do_tagged_op(`%cp%`, right_tags, x, y@mat, empty_tags(nrow(x)), y@tags) }
)

setMethod(
  "%cp%",
  signature = c("tagged", "dMatrix"),
  function(x, y){ do_tagged_op(`%cp%`, left_tags, x@mat, y, x@tags, empty_tags(nrow(y))) }
)

#' rbind operator
#' @noRd
`%<>%` <- function(x, y){
  out <- rbind(x, y)
  dimnames(out) <- dimnames(x)
  out
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

#' all marginal and pairwise combinations
#' @noRd
setGeneric("%pw%", function(x, y) standardGeneric("%pw%"))

setMethod(
  "%pw%",
  signature = c("dMatrix", "dMatrix"),
  function(x, y){ (x %+%  y) %<>% (x %cp% y) }
)

setMethod(
  "%pw%",
  signature = c("integer", "integer"),
  function(x, y){ x+y + (x*y) }
)

setMethod(
  "%pw%",
  signature = c("tagged", "tagged"),
  function(x, y){ do_tagged_op(`%pw%`, cross_tags2, x@mat, y@mat, x@tags, y@tags) }
)

setMethod(
  "%pw%",
  signature = c("dMatrix", "tagged"),
  function(x, y){ do_tagged_op(`%pw%`, right_tags2, x, y@mat, empty_tags(nrow(x)), y@tags) }
)

setMethod(
  "%pw%",
  signature = c("tagged", "dMatrix"),
  function(x, y){ do_tagged_op(`%pw%`, left_tags2, x@mat, y, x@tags, empty_tags(nrow(y))) }
)


#' Subset to particular levels
#' @noRd
.zoom <- function(x, levels = 1){
  Matrix::sparseMatrix(i = rep.int(1, length(levels)),
                       j = levels,
                       dimnames = dimnames(x),
                       x = 1L, dims = c(1L, ncol(x)))
}

z <- .zoom

get_zoom_size <- function(expr){
  f <- match.call(.zoom, expr)
  length(f[["levels"]])
}


#' An internal function for creating all no-way, 1-way, 2-way, ... n-way
#' combinations from a set of identity matrices.
#' @noRd
.allway <- function(...){
  dots <- list(...)
  Reduce("%p0r%", dots, init = Matrix::sparseMatrix({}, {}, dims = c(1, 0))) %<>%
  Reduce(f = "%pw%", dots)
}


