#' Internal matrix operators
#' @importFrom Matrix bdiag sparseMatrix kronecker
#' @noRd
`%+%` <- function(x, y){

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
`%cp%` <- function(x, y){
  out <- kronecker(x, rep.int(1, nrow(y))) %||% kronecker(rep.int(1, nrow(x)), y)
  dimnames(out) <- list(NULL, c(dimnames(x)[[2]], dimnames(y)[[2]]))
  out
}

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
`%pw%` <- function(x, y){
    (x %+%  y)  %<>%
    (x %cp% y)
}

#' Filter to a particular level
#' @noRd
.zoom <- function(x, levels = 1){
  Matrix::sparseMatrix(i = rep.int(1, length(levels)),
                       j = levels,
                       dimnames = dimnames(x),
                       x = 1L, dims = c(1L, ncol(x)))
}


#' An internal function for creating all no-way, 1-way, 2-way, ... n-way
#' combinations from a set of identity matrices.
#' @noRd
.allway <- function(...){
  dots <- list(...)
  Reduce("%p0r%", dots, init = Matrix::sparseMatrix({}, {}, dims = c(1, 0))) %<>%
  Reduce(f = "%pw%", dots)
}


