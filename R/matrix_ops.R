#' Internal matrix operators
#' @importFrom Matrix bdiag sparseMatrix kronecker
#' @noRd

`%+%` <- function(x, y){
  Matrix::bdiag(x, y)
}

`%||%` <- function(x, y){
  cbind(x, y)
}

`%rp%` <- function(x, y){
  x %||% Matrix::sparseMatrix(i = {}, j = {}, dims = c(nrow(x), ncol(y)))
}

`%cp%` <- function(x, y){
  kronecker(x, rep.int(1, nrow(y))) %||% kronecker(rep.int(1, nrow(x)), y)
}

`%<>%` <- function(x, y){
  rbind(x, y)
}

pairway <- function(x, y){
    (x %+%  y)  %<>%
    (x %cp% y)
}

#' An internal function for creating all no-way, 1-way, 2-way, ... n-way
#' combinations from a set of identity matrices.

.allway <- function(...){
  dots <- list(...)
  Reduce("%rp%", dots, init = Matrix::sparseMatrix({}, {}, dims = c(1, 0))) %<>%
  Reduce(f = pairway, dots)
}


