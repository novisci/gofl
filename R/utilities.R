
as_bin <- function(x){
  sum(2^(0:(length(x)-1)) * x)
}

bin_rep <- function(x){
  apply(x, 1, as_bin)
}
