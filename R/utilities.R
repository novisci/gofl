as_bin <- function(x){
  sum(2^(0:(length(x)-1)) * x)
}
