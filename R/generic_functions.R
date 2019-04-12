#' @export
output_directory<- function(x){
  UseMethod("output_directory", x)
}

output_directory.bayesbench_cfg <- function(x){
  x$output_directory
}

output_directory.bayesbench_output <- function(x){
  output_directory(x$cfg)
}