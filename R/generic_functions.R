#' @export
output_name <- function(x){
  UseMethod("output_name", x)
}

output_name.bayesbench_cfg <- function(x){
  x$config_name
}

output_name.bayesbench_output <- function(x){
  output_name(x$cfg)
}


#' @export
output_directory<- function(x){
  UseMethod("output_directory", x)
}

output_directory.bayesbench_cfg <- function(x){
  x$output$directory
}

output_directory.bayesbench_output <- function(x){
  output_directory(x$cfg)
}

#' @export
output_type<- function(x){
  UseMethod("output_type", x)
}

output_type.bayesbench_cfg <- function(x){
  if(is.null(x$output$type)){
    return("zip")
  } else {
    return(x$output$type)
  }
}

output_type.bayesbench_output <- function(x){
  output_type(x$cfg)
}


#' @export
verbose<- function(x){
  UseMethod("verbose", x)
}

verbose.bayesbench_cfg <- function(x){
  TRUE
}

verbose.bayesbench_output <- function(x){
  verbose(x$cfg)
}

