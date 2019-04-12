#' @export
inference_engine_function <- function(x){
  UseMethod("inference_engine_function", x)
}

inference_engine_function.bayesbench_cfg <- function(x){
  ie <- x$inference_engine
  if(checkmate::test_choice(ie, choices = ls(getNamespace("bayesbenchr")))){
    eval(parse(text = paste0("ie_fun <- ", ie))) 
  } else if(checkmate::test_file_exists(ie)){
    tmpenv <- new.env()
    sys.source(file = ie, envir = tmpenv, toplevel.env = tmpenv)
    if(length(ls(tmpenv)) > 1) stop("Too many objects in ", ie, call. = FALSE)
    eval(parse(text = paste0("ie_fun <- ", ls(tmpenv)))) 
    message("Using ", ls(tmpenv), " as inference_engine.")
  } else {
    stop("Incorrect inference_engine: ", ie, call. = FALSE)
  }
  assert_inference_engine_function(ie_fun)
  ie_fun
}

assert_inference_engine_function <- function(ie_fun){
  checkmate::assert_function(ie_fun, args = "cfg", nargs = 1)
}

inference_engine_function.bayesbench_job_cfg <- function(x){
  inference_engine_function.bayesbench_cfg(x)
}

#' @export
config_name <- function(x){
  UseMethod("config_name", x)
}

config_name.bayesbench_cfg <- function(x){
  x$config_name
}

config_name.bayesbench_job_cfg <- function(x){
  NextMethod()
}

config_name.bayesbench_output <- function(x){
  config_name(x$cfg)
}


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

