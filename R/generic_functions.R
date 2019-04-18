
#' @export
bayesbench_cfg_as_data.frame <- function(x){
  if(is.null(x)) return(NULL)
  UseMethod("bayesbench_cfg_as_data.frame", x)
}

#' @export
bayesbench_cfg_as_data.frame.bayesbench_cfg <- function(x){
  as.data.frame.bayesbench_cfg(x)
}
#' @export
bayesbench_cfg_as_data.frame.bayesbench_job_cfg <- function(x){
  as.data.frame.bayesbench_cfg(x)
}
#' @export
bayesbench_cfg_as_data.frame.bayesbench_output <- function(x){
  as.data.frame.bayesbench_cfg(x$cfg)
}

  

as.data.frame.bayesbench_cfg <- function(x, row.names = NULL, optional = FALSE, ...){
  job_cfgs <- expand_bayesbench_cfg_to_job_cfgs(list(x))
  do.call(rbind, lapply(job_cfgs, as.data.frame.bayesbench_job_cfg))
}

as.data.frame.bayesbench_job_cfg <- function(x, row.names = NULL, optional = FALSE, ...){
  
  to_df <- x[!unlist(lapply(x, is.list))]
  to_df$diagnostics <- paste(to_df$diagnostics, collapse = "; ")
  
  # Add ie arguments
  ie_pos <- which(names(to_df) == "inference_engine")
  ieargs <- x$inference_engine_arguments
  names(ieargs) <- paste0("inference_engine.", names(ieargs))
  to_df <- c(to_df[1:ie_pos], ieargs, to_df[(ie_pos+1):length(to_df)])
  
  # Add outputs
  outpt <- c(x$output, list(output_name = output_name.bayesbench_cfg(x)), list(output_file_path = output_file_path.bayesbench_job_cfg(x)))
  names(outpt) <- paste0("output.", names(outpt))
  to_df <- c(to_df,outpt)
  as.data.frame(to_df, stringsAsFactors = FALSE)
}

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
    if(length(ls(tmpenv)) > 1) stop("Too many objects in ", ie, ". Only one function (inference engine) should be supplied.", call. = FALSE)
    ie_fun <-  tmpenv[[ls(tmpenv)]]
    message("Using ", ls(tmpenv), "() as inference_engine.")
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
output_file_path <- function(x){
  UseMethod("output_file_path", x)
}

output_file_path.bayesbench_cfg <- function(x){
  job_cfgs <- expand_bayesbench_cfg_to_job_cfgs(list(x))
  unlist(lapply(job_cfgs, output_file_path.bayesbench_job_cfg))
}

output_file_path.bayesbench_job_cfg <- function(x){
  output_dir <- output_directory(x)
  if(is.null(output_dir)) return(NULL)
  file_ext <- output_type(x)
  if(file_ext == "zip") file_ext <- "json.zip"
  paste0(file.path(output_dir, output_name(x)), ".", file_ext)
}

output_file_path.bayesbench_output <- function(x){
  output_file_path(x$cfg)
}

output_file_exist <- function(x){
  UseMethod("output_file_exist", x)
}
output_file_exist.bayesbench_cfg <- function(x){
  ofp <- output_file_path(x)
  !is.null(ofp) && file.exists(ofp)
}
output_file_exist.bayesbench_job_cfg <- function(x){
  output_file_exist.bayesbench_cfg(x)
}
output_file_exist.bayesbench_output <- function(x){
  output_file_exist.bayesbench_cfg(x$cfg)
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
#' @export
'output_directory<-' <- function(x, value){
  UseMethod('output_directory<-', x)
}

output_directory.bayesbench_cfg <- function(x){
  x$output$directory
}
'output_directory<-.bayesbench_cfg' <- function(x, value){
  if(is.null(x$output)){
    x$output <- list(directory = value)
  } else {
    x$output$directory <- value
  }
  bayesbench_cfg(x)
  return(x)
}

output_directory.bayesbench_job_cfg <- function(x){
  output_directory.bayesbench_cfg(x)
}
'output_directory<-.bayesbench_job_cfg' <- function(x, value){
  x <- `output_directory<-.bayesbench_cfg`(x, value)
  assert_bayesbench_job_cfg(x)
  return(x)
}

output_directory.bayesbench_output <- function(x){
  output_directory(x$cfg)
}
'output_directory<-.bayesbench_output' <- function(x, value){
  output_directory(x$cfg) <- value
  assert_bayesbench_output(x)
  return(x)
}

#' @export
output_type<- function(x){
  UseMethod("output_type", x)
}
#' @export
'output_type<-' <- function(x, value){
  UseMethod('output_type<-', x)
}

output_type.bayesbench_cfg <- function(x){
  x$output$type
}
'output_type<-.bayesbench_cfg' <- function(x, value){
  if(is.null(x$output)){
    x$output <- list(type = value)
  } else {
    x$output$type <- value
  }
  assert_bayesbench_cfg(x)
  return(x)
}

output_type.bayesbench_job_cfg <- function(x){
  output_type.bayesbench_cfg(x)
}
'output_type<-.bayesbench_job_cfg' <- function(x, value){
  x <- `output_type<-.bayesbench_cfg`(x, value)
  x <- bayesbench_job_cfg(x)
  return(x)
}

output_type.bayesbench_output <- function(x){
  output_type(x$cfg)
}
'output_type<-.bayesbench_output' <- function(x, value){
  output_type(x$cfg) <- value
  assert_bayesbench_output(x)
  return(x)
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


#' @export
as.bayesbench_job_cfg<- function(x){
  UseMethod("as.bayesbench_job_cfg", x)
}
#' @export
as.bayesbench_job_cfg.bayesbench_cfg <- function(x){
  expand_bayesbench_cfg_to_job_cfgs(list(x))
}
#' @export
as.bayesbench_job_cfg.bayesbench_job_cfg <- function(x){
  x
}
