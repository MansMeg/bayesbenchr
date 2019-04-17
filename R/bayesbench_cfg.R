#' Bayesbench config
#' 
#' @details 
#' Creates a Bayesbench configuration file. The only 
#' 
#' @param ... Bayesbench parameters to set.
#' @param file_path File path to bayesbench configuration file.
#' 
#' @examples 
#' file_path <- system.file("extdata", "examples", "test_8schools_advi.yml", package = "bayesbenchr")
#' x <- read_bayesbench_cfg_from_file(file_path)
#' 
#' @export
bayesbench_cfg <- function(...){
  cfg <- list(...)
  # If a bayesbench_cfg is supplied
  if(length(cfg) == 1 && checkmate::test_class(cfg[[1]], "bayesbench_cfg")){
    assert_bayesbench_cfg(cfg[[1]])
    return(cfg[[1]])
  }
  
  # If a list with arguments is supplied
  if(length(cfg) == 1 && checkmate::test_list(cfg[[1]])){
    cfg <- cfg[[1]]
  }
  
  checkmate::assert_list(cfg)
  checkmate::assert_names(names(cfg), must.include = c("inference_engine", "posterior_name"))
  
  if(is.null(cfg$config_name)) {
    cfg$config_name <- make.names(paste0("temp_config_", Sys.time()))
    cfg <- cfg[c(length(cfg), 1:(length(cfg)-1))]
  }
  class(cfg) <- c("bayesbench_cfg", "list")
  assert_bayesbench_cfg(cfg)
  cfg
}

#' @export
bayesbench_job_cfg <- function(x){
  x <- bayesbench_cfg(x)
  checkmate::assert_string(x$inference_engine)
  checkmate::assert_string(x$posterior_name)
  
  
  ie_args <- x$inference_engine_arguments
  i <- 1
  while(i <= length(ie_args)){
    if(is.list(ie_args[[i]])){
      sublist <- ie_args[[i]]
      names(sublist) <- paste0(names(ie_args)[i], ".", names(sublist))
      ie_args <- c(ie_args, sublist)
      ie_args[[i]] <- NULL
    } else {
      checkmate::assert_scalar(ie_args[[i]], .var.name = names(ie_args)[i])
      i <- i + 1
    }
  }
  class(x) <- c("bayesbench_job_cfg", class(x))
  x <- add_default_arguments(x)
  x
}


#' @rdname bayesbench_cfg
#' @export
read_bayesbench_cfg_from_file <- function(file_path){
  checkmate::assert_file_exists(file_path, extension = c("yaml", "yml", "json"))
  file_ext <- file_extension(file_path)
  if(file_ext %in% c("yaml", "yml")){
    cfg_object <- yaml::read_yaml(file_path)
  } else if (file_ext %in% c("json")){
    cfg_object <- jsonlite::read_json(file_path)
  } 
  parse_read_cfg_object(x = cfg_object)
}

#' @rdname bayesbench_cfg
#' @export
write_bayesbench_cfg_to_file <- function(cfg, file_path){
  checkmate::assert_class(cfg, "bayesbench_cfg")
  checkmate::assert_path_for_output(file_path)
  file_ext <- file_extension(file_path)
  checkmate::assert_choice(file_ext, choices = c("yaml", "yml", "json"))
  if(file_ext %in% c("yaml", "yml")){
    yaml::write_yaml(x = cfg, file = file_path)
  } else if (file_ext %in% c("json")){
    jsonlite::write_json(bayesbench_cfg_toJSON(cfg), path = file_path)
  } 
  return(invisible(TRUE))
}

file_extension <- function(x){
  x <- strsplit(x, "\\.")[[1]]
  x[length(x)]
}

parse_read_cfg_object <- function(x){
  checkmate::assert_list(x)
  if(checkmate::test_names(names(x), must.include = c("inference_engine", "posterior_name"))){
    cfgs <- list(bayesbench_cfg(x))
  } else {
    cfgs <- parse_cfg_list(cfgs = x)    
  }
  cfgs
}


parse_cfg_list <- function(cfgs){
  checkmate::assert_list(cfgs)
  cfg_list <- list()
  for(i in seq_along(cfgs)){
    cfg_list[[i]] <- bayesbench_cfg(cfgs[[i]])
    if(is.null(cfg_list[[i]]$config_name)) {
      # Add config names if missing
      cfg_list[[i]]$config_name <- make.names(paste0("temp_config_",i,"_", Sys.time()))
      cfg_list[[i]] <- cfg_list[[i]][c(length(cfg_list[[i]]), 1:(length(cfg_list[[i]]) - 1))]
    }
    cfg_list[[i]] <- bayesbench_cfg(cfg_list[[i]]) # Just to reorder
  }
  cfg_list
}


assert_bayesbench_cfg <- function(x){
  checkmate::assert_class(x, classes = c("bayesbench_cfg", "list"))
  checkmate::assert_names(names(x), must.include = c("inference_engine", "posterior_name"))
  xjson <- bayesbench_cfg_toJSON(x)
  schema_results <- validatejsonr::validate_json_with_schemafile(xjson, schemafn = system.file("extdata", "schemas", "bayesbench_cfg_schema.json", package = "bayesbenchr"))
  if(schema_results$value > 0) stop("(", schema_results$value, ") ",  schema_results$message, "\nSchema: ", schema_results$schema, "\nJSON:\n", schema_results$jsonfile)
}


bayesbench_cfg_json_prepare <- function(x){
  checkmate::assert_class(x, classes = c("bayesbench_cfg"))
  x$output$directory <- jsonlite::unbox(x$output$directory)
  x$config_name <- jsonlite::unbox(x$config_name)
  x$posterior_database_path <- jsonlite::unbox(x$posterior_database_path)
  x
}

bayesbench_cfg_toJSON <- function(x){
  checkmate::assert_class(x, classes = c("bayesbench_cfg"))
  x <- bayesbench_cfg_json_prepare(x)
  x <- jsonlite::toJSON(x, pretty = TRUE)
  x
}

bayesbench_cfg_toYAML <- function(x){
  checkmate::assert_class(x, classes = c("bayesbench_cfg"))
  yaml::as.yaml(x)
}

#'@export
print.bayesbench_cfg <- function(x,...){
  if(checkmate::test_class(x, "bayesbench_job_cfg")){
    cat("### BAYESBENCH (JOB) CONFIG ###\n")
  } else {
    cat("### BAYESBENCH CONFIG ###\n")
  }
  cat(bayesbench_cfg_toYAML(x))
}



#' Expand \code{bayesbench_cfg} objects to \code{bayesbench_job_cfg}s
#' 
#' @details job configs are individual configurations that can be run independently
#' 
#' @param cfg a list of \code{bayesbench_cfg} to expand.
#' 
expand_bayesbench_cfg_to_job_cfgs <- function(cfg){
  checkmate::assert_list(cfg)
  for(i in seq_along(cfg)){
    checkmate::assert_class(cfg[[i]], "bayesbench_cfg")
  }
  all_jobs <- list()
  for(i in seq_along(cfg)){
    all_jobs <- c(all_jobs, bayesbench_job_cfg_from_cfg(x = cfg[[i]]))
  }
  all_jobs
}

bayesbench_job_cfg_from_cfg <- function(x){
  checkmate::assert_class(x, "bayesbench_cfg")
  expand_list <- list()
  expand_list[["inference_engine"]] <- x$inference_engine
  expand_list[["posterior_name"]] <- x$posterior_name
  
  ie_args <- x$inference_engine_arguments
  if(!is.null(ie_args)){
    names(ie_args) <- paste0("[[\"", names(ie_args), "\"]]")
    i <- 1
    while(i <= length(ie_args)){
      if(is.list(ie_args[[i]])){
        sublist <- ie_args[[i]]
        names(sublist) <- paste0(names(ie_args)[i], "[[\"", names(sublist), "\"]]")
        ie_args[[i]] <- NULL
        ie_args <- c(ie_args, sublist)
        next()
      } 
      if(length(ie_args[[i]]) > 1) {
        expand_list[[names(ie_args)[i]]] <- ie_args[[i]]
      }
      i <- i + 1
    }
  }

  expand_df <- expand.grid(expand_list, stringsAsFactors = FALSE)
  if(nrow(expand_df) == 1) {
    x$config_name <- paste0(x$config_name, "_job1")
    return(list(bayesbench_job_cfg(x)))
  }
  job_cfg_list <- list()
  for(i in 1:nrow(expand_df)){
    job_cfg_list[[i]] <- x
    job_cfg_list[[i]]$config_name <- paste0(job_cfg_list[[i]]$config_name, "_job", i)
    job_cfg_list[[i]]$inference_engine <- expand_df$inference_engine[i]
    job_cfg_list[[i]]$posterior_name <- expand_df$posterior_name[i]
    if(ncol(expand_df)>3){
      for(j in 3:ncol(expand_df)){
        eval(parse(text = paste0("job_cfg_list[[i]]$inference_engine_arguments",
                                 names(expand_df)[j],
                                 " <- expand_df[i,j]")))
      }
    }
    job_cfg_list[[i]] <- bayesbench_job_cfg(job_cfg_list[[i]])
  }
  return(job_cfg_list)
}


add_default_arguments <- function(cfg){
  checkmate::assert_class(cfg, "bayesbench_job_cfg")
  if(!is.null(cfg$output)){
    if(is.null(output_directory(cfg))) output_directory(cfg) <- "temp_output"
    if(is.null(output_type(cfg))) output_type(cfg) <- "zip"
  }
  cfg
}

