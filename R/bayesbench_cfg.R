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
  bayesbench_cfg_from_list(...)
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
  parse_read_cfg_object(cfg_object)
}


file_extension <- function(x){
  x <- strsplit(x, "\\.")[[1]]
  x[length(x)]
}


parse_read_cfg_object <- function(x){
  checkmate::assert_list(x)
  if(checkmate::test_names(names(x), must.include = c("inference_engine", "posterior_name"))){
    cfgs <- list(bayesbench_cfg_from_list(x))
  } else {
    cfgs <- parse_cfg_list(x)    
  }
  cfgs
}

bayesbench_cfg_from_list <- function(cfg){
  checkmate::assert_list(cfg)
  checkmate::assert_names(names(cfg), must.include = c("inference_engine", "posterior_name"))
  class(cfg) <- c("bayesbench_cfg", "list")
  assert_bayesbench_cfg(cfg)
  cfg
}

parse_cfg_list <- function(cfgs){
  checkmate::assert_list(cfgs)
  cfg_list <- list()
  for(i in seq_along(cfgs)){
    cfg_list[[i]] <- bayesbench_cfg_from_list(cfgs[[i]])
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
  x$output_directory <- jsonlite::unbox(x$output_directory)
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
  cat("=== BAYESBENCH CONFIG ===\n")
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
    all_jobs <- c(all_jobs, bayesbench_job_cfg_from_cfg(cfg[[i]]))
  }
  all_jobs
}

bayesbench_job_cfg_from_cfg <- function(x){
  checkmate::assert_class(x, "bayesbench_cfg")
  expand_list <- list()
  expand_list[["inference_engine"]] <- x$inference_engine
  expand_list[["posterior_name"]] <- x$posterior_name
  for(i in seq_along(x$method_specific_arguments)){
    if(length(x$method_specific_arguments[[i]]) > 1) expand_list[[names(x$method_specific_arguments)[i]]] <- x$method_specific_arguments[[i]]
  }
  expand_df <- expand.grid(expand_list, stringsAsFactors = FALSE)
  if(nrow(expand_df) == 1) {
    return(list(bayesbench_job_cfg(x)))
  }
  job_cfg_list <- list()
  for(i in 1:nrow(expand_df)){
    job_cfg_list[[i]] <- x
    job_cfg_list[[i]]$inference_engine <- expand_df$inference_engine[i]
    job_cfg_list[[i]]$posterior_name <- expand_df$posterior_name[i]
    for(j in 3:ncol(expand_df)){
      job_cfg_list[[i]]$method_specific_arguments[[names(expand_df)[j]]] <- expand_df[i,j]
    }
    job_cfg_list[[i]] <- bayesbench_job_cfg(job_cfg_list[[i]])
  }
  return(job_cfg_list)
}


bayesbench_job_cfg <- function(x){
  checkmate::assert_class(x, "bayesbench_cfg")
  x <- bayesbench_cfg(x)
  checkmate::assert_string(x$inference_engine)
  checkmate::assert_string(x$posterior_name)
  for(i in seq_along(x$method_specific_arguments)){
    checkmate::assert_true(length(x$method_specific_arguments[[i]]) == 1L)
  }
  class(x) <- c("bayesbench_job_cfg", class(x))
  x
}
