#' Bayesbench config
#' 
#' @details 
#' Creates a Bayesbench configuration file. The only 
#' 
#' @param ... Bayesbench parameters to set.
#' @param file_path File path to bayesbench configuration file.
#' 
#' @examples 
#' path <- system.file("extdata", "examples", "test_8schools_advi.yml", package = "bayesbenchr")
#' x <- read_bayesbench_cfg_from_file(path)
#' 
#' @export
bayesbench_cfg <- function(...){
  bayesbench_cfg_from_list(list(inference_engine, posterior_name, ...))
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


bayesbench_cfg_toJSON <- function(x){
  checkmate::assert_class(x, classes = c("bayesbench_cfg"))
  x$output_directory <- jsonlite::unbox(x$output_directory)
  x$posterior_database_path <- jsonlite::unbox(x$posterior_database_path)
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

