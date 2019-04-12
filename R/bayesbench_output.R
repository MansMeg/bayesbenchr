#' Create a Bayesbench ouput object
#' 
#' @param cfg The config object to return
#' @param posterior A list of posterior, one array per parameter
#' @param posterior_predictive A list of posterior predictives, one array per parameter predictive
#' @param log_p The log target density (log posterior mostly) for each draw
#' @param log_q The log proposal density (log approximate posterior) for each draw
#' @param inference_engine_content a list of inference_engine specific content (these need stored be handled separately)
#' @param text_log eventual output during inference engine run
#' 
#' @export
bayesbench_output <- function(cfg, 
                              posterior,
                              posterior_predictive = NULL,
                              log_p=NULL, 
                              log_g=NULL, 
                              inference_engine_content = NULL, 
                              output_log = NULL){
  
  output_list <- list()
  output_list$cfg <- cfg
  output_list$posterior <- posterior
  output_list$diagnostics <- list(log_p = log_g, 
                                  log_g = log_g)
  output_list$inference_engine_content <- inference_engine_content
  output_list$output_log <- output_log
  
  class(output_list) <- c("bayesbench_output", "list")
  assert_bayesbench_output(x = output_list)
  output_list
}



assert_bayesbench_output <- function(x){
  checkmate::assert_class(x, classes = c("bayesbench_output", "list"))
  xjson <- bayesbench_output_toJSON(x)
  schema_results <- validatejsonr::validate_json_with_schemafile(xjson, schemafn = system.file("extdata", "schemas", "bayesbench_output_schema.json", package = "bayesbenchr"))
  if(schema_results$value > 0) stop("(", schema_results$value, ") ",  schema_results$message, "\nSchema: ", schema_results$schema)
}

bayesbench_output_json_prepare <- function(x){
  checkmate::assert_class(x, classes = c("bayesbench_output", "list"))
  x$cfg <- bayesbench_cfg_json_prepare(x$cfg)
  x
}

bayesbench_output_toJSON <- function(x){
  checkmate::assert_class(x, classes = c("bayesbench_output", "list"))
  x <- bayesbench_output_json_prepare(x)
  x <- jsonlite::toJSON(x, pretty = TRUE, digits = NA)
  x
}

print.bayesbench_output <- function(x,...){
  cat("=== BAYESBENCH OUTPUT FOR ===\n")
  cat(bayesbench_cfg_toYAML(x$cfg))
}



write_bayesbench_output <- function(bayesbench_outputs){
  if(checkmate::test_class(bayesbench_outputs, "bayesbench_output")){
    cfg <- list(cfg)
  }
  for(i in seq_along(cfg)){
    checkmate::assert_class(bayesbench_outputs[[i]], "bayesbench_output")
  }
  
  tmpfn <- tempfile()
  for(i in seq_along(bayesbench_outputs)){
    dir.create.bayesbench_output(bayesbench_outputs[[i]])
    bbojson <- bayesbench_output_toJSON(bayesbench_outputs[[i]])
    file_name <- paste0("bayesbench_output_", i, ".json")
    tmp_file_path <- file.path(tempdir(), file_name)
    writeLines(tmp_file_path, text = bbojson)
    zip(files = tmp_file_path, zipfile = file.path(output_directory(bayesbench_outputs[[1]]), paste0(file_name, ".zip")), flags = "-j")
  }
  return(TRUE)
}

dir.create.bayesbench_output <- function(bayesbench_output){
  checkmate::assert_class(bayesbench_output, "bayesbench_output")
  dir.create(output_directory(bayesbench_output), showWarnings = FALSE, recursive = TRUE)
}

read_bayesbench_output_directory <- function(dir){
  stop("Not implemented")
  checkmate::assert_directory_exists(dir)
  files <- dir()
  read_bayesbench_output
}

