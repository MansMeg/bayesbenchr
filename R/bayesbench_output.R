#' Create a Bayesbench ouput object
#' 
#' @param cfg The config object to return
#' @param posterior A list of posterior, one array per parameter
#' @param posterior_predictive A list of posterior predictives, one array per parameter predictive
#' @param log_p The log target density (log posterior mostly) for each draw
#' @param log_q The log proposal density (log approximate posterior) for each draw
#' @param method_diagnostic a list of method specific diagnostics (these need sto be handled separately)
#' @param text_log eventual output during inference engine run
#' 
#' @export
bayesbench_output <- function(cfg, 
                              posterior,
                              posterior_predictive = NULL,
                              log_p=NULL, 
                              log_g=NULL, 
                              method_diagnostic = NULL, 
                              text_log = NULL){
  
  output_list <- list()
  output_list$cfg <- cfg
  output_list$posterior <- posterior
  output_list$diagnostics <- list(log_p = log_g, 
                                  log_g = log_g)
  output_list$method_diagnostic <- method_diagnostic
  output_list$text_log <- text_log
  
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
  x <- jsonlite::toJSON(x, pretty = TRUE)
  x
}

print.bayesbench_output <- function(x,...){
  cat("=== BAYESBENCH OUTPUT FOR ===\n")
  cat(bayesbench_cfg_toYAML(x$cfg))
}
