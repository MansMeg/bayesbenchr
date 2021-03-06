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
                              diagnostics = NULL, 
                              inference_engine_content = NULL, 
                              output_log = NULL,
                              output_time_stamps = NULL){
  
  checkmate::assert_character(output_log)
  checkmate::assert_character(output_time_stamps)
  checkmate::assert_named(output_time_stamps)
  
  output_list <- list()
  output_list$cfg <- cfg
  output_list$posterior <- posterior
  output_list$diagnostics <- diagnostics
  output_list$inference_engine_content <- inference_engine_content
  output_list$output_log <- output_log
  r <- git2r::repository()
  output_list$output_githash <- capture.output(print(r))
  output_list$output_time_stamps <- c(output_time_stamps, c(output_created=as.character(Sys.time())))
  
  class(output_list) <- c("bayesbench_output", "list")
  assert_bayesbench_output(x = output_list)
  output_list
}



assert_bayesbench_output <- function(x){
  checkmate::assert_class(x, classes = c("bayesbench_output", "list"))
  x$inference_engine_content <- NULL
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

#' @export
print.bayesbench_output <- function(x,...){
  cat("=== BAYESBENCH OUTPUT ===\n")
  cat(paste0(names(x$output_time_stamps),": ", x$output_time_stamps), sep="\n")
  cat(x$output_githash[3], "\n\n")
  cat(bayesbenchr:::bayesbench_cfg_toYAML(x$cfg))
}


#' Write bayesbench outputs to file
#' 
#' @details 
#' Will write \code{bayesbench_outputs} to file(s) according to the \code{bayesbench_outputs} configs.
#' Specifying \code{output_directory} or \code{output_type} will update the config with these values
#' and then write it to file(s).
#' 
#' @export
write_bayesbench_outputs <- function(bayesbench_outputs, output_directory = NULL, output_type = NULL){
  if(checkmate::test_class(bayesbench_outputs, "bayesbench_output")){
    bayesbench_outputs <- list(bayesbench_outputs)
  }
  for(i in seq_along(bayesbench_outputs)){
    checkmate::assert_class(bayesbench_outputs[[i]], "bayesbench_output", null.ok = TRUE)
  }
  
  
  for(i in seq_along(bayesbench_outputs)){
    if(!is.null(output_directory)) output_directory(bayesbench_outputs[[i]]) <- output_directory
    if(!is.null(output_type)) output_type(bayesbench_outputs[[i]]) <- output_type
    
    if(is.null(bayesbench_outputs[[i]])) next()
    if(is.null(output_directory(bayesbench_outputs[[i]]))) next()
    dir.create.bayesbench_output(bayesbench_outputs[[i]])
    write_bayesbench_output(bayesbench_output = bayesbench_outputs[[i]])
  }
  
  return(bayesbench_outputs)
}

write_bayesbench_output <- function(bayesbench_output){
  checkmate::assert_class(bayesbench_output, "bayesbench_output")
  file_name <- output_name(bayesbench_output)
  ofp <- output_file_path(bayesbench_output)
  if(output_type(bayesbench_output) == "zip"){
    bbojson <- bayesbench_output_toJSON(bayesbench_output)
    tmp_file_path <- file.path(tempdir(), file_name)
    writeLines(tmp_file_path, text = bbojson)
    zip(files = tmp_file_path, zipfile = ofp, flags = "-j")
  } else if(output_type(bayesbench_output) == "json"){
    bbojson <- bayesbench_output_toJSON(bayesbench_output)
    writeLines(con = ofp, text = bbojson)
  } else if(output_type(bayesbench_output) == "rds") {
    saveRDS(bayesbench_output, file = ofp)
  } else{
    warning("'", output_type(bayesbench_output), "' output format is not implemented. No output written.")
  }
  return(TRUE)
}



dir.create.bayesbench_output <- function(bayesbench_output){
  checkmate::assert_class(bayesbench_output, "bayesbench_output")
  dir.create(output_directory(bayesbench_output), showWarnings = FALSE, recursive = TRUE)
}

#' Read Bayesian outputs
#' 
#' @details 
#' Read in bayesbench output jobs
#' 
#' @param x either file paths or a cfg object
#' 
#' @export
read_bayesbench_outputs <- function(x){
  if(checkmate::test_class(x, "bayesbench_cfg")){
    file_paths <- output_file_path(x)
  } else if(checkmate::test_file_exists(x)){
    file_paths <- x
  } else {
    stop("x is of class ", class(x))
  }
  bbs <- list()
  for(i in seq_along(file_paths)){
    bbs[[i]] <- read_bayesbench_output(path = file_paths[i])
  }
  bbs
}

read_bayesbench_output <- function(path){
  if(!checkmate::test_file_exists(path)){
    warning(checkmate::check_file_exists(path))
    return(NULL)
  }
  if(!checkmate::test_choice(tolower(file_extension(path)), c("zip", "json", "rds"))){
    warning(path, " is not read. ", checkmate::check_choice(tolower(file_extension(path)), c("zip", "json", "rds")))
    return(NULL)
  }
  
  if(tolower(file_extension(path)) == "zip"){
    tmp_dir <- file.path(tempdir(), "tmp_json")
    dir.create(tmp_dir, showWarnings = FALSE)
    path <- unzip(zipfile = path, exdir = tmp_dir)
  }
  if(tolower(file_extension(path)) == "json"){
    bbjson <- jsonlite::read_json(path, simplifyVector = TRUE)
    return(bayesbench_output(bbjson)) 
  }
  if(tolower(file_extension(path)) == "rds"){
    bbo <- readRDS(path)
    checkmate::assert_class(bbo, "bayesbench_output")
    return(bbo) 
  }
}

