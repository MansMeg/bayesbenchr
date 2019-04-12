#' Return the job configs from a config object
get_job_configs <- function(cfg){
  if(checkmate::test_class(cfg, "bayesbench_cfg")){
    cfg <- list(cfg)
  }
  for(i in seq_along(cfg)){
    checkmate::assert_class(cfg[[i]], "bayesbench_cfg")
  }
  expand_bayesbench_cfg_to_job_cfgs(cfg)
}


# Get method specific arguments
inference_engine_arguments <- function(cfg){
  checkmate::assert_class(cfg, "bayesbench_job_cfg")
  cfg$inference_engine_arguments
}

posterior_name <- function(cfg){
  checkmate::assert_class(cfg, "bayesbench_job_cfg")
  cfg$posterior_name
}



stancode_file_path <- function(cfg, tempdir = TRUE){
  checkmate::assert_class(cfg, "bayesbench_job_cfg")
  pstdb <- get_posterior_from_database(cfg)
  path_vector <- strsplit(pstdb$model$stan, "/")[[1]]
  fp <- file.path(cfg$posterior_database_path, paste(path_vector, collapse = .Platform$file.sep))
  checkmate::assert_file_exists(fp)
  if(tempdir){
    model_dir <- paste(path_vector[-length(path_vector)], collapse = .Platform$file.sep)
    tmp_dir <- file.path(tempdir(), model_dir)
    tmp_fp <- file.path(tmp_dir, path_vector[length(path_vector)])
    dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
    file.copy(fp, to = tmp_fp, overwrite = TRUE)
    fp <- tmp_fp
  }
  checkmate::assert_file_exists(fp)
  fp
}


get_posterior_from_database <- function(cfg){
  checkmate::assert_class(cfg, "bayesbench_job_cfg")
  fp <- file.path(cfg$posterior_database_path, "posteriors", paste0(cfg$posterior_name, ".json"))
  checkmate::assert_file_exists(fp)
  jsonlite::fromJSON(readLines(fp))
}

data_file_path <- function(cfg){
  checkmate::assert_class(cfg, "bayesbench_job_cfg")
  pstdb <- get_posterior_from_database(cfg)
  path_vector <- strsplit(pstdb$data, "/")[[1]]
  
  zip_fp <- paste0(file.path(cfg$posterior_database_path, paste(path_vector, collapse = .Platform$file.sep)), ".zip")
  checkmate::assert_file_exists(zip_fp)
  
  data_dir <- paste(path_vector[-length(path_vector)], collapse = .Platform$file.sep)
  tmp_dir <- file.path(tempdir(), data_dir)
  tmp_unzip_fp <- file.path(tmp_dir, path_vector[length(path_vector)])
  if(checkmate::test_file_exists(tmp_unzip_fp)){
    return(tmp_unzip_fp)
  } else {
    unzip(zip_fp, exdir = tmp_dir)
    checkmate::assert_file_exists(tmp_unzip_fp)
    return(tmp_unzip_fp)
  }
}



#scf <- stancode_file(cfg)
#sd <- standata(cfg)
#args <- method_specific_arguments(cfg)