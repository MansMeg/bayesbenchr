#' Run Bayesbench 
#' 
#' @description 
#' Run Bayesbench for a given set of configurations
#' 
#' @param cfg a \code{bayesbench_cfg} object or a list of \code{bayesbench_cfg} object.
#' 
#' @examples 
#' path <- system.file("extdata", "examples", "test_8schools_advi.yml", package = "bayesbenchr")
#' cfg <- read_bayesbench_cfg_from_file(path)
#' # results <- bayesbench_run(cfg)
#' 
#' @export
bayesbench_run <- function(cfg){
  if(checkmate::test_class(cfg, "bayesbench_cfg")){
    cfg <- list(cfg)
  }
  for(i in seq_along(cfg)){
    checkmate::assert_class(cfg[[i]], "bayesbench_cfg")
  }
  # cfg is a list of bayesbench_cfg objects from now on.

  # Create job configs
  cfgs <- expand_bayesbench_cfg_to_job_cfgs(cfg)
  
  # Check that all configs are ok.
  
  # Run all jobs
  pb <- progress::progress_bar$new(format = "Running Bayesbench [:bar] :percent in :elapsed", total = length(cfgs), clear = FALSE)
  results <- list()
  for(i in seq_along(cfgs)){
    pb$tick()
    start_time <- Sys.time()
    eval(parse(text = paste0("results[[i]] <- ", cfgs[[i]]$inference_engine, "(cfgs[[i]])")))
    end_time <- Sys.time()
    #add_start_time(results[[i]]) <- start_time
    #add_end_time(results[[i]]) <- end_time
  }
  
  # Write results
  write_bayesbench_output(results)

  # Convert to bayesbench output
  return(results)
}

#' @rdname bayesbench_run
#' @param bash_folder Folder to write bash scripts to
#' @export
bayesbench_bash_run <- function(cfg, bash_folder = "temp_bayesbench"){
  if(checkmate::test_class(cfg, "bayesbench_cfg")){
    cfg <- list(cfg)
  }
  for(i in seq_along(cfg)){
    checkmate::assert_class(cfg[[i]], "bayesbench_cfg")
  }
  # cfg is a list of bayesbench_cfg objects from now on.
  
  # Create job configs
  cfgs <- expand_bayesbench_cfg_to_job_cfgs(cfg)
  
  # Create temp dir for runs  
  dir.create(bash_folder, showWarnings = FALSE)
  dir.create(file.path(bash_folder, "cfgs"), showWarnings = FALSE)
  
  # Copy run_bayesbench
  file.copy(system.file("extdata", "rscripts", "bayesbench.R", package = "bayesbenchr"),
            file.path(bash_folder, "bayesbench.R"), overwrite = TRUE)

  # Create configs
  cfg_path <- character(length(cfgs))
  for(i in seq_along(cfgs)){
    cfg_path[i] <- file.path(bash_folder, "cfgs", paste0("cfg",i,".yml"))
    write_bayesbench_cfg_to_file(cfg = cfgs[[i]], cfg_path[i])
  }
  
  # Create bash
  bash_file <- c("#!/bin/bash", "", paste0("Rscript ", file.path(bash_folder, "bayesbench.R"), " --cfg_path=\"", cfg_path, "\""))
  writeLines(bash_file, con = file.path(bash_folder, "run_bayesbench.sh"))
  # Convert to bayesbench output
  
  cat("Run ", file.path(bash_folder, "run_bayesbench.sh"), " in bash.\n")
  return(invisible(TRUE))
}
