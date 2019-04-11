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
#' 
#' @export
run_bayesbench <- function(cfg){
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
  results <- list()
  for(i in seq_along(cfgs)){
    eval(parse(text = paste0("results[[i]] <- ", cfgs[[i]]$inference_engine, "(cfgs[[i]])")))
  }
  
  # Convert to bayesbench output
  return(results)
}

