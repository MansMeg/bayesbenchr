#' Run Stan VB functionality
#' 
#' @details 
#' Runs Stans VB inferences.
#' 
#' @param cfg a \code{bayesbench_cfg} config object
#'
#' @return a \code{bayesbench_output} object
stan_advi <- function(cfg){
  checkmate::assert_class(cfg, "bayesbench_job_cfg")
  
  pnm <- posterior_name(cfg)
  scf <- stancode_file_path(cfg)
  sd <- jsonlite::read_json(data_file_path(cfg), simplifyVector = TRUE)
  ie_args <- inference_engine_arguments(cfg)

  require(rstan)
  rstan_options(auto_write = TRUE)
  sm <- rstan::stan_model(file = scf, model_name = make.names(pnm))
  args <- c(list(object = sm, data = sd), ie_args)
  suppressMessages(suppressWarnings(logs <- capture.output(results <- do.call(what = rstan::vb, args))))
  
  x <- bayesbench_output(cfg = cfg,
                         posterior = results@sim$samples[[1]], 
                         log_p = results@sim$diagnostics[[1]]$log_p__,
                         log_g = results@sim$diagnostics[[1]]$log_g__,
                         inference_engine_content = results@sim$diagnostics[2:length(results@sim$diagnostics)],
                         output_log = logs)
  return(x)
}

bayesbench_stan_advi <- function(cfg){
  stan_advi(cfg)
}