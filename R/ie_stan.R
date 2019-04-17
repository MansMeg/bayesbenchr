#' Run Stan VB
#' 
#' @details 
#' Runs Stans VB inferences.
#' 
#' @param cfg a \code{bayesbench_cfg} config object
#'
#' @return a \code{bayesbench_output} object
stan_vb <- function(cfg){
  checkmate::assert_class(cfg, "bayesbench_job_cfg")
  
  pnm <- posterior_name(cfg)
  sm <- stan_model(cfg)
  sd <- jsonlite::read_json(data_file_path(cfg), simplifyVector = TRUE)
  cfg <- add_default_arguments_stan(cfg)
  ie_args <- inference_engine_arguments(cfg)

  args <- c(list(object = sm, data = sd), ie_args$vb)
  opt_warn <- getOption("warn", default = NULL)
  options(warn=1) 
  mess_logs <- capture.output(logs <- capture.output(results <- do.call(what = rstan::vb, args), split = verbose(cfg)), type = "message")
  options(warn=opt_warn) 

  iec <- list()
  if(ie_args$keep_stan_object) {
    iec[["stan_object"]] <- results
  }
  iec[["vb_diagnostics"]] <- results@sim$diagnostics[2:length(results@sim$diagnostics)]
  
  x <- bayesbench_output(cfg = cfg,
                         posterior = results@sim$samples[[1]], 
                         diagnostics = list(log_p = results@sim$diagnostics[[1]]$log_p__,
                                            log_g = results@sim$diagnostics[[1]]$log_g__),
                         inference_engine_content = iec,
                         output_log = c(logs, mess_logs))
  return(x)
}

#' Run Stan MCMC
#' 
#' @param cfg a \code{bayesbench_cfg} config object
#'
#' @return a \code{bayesbench_output} object
stan_sampling <- function(cfg){
  checkmate::assert_class(cfg, "bayesbench_job_cfg")
  
  pnm <- posterior_name(cfg)
  sm <- stan_model(cfg)
  sd <- jsonlite::read_json(data_file_path(cfg), simplifyVector = TRUE)
  cfg <- add_default_arguments_stan(cfg)
  ie_args <- inference_engine_arguments(cfg)

  args <- c(list(object = sm, data = sd), ie_args$sampling)
  opt_warn <- getOption("warn", default = NULL)
  options(warn=1) 
  mess_logs <- capture.output(logs <- capture.output(results <- do.call(what = rstan::sampling, args), split = verbose(cfg)), type = "message")
  options(warn=opt_warn) 
  
  iec <- list()
  if(ie_args$keep_stan_object) {
    iec[["stan_object"]] <- results
  }
  
  x <- bayesbench_output(cfg = cfg,
                         posterior = extract(results), 
                         inference_engine_content = iec,
                         output_log = c(logs, mess_logs))
  return(x)
}

#' Run Stan optimizing/laplace
#' 
#' @param cfg a \code{bayesbench_cfg} config object
#'
#' @return a \code{bayesbench_output} object
stan_optimizing <- function(cfg){
  checkmate::assert_class(cfg, "bayesbench_job_cfg")
  
  pnm <- posterior_name(cfg)
  sm <- stan_model(cfg)
  sd <- jsonlite::read_json(data_file_path(cfg), simplifyVector = TRUE)
  cfg <- add_default_arguments_stan(cfg)
  ie_args <- inference_engine_arguments(cfg)
  
  
  args <- c(list(object = sm, data = sd), ie_args$optimizing)
  opt_warn <- getOption("warn", default = NULL)
  options(warn=1) 
  mess_logs <- capture.output(logs <- capture.output(results <- do.call(what = rstan::optimizing, args), split = verbose(cfg)), type = "message")
  options(warn=opt_warn) 
  
  iec <- list()
  if(ie_args$keep_stan_object) {
    iec[["stan_object"]] <- results
  }
  
  x <- bayesbench_output(cfg = cfg,
                         posterior = NULL, 
                         inference_engine_content = iec,
                         output_log = c(logs, mess_logs))
  return(x)
}


stan_model <- function(cfg){
  stan_code_file <- stancode_file_path(cfg)
  pnm <- posterior_name(cfg)
  require(rstan)
  sm <- rstan::stan_model(file = stan_code_file, model_name = make.names(pnm), auto_write = TRUE)
}


add_default_arguments_stan <- function(cfg){
  checkmate::assert_class(cfg, "bayesbench_job_cfg")
  if(is.null(cfg$inference_engine_arguments)){
    cfg$inference_engine_arguments <- list()
  }
  if(is.null(cfg$inference_engine_arguments$keep_stan_object)){
    cfg$inference_engine_arguments$keep_stan_object <- FALSE
  }
  bayesbench_job_cfg(cfg)
}
