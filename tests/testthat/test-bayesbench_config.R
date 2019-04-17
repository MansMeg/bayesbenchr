context("config")

test_that("test bayesbench config and job config", {
  # Read from file
  expect_silent(cfg <- read_bayesbench_cfg_from_file(file_path = "cfgs/test_stan_inference.yml"))
  expect_error(bayesbench_job_cfg(cfg[[1]]))
  expect_silent(bayesbench_job_cfg(cfg[[2]]))
  expect_error(bayesbench_job_cfg(cfg[[3]]))
  
  # Complex config
  expect_silent(
    cfg <- bayesbench_cfg(posterior_name = "sblrD5N500I|blr",
                          inference_engine = "stan_sampling",
                          inference_engine_arguments = list(sampling = list(chains = 2L),
                                                            vb = list(iter = c(10, 10000L),
                                                                      algorithm = "meanfield",
                                                                      adapt_engaged = 0L,
                                                                      eta = 0.1,
                                                                      tol_rel_obj = c(0.01, 0.0000001)),
                                                            keep_stan_object = TRUE),
                          posterior_database_path = "minimal_posterior_database")
  )
  
  # Test expanding complex config
  expect_silent(cfgs <- bayesbenchr:::bayesbench_job_cfg_from_cfg(cfg))
  expect_length(cfgs, 4)
  
  # Test R constructor
  expect_silent(cfg  <-  bayesbench_cfg(posterior_name = "8_schools|noncentered",
                                        inference_engine = "stan_sampling",
                                        posterior_database_path = "minimal_posterior_database"))
  
  expect_silent(cfg  <-  bayesbench_cfg(posterior_name = c("8_schools|noncentered", "8_schools|centered"),
                                        inference_engine = "stan_sampling",
                                        posterior_database_path = "minimal_posterior_database"))
  
  skip("Fix later")
  # Test for warnings with duplicates/erroneus engine
  expect_warning(cfg  <-  bayesbench_cfg(posterior_name = c("8_schools|noncentered", "8_schools|noncentered"),
                                        inference_engine = "stan_sampling",
                                        posterior_database_path = "minimal_posterior_database"))
  expect_warning(cfg  <-  bayesbench_cfg(posterior_name = c("8_schools|noncentered", "blah"),
                                         inference_engine = "stan_sampling",
                                         posterior_database_path = "minimal_posterior_database"))
  expect_warning(cfg  <-  bayesbench_cfg(posterior_name = c("8_schools|noncentered"),
                                         inference_engine = c("stan_sampling", "blah"),
                                         posterior_database_path = "minimal_posterior_database"))
  expect_warning(cfg  <-  bayesbench_cfg(posterior_name = c("8_schools|noncentered"),
                                         inference_engine = c("stan_sampling"),
                                         posterior_database_path = c("minimal_posterior_database", "blah")))
  
  expect_error(cfg  <-  bayesbench_cfg(inference_engine = "stan_sampling",
                                       posterior_database_path = "minimal_posterior_database"))
  expect_error(cfg  <-  bayesbench_cfg(posterior_name = "8_schools|noncentered",
                                       posterior_database_path = "minimal_posterior_database"))
  expect_error(cfg  <-  bayesbench_cfg(posterior_name = "8_schools|noncentered",
                                       inference_engine = "stan_sampling"))
  expect_error(cfg  <-  bayesbench_cfg(posterior_name = "blah",
                                       inference_engine = "stan_sampling",
                                       posterior_database_path = "minimal_posterior_database"))
  expect_error(cfg  <-  bayesbench_cfg(posterior_name = "8_schools|noncentered",
                                       inference_engine = "blah",
                                       posterior_database_path = "minimal_posterior_database"))
  expect_error(cfg  <-  bayesbench_cfg(posterior_name = "8_schools|noncentered",
                                       inference_engine = "stan_sampling",
                                       posterior_database_path = "blah"))  

})