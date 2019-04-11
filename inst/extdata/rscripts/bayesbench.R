suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(bayesbenchr))

option_list <- list( 
  make_option(c("-v", "--verbose"), action="store_true", default=TRUE,
              dest="verbose", help="Print extra output [default]"),
  make_option(c("--cfg_path"), action="store", 
              dest="cfg_path", help="Path to config file")
) 
opt <- parse_args(OptionParser(option_list=option_list))

# Get cfg
cfg <- read_bayesbench_cfg_from_file(opt$cfg_path)

bayesbench_run(cfg)
