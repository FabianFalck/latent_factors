force_load_data <- FALSE
force_preproc_data <- FALSE
force_analyse_data <- FALSE
estimate_types <- c("treatment_means", "contrasts_vs_placebo")
n_fac <- 3


# try({ 
#   ###################################################
#   # Get packages in .libPaths()
#   .libPaths(c("/apps/eb/software/R-bundle-Bioconductor/3.11-foss-2020a-R-4.0.0",
#               "/apps/eb/software/R/4.0.0-foss-2020a/lib64/R/library"))
#   # TODO the package should no longer be loaded, but if commented out, error is caused!
#   devtools::load_all("../mlfa_package")  # not loading it, because we are currently working with a development script for mfd which is loaded in 03_analysis.R, see source("scripts/mfd_develop.R")
# })

devtools::load_all("mlfa_package")

#######################################
# Source functions
fn_files_to_source <- list.files("functions/", full.names = TRUE)
for (file_curr in fn_files_to_source) {
  source(file_curr)
}

#######################################
# Folder structure
DIR <- get_DIR()

#######################################
# Load data
if (!"d_step_1.RDS" %in% list.files("preproc_data") | force_load_data) {
  source("scripts/01_load_PsA_RA_newRelease.R")
} else {
  d_step1 <- readRDS(file = "../preproc_data/d_step_1.RDS")
  data_vars_all <- readRDS(file = "../preproc_data/data_vars_all.RDS")
  d_subject_00 <- readRDS(file = "../preproc_data/d_subject_00.RDS")
}

#######################################
# Preprocess data
if (!"d_time.RDS" %in% list.files("preproc_data") | force_preproc_data) {
  source("scripts/02_preprocess_PsA_RA.R")
} else {
  d_time <- readRDS(file = "../preproc_data/d_time.RDS")
  d_subject <- readRDS(file = "../preproc_data/d_subject.RDS")
  nms_params_v2 <- readRDS(file = "../preproc_data/nms_params_v2")
}
endpts <- nms_params_v2$PARAM
n_endpt <- length(endpts)

#######################################
# Analyse the data
if (!"analysis_results_list.RDS" %in% list.files("output") | force_analyse_data) {
  source("scripts/03_analysis.R")
} else {
  analysis_results_list <- readRDS(file = file.path("output", "analysis_results_list.RDS"))
}

#######################################
# Generate plots
export_plots <- TRUE
source("scripts/04b_plot_single_study.R")
source("scripts/04d_meta_analysis.R")
source("scripts/04f_compare_indications.R")
source("scripts/04f_compare_indications_same_plot.R")
source("scripts/04g_compare_loadings.R")
source("scripts/04h_intro_to_factor_analysis_plot.R")
source("scripts/04i_intro_to_stage_2_single_factor.R")
system("rm plots.tar.gz")
system("(cd figures && tar cvzf ../plots.tar.gz *)")

# source("scripts/04z_run_paper_plot_scripts.R")

# source("scripts/04_plotting_wrapper.R")

