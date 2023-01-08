export_plots <- TRUE
source("scripts/04b_plot_single_study.R")
source("scripts/04d_meta_analysis.R")
source("scripts/04f_compare_indications.R")
source("scripts/04f_compare_indications_same_plot.R")
source("scripts/04g_compare_loadings.R")
source("scripts/04h_intro_to_factor_analysis_plot.R")
system("rm plots.tar.gz")
system("(cd figures && tar cvzf ../plots.tar.gz *)")



