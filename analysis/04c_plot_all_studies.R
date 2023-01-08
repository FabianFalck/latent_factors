source("scripts/04_plotting_intro.R")
########################################
# Plot single study results
for (cross_indication_loadings in c(TRUE, FALSE)) {
  for (estimate_type in score_plot_types) {
    if (export_plots){
      pdf(file = paste0("figures/all_studies_", estimate_type, "_cross_indication_loadings_", cross_indication_loadings, ".pdf"), 12, 12)
    }
    plot_num <- 0
    # layout(mat = matrix(c(1, 2, 3, 4, 1, 5, 6, 7), nrow = 2, ncol = n_fac + 1, byrow = TRUE))
    par(mfcol = c(4, 4), mar = c(3, 3, 3, 3), oma = c(4, 10, 5, 10))
    if (cross_indication_loadings) {
      single_studies <- paste0(c("F2312", "F2342", "F2302", "F2309"), "_cross_indication_loadings")
    } else {
      single_studies <- c("F2312", "F2342", "F2302", "F2309")
    }
    for (study_to_plot in single_studies) {
      plot_loadings(control, 
                    analysis_results_list, 
                    analysis_plot = study_to_plot,
                    add_legend = study_to_plot == single_studies[1])
      add_letter(letters[plot_num <- plot_num + 1], y_fac = .025)
    }
    for (fac_curr in 1:n_fac) {
      for (study_to_plot in single_studies) {
        plot_scores(control, 
                    analysis_results_list,
                    analysis_plot = study_to_plot,
                    fac_plot = fac_curr,
                    estimate_type = estimate_type,
                    add_legend = fac_curr == n_fac)
        plotnum <- plot_num + 1
        add_letter(letters[plot_num <- plot_num + 1])
      }
    }
    mtext(outer = TRUE, at = seq(.25, .75, by = .25) + .125, text = control$fac_labs)
    if (export_plots){
      dev.off()
    }
  }
}  





