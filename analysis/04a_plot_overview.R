source("scripts/04_plotting_intro.R")

########################################
# Plot all results
analyses_plot <- names(control$do_list)
n_analyses_plot <- length(analyses_plot)
dir.create("figures", showWarnings = FALSE)
for (estimate_type in estimate_types) {
  if (estimate_type == "treatment_means") {
    arm_include_label <- "arm_all"
    title_text <- "Treatment mean trajectories"
    ylim_curr <- c(-1, 1) * .75
  }
  if (estimate_type == "contrasts_vs_placebo") {
    arm_include_label <- "arm_no_plac"
    title_text <- "Active treatment arms relative to placebo"
    ylim_curr <- c(-1, .25)
  }
  if (export_plots){
    pdf(file = paste0("figures/global_", estimate_type, ".pdf"), 12, 20)
  }
  par(mfcol = c(n_analyses_plot, n_fac + 1), mar = c(3, 3, 1, 1), oma = c(4, 10, 5, 12))
  cexax <- .8
  col_heat <- c(rgb(0, 0, 1, alpha = seq(1, 0, len = 500)), rgb(1, 0, 0, alpha = seq(0, 1, len = 500)))
  col_arm <- viridis::viridis(n = 5, alpha = .5)
  col_arm_opaque <- viridis::viridis(n = 5)
  arm_all <- c("Placebo", "75mg", "150mg", "300mg", "Abatacept")
  names(col_arm) <- arm_all
  for (analysis_curr in analyses_plot) {
    A_curr <- analysis_results_list[[analysis_curr]]$mfd_object$A
    image(1:n_fac, 1:n_endpt, t(A_curr),
          col = col_heat, zlim = c(-1, 1), xaxt = "n", yaxt = "n")
    axis(side = 2, at = 1:n_endpt, labels = endpts, las = 2, cex = cexax)
    mtext(side = 2, line = 8, text = analysis_curr)
  }
  for (fac_curr in 1:n_fac) {
    for (analysis_curr in analyses_plot) {
      arms_in <- analysis_results_list[[analysis_curr]][[arm_include_label]]
      m_pl <- analysis_results_list[[analysis_curr]][[fac_curr]][[estimate_type]]$m[, arms_in]
      l_pl <- analysis_results_list[[analysis_curr]][[fac_curr]][[estimate_type]]$l[, arms_in]
      u_pl <- analysis_results_list[[analysis_curr]][[fac_curr]][[estimate_type]]$u[, arms_in]
      t_unique <- as.numeric(rownames(m_pl))
      matplot(x = t_unique, y = m_pl, ylim = ylim_curr,
              ty = "n", xlab = "", ylab = "", las = 1)
      if(match(analysis_curr, analyses_plot) == 1) {
        mtext(side = 3, text = paste0("Factor ", fac_curr))
        if(fac_curr == n_fac) {
          par(xpd = NA)
          legend(x = max(t_unique) * 1.25, y = 0, legend = arm_all, col = col_arm,
                 pch = 22, pt.bg = col_arm, cex = 1.5)
          par(xpd = F)
        }
      }
      for (arm_curr in arms_in) {
        lines(x = t_unique, y = m_pl[, arm_curr], col = col_arm[arm_curr], lwd = 2)
        polygon(x = c(t_unique, rev(t_unique)),
                y = c(l_pl[, arm_curr], rev(u_pl[, arm_curr])),
                col = col_arm[arm_curr],
                border = 1)
        lines(x = t_unique, y = m_pl[, arm_curr], col = 1, lwd = 2)
        lines(x = t_unique, y = m_pl[, arm_curr], col = col_arm_opaque[arm_curr], lwd = 2)
      }
      abline(h = 0)
    }
  }
  mtext(outer = T, side = 3, line = 1.5, text = title_text, cex = 1.5)
  if (export_plots){
    dev.off()
  }
}

