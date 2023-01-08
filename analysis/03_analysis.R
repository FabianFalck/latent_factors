
# list of all studies (per discussion with Xuan, Jan 2022): 
# RA phase 2: "CAIN457F2201", "CAIN457F2206", “CAIN457F2208”
# RA phase 3: "CAIN457F2302", "CAIN457F2309", "CAIN457F2311"
# ---
# PsA phase 2: N/A
# PsA phase 3: "CAIN457F2306", "CAIN457F2312", "CAIN457F2318", "CAIN457F2336", "CAIN457F2342", "CAIN457F2366"

control <- get_control_parameters()

analysis_names <- names(control$do_list)

##########################################################################################
# Gather study data subsets
analysis_sub_data <- list()
for (analysis_curr in analysis_names) {
  d_time_sub <- d_time %>% 
    filter(study %in% control$do_list[[analysis_curr]]$stage1) %>%
    filter(USUBJID %in% d_subject$USUBJID) %>%
    filter(AVISITN %in% c(0, 2, 4, 8, 12, 16))
  d_subject_sub <- d_subject %>% 
    filter(USUBJID %in% d_time_sub$USUBJID)
  ###################################################
  # Normal score transformation happening here
  d_time_sub <- mutate_if(as.data.frame(d_time_sub), colnames(d_time_sub) %in% nms_params_v2$PARAMCD, normal_quantile_map)
  colnames(d_time_sub)[colnames(d_time_sub) %in% nms_params_v2$PARAMCD] <- 
    nms_params_v2$PARAM[match(colnames(d_time_sub)[colnames(d_time_sub) %in% nms_params_v2$PARAMCD], nms_params_v2$PARAMCD)]
  d_time_sub <- as_tibble(d_time_sub)  
  analysis_sub_data[[analysis_curr]] <- list(time = d_time_sub, subject = d_subject_sub)
  analysis_sub_data[[analysis_curr]]$subject$newTRT <- gsub(" ", "", analysis_sub_data[[analysis_curr]]$subject$newTRT)
  
}

examine_scale_of_qn_outputs <- FALSE
if (examine_scale_of_qn_outputs) {
  par(mfrow = c(3, 4))
  for (parc in intersect(nms_params_v2$PARAM, names(analysis_sub_data[[analysis_curr]]$time))) {
    yc <- na.omit(as.data.frame(analysis_sub_data[[analysis_curr]]$time)[, parc])
    hist(yc)
    print(t(c(mean(yc), sd(yc))))
  }
}

##########################################################################################
# Stage 1 fits
mfd_object_list <- list()
for (analysis_curr in analysis_names) {
  mfd_object_list[[analysis_curr]] <- mfd(data = analysis_sub_data[[analysis_curr]]$time, 
                     vars = nms_params_v2$PARAM, 
                     loadings.n_PCs = n_fac, 
                     seed = 1,
                     time = "AVISITN", 
                     subject = "USUBJID",
                     normalize.type = c("per_meas_standard", "standardize", "per_center_baseline_scale")[1],
                     Time_base = 0, 
                     standardize_A_type = c("none", "L2", "largest_abs")[3],
                     loadings.sparse_abs_thres = 0,
                     loadings.rotation = c("varimax", "promax")[1],
                     scores.type = 'a_posteriori')

}

(mfd_object_list[[7]]$ppca_object@R2cum)
# (mfd_object_list[[1]]$ppca_object@R2cum)
# (mfd_object_list[[7]]$ppca_object@R2)


##########################################################################################
# Ensure the loadings matrices columns correspond across studies
analysis_to_match_loadings_to <- 'PsA_F2312'
for (analysis_curr in setdiff(analysis_names, analysis_to_match_loadings_to)) {
    prox_mat <- t(mfd_object_list[[analysis_curr]]$A) %*% mfd_object_list[[analysis_to_match_loadings_to]]$A
    index_move_from <- apply(prox_mat, 2, which.max)
    mfd_object_list[[analysis_curr]]$A <- mfd_object_list[[analysis_curr]]$A[, index_move_from]
    mfd_object_list[[analysis_curr]]$Z <- mfd_object_list[[analysis_curr]]$Z[, index_move_from]
    mfd_object_list[[analysis_curr]]$S <- mfd_object_list[[analysis_curr]]$S[, index_move_from]
    mfd_object_list[[analysis_curr]]$data_out[, paste0("z", 1:3)] <- 
      mfd_object_list[[analysis_curr]]$data_out[, paste0("z", 1:3)[index_move_from]]
    mfd_object_list[[analysis_curr]]$data_out[, paste0("s", 1:3)] <- 
      mfd_object_list[[analysis_curr]]$data_out[, paste0("s", 1:3)[index_move_from]]
}

########################################
# Use lme() to fit trajectories
analysis_results_list <- list()
for (analysis_curr in analysis_names) {
  placebo_label <- "Placebo"
  multiple_studies_flag <- length(control$do_list[[analysis_curr]]$stage2) > 1
  arm_all <- unique(analysis_sub_data[[analysis_curr]]$subject$newTRT)
  n_arm <- length(arm_all)
  arm_no_plac <- setdiff(arm_all, placebo_label)
  studies_in_stage_2 <- control$do_list[[analysis_curr]]$stage2
  results_df <- as.data.frame(mfd_object_list[[analysis_curr]]$data_out)
  results_df$STUDYID <- sapply(strsplit(results_df$USUBJID, split = "_"), function(x) x[[1]][1])
  results_df$study <- gsub("CAIN457", "", results_df$STUDYID)
  results_df <- results_df[results_df$study %in% studies_in_stage_2, ]
  results_df$sex <- d_subject[match(results_df$USUBJID, d_subject$USUBJID), ] %>% pull(SEX)
  results_df$sex <- C(factor(results_df$sex), contr = contr.sum)
  results_df$arm <- as.factor(analysis_sub_data[[analysis_curr]]$subject[
    match(results_df$USUBJID, analysis_sub_data[[analysis_curr]]$subject$USUBJID), ] %>% pull(newTRT))
  results_df$arm <- relevel(results_df$arm, ref = placebo_label)
  results_df$time_fac <- factor(results_df$AVISITN)
  t_unique <- sort(unique(results_df$AVISITN))
  nt <- length(t_unique)
  m_coef_mat<-u_coef_mat<-l_coef_mat <- matrix(NA, nt, n_arm, dimnames = list(t_unique, arm_all))
  results_list <- list()
  for (fac_curr in 1:n_fac) {
    results_list[[fac_curr]] <- list()
    results_df$z <- results_df[, paste0("z", fac_curr)]
    results_df$s <- results_df[, paste0("s", fac_curr)]
    for (estimate_type in estimate_types) {
      #################################
      # Call nlme::lme() here
      # z ~ -1 + time_fac + arm:time_fac,   # -1: no default inclusion of a single intercept, 
      # see "Mixed-Effect Models in S and S-Plus", p. 6
      if (multiple_studies_flag) {
        results_df$study <- C(factor(results_df$study), contr = contr.sum)
        fixed_form_curr <- switch(estimate_type,
                                treatment_means = z ~ -1 + arm:time_fac + sex:time_fac, #  sex:arm:time_fac
                                contrasts_vs_placebo = z ~ -1 + time_fac + arm:time_fac + study:time_fac + sex:time_fac)
      } else {
        fixed_form_curr <- switch(estimate_type,
                                  treatment_means = z ~ -1 + arm:time_fac + sex:time_fac, 
                                  contrasts_vs_placebo = z ~ -1 + time_fac + arm:time_fac + sex:time_fac)
      }
      lme_out <- nlme::lme(fixed = fixed_form_curr,
                           random = list(USUBJID = ~ 1), 
                           correlation = nlme::corCAR1(form = ~ AVISITN | USUBJID),
                           weights = nlme::varFixed(~s^2),
                           data = results_df,
                           control = nlme::lmeControl(sigma = 1),
                           method = "ML")
      summ_curr <- summary(lme_out)
      mn_fixed <- summ_curr$coefficients$fixed
      cov_fixed <- summ_curr$varFix
      se_fixed <- sqrt(diag(cov_fixed))
      for (arm_curr in arm_all) {
        if (estimate_type == "contrasts_vs_placebo") {
          if (arm_curr == placebo_label) {
            coef_names <- paste0("time_fac", t_unique)
          } else {
            coef_names <- paste0("time_fac", t_unique, ":arm", arm_curr)
          }
        }
        if (estimate_type == "treatment_means") {
          coef_names <- paste0("arm", arm_curr, ":time_fac", t_unique)
        }
        m_coef_mat[, arm_curr] <- mn_fixed[coef_names]
        u_coef_mat[, arm_curr] <-mn_fixed[coef_names] + 2 * se_fixed[coef_names]
        l_coef_mat[, arm_curr] <- mn_fixed[coef_names] - 2 * se_fixed[coef_names]
      }
      results_list[[fac_curr]][[estimate_type]] <- list(m = m_coef_mat,
                                                        u = u_coef_mat,
                                                        l = l_coef_mat)
    }
  }
  analysis_results_list[[analysis_curr]] <- results_list
  analysis_results_list[[analysis_curr]]$mfd_object <- mfd_object_list[[analysis_curr]]
  analysis_results_list[[analysis_curr]]$arm_all <- arm_all
  analysis_results_list[[analysis_curr]]$arm_no_plac <- arm_no_plac
  
}
saveRDS(analysis_results_list, file = file.path("output", "analysis_results_list.RDS"))
# saveRDS(mfd_object_list, file = file.path("output", "mfd_object_list.RDS"))
# saveRDS(control$do_list, file = file.path("output", "do_list.RDS"))


########################################
# Plot results
export_plots <- T
analyses_plot <- analysis_names
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
  for (analysis_curr in analyses_plot) {
    image(1:n_fac, 1:n_endpt, t(mfd_object_list[[analysis_curr]]$A),
          col = col_heat, zlim = c(-1, 1), xaxt = "n", yaxt = "n")
    axis(side = 2, at = 1:n_endpt, labels = endpts, las = 2, cex = cexax)
    mtext(side = 2, line = 8, text = analysis_curr)
  }
  col_arm <- viridis::viridis(n = 5, alpha = .5)
  col_arm_opaque <- viridis::viridis(n = 5)
  arm_all <- c("Placebo", "75mg", "150mg", "300mg")
  names(col_arm) <- arm_all
  for (fac_curr in 1:n_fac) {
    for (analysis_curr in analyses_plot) {
      arms_in <- analysis_sub_data[[analysis_curr]][[arm_include_label]]
      m_pl <- analysis_results_list[[analysis_curr]][[fac_curr]][[estimate_type]]$m[, arms_in]
      l_pl <- analysis_results_list[[analysis_curr]][[fac_curr]][[estimate_type]]$l[, arms_in]
      u_pl <- analysis_results_list[[analysis_curr]][[fac_curr]][[estimate_type]]$u[, arms_in]
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
      for (arm_curr in analysis_sub_data[[analysis_curr]][[arm_include_label]]) {
        lines(x = t_unique, y = m_pl[, arm_curr], col = col_arm[arm_curr], lwd = 2)
        polygon(x = c(t_unique, rev(t_unique)),
                y = c(l_pl[, arm_curr], rev(u_pl[, arm_curr])),
                col = col_arm[arm_curr],
                border = 1)
      }
      for (arm_curr in analysis_sub_data[[analysis_curr]][[arm_include_label]]) {
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



























# col_arm <- RColorBrewer::brewer.pal(n = 5, name = "Dark2", alpha = .5)
# col_arm_opaque <- RColorBrewer::brewer.pal(n = 5, name = "Dark2")











