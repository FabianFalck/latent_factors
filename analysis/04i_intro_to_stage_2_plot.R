source("scripts/04_plotting_intro.R")


study_plot <- "F2342"
analysis_to_plot <- "PsA_F2342"
subj_unique <- unique(d_time$USUBJID[d_time$study == study_plot])
n_miss<-n_week <- c()

for (subj_plot in subj_unique) {
  d_time_sub <- d_time[which(d_time$USUBJID == subj_plot & d_time$AVISITN %in% c(0, 2, 4, 8, 12, 16)), ]
  ymat <- d_time_sub[, nms_params_v2$PARAMCD]
  n_miss[subj_plot] <- sum(is.na(ymat))
  n_week[subj_plot] <- nrow(ymat)
}

treatment <- d_time[match(subj_unique, d_time$USUBJID), "newTRT"]
names(treatment) <- subj_unique

table(n_miss)
n_each_plot <- 4
n_week_keep <- 6
set.seed(1)
arms_plot <- c("Placebo", "300mg")
subj_plot_list <- list()
subj_plot_list[["Placebo"]] <- sample(subj_unique[treatment == "Placebo" & n_week == n_week_keep], size = n_each_plot)
subj_plot_list[["300mg"]] <- sample(subj_unique[treatment == "300mg" & n_week == n_week_keep], size = n_each_plot)

# subj_unique[which(n_miss == 8)[1]]#"CAIN457F2206_0153_00002"


# # control$col_arm["300mg"] <- control$col_arm["300mg"]
# graphics.off()
# export_plots <- TRUE
# if (export_plots){
#   pdf(file = paste0("figures/intro_to_factor_models.pdf"), 12, 8)
# }





# control$col_arm["300mg"] <- control$col_arm["300mg"]
graphics.off()
export_plots <- F
if (export_plots){
  pdf(file = paste0("figures/intro_to_stage_2.pdf"), 12, 8)
}



week_plot <- c(0, 2, 4, 8, 12, 16)
side_marg_wid <- .4
cexax <- .75
for (arm_plot in arms_plot[1:2]) {
  for (subj_num in 1:n_each_plot) {
    subj_plot <- subj_plot_list[[arm_plot]][subj_num]
    d_time_sub <- d_time[which(d_time$USUBJID == subj_plot & d_time$AVISITN %in% week_plot), ]
    week_plot_sub <- d_time_sub$AVISITN
    study <- d_time_sub$study[1]
    indication <- control$study_indic[grep(study, names(control$study_indic))]
    mfd_obj <- analysis_results_list[[paste0(indication, "_", study_plot)]]$mfd_object
    Z <- mfd_obj$Z[which(mfd_obj$data_out$USUBJID == subj_plot), ]
    S <- mfd_obj$S[which(mfd_obj$data_out$USUBJID == subj_plot), ]
    
    ymat <- d_time_sub[, nms_params_v2$PARAMCD]
    rownames(ymat) <- d_time_sub$AVISITN
    n_meas <- nrow(nms_params_v2)
    
    ##########################
    # Add scores
    par(mar = c(0, 0, 0, 0), oma = c(1, 5, 4, 1))
    fac_coords <- list()
    score_y_offset <- .333
    score_plot_hei <- .09
    centre_y <- .5
    treat_plac_offset <- .075
    score_plot_wid <- side_marg_wid / 1.5 / n_each_plot
    horiz_offset <- 0#score_plot_wid / 2
    for (fac_num in 1:3) {
      if (arm_plot == "Placebo") {
        fac_coords[[fac_num]] <- c(subj_num / (n_each_plot + 1) * side_marg_wid + c(-1, 1) * score_plot_wid / 2,
                                   centre_y + c(1, 0, -1)[fac_num] * score_y_offset + c(-1, 1) * score_plot_hei / 2 + treat_plac_offset)
      }
      if (arm_plot == "300mg") {
        fac_coords[[fac_num]] <- c(subj_num / (n_each_plot + 1) * side_marg_wid + c(-1, 1) * score_plot_wid / 2 + horiz_offset,
                                   centre_y + c(1, 0, -1)[fac_num] * score_y_offset + c(-1, 1) * score_plot_hei / 2 - treat_plac_offset)
      }
      par(fig = fac_coords[[fac_num]], new = T)
      
      plot(week_plot, Z[, fac_num], ty = "n", 
           ylim = c(-1, 1) * 3,#base::range(c(c(Z + 2 * S), c(Z - 2 * S)), na.rm = T),
           xaxt = "n",
           yaxt = "n",
           las = 1,
           ylab = "",
           xlab = "")
      col_curr <- control$col_arm[arm_plot]
      lines(week_plot, Z[, fac_num], lwd = 2, col = col_curr)
      lines(week_plot, Z[, fac_num] + 2 * S[, fac_num], lwd = 1, col = col_curr)
      lines(week_plot, Z[, fac_num] - 2 * S[, fac_num], lwd = 1, col = col_curr)
      if (subj_num == 1) {
        axis(side = 2, las = 2, cex.axis = .8,
             at = c(-2, 0, 2), labels = c(-2, 0, 2))
      }
      axis(side = 1,
           at = c(0, 15),
           labels = NA,
           cex.axis = cexax,
           tcl = -.25)
      mtext(side = 1,
            at = c(0, 15),
            text = c(0, 15),
            cex = cexax)
      abline(h = 0, lty = 3)
      lab_curr <- paste0(ifelse(arm_plot == "Placebo", "Placebo", "Treated"), " ", subj_num)
      mtext(side = 3, text = lab_curr, cex = cexax)
      # axis(side = 4, 
      #      cex.axis = cexax,
      #      las = 2)
      # mtext(side = 3, 
      #       text = paste0("Factor ", fac_num), 
      #       line = .15,
      #       cex = .9)
      # mtext(side = 1, 
      #       line = .05, 
      #       text = "Week", 
      #       cex = cexax)
    }
    # mtext(side = 3, text = "Outputs: scores, Z", line = 2)
  }  
}

for (fac_num in 1:3) {
  mtext(outer = T,
      at = c(5, 3, 1)[fac_num] / 6, 
      side = 2, 
      text = paste0("Factor ", fac_num),
      las = 0)
}

mean_score_plot_wid <- .1
mean_score_plot_hei <- .15
score_coords<-ri_coords<-ar1_coords<-res_coords <- list()
for (fac_num in 1:n_fac) {
  score_coords[[fac_num]] <- c(side_marg_wid + (1 - side_marg_wid) * .15 + c(-1, 1) * mean_score_plot_wid / 2,
                               centre_y + c(1, 0, -1)[fac_num] * score_y_offset + c(-1, 1) * mean_score_plot_hei / 2)
  ri_coords[[fac_num]] <- c(side_marg_wid + (1 - side_marg_wid) * .45 + c(-1, 1) * mean_score_plot_wid / 2,
                               centre_y + c(1, 0, -1)[fac_num] * score_y_offset + c(-1, 1) * mean_score_plot_hei / 2)
  ar1_coords[[fac_num]] <- c(side_marg_wid + (1 - side_marg_wid) * .65 + c(-1, 1) * mean_score_plot_wid / 2,
                             centre_y + c(1, 0, -1)[fac_num] * score_y_offset + c(-1, 1) * mean_score_plot_hei / 2)
  res_coords[[fac_num]] <- c(side_marg_wid + (1 - side_marg_wid) * .85 + c(-1, 1) * mean_score_plot_wid / 2,
                             centre_y + c(1, 0, -1)[fac_num] * score_y_offset + c(-1, 1) * mean_score_plot_hei / 2)
  
  par(fig = score_coords[[fac_num]], new = T)
  plot_scores(control, 
              analysis_results_list,
              analysis_plot = analysis_to_plot,
              fac_plot = fac_num,
              estimate_type = "treatment_means",
              arms_in = arms_plot,
              add_legend = FALSE,
              cex.axis = .7,
              xaxt = "n",
              yaxt = "n",
              add_zero_horiz_line = F)
  mtext(side = 3, text = paste0("Factor ", fac_num))
  abline(h = 0, lty = 3)
  axis(side = 2, las = 2, cex.axis = .8,
       at = c(-1, 0, 1), labels = c(-1, 0, 1))
  axis(side = 1,
       at = c(0, 15),
       labels = NA,
       cex.axis = cexax,
       tcl = -.25)
  mtext(side = 1,
        at = c(0, 15),
        text = c(0, 15),
        cex = cexax)
  ar1_covariance <- function(n_time, rho, noise_sd, intercept_sd) {
    R_AR1 <- rho ^ abs(outer(1:n_time, 1:n_time, "-")) # AR1 temporal correlation
    R_diagonal <- diag(n_time)
    R_ones <- matrix(1, n_time, n_time)
    delta_cov <- noise_sd^2 * R_AR1 + intercept_sd^2 * R_ones
    return(delta_cov)
  }
  
  greys <- grey(seq(1, 0, len = 1000))
  ar1_cov <- ar1_covariance(n_time = 17, rho = .95, noise_sd = 1, intercept_sd = 0)
  ar1_cov <- ar1_cov[, rev(1:nrow(ar1_cov))]
  par(fig = ar1_coords[[fac_num]], new = T)
  image(x = 1:17, y = 1:17, xaxt = "n", yaxt = "n", z = ar1_cov, col = greys)
  axis(side = 1,
       at = c(1, 16),
       labels = NA,
       cex.axis = cexax,
       tcl = -.25)
  mtext(side = 1,
        at = c(1, 16),
        text = c(0, 15),
        cex = cexax)
  axis(side = 2,
       at = c(1, 16),
       labels = NA,
       cex.axis = cexax,
       tcl = -.25)
  mtext(side = 2,
        at = c(1, 16),
        text = c(0, 15),
        cex = cexax)
  
  
  ri_cov <- ar1_cov
  ri_cov[] <- 1
  par(fig = ri_coords[[fac_num]], new = T)
  image(x = 1:17, y = 1:17, xaxt = "n", yaxt = "n", z = ri_cov, col = grey(0))
  axis(side = 1,
       at = c(1, 16),
       labels = NA,
       cex.axis = cexax,
       tcl = -.25)
  mtext(side = 1,
        at = c(1, 16),
        text = c(0, 15),
        cex = cexax)
  axis(side = 2,
       at = c(1, 16),
       labels = NA,
       cex.axis = cexax,
       tcl = -.25)
  mtext(side = 2,
        at = c(1, 16),
        text = c(0, 15),
        cex = cexax)
  
  
  res_cov <- ar1_cov
  res_cov[] <- 0
  diag(res_cov) <- runif(17, .75, 1.25)
  res_cov <- res_cov[, rev(1:nrow(res_cov))]
  
  par(fig = res_coords[[fac_num]], new = T)
  image(x = 1:17, y = 1:17, xaxt = "n", yaxt = "n", z = res_cov, col = greys)
  axis(side = 1,
       at = c(1, 16),
       labels = NA,
       cex.axis = cexax,
       tcl = -.25)
  mtext(side = 1,
        at = c(1, 16),
        text = c(0, 15),
        cex = cexax)
  axis(side = 2,
       at = c(1, 16),
       labels = NA,
       cex.axis = cexax,
       tcl = -.25)
  mtext(side = 2,
        at = c(1, 16),
        text = c(0, 15),
        cex = cexax)
  
}
if (export_plots) {
  dev.off()
}
















week_plot <- c(0, 2, 4, 8, 12, 16)
side_marg_wid <- .4
cexax <- .75
for (arm_plot in arms_plot[1:2]) {
  for (subj_num in 1:n_each_plot) {
    subj_plot <- subj_plot_list[[arm_plot]][subj_num]
    d_time_sub <- d_time[which(d_time$USUBJID == subj_plot & d_time$AVISITN %in% week_plot), ]
    week_plot_sub <- d_time_sub$AVISITN
    study <- d_time_sub$study[1]
    indication <- control$study_indic[grep(study, names(control$study_indic))]
    mfd_obj <- analysis_results_list[[paste0(indication, "_", study_plot)]]$mfd_object
    Z <- mfd_obj$Z[which(mfd_obj$data_out$USUBJID == subj_plot), ]
    S <- mfd_obj$S[which(mfd_obj$data_out$USUBJID == subj_plot), ]
    
    ymat <- d_time_sub[, nms_params_v2$PARAMCD]
    rownames(ymat) <- d_time_sub$AVISITN
    n_meas <- nrow(nms_params_v2)
    
    ##########################
    # Add scores
    par(mar = c(0, 0, 0, 0))
    fac_coords <- list()
    score_y_offset <- .21
    score_plot_hei <- .1
    centre_y <- .5
    score_plot_wid <- side_marg_wid / 2 / n_each_plot
    for (fac_num in 1:3) {
      if (arm_plot == "Placebo") {
        fac_coords[[fac_num]] <- c(subj_num / (n_each_plot + 1) * side_marg_wid + c(-1, 1) * score_plot_wid / 2,
                                   centre_y + c(-1, 0, 1)[fac_num] * score_y_offset + c(-1, 1) * score_plot_hei / 2)
      }
      if (arm_plot == "300mg") {
        fac_coords[[fac_num]] <- c(1 - subj_num / (n_each_plot + 1) * side_marg_wid + c(-1, 1) * score_plot_wid / 2,
                                   centre_y + c(-1, 0, 1)[fac_num] * score_y_offset + c(-1, 1) * score_plot_hei / 2)
      }
      par(fig = fac_coords[[fac_num]], new = T)
      
      plot(week_plot, Z[, fac_num], ty = "n", 
           ylim = base::range(c(c(Z + 2 * S), c(Z - 2 * S)), na.rm = T),
           xaxt = "n",
           yaxt = "n",
           las = 1,
           ylab = "",
           xlab = "")
      col_curr <- control$col_arm[arm_plot]
      lines(week_plot, Z[, fac_num], lwd = 2, col = col_curr)
      lines(week_plot, Z[, fac_num] + 2 * S[, fac_num], lwd = 1, col = col_curr)
      lines(week_plot, Z[, fac_num] - 2 * S[, fac_num], lwd = 1, col = col_curr)
      # axis(side = 1, 
      #      at = c(0, 15), 
      #      labels = NA,
      #      cex.axis = cexax,
      #      tcl = -.25)
      # mtext(side = 1, 
      #       at = c(0, 15), 
      #       text = c(0, 15),
      #       cex = cexax)
      # axis(side = 4, 
      #      cex.axis = cexax,
      #      las = 2)
      # mtext(side = 3, 
      #       text = paste0("Factor ", fac_num), 
      #       line = .15,
      #       cex = .9)
      # mtext(side = 1, 
      #       line = .05, 
      #       text = "Week", 
      #       cex = cexax)
    }
    # mtext(side = 3, text = "Outputs: scores, Z", line = 2)
  }  
}

mean_score_plot_wid <- .15
mean_score_plot_hei <- .15
score_coords <- list()
for (fac_num in 1:n_fac) {
  score_coords[[fac_num]] <- c(.5 + c(-1, 1) * mean_score_plot_wid / 2,
                               centre_y + c(-1, 0, 1)[fac_num] * score_y_offset + c(-1, 1) * mean_score_plot_hei / 2)
  
  par(fig = score_coords[[fac_num]], new = T)
  plot_scores(control, 
              analysis_results_list,
              analysis_plot = analysis_to_plot,
              fac_plot = fac_num,
              estimate_type = "treatment_means",
              arms_in = arms_plot,
              add_legend = FALSE)
  
}








graphics.off()
if (export_plots){
  pdf(file = paste0("figures/intro_to_factor_models.pdf"), 12, 8)
}

upper_band_wid <- c(.1)
right_band_wid <- .25
left_cen <- .12
upper_levels <- c(.75, .875)
right_cen <- .75#1 - right_band_wid - left_cen
arrow_col <- grey(.7)
arrow_length <- .25
arrow_width <- arrow_length * .75

##########################
# Add raw plots
upper_centres_x <- seq(left_cen, right_cen, length.out = n_meas)
upper_centres_y <- rep(upper_levels, length.out = n_meas)
names(upper_centres_y) <- names(upper_centres_x) <- nms_params_v2$PARAMCD
load_plot_hei <- .3
load_plot_wid <- .6
load_centre_x <- mean(upper_centres_x)
load_centre_y <- upper_levels[1] / 2
upper_plot_wid <- diff(base::range(upper_centres_x)) / n_meas 
upper_plot_hei <- upper_plot_wid * 1.15
par(mar = c(0, 0, 0, 0) * 2)
coords <- list()
plot(0, axes = F, ty = "n")
par(xpd = NA)
meas_codes <- nms_params_v2$PARAMCD
for (meas in meas_codes) {
  coords[[meas]] <- c(upper_centres_x[meas] + c(-1, 1) * upper_plot_wid / 2, 
                      upper_centres_y[meas] + c(-1, 1) * upper_plot_hei / 2)
  
  par(fig = coords[[meas]], new = T)
  ypl <- ymat[as.character(week_plot), meas]
  par(mar = c(0, .5, 0, 0))
  cexax <- .75
  plot(week_plot, ypl, 
       ty = "b", 
       axes = T,
       xaxt = "n",
       yaxt = "s",
       ylab = "",
       xlab = "",
       bty = "l",
       cex = .6,
       pch = 19,
       las = 1,
       cex.axis = cexax)
  axis(side = 1, 
       at = c(0, 15), 
       labels = NA,
       cex.axis = cexax,
       tcl = -.25
  )
  mtext(side = 1, 
        at = c(0, 15), 
        text = c(0, 15),
        cex = cexax)
  mtext(side = 1, 
        line = .05, 
        text = "Week", 
        cex = cexax)
}
text(x = grconvertX(mean(upper_centres_x), from = "ndc"),
     y = grconvertY(upper_levels[2] + (1 - upper_levels[2]) * .65, from = "ndc"),
     labels = "Inputs: raw data from a single patient, Y")


# text_transform <- "Transform and take weighted average to generate factor scores (loading weights below)"
# llegend(x = grconvertX(left_cen / 2, from = "ndc"),
#      y = grconvertY(upper_levels[1] - (1 - upper_levels[1]) * .6, from = "ndc"),
#      labels = "Weighted average")

##########################
# Add scores
par(mar = c(0, 0, 0, 0))
fac_coords <- list()
score_y_offset <- .21
score_plot_hei <- .14
score_plot_wid <- .1
for (fac_num in 1:3) {
  fac_coords[[fac_num]] <- c(1 - right_band_wid * .4 + c(-1, 1) * score_plot_wid / 2,
                             load_centre_y + c(-1, 0, 1)[fac_num] * score_y_offset + c(-1, 1) * score_plot_hei / 2)
  par(fig = fac_coords[[fac_num]], new = T)
  
  plot(week_plot, Z[, fac_num], ty = "n", 
       ylim = base::range(c(c(Z + 2 * S), c(Z - 2 * S)), na.rm = T),
       xaxt = "n",
       yaxt = "n",
       las = 1,
       ylab = "",
       xlab = "")
  lines(week_plot, Z[, fac_num], lwd = 2)
  lines(week_plot, Z[, fac_num] + 2 * S[, fac_num], lwd = 1)
  lines(week_plot, Z[, fac_num] - 2 * S[, fac_num], lwd = 1)
  axis(side = 1, 
       at = c(0, 15), 
       labels = NA,
       cex.axis = cexax,
       tcl = -.25)
  mtext(side = 1, 
        at = c(0, 15), 
        text = c(0, 15),
        cex = cexax)
  axis(side = 4, 
       cex.axis = cexax,
       las = 2)
  mtext(side = 3, 
        text = paste0("Factor ", fac_num), 
        line = .15,
        cex = .9)
  mtext(side = 1, 
        line = .05, 
        text = "Week", 
        cex = cexax)
}
mtext(side = 3, text = "Outputs: scores, Z", line = 2)

##########################
# Add loadings heatmap
coords$loadings <- c(coords[[meas_codes[1]]][1], coords[[meas_codes[length(meas_codes)]]][2],
                     load_centre_y + c(-1, 1) * load_plot_hei / 2)
coords$loadings
par(fig = coords$loadings, new = T)

par(mar = c(0, 0, 0, 0))
plot_loadings(control, 
              analysis_results_list, 
              analysis_plot = "PsA_F2312_cross_indication_loadings",
              add_legend = F, 
              rotate = TRUE, 
              add_numbers = T)

for (meas in meas_codes) {
  xends <- grconvertX(rep(mean(coords[[meas]][1:2]), 2), from = "ndc", to = "user")
  yends <- grconvertY(c(coords[[meas]][3] - .04, coords$loadings[4]), from = "ndc", to = "user")
  shape::Arrows(x0 = xends[1], x1 = xends[2],
                y0 = yends[1], y1 = yends[2],
                lwd = 1, 
                arr.type = "triangle",
                col = arrow_col,
                arr.col = arrow_col,
                arr.adj = 1,
                arr.length = arrow_length,
                arr.width = arrow_width)
}

for (fac_num in 1:3) {
  yends <- c(fac_num, grconvertY(mean(fac_coords[[fac_num]][3:4]), from = "ndc", to = "user"))
  xends <- c(n_meas + .5, grconvertX(fac_coords[[fac_num]][1], from = "ndc", to = "user"))
  shape::Arrows(x0 = xends[1], x1 = xends[2],
                y0 = yends[1], y1 = yends[2],
                lwd = 1, 
                arr.type = "triangle",
                col = arrow_col,
                arr.col = arrow_col,
                arr.adj = 1,
                arr.length = arrow_length,
                arr.width = arrow_width)
  
}

text_transform <- "Take weighted average of transformed data using loadings, A, below"

legend(x = grconvertX(mean(upper_centres_x), from = "ndc"),
       y = grconvertY(upper_levels[1] - (1 - upper_levels[1]) * .6, from = "ndc"),
       legend = text_transform, 
       bg = "white",
       xjust = .5,
       yjust = .5,
       box.col = "white")

if (export_plots){
  dev.off()
}


