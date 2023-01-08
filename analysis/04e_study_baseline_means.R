source("scripts/04_plotting_intro.R")
##########################################################
# Explore variation across indication at baseline
meas_names <- nms_params_v2$PARAMCD
n_meas <- length(meas_names)
single_studies <- c("F2312", "F2342", "F2302", "F2309")
single_studies_with_indic <- paste(c("F2312", "F2342", "F2302", "F2309"), rep(c(" (PsA)", " (RA)"), each = 2))
mns <- sapply(single_studies, function(x) 
  colMeans(d_step1[d_step1$STUDYID == paste0("CAIN457", x)& d_step1$AVISITN == 0, meas_names], na.rm = T))

ses <- sapply(single_studies, function(x) 
  apply(d_step1[d_step1$STUDYID == paste0("CAIN457", x) & d_step1$AVISITN == 0, meas_names], 2, 
        function(v) sd(v, na.rm = T) / sqrt(sum(!is.na(v)))))

graphics.off()
par(oma = c(8, 3, 1, 1))
eps<- .15
epsmat <- matrix(seq(-eps, eps, length.out = 4), nrow = n_meas, ncol = 4, byrow = TRUE)
atmat <- matrix(1:n_meas, nrow = n_meas, ncol = 4, byrow = FALSE) + epsmat
matplot(x = atmat, y = mns, ty = "p", pch = 19, xaxt = "n", log = "y", xaxs = "i", 
        xlim = c(0.5, n_meas + .5), cex = .75, xlab = "", las = 1, 
        ylab = "Mean +/- 2*SE (log scale)",
        main = "Baseline study means (+/- 2*SE)")
abline(v = 1:nrow(atmat) - .5)
for (i in 1:nrow(atmat)) {
  for (j in 1:4) {
    lines(x = rep(atmat[i, j], 2), y = mns[i, j] + c(-2, 2) * ses[i, j], col = j)
  }
}
axis(side = 1, at = 1:n_meas, labels = nms_params_v2$PARAM, las = 2)
legend(x = "topright", legend = single_studies_with_indic, pch = 19, col = 1:4)


