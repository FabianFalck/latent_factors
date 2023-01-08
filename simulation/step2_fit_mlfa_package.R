


## packages before reading mlfa packages in NVS davinci system (but it may work directly in BDI system)
library("pcaMethods")
library("rapportools")
library("gsubfn")
library("itsadug") 
library("itertools")
library("gplots")
library("maditr")
library("corrplot")
library(mgcv)
library("maditr")

########### reading mlfa package in Fabian's repository 
path_LFpackage<-"/../../mlfa_package/R"
source(file.path(path_LFpackage,"other.R"))
source(file.path(path_LFpackage,"mfd.R"))
source(file.path(path_LFpackage,"mlfa.R"))
source(file.path(path_LFpackage,"misc.R"))




## packages to plot, read, or reshape the data format
library("tidyverse")
library(tidyr)
library(ggplot2)
library(readr)
library("maditr")
library("tibble")
library("reshape")
library("RColorBrewer")
library("labeling")
library("farver")

###packages for simulating data in NVS davinci system
library(mvtnorm)
library("matrixsampling")
library(nlme)
 


#### To simulate data from step 1 script
test_iters<-simulation_iterations_data(Iterations=10,seed=1,
                                       n = 200,p = 10,k = 5,TRT_prob=0.5,endpoint_proportions= c(0.2,0.3, 0.2,0.1, 0.2), 
                                       time=c(0,4,8,12,16),cor_ar1_lag=0.2,E0_null=0,E50_null=7,
                                       Emax_null=3,beta_E0 = 0.25,beta_E50= -0.9,beta_Emax=1.5)



##########################################
## step 2: fit mlfa package ##############
###########################################
test_data<-test_iters[[1]]

test_data_Y<-test_data$Y
test_data_subj<-test_data$dbase

## begin to fit
test_data_Y_v2<-as.tibble(test_data_Y)
vr_nms<-paste0("Y",1:10)
###################
## NS.transform
# Transform numeric columns with normal score transformation:
ns.transform <- function(y) {
  library("tidyverse")
  # Normal Q-Q plot:
  ns <- qqnorm(y, plot.it=FALSE)
  ns <- tbl_df(data.frame(x=ns$x, y=ns$y)) %>% dplyr::group_by(factor(y)) %>% dplyr::summarise(x=mean(x), y=mean(y))
  # Find the functional relationship between normal and empirical quantiles:
  ns.transform.fun <- approxfun(ns$y,ns$x, rule=2)
  yt <- ns.transform.fun(y)
  return(yt)
}

test_data_Y_v3 <- mutate_if(as.data.frame(test_data_Y_v2), colnames(test_data_Y_v2) %in% vr_nms, ns.transform)

kk2<-test_data_Y_v3 %>% as.tibble()
############

## 5 factors ############
n_factors<-5
mfd_object0 <-mfd( data = kk2, type="per_largest_abs",standardize_A_type = "none",
                   vars =vr_nms, time = "Time", subject = "ID",loadings.n_PCs = n_factors,seed=1, 
                   Time_base=0,loadings.sparse_abs_thres = 0)

shrinkage_level<-0.5  ## threshold to cut off for A loading matrix

# plot to see coefficients in A loading matrix if not shrinking
plot_coefficients(mfd_object0$A, save_path= NULL, axis_y_text_size = 15,by.facet="N",
                  add_dashline=shrinkage_level,color_dashline="black")

mfd_object <-mfd(
  data = kk2, type="per_largest_abs",standardize_A_type = "none", 
  vars =vr_nms, time = "Time", subject = "ID",loadings.n_PCs = n_factors,
  seed=1,Time_base=0,loadings.sparse_abs_thres = shrinkage_level)


# plot for variance explained in the result
plot_mfd_ppca(mfd_object,color="#003333",labels=paste0("Domain",c(1:n_factors)))

A_matrix<-mfd_object$A

color_pallette<-c("darkred","white","navy")

A_matrix_v2<-A_matrix

## heatmap for A loading matrix
plot_loadings_heatmap(input_coefficients=A_matrix_v2, save_path= NULL, 
                      axis_y_text_size = 1,axis_x_text_size=2,title_text_size=NULL,
                     color.pallette=color_pallette,
                      num_color_bins = NULL,margins=c(10,14),
                      title="     ",border_color="grey")  

########### 
mfd_object$data_out$Time<-as.numeric(mfd_object$data_out$Time)
# plot for Z trajectories
plot_z_by_group(mfd_object, save_path=NULL,
                groups="TRT",time="Time",subject="ID",subject_data = test_data_subj, font.size=15,
                type="CI",selected_subjects = NULL,xlab="Time (Week)",
                ylab="Score of change from baseline",labels=paste0("Domain",c(1:n_factors)),
                # ylim=c(-3,0.5),
                ylim=c(-1,1),
                x.breaks=c(0,4,8,12,16,20,24),
                color.theme="Set1",
                ncol=3)









