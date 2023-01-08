set.seed(2000)

library(mvtnorm)
library(nlme)
library("matrixsampling")
library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)

##########################################
# n: number of patients
# p: number of clinical endpoints
# k: number of factors
# TRT_prob: probability to randomly generate treatment, where 1 is active and 0 is placebo  
# time: Time vector
# A_prob: probability to randomly sample from (1,0) for A matrix
# E0_null: E0 population level NULL parameter 
# E50_null: E50 population level NULL parameter 
# Emax_null: Emax population level NULL parameter 
# beta_E0:  treatment effect in E0
# beta_E50: treatment effect in E50
# beta_Emax: treatment effect in Emax

simu_single_data<-function(seed=1,
                           n = 200,p = 10,k = 5,TRT_prob=0.5,endpoint_proportions= c(0.2,0.3, 0.2,0.1, 0.2), 
                           time=c(0,4,8,12,16),cor_ar1_lag=0.2,
                           E0_null=0,E50_null=7,Emax_null=3,beta_E0 = 0.25,beta_E50= -0.9,beta_Emax=1.5){
  #treatment
  # trt=sample(c(1,0), n, replace = TRUE, prob = c(TRT_prob,(1-TRT_prob))) 
  fixed_proportion_set <- c(rep(0, as.integer(TRT_prob * n)), rep(1, as.integer((1-TRT_prob) * n)))
  trt <- sample(fixed_proportion_set, n, replace = FALSE)
  # loading matrix A
  # Constructing block-wise A, where 
  # each entry is either 0 or 1, and
  # each endpoint is 1 ("active") in exactly one factor
  A_loadings <- matrix(0, nrow = k, ncol = p)
  start_index <- 1
  for (i in 1:k) {
    end_index <- start_index + as.integer(endpoint_proportions[i] * p) - 1
    count <- end_index - start_index + 1
    A_loadings[i, start_index:end_index] <- rep(1, count)
    start_index <- end_index + 1
  }
  
  rownames(A_loadings)<-paste0("K",c(1:k))
  colnames(A_loadings)<-paste0("P",c(1:p))
  
  # dataset with patient ID and treatment
  dbase<-data.frame(ID=c(1:n),TRT=trt)
  
  # random intercept theta_RI
  theta_RI=1
  # random residual sd
  theta_resid=1
  
  #simulate Z matrix
  Z_matrix<-NULL
  for (K_th in 1:k){
    
    # E0 is equal to E0_null +trt effect if active arm and plus random intercept
    Z_E0<-E0_null*(1+t(beta_E0 %*% t(as.matrix(trt)))) + rnorm(n, mean=0, sd=theta_RI)
    
    # E50 and Emax does not have random error
    Z_E50<-E50_null*(1+t(beta_E50 %*% t(as.matrix(trt))))  
    Z_Emax<-Emax_null*(1+t(beta_Emax %*% t(as.matrix(trt))))  
    
    Z_kth<-NULL
    for (t_th in unique(time)){
      
      Z_nt<-Z_E0+(Z_Emax*(t_th))/(Z_E50+t_th)+rnorm(n, mean=0, sd=theta_resid)
      Z_nt_tmp<-data.frame(Z=Z_nt,Time=t_th,K=paste0("Z",K_th),ID=1:n,TRT=trt)
      Z_kth<-rbind(Z_kth,Z_nt_tmp)
    }
    ## CAR1
    cs1AR1 <- corAR1(cor_ar1_lag, form = ~ week)
    d_demo <- data.frame(week = time)
    cs1AR1. <- Initialize(cs1AR1, data = d_demo)
    CAR1_theta<-as.matrix(corMatrix(cs1AR1.) )
    Z_kth2<-Z_kth %>% spread(Time, Z) 
    Z_kth3<-(Z_kth2 %>% select(!c(K,ID,TRT)) %>% as.matrix()) %*%  CAR1_theta
    colnames(Z_kth3)<-time
    
    Z_kth_final<-melt(cbind(Z_kth2 %>% select(c(K,ID,TRT)),Z_kth3),id=c("K","ID","TRT"))%>% 
      dplyr:: rename(Time=variable,Z=value)
    
    Z_matrix<-rbind(Z_matrix,Z_kth_final)
  }
  
  Z<- Z_matrix %>% spread(K, Z) 
  
  # simulate Y matrix
  sigma<-1
  Z_scores<-Z %>% select(!c("ID","Time","TRT"))
  
  Y_resid<-matrix(rnorm(n*p*length(time), mean=0, sd=sigma),n*length(time))
  
  Y_scores<-as.matrix(Z_scores) %*% (A_loadings)+Y_resid
  colnames(Y_scores)<-paste0("Y",1:p)
  Y<-cbind(Z %>% select(c("ID","Time","TRT")),Y_scores)
  #
  
  output<-list(A_loadings=A_loadings, Z=Z, Y=Y, time=time,dbase=dbase,
               parameters=c(seed=seed, 
                            n = n, p = p,k = k,TRT_prob=TRT_prob,  
                            E0_null=E0_null,E50_null=E50_null,Emax_null=Emax_null,
                            beta_E0 = beta_E0,beta_E50= beta_E50,beta_Emax=beta_Emax))
  return(output)
  
}

##
# test_Y<-simu_single_data(seed=1,n = 200,p = 10,k = 5,TRT_prob=0.5,A_prob=0.3, time=c(0,4,8,12,16),
#                E0_null=0,E50_null=7,Emax_null=3,beta_E0 = 0.25,beta_E50= -0.9,beta_Emax=1.5)
# 
# names(test_Y)
####

#################################################################

simulation_iterations_data<-function(Iterations=100,seed=1,
                                     n = 200,p = 10,k = 5,TRT_prob=0.5,endpoint_proportions= c(0.2,0.3, 0.2,0.1, 0.2), 
                                     time=c(0,4,8,12,16),cor_ar1_lag=0.2,
                                     E0_null=0,E50_null=7,Emax_null=3,beta_E0 = 0.25,beta_E50= -0.9,beta_Emax=1.5){
  
  result<-list()
  for (iter in 1:Iterations){
    seed.iter<-seed+iter
    
    result_iter<-simu_single_data(seed=seed.iter,n = n,p = p,k = k,TRT_prob=TRT_prob,cor_ar1_lag=cor_ar1_lag,
                                  endpoint_proportions=endpoint_proportions, time=time,
                                  E0_null=E0_null,E50_null=E50_null,Emax_null=Emax_null,
                                  beta_E0 = beta_E0,beta_E50= beta_E50,beta_Emax=beta_Emax)
    
    result[[iter]]<-result_iter
  }
  
  return(result)
}

################

test_iters<-simulation_iterations_data(Iterations=10,seed=1,
                                       n = 200,p = 10,k = 5,TRT_prob=0.5,endpoint_proportions= c(0.2,0.3, 0.2,0.1, 0.2), 
                                       time=c(0,4,8,12,16),cor_ar1_lag=0.2,E0_null=0,E50_null=7,
                                       Emax_null=3,beta_E0 = 0.25,beta_E50= -0.9,beta_Emax=1.5)


test_iters[[1]]







######################################################################
###################################################################

##plot
Y_melt<-melt(test_iters[[1]]$Y,id=c("ID","Time","TRT"))  

font.size<-12
ggplot(data=Y_melt, 
       aes(x=Time, y=value,colour=variable))+
  geom_line(alpha=0.2, aes(group=ID))+
  facet_grid(TRT~variable)+
  theme_bw() + 
  theme(axis.text.x = element_text(size=font.size), axis.text.y = element_text(size=font.size)) +
  theme(axis.title.x = element_text(size=font.size), axis.title.y = element_text(size=font.size)) +
  theme(legend.position = "bottom", legend.text=element_text(size=font.size),legend.title = element_blank()) +
  theme(legend.key.width = unit(2, "cm")) 


Z_melt<-melt(test_iters[[1]]$Z,id=c("ID","Time","TRT"))  

ggplot(data=Z_melt, 
       aes(x=Time, y=value,colour=variable))+
  geom_line(alpha=0.4, aes(group=ID))+
  facet_grid(TRT~variable)+
  theme_bw() + 
  theme(axis.text.x = element_text(size=font.size), axis.text.y = element_text(size=font.size)) +
  theme(axis.title.x = element_text(size=font.size), axis.title.y = element_text(size=font.size)) +
  theme(legend.position = "bottom", legend.text=element_text(size=font.size),legend.title = element_blank()) +
  theme(legend.key.width = unit(2, "cm")) 














