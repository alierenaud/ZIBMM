## DATA GENERATION
# reproducibility
#library(bayesplot)
#set.seed(1839)

rm(list=ls())
require(tidyverse)

#-------------------------------
# Import Data
#-------------------------------
dat=read.table("student-mat.csv",sep=";",header=TRUE) %>%
  rownames_to_column(var="id") %>%
  mutate(G1=G1/20,G2=G2/20,G3=G3/20) %>%
  pivot_longer(col=c(G1,G2,G3),names_to="Period",values_to="Grade") %>%
  mutate(sexn=ifelse(sex=="F",1,0),
         famsupn=ifelse(famsup=="yes",1,0),
         schoolsupn=ifelse(schoolsup=="yes",1,0),
         highern=ifelse(higher=="yes",1,0),
         G1=ifelse(Period=="G1",1,0),
         G2=ifelse(Period=="G2",1,0),
         G3=ifelse(Period=="G3",1,0),
         Grade=ifelse(Grade==1,0.9995,Grade), #There is only one 100% in the dataset
         nonzeros=ifelse(Grade==0,0,1),
         intercept =1,
         age_f = age*sexn
  ) %>% filter(G3==1)

View(dat)

covar.list <- c("sexn","age", "age_f", "failures","schoolsupn","highern")
d <- length(covar.list)  #number of covariates
N <- nrow(dat) #total number observations


### to compute the random effect design matrix
z <- lm(Grade ~ school-1, data=dat, x=TRUE)$x
k <- ncol(z)


library(rstan)

stan_d <- list(y = dat$Grade, N = N,
               x_woint=dat[covar.list],d = d, z=z, k=k)

m_init <- stan_model('ZIB-student.stan')
ptm = proc.time()
m_fit <- sampling(m_init, data = stan_d, 
                  pars = c('coef_a', 'coef_m', 'coef_p',
                           "coef_b1","m0",'eta1',
                           "coef_b2","a0",'eta2',
                           "coef_b3","p0",'eta3'),
                  iter = 2000, warmup = 1000, thin = 1,
                  cores = 4)
proc.time()-ptm


draws <- as.matrix(m_fit)
draws <- draws[, !(colnames(draws) == 'lp__')]

