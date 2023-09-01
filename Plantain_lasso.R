rm(list = ls())

library(foreign)
library(help=foreign)

install.packages("readstata13")
library(readstata13)

install.packages("glmnet")
library(glmnet)

dta <- read.dta13(file=".../plantain_analysis3old.dta")
summary(dta)

dta10_rbd <- subset(dta, year==2010)
dta9_rbd <- subset(dta, year==2009)
names(dta10_rbd)

dta10 <- subset(dta, year==2010 & rbd==0) #for the year 2010
dta9 <- subset(dta, year==2009 & rbd==0) #for the year 2009
names(dta10)

#dependent vars and their column numbers: 
#ltc-27 lf-26 sales-13 sales_permz-31 p_t-18 rbd-19 y_t-37 p_1-17 y_1-30
#ip-14 irrig-15 a_ip-16 plt_ar-4 ann-5 tcvol-32

foldid_outcome = sample(1:10, size=50, replace=TRUE)

y10dta <- data.matrix(dta10[,c(7:20)])
y9dta <- data.matrix(dta9[,c(7:20)])
x10 <- data.matrix(dta10[,-c(1:20)])
x9 <- data.matrix(dta9[,-c(1:20)])
best_lambda9 <- matrix(nrow=14,ncol=1)
best_lambda10 <- matrix(nrow=14,ncol=1)
c9 <- matrix(nrow=275,ncol=14)
c10 <- matrix(nrow=275,ncol=14)

for (i in 1:14){
  y10 <- y10dta[,i]
  y9 <- y9dta[,i]
  #1. Cross validation: 10 fold
  
  cvfit9 = cv.glmnet(x9, y9, foldid= foldid_outcome, type.measure = "mse", alpha=1)
  #plot(cvfit9)
  best_lambda9[i,] <- cvfit9$lambda.min  
  #best_lambda9
  c9[,i] = as.matrix(coef(cvfit9, s = "lambda.min"))
  
  cvfit10 = cv.glmnet(x10, y10, foldid= foldid_outcome, type.measure = "mse", alpha=1)
  #plot(cvfit10)
  best_lambda10[i,] <- cvfit10$lambda.min
  #best_lambda10
  c10[,i] = as.matrix(coef(cvfit10, s = "lambda.min"))
  
}

write.csv(c9,file="c9.csv")
write.csv(c10,file="c10.csv")
write.csv(best_lambda10,file="best_lambda10.csv")
write.csv(best_lambda9,file="best_lambda9.csv")

# Lasso RBD

xrbd9 <- data.matrix(dta9_rbd[,-c(1:20)])
xrbd10 <- data.matrix(dta10_rbd[,-c(1:20)])
yrbd9 <- dta9_rbd[,6]
yrbd10 <- dta10_rbd[,6]


#foldid_rbd = sample(1:10, size=length(yrbd9),replace=TRUE)
cvfit_rbd9 = cv.glmnet(xrbd9, yrbd9, family = "binomial", foldid=foldid_rbd, type.measure = "class", nfolds = 10)
plot(cvfit_rbd9)
best_lambda_rbd9 <- cvfit_rbd9$lambda.min
best_lambda_rbd9
c_rbd9 = as.matrix(coef(cvfit_rbd9, s = "lambda.min"))
write.csv(c_rbd9,file="c_rbd9.csv")

cvfit_rbd10 = cv.glmnet(xrbd10, yrbd10, family = "binomial", foldid=foldid_rbd, type.measure = "class", nfolds = 10)
plot(cvfit_rbd10)
best_lambda_rbd10 <- cvfit_rbd10$lambda.min
best_lambda_rbd10
c_rbd10 = as.matrix(coef(cvfit_rbd10, s = "lambda.min"))
write.csv(c_rbd10,file="c_rbd10.csv")

