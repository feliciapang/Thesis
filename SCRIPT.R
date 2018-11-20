##########################################################################################
##                              Bachelor thesis in Economics                            ##
##########################################################################################
## Felicia Pang och Paula Nsubaga

## Set working directory (paste adress from folder where data is)
setwd("C:/Users/Feliz navidad/Downloads")

## Load excelfile by clicking on import dataset --> From Excel --> select excel data
## --> mark all except year as "numeric" --> Import

## Use the library sandwich by using the following command
library(lmtest)
library(sandwich)

## Create regression of Y=antalpersonbilar X=befolkning X2=arbetslöshet X3=trängselskatt
## X4=bnp X5=prisnivå X6=bensinpris and name the regression reg
regression <-lm(köpesskilling~förvärvsinkomst+befolkning+arbetslöshet+skattesats+bolåneränta,data=data_1_)

## Use following command to compute robust standard errors
coeftest(lm(regression), vcov=(vcovHC(lm(regression), "HC0")))

# Install package plm
install.packages("plm")
library(plm)
attach(Kopia_av_data_1)

Y <- cbind(köpesskilling)
X <- cbind(förvärvsinkomst, befolkning, bolåneränta, skattesats, arbetslöshet)

# Set data as panel data
pdata <- plm.data(Kopia_av_data_1, index=c("id","t"))

# Pooled OLS estimator
pooling <- plm(Y~X, data=Kopia_av_data_1, model = "pooling")
summary(pooling)

# Between estimator
between <- plm(Y~X, data=Kopia_av_data_1, model="between")
summary(between)

# First difference estimator
firstdiff <- plm(Y~X, data=Kopia_av_data_1, model="fd")
summary(firstdiff)

# Fixed effexts or within estimator
fixed <- plm(Y~X, data= Kopia_av_data_1, model="within")
summary(fixed)

# Random effects estimator
random <- plm(Y~X, data=Kopia_av_data_1, model="random")
summary(random)

# LM test for fixed effects vs OLS
pFtest(fixed, pooling)
