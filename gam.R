##GAM 
#ocena za lambda
#uporabljeni so podatki num in datoteke Modelling_EVA.R

library(nlme)
library(mgcv)
library(QRM)


#####
# KAKO DELUJETA lambda.fit in lambda.predict
#Primer, ko samo konstanta
(lam_gam_d <- gam(nb~1, data=num_loss, family=poisson))

#1. lambda.fit: get.lambda.fit da fitted values, ki so izracunane z modelom gam. Fitted values so 
#dobljene z minimizacijo penalized loglikelihood funkcije

(lam_fit_d <- get.lambda.fit(lam_gam_d))
unique(lam_gam_d$fitted.values)
#unique(lam_gam_d$fitted.values) %in% lam_fit_d$fit 

#2. lamdba.predict
#uporabi p<- predict(gam, se.fit=T) in za predicted values vzame f <- exp(p$fit), 
#CI pa exp(f +/- p$se.fit * q), kjer q 1-a/2 qvantil standardne normalne porazdelitve

(lam_pred_d <- lambda.predict(lam_gam_d, alpha=a))

unique(exp(predict(lam_gam_1, se.fit=T)$fit))

#primer: unique(exp(predict(lam_gam_1, se.fit=T)$fit))==lam_pred_1$predict


#########
#lambda s kubicnimi zlepki

(lam_gam_1 <- gam(nb~years-1, data=num_loss, family=poisson))
(lam_fit_1 <- get.lambda.fit(lam_gam_1))
(lam_pred_1 <- lambda.predict(lam_gam_1, alpha=a))

## plot settings
par(mfrow=c(4,2))
par(oma=c(1,1,0,0), mar=c(1,1,1,1))

xlim <- range(lam_pred_1$covar$year)

ylim <- c(0, max(num_loss$nb)) 
  
#predicted lambda
plot(lam_pred_1$covar$year, lam_pred_1$predict,type="l",
     xlim=xlim, ylim=ylim, xaxt='n')

#CI
lines(lam_pred_1$covar$year, lam_pred_1$CI.low, lty=2 )
lines(lam_pred_1$covar$year, lam_pred_1$CI.up, lty=2 )

#fitted lamdba
points(lam_fit_1$covar, lam_fit_1$fit)

#data
points(num_loss$years, num_loss$nb, col=4, pch=20)

text(min(xlim)+0.05*diff(xlim), min(ylim)+0.95*diff(ylim), labels="edof = ", font=2)
text(min(xlim)+0.15*diff(xlim), min(ylim)+0.95*diff(ylim), labels= 1, font=2)


for (edof in 2:8){
  
  model <- gam(nb~s(years, fx=TRUE, k=edof+1, bs="cr")-1, 
               data=num_loss, family=poisson)# => fine (interaction)
  
  lam_fit <- get.lambda.fit(model)
  
  lam_pred <- lambda.predict(model, alpha = a)
  
  aic[edof] <- AIC(model)
      
  #predicted lambda
  plot(lam_pred$covar$year, lam_pred$predict,type="l",
       xlim=xlim, ylim=ylim,
       yaxt=if(edof%%2==1) "s" else "n",
       xaxt=if(edof==7 | edof==8) "s" else "n")
    
  #CI
  lines(lam_pred$covar$year, lam_pred$CI.low, lty=2 )
  lines(lam_pred$covar$year, lam_pred$CI.up, lty=2 )
    
  #fitted lamdba
  points(lam_pred$covar$year, lam_fit$fit)
  
  #data
  points(num_loss$years, num_loss$nb, col=4, pch=20)
  
  text(min(xlim)+0.05*diff(xlim), min(ylim)+0.95*diff(ylim), labels="edof = ", font=2)
  text(min(xlim)+0.15*diff(xlim), min(ylim)+0.95*diff(ylim), labels=edof, font=2)
  
}
