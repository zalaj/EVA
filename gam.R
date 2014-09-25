##GAM 
#ocena za lambda

library(nlme)
library(mgcv)
library(QRM)

a <- 0.05
(u <- quantile(data$loss_corr, c(0,0.1,0.2,0.3,0.4,0.5)))   #vektor kvantilov 

#data_GPD so podatki v repu porazdelitve, ki so vecji od izbranega prgau u
data_GPD <- data[data$loss_corr>u[6],]

#presteti dogodke za vse kombincaije leto-BL
number_events <- sapply(split(data_GPD$loss_corr, 
                              factor(paste(data_GPD$BL, data_GPD$years), levels=level_BL_years)),
                        length)

nrows_lambda <- n_yrs * n_BL 

num_loss <- data.frame(years = sort(rep(yrs,n_BL)),
                  BL = rep(BL_short, n_yrs),
                  nb =number_events,
                  row.names = seq_len(nrows_lambda))

####
(lam_gam_1 <- gam(nb~1, data=num_loss, family=poisson))

(lam_gam_2 <- gam(nb~BL-1, data=num_loss, family=poisson))

(lam_gam_3 <- gam(nb~BL+years-1, data=num_loss, family=poisson))

AIC(lam_gam_1)
AIC(lam_gam_2)
AIC(lam_gam_3)

#####
(lam_fit_1 <- get.lambda.fit(lam_gam_1))
(lam_pred_1 <- lambda.predict(lam_gam_1), alpha=a)

(lam_fit_2 <- get.lambda.fit(lam_gam_2))
(lam_pred_2 <- lambda.predict(lam_gam_2), alpha=a)

(lam_fit_3 <- get.lambda.fit(lam_gam_3))
(lam_pred_3 <- lambda.predict(lam_gam_3), alpha=a)


#lambda s kubicnimi zlepki

(lam_gam_year <- gam(nb~years-1, data=num_loss, family=poisson))
(lam_fit_1 <- get.lambda.fit(lam_gam_year))
(lam_pred_1 <- lambda.predict(lam_gam_year, alpha=a))

## plot settings
par(mfrow=c(4,2))

xlim <- range(lam_pred_1$covar$year)

ylim <- c(0, 5) 
  
#predicted lambda
plot(lam_pred_1$covar$year, lam_pred_1$predict,type="l",
     xlim=xlim, ylim=ylim, xaxt='n')

#CI
lines(lam_pred_1$covar$year, lam_pred_1$CI.low, lty=2 )
lines(lam_pred_1$covar$year, lam_pred_1$CI.up, lty=2 )

#fitted lamdba
points(lam_pred_1$covar$year, lam_fit_1$fit)

text(min(xlim)+0.05*diff(xlim), min(ylim)+0.95*diff(ylim), labels="edof = ", font=2)
text(min(xlim)+0.15*diff(xlim), min(ylim)+0.95*diff(ylim), labels= 1, font=2)

aic <- c(AIC(lam_gam_year))

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
  
  text(min(xlim)+0.05*diff(xlim), min(ylim)+0.95*diff(ylim), labels="edof = ", font=2)
  text(min(xlim)+0.15*diff(xlim), min(ylim)+0.95*diff(ylim), labels=edof, font=2)
  
}
