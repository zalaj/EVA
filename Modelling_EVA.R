#rm(list=ls())- izbrise vse podatke
##Podatki skodnih dogodkih 
#data_full vsebuje 1516 podatkov o izgubah :  dim(data_full)
#podatki so od leta 1970 do leta 2014 

library(nlme)
library(mgcv)
library(ismev)
library(evir)
library(base)
library(QRM)

library(foreign)
library(survival)
library(epicalc)

require(mgcv)

#######################
#1. PRIPRAVA PODATKOV
#######################

####
# 1.1. Podatki
###

setwd('/Users/zala/GitHub/EVA/')

data_full <- read.csv('loss_data_full_view_popr.csv', sep = ';', header = TRUE,fill = TRUE )

summary(data_full[,2:10])

data_1984 <-data_full[data_full$Event.Date>1984,]    #podatki o izgubah od leta 1984 dalje

#dim(data_1984) :  1449   16 

#nova matrika, samo podatki, ki potrebni za analizo za izgube od leta 1984 naprej
ref <- data_1984$Ref
org <- data_1984$Organisation
years <- data_1984$Event.Date
gross_loss <- data_1984$Gross.Loss.GBP
ET <- data_1984$Basel.Loss.Event
BL <- as.character(data_1984$Business.Line)

#Op.: Za vse BL, ki imajo vrednost n/a, se nastavi vrednost Unallocated Business Line
BL[BL=='n/a']<- 'Unallocated Business Line'

#Op.: Za BL Insurance(life) in Insurance (non-life) se nastavi skupni BL Insurance
BL[BL=='Insurance(life)'] <- 'Insurance'
BL[BL=='Insurance (non-life)'] <- 'Insurance'


data <- data.frame(ref, org, years, gross_loss, ET=ET, BL=BL)   

#odstranjeni podatki, kjer loss=NA
data <-na.omit(data) 

#odstranjeni se podatki, kjer loss==0, saj za analizo nimajo pomena
data <- data[!data$gross_loss==0,]

#dim(data): 968   4

###
# 1.2. INDEKS INFLACIJE - popravljene izgube za indeks inflacije
###

index <- read.csv('CPI_index_yearly.csv', sep = ',', header = TRUE) #inflation indexes 1700-2013

index_2014_monthly <- read.csv('CPI_index_monthly.csv', 
                               sep = ',', header = TRUE)[1014:1020,] #mesecni indeksi inflacije jan-jul 2014

index_2014 <- mean(as.numeric(as.vector(index_2014_monthly$CDKO))) #povprecje mesecnih indeksonv 2014

index <- rbind(index[index$X>1984,], c(2014,index_2014 ))         #letni ideksi 1984-2014

data <- merge(data,
              index, 
              by.x= "years",
              by.y= "X")        #leto, loss, inflation ideks za leto izgube

###
# 1.3. CORRECTED_LOSS: popravljene izgube za indeks inflacije
###

corrected_loss <- data$gross_loss*index_2014/data$CDKO     #popravljenje izgube za inflacijo
loss_mio <- corrected_loss/10^6

data <- cbind(data, loss = corrected_loss, loss_mio = loss_mio)   #dodan stolpec popravljenje izgube

(data[order(data$loss, decreasing = TRUE),][1:10,])
summary(data)

#dim(data)

###
# 1.4. EVENT TYPES EXPLAINED : http://www.bis.org/bcbs/qis/oprdata.pdf
###

ET_sub <- sort(unique(data$ET))  #vsi ET, ki so v podatkih, urejeni od 1-7

ET_main <- c(rep(1,3),rep(2,2), rep(3,2), rep(4,5), 5, 6, rep(7,7)) 
#vektor 1-7, ki podkategorijam ET doloci glavne kategorije ET

ET_kratice <- c('IF','EF', 'EPWS', 'CPBP', 'DPA', 'BDSF', 'EDPM')   #vektor s kraticami ET

ET_short <- data.frame(ET.index = 1:7, ET_kratice) #povezovalna matrika med st. ET in kratico ET

#koncna povezovalna matrika za vsak podtip ET kratica za glavni ET
ET_all <- merge(data.frame(ET_sub=ET_sub, ET.index = ET_main), ET_short,
            by.x = 'ET.index', 
            by.y = 'ET.index')


###
# 1.5. BL
###


#poslovna podrocja - Slovenski termini
PP <- list()
PP_vekt <- c('Agentske storitve','Uporavljanje s sredstvi', 'Komercialno bancnistvo', 
             'Podjetnisko financiranje', 'Zavarovanje', 'Placilni Instrumenti',
             'Poslovanje s prebivalstvom',
             'Posredovanje pri kupoprodaji vrednostnih papirjev prebivalstva',
             'Posli trgovanja','Nedoloceno Poslovno Podrocje')

PP_kratice<- c('AS', 'US', 'KB', 'PF', 'Z', 'PI', 'PPr', 'PVP', 'PT','NPP')

#BL
BL_unique <- sort(unique(data$BL))

#referencna matrika
PP_ref <- data.frame(BL = BL_unique, PP_vekt, PP=PP_kratice)


###
# 1.6. koncni podatki v matriki data
###

data <- merge (data,
               data.frame(ref, basel_event = data_1984$Basel.Loss.Event, BL = BL ))

data <- merge (data,
               ET_all[,c(2,3)],
               by.x = 'basel_event',
               by.y = 'ET_sub')

#dodana PP
data <- merge(data, 
              PP_ref,
              by.x='BL',
              by.y='BL')

data[,c(1,13)]

###############################
#2. BASEL MATIRKA IN VEKTOR
###############################

###
# 2.1. Stevilo dogodkov za Basel matriko
###

ET_unique <- unique(sort(data$ET_kratice))

PP_ET <- expand.grid(PP=PP_kratice, ET=ET_unique) # vse kombinacije PP-ET

lev <- apply(PP_ET, 1, paste, collapse=" ")     # vsi leveli - stringi vseh kombinacij PP-ET

number_PP_ET <- sapply(split(data$loss, factor(paste(data$PP, data$ET), levels=lev)), length)

###
# 2.2. Basel matrika
###

yrs <- 1984:2014

n_PP <- length(PP_kratice)
n_ET <- length(ET_unique)
n_yrs <- length(yrs)


Basel_matrika <- matrix(number_PP_ET,ncol = n_ET , nrow=n_PP )
colnames(Basel_matrika) <- as.character(ET_unique)
rownames(Basel_matrika) <- as.character(PP_kratice)

Basel_matrika

###
#2.3. Besl vektor
###

(Basel_vector <- rowSums(Basel_matrika))


####################################
#3. GRAFI ZA VSE SKODNE DOGODKE
####################################

#stevilo dogodkov po letih
number_events_all_year<- sapply(split(data$loss, factor(paste(data$years), levels=yrs)), length)

#vsota vseh izgub v mio GPD na leto
gross_losses_year <- sapply(split(data$loss_mio, factor(paste(data$years), levels=yrs)), sum)

x_yrs <- range(yrs)       #xlim za leta

###
#3.1. graf skodnih dogodkov po letih
###

par(mfrow=c(1,1))
par(mar=c(5, 5, 4, 5) + 0.1, oma = rep(0,4))

plot(yrs, number_events_all_year, 
     ylim = c(0, max(number_events_all_year)), xlim = x_yrs, 
     xlab = '', ylab='',
     type = "l", lty = 1, col=4,
     main = '')

mtext(2, text='Stevilo skodnih dogodkov z znano bruto izgubo', line=3)

###
#3.2 Graf Bruto izgube
###

par(new=TRUE)

plot(yrs, gross_losses_year,
     ylim = c(0, max(gross_losses_year)), xlim = x_yrs, axes = F, 
     xlab = '', ylab='',
     type = "l", lty = 1, ,col = 3,
     main = '')

axis(4, ylim = c(0, max(gross_losses_year) ), lwd = 1, col=1) 

mtext(4, text='Skupa znana bruto izguba v mio GBP',line = 3)

legend(x = 'topleft',legend = c('Stevilo skodnih dogodkov','Bruto izgube v mio GBP'), 
       lty = 1, col = c(4,3))

###
#3.3. Graf stevilo izgub skozi leta po PP
###

PP_years <- expand.grid(PP=PP_kratice, yrs) # vse kombinacije BL-years

level_PP_years <- apply(PP_years, 1, paste, collapse=" ")     # vsi leveli - stringi vseh kombinacij PP-years

number_PP_years <- sapply(split(data$loss, factor(paste(data$PP, data$years), 
                                                       levels=level_PP_years)), length) #st. dogodkov za vsako kombinacijo PP-years

#Matrika st. dogodkov za vsako po PP za vsako leto
PP_years_M <- matrix(number_PP_years,ncol = n_yrs, nrow=n_PP )
colnames(PP_years_M) <- as.character(yrs)
rownames(PP_years_M) <- as.character(PP_kratice)

#priprava za risanje
y_PP <-c(0, max(PP_years_M))  #ylim do max stevila skodnih dogodkov 
col <- rainbow(10)

par(mfrow=c(1,1))
par(mar=c(5, 5, 4, 5) + 0.1, oma = rep(0,4))


for (i in 1:n_PP){
  
  if (i==1) par(new=F, mar=c(5, 5, 4, 5) + 0.1) else  par(new=T) 
  
  plot(yrs, PP_years_M[i,], 
       ylim = y_PP, xlim = x_yrs, 
       axes = if(i==1) T else F,
       xlab = '', ylab='',
       type = "l", lty = 1, main = '', col = col[i])  
}

legend(x = "topleft",legend = PP_kratice, col= col, lty=1, ncol=2)

mtext(1, text='Leto', line=3)
mtext(2, text='Stevilo skodnih dogodkov', line=3)


###
#3.4 GRAF log skodnih dogodkov po PP skozi leta
###

#matrika leto, izguba v log od  mio GBP
PP_over_years <- data.frame(loss=log((data$loss_mio), base = 10), years=data$years)
       

data_loceni_PP <- split(PP_over_years, factor(as.character(sort(data$PP)))) #list izgub za vsak PP 

##Priprava za risanje
y_PP <-range(PP_over_years$loss) #meja za y do najvecje izguve

layout.n_PP <- matrix(1:n_PP, ncol=2, byrow=TRUE) # razporeditv polj za risanje grafa

layout.n_PP <- rbind(layout.n_PP, c(n_PP+1, n_PP+2)) # dodano polje za napise na x osi

layout.n_PP <- cbind(c(n_PP+3,0), layout.n_PP) # dodano polje za napise na x osi

layout(layout.n_PP, widths=c(0.5,1,1), heights=rep.int(1,10)) # layout

opar <- par(mar=rep.int(0,4), oma=rep.int(3,4))

for (i in 1: n_PP){
  pp <- as.character(sort(PP_kratice)[i])
  
  x <- as.data.frame(data_loceni_PP[i])[,2]       #leta
  log_loss <- as.data.frame(data_loceni_PP[i])[,1]    #izbube
  
  plot(x,log_loss ,
       xlim = x_yrs, ylim = y_PP,
       yaxt=if(i%%2==1) "s" else "n",
       xaxt= "n")
  abline(h = log(u/10^6, base=10), lty=2)
  
  #years na x osi
  if(i==9 | i==10) axis(1, at =seq(1980,2015,5), labels = rep('',8))
  if(i==9 | i==10) axis(1, at =seq(1980,2010,10), labels = as.character(seq(1980,2010,10)), lwd = 1, col=1)
  
  text(min(x_yrs)+0.05*diff(x_yrs), min(y_PP)+0.95*diff(y_PP),
       labels=pp, font=2)

}


##Napisi na X osi
plot.new()

text(0.1, 0.1, labels="Leto")

plot.new()

legend(0.0, 0.35, lty=c(NA,2), pch=c(1, NA), bty="n", horiz=TRUE,
       legend=c('Skodni dogodki', 'Prag' ))

## y axis label
plot.new()

text(0.5, 0.5, srt=90,labels="TUKAJ PRIDE NAPIS NA Y OSI")

###!!!!!!!! Popravi se napise na X osi in Y osi - X os je 10^4, Y os se napisi ne smejo prikirvati




###########################
#4. OCENA PARAMETROV
###########################

#tu upostevamo samo podatke, ki so visnji od pragu u
  #vektor kvantilov 

u <-as.numeric(u_kvant[5]) #treshold u

###data_GPD so podatki, ki so vecji od izbranega prgau u
data_GPD <- data[data$loss > u,]

####
#4.1. ocena parametra lambda
###

#za oceno parametra lambda naredimo matriko stevilo dogodkov za vsak PP/years

number_events <- sapply(split(data_GPD$loss, factor(paste(data_GPD$PP, data_GPD$years), 
                                                       levels=level_PP_years)), length)  #st. dogodkov

nrows_lambda <-n_yrs * n_PP 

#matrika num 
num <- data.frame(years = sort(rep(yrs,n_PP)),
               PP = rep(PP_kratice, n_yrs),
               nb =number_events,
               row.names = seq_len(nrows_lambda))

#######
# a) gam za lambda za vec razlicnih modelov
##Op.: v primeru, ko dodas -1 ni INTERCEPT!

(lam_gam1 <- gam(nb~1, data=num, family=poisson)) #model1: konstanta

(lam_gam2 <- gam(nb~PP-1, data=num, family=poisson)) #model2: faktorska f. za PP

(lam_gam3 <- gam(nb~PP+years-1, data=num, family=poisson)) #model3: linearni model faktorska f. PP + leta

#AIC za prve 3 modele
lam_gam1$aic
lam_gam2$aic
lam_gam3$aic

#Rocno izracunan AIC za lam_gam3
c(-2*logLik(lam_gam3) + 2*sum(lam_gam3$edf), AIC(lam_gam3))

#LR_Test
lr_21 <- lr_test(logLik(lam_gam1), logLik(lam_gam2), 9,0.05)
lr_32 <- lr_test(logLik(lam_gam2), logLik(lam_gam3), 1,0.05)

####
#b) izbor najboljsega modela glede na AIC

aic <- c(lam_gam3$aic)

#model je faktorska f. PP + f. za Years z razliznimi EDOF

for (i in 2:8){
  lam_gam <- gam(nb ~ PP + s(years, k=i+1, fx=T, bs="cr")-1,
               data=num, family=poisson)
  
  aic[i]<- AIC(lam_gam)
}


#GRAF AIC
par(mfrow=c(1,1))
plot(1:8, aic, type = 'b')

#najboljsi edf je najmanjsi edf, pri katerem se AIC ne zmanjsa, ce dodamo eno dodatno edof
edf <- 3

###
#c) ocena parametra lambda
a <- 0.05

lam_gam <-gam(nb ~ PP + s(years, k= edf + 1,  bs="cr"), 
                           data=num, family=poisson)

#-2*logLik(lam_gam)+2*sum(lam_gam$edf)

####
#d) lambda: Kako delujeta funkciji get.lambda.fit() in lambda.predict je opisano v gam.R

lamFit <- get.lambda.fit(lam_gam) #Fitted lamdba

lamPred <- lambda.predict(lam_gam , alpha=a) #Predicted lambda


####
#e) graf za lambda in intervali zaupanja

y_lam <- c(min(lamPred$CI.low), max(lamPred$CI.up))

# layout
layout(layout.n_PP, widths=c(0.5,1,1), heights=rep.int(1,10)) 

par(mar=rep.int(0,4), oma=rep.int(3,4))

#graf
for (i in 1: n_PP){
  
  #
  pp.<- sort(PP_kratice)[i]
  pp <- lamPred$covar$PP==pp.
  
  lambda <-  lamPred$predict[pp]
  
  #predicted lambda
  plot(yrs, lambda, type='l', xlim = x_yrs, ylim = y_lam,
       yaxt= "n",
       xaxt= "n")
  
  #CI
  CI_up <- lamPred$CI.up[pp]
  lines(yrs, CI_up, lty=2, xlim = x_yrs, ylim = y_lam)
  
  CI_low <- lamPred$CI.low[pp]
  lines(yrs, CI_low, lty=2, xlim = x_yrs, ylim = y_lam)
  
  #fitted lambda
  y <- lamFit$covar$years[pp]
  fit <- lamFit$fit[pp]
  points(yrs, lamFit$fit[pp], pch=20)
  
  
  #skala na x osi
  if(i==9 | i==10) axis(1, at =seq(1980,2015,5), labels = rep('',8))
  if(i==9 | i==10) axis(1, at =seq(1980,2010,10), labels = as.character(seq(1980,2010,10)), lwd = 1, col=1)
  
  #skala na y osi
  if(i%%2==1) axis(2, at =seq(0,14,2), labels = rep('',8) )
  if(i%%2==1) axis(2, at =seq(0,12,4), labels = as.character(seq(0,12,4)), las = 1)
     
  #text
  text(min(x_yrs)+0.05*diff(x_yrs), min(y_lam)+0.90*diff(y_lam),
       labels=pp., font=2)
  
} 

##Napisi na X osi
plot.new()

text(0.1, 0.1, labels="Leto")

plot.new()

legend(0.1, 0.35, lty=c(1,2), pch=c(20,NA), bty="n", horiz=TRUE,
       legend=c(expression(hat(lambda)),
                substitute(a.~"CI", list(a.=1-a))),
       text.width=strwidth("oooooooo"))

## y axis label
plot.new()

text(0.1, 0.5, srt=90,
          labels=substitute(hat(lambda)~~"z dvostranskim asimptoticnim "*a.*"% intervalom zaupanja",
                            list(a.=1-a)))



################################################################################################
################################################################################################
#5. ocena parametrov xi, beta
#####

#u je izbran glede na prileganje QQplota residualov v loss_severity.R
u <- as.numeric(u_kvant[5])

#modela za xi in nu sta izbrana z LR testom v datoteki loss_severity za vsak u

l_severity <- loss_severity[[5]]

model_xi <- formula(l_severity$xi)
model_nu <- formula(l_severity$nu)


a <- 0.05
B <- 30
eps <- 10^(-3)
niter <- 20


#FITTED VALUES
#gamGPDfit ti v vsakem koraku izpise povprecno relativno razliko in ko je manjsa od eps, konca
fit_xibeta_all <- gamGPDfit(x=data_GPD, threshold=u, datvar="loss",
                 xiFrhs = model_xi, # interaction
                 nuFrhs = model_nu, # interaction
                 eps.xi=eps, eps.nu=eps, niter=niter,
                 include.updates=T)


#BOOTSTRAPPED CI
boot_xibeta <- gamGPDboot(x = data_GPD, B=B, threshold=u, datvar="loss",
                      xiFrhs = model_xi, # xi
                      nuFrhs = model_nu, # nu
                      niter=niter, eps.xi=eps, eps.nu=eps,
                      include.updates=T)


#fitted values urejene po atributih
fit_xibeta <- get.GPD.fit(boot_xibeta, alpha = a)

#predicted values
pred_xibeta <- GPD.predict(boot_xibeta)

#atributi za xi
xi_atr <- fit_xibeta$xi$covar

###############
#graf za xi
## layout

layout.mat <- matrix(1:1, ncol=1, byrow=TRUE) # plot matrix layout

layout.mat <- rbind(layout.mat, 2) # add plot regions for x axis label

layout.mat <- cbind(c(3, 0), layout.mat) # add plot regions for y axis label

layout(layout.mat, widths=c(0.2, 1, 1), heights=c(1, 0.2)) # layout

par(mar=rep.int(0,4), oma=rep.int(3,4))

#fitted xi
xifit <- fit_xibeta$xi$fit

#CI za xi
xi_ci_low <- fit_xibeta$xi$CI.low
xi_ci_up <- fit_xibeta$xi$CI.up



y_xi <- c(min(xi_ci_low), max(xi_ci_up))

plot (1:10 ,xifit,  type='p', pch=19,
      ylim = y_xi, ylab='',
      xaxt= "n", xlab= 'Poslovna podrocja')
axis(1, at = 1:10, labels =xi_atr$PP)

#CI za 
r <- 0.3 #dolzina crtice pri spodnji iz zgornji meji

for(i in 1:10) {
  lines(c(i, i), c(xi_ci_low[i], xi_ci_up[i]), lty=2)       # veritkalna crta
  lines(c(i-r, i+r), c(xi_ci_low[i], xi_ci_low[i]))  # spodnja meja
  lines(c(i-r, i+r), c(xi_ci_up[i], xi_ci_up[i])) #zgornja meja
}

plot.new()
text(0.5, 0.1,
     labels='Poslovna podrocja')

plot.new()
text(0.1, 0.5, srt=90,
     labels=substitute(hat(xi)~~"z dvostranskim bootstrap "*a.*"% int. zaupanja",
                       list(a.=1-a)))




##########################################################
# GRAF ZA BETA

##Priprava za risanje

layout.n_PP <- matrix(1:n_PP, ncol=2, byrow=TRUE) # razporeditv polj za risanje grafa
layout.n_PP <- rbind(layout.n_PP, c(n_PP+1, n_PP+2)) # dodano polje za napise na x osi
layout.n_PP <- cbind(c(n_PP+3,0), layout.n_PP) # dodano polje za napise na x osi
layout(layout.n_PP, widths=c(0.5,1,1), heights=rep.int(1,10)) # layout

opar <- par(mar=rep.int(0,4), oma=rep.int(3,4))

x_beta <- range(yrs)
y_beta <- c(min(log(fit_xibeta$beta$CI.low, base = 10)), max(log(fit_xibeta$beta$CI.up, base = 10)))

#atributi za beta
beta_atr <- fit_xibeta$beta$covar 


for (i in 1: n_PP){
  pp <- PP_kratice[i]
  
  #vektor, ki izloci samo vrednosti za to PP
  b_pred <- pred_xibeta$beta$covar$PP == pp
  b_fit <- fit_xibeta$beta$covar$PP == pp

  y_pred <- pred_xibeta$beta$covar$years[b_pred]
  beta_pred <- log(pred_xibeta$beta$predict[b_pred], base= 10)
  
  y_fit <- fit_xibeta$beta$covar$years[b_fit]
  beta_fit <- log(fit_xibeta$beta$fit[b_fit], base = 10)
  
  beta_ci_low <- log(fit_xibeta$beta$CI.low[b_fit], base = 10)
  beta_ci_up <- log(fit_xibeta$beta$CI.up[b_fit], base = 10)
  
  
  #predvidene vrednosti
  plot(y_pred, beta_pred, type = 'l', pch= 20, xlim = x_beta, ylim = y_beta,
       yaxt=if(i%%2==1) "s" else "n", xaxt= "n")
  
  #fitted vrednosti
  points(y_fit, beta_fit, pch=20)
  
  #CI
  r <- 0.3 #dolzina crtice pri spodnji iz zgornji meji
  
  for(j in 1:length(y_fit)) {
    lines(c(y_fit[j], y_fit[j]), c(beta_ci_low[j], beta_ci_up[j]), lty=2)       # veritkalna crta
    lines(c(y_fit[j]-r, y_fit[j]+r), c(beta_ci_low[j], beta_ci_low[j]))  # spodnja meja
    lines(c(y_fit[j]-r, y_fit[j]+r), c(beta_ci_up[j], beta_ci_up[j])) #zgornja meja
  } 
   
  #years na x osi
  if(i==9 | i==10) axis(1, at =seq(1980,2015,5), labels = rep('',8))
  if(i==9 | i==10) axis(1, at =seq(1980,2010,10), labels = as.character(seq(1980,2010,10)), lwd = 1, col=1)
  
  #PP
  text(min(x_yrs)+0.05*diff(x_yrs), min(y_beta)+0.95*diff(y_beta),
       labels=pp, font=2)
  
}



#zacetne vrednosti
mle_par <- fit.GPD(data=data_GPD$loss, threshold=u)$par.ests


#predicted values

sort(xibetaPred$xi$predict)

#unique(fit$xi) %in% xi_fit$xi$fit




fit$xi.covar
fit$xiUpdates16
fit$MRD

AIC(fit)

get.GPD.fit(bootGPD)


fit$beta
fit$nu.covar
bootGPD$nu.updates





## compute predicted values
xibetaPred <- GPD.predict(bootGPD)
x



fit
-2*-11517.27 +2*-11502.59
