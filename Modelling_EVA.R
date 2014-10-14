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

ET_short <- data.frame(ET.index = 1:7, ET) #povezovalna matrika med st. ET in kratico ET

#koncna povezovalna matrika za vsak podtip ET kratica za glavni ET
ET_all <- merge(data.frame(ET_sub=ET_sub, ET.index = ET_main), ET_short,
            by.x = 'ET.index', 
            by.y = 'ET.index')


###
# 1.5. BL
###

unique(data$BL)



#poslovna podrocja - Slovenski termini
PP <- list()
PP_vekt <- c('Agentske storitve','Uporavljanje s sredstvi', 'Komercialno bancnistvo', 
             'Podjetnisko financiranje', 'Zavarovanje', 'Placilni Instrumenti',
             'Poslovanje s prebivalstvom',
             'Posredovanje pri kupoprodaji vrednostnih papirjev prebivalstva',
             'Posli trgovanja','Nedoloceno Poslovno Podrocje')

PP_kratice<- c('AS', 'US', 'KB', 'PF', 'Z', 'PI', 'PPr', 'PVP', 'PT','NPP')

#BL
BL_unique <- unique(BL)

#referencna matrika
PP_ref <- data.frame(BL = BL_unique, PP_vekt, PP=PP_kratice)


###
# 1.6. koncni podatki v matriki data
###

loss <- merge (loss_corrected,
               data.frame(ref, basel_event = data_1984$Basel.Loss.Event, BL = BL ))

data <- merge (loss,
               ET_all,
               by.x = 'basel_event',
               by.y = 'ET_sub')

#dodana PP
data <- merge(data, 
              PP_ref,
              by.x='BL',
              by.y='BL')

summary(data)

###############################
#2. BASEL MATIRKA IN VEKTOR
###############################

###
# 2.1. Stevilo dogodkov za Basel matriko
###

ET_unique <- unique(sort(data$ET))

PP_ET <- expand.grid(PP=PP_kratice, ET=ET_unique) # vse kombinacije PP-ET

lev <- apply(PP_ET, 1, paste, collapse=" ")     # vsi leveli - stringi vseh kombinacij PP-ET

number_PP_ET <- sapply(split(data$loss, factor(paste(data$PP, data$ET), levels=lev)), length)

###
# 2.2. Basel matrika
###

yrs <- 1984:2014

n_PP <- length(PP_krat)
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

legend(x = "topleft",legend = PP_short, col= col, lty=1, ncol=2)

mtext(1, text='Leto', line=3)
mtext(2, text='Stevilo skodnih dogodkov', line=3)


###
#3.4 GRAF skodni dogodnik po PP skozi leta
###

#matrika leto, izguba v log od  mio GBP
PP_over_years <- data.frame(loss=log((data$loss_mio), base = 10), years=data$years)
       

data_loceni_PP <- split(PP_over_years, factor(as.character(data$PP))) #list izgub za vsak PP 

##Priprava za risanje
y_PP <-range(PP_over_years$loss) #meja za y do najvecje izguve

layout.n_PP <- matrix(1:n_PP, ncol=2, byrow=TRUE) # razporeditv polj za risanje grafa

layout.n_PP <- rbind(layout.n_PP, c(n_PP+1, n_PP+2)) # dodano polje za napise na x osi

layout.n_PP <- cbind(c(n_PP+3,0), layout.n_PP) # dodano polje za napise na x osi

layout(layout.n_PP, widths=c(0.5,1,1), heights=rep.int(1,10)) # layout

opar <- par(mar=rep.int(0,4), oma=rep.int(3,4))

for (i in 1: n_PP){
  x <- as.data.frame(data_loceni_PP[i])[,2]       #leta
  log_loss <- as.data.frame(data_loceni_PP[i])[,1]    #izbube
  
  plot(x,log_loss ,
       xlim = x_yrs, ylim = y_PP,
       yaxt=if(i%%2==1) "s" else "n",
       xaxt= "n")
  
  #years na x osi
  if(i==9 | i==10) axis(1, at =seq(1980,2015,5), labels = rep('',8))
  if(i==9 | i==10) axis(1, at =seq(1980,2010,10), labels = as.character(seq(1980,2010,10)), lwd = 1, col=1)
  
  text(min(x_yrs)+0.05*diff(x_yrs), min(y_PP)+0.95*diff(y_PP),
       labels=PP_short[i], font=2)

}


##Napisi na X osi
plot.new()

text(0.1, 0.1, labels="Year")

plot.new()

text(0.3,0.1, labels = "Skodni dogodki")
points(0,0.1)

## y axis label
plot.new()

text(0.5, 0.5, srt=90,labels="TUKAJ PRIDE NAPIS NA Y OSI")

###!!!!!!!! Popravi se napise na X osi in Y osi - X os je 10^4, Y os se napisi ne smejo prikirvati






###########################
#4. OCENA PARAMETROV
###########################

#tu upostevamo samo podatke, ki so visnji od pragu u

u_kvant <- quantile(data$loss, c(0,0.1,0.2,0.3,0.4,0.5))   #vektor kvantilov 

u <-as.numeric(u_kvant[5]) #treshold u

###data_GPD so podatki, ki so vecji od izbranega prgau u
data_GPD <- data[data$loss > u,]

####
#4.1. ocena parametra lambda
###

#za oceno parametra lambda naredimo matriko stevilo dogodkov za vsak PP/years

number_events <- sapply(split(data_GPD$loss, factor(paste(data_GPD$BL, data_GPD$years), 
                                                       levels=level_BL_years)), length)  #st. dogodkov

nrows_lambda <-n_yrs * n_BL 

#matrika num 
num <- data.frame(years = sort(rep(yrs,n_BL)),
               BL = rep(BL_short, n_yrs),
               nb =number_events,
               row.names = seq_len(nrows_lambda))

#######
# a) gam za lambda za vec razlicnih modelov
##Op.: v primeru, ko dodas -1 ni INTERCEPT!

(lam_gam1 <- gam(nb~1, data=num, family=poisson)) #model1: konstanta

(lam_gam2 <- gam(nb~BL-1, data=num, family=poisson)) #model2: faktorska f. za BL

(lam_gam3 <- gam(nb~BL+years-1, data=num, family=poisson)) #model3: linearni model faktorska f. BL + leta

#AIC za prve 3 modele
lam_gam1$aic
lam_gam2$aic
lam_gam3$aic

#Rocno izracunan AIC za lam_gam3
c(-2*logLik(lam_gam3) + 2*sum(lam_gam3$edf), AIC(lam_gam3))


####
#b) izbor najboljsega modela glede na AIC

aic <- c(lam_gam3$aic)

#model je faktorska f. BL + f. za Years z razliznimi EDOF

for (i in 2:8){
  lam_gam <- gam(nb ~ BL + s(years, k=i+1, fx=T, bs="cr")-1,
               data=num, family=poisson)
  
  aic[i]<- AIC(lam_gam)
}


#GRAF AIC
par(mfrow=c(1,1))
plot(1:8, aic, type = 'b')

#najboljsi edf je najmanjsi edf, pri katerem se AIC ne zmanjsa, ce dodamo eno dodatno edof
edf <- 2

###
#c) ocena parametra lambda
a <- 0.05

lam_gam <-gam(nb ~ BL + s(years, k= edf + 1,  bs="cr"), 
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
layout(layout.n_BL, widths=c(0.5,1,1), heights=rep.int(1,10)) 

par(mar=rep.int(0,4), oma=rep.int(3,4))

#graf
for (i in 1: n_BL){
  
  #BL
  bl <- lamPred$covar$BL==BL_short[i]
  
  lambda <-  lamPred$predict[bl]
  
  #predicted lambda
  plot(yrs, lambda, type='l', xlim = x_yrs, ylim = y_lam,
       yaxt= "n",
       xaxt= "n")
  
  #CI
  CI_up <- lamPred$CI.up[bl]
  lines(yrs, CI_up, lty=2, xlim = x_yrs, ylim = y_lam)
  
  CI_low <- lamPred$CI.low[bl]
  lines(yrs, CI_low, lty=2, xlim = x_yrs, ylim = y_lam)
  
  #fitted lambda
  y <- lamFit$covar$years[bl]
  fit <- lamFit$fit[bl]
  points(yrs, lamFit$fit[bl], pch=20)
  
  
  #skala na x osi
  if(i==9 | i==10) axis(1, at =seq(1980,2015,5), labels = rep('',8))
  if(i==9 | i==10) axis(1, at =seq(1980,2010,10), labels = as.character(seq(1980,2010,10)), lwd = 1, col=1)
  
  #skala na y osi
  if(i%%2==1) axis(2, at =seq(0,14,2), labels = rep('',8) )
  if(i%%2==1) axis(2, at =seq(0,12,4), labels = as.character(seq(0,12,4)), las = 1)
     
  #text
  text(min(x_yrs)+0.05*diff(x_yrs), min(y_lam)+0.90*diff(y_lam),
       labels=BL_short[i], font=2)
  
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
fit_xibeta <- gamGPDfit(x=data_GPD, threshold=u, datvar="loss",
                 xiFrhs = model_xi, # interaction
                 nuFrhs = model_nu, # interaction
                 eps.xi=eps, eps.nu=eps, niter=niter,
                 include.updates=T)



boot_xibeta <- gamGPDboot(x = data_GPD, B=B, threshold=u, datvar="loss",
                      xiFrhs = model_xi, # xi
                      nuFrhs = model_nu, # nu
                      niter=niter, eps.xi=eps, eps.nu=eps,
                      include.updates=T)



fit_xibeta <- get.GPD.fit(boot_xibeta, alpha = a)

pred_xibeta <- GPD.predict(boot_xibeta)

xi_covar <- fit_xibeta$xi$covar
beta_covar <- fit_xibeta$beta$covar



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
