#rm(list=ls())- izbrise vse podatke
##Podatki Williso skodnih dogodkih 
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

year_loss_1984 <- data.frame(ref, org, years, gross_loss)   

#odstranjeni podatki, kjer loss=NA
year_loss_1984 <-na.omit(year_loss_1984) 

#odstranjeni se podatki, kjer loss==0, saj za analizo nimajo pomena
year_loss_1984 <- year_loss_1984[!year_loss_1984$gross_loss==0,]

#dim(year_loss_1984): 968   4

###
# 1.2. INDEKS INFLACIJE - popravljene izgube za indeks inflacije
###

index <- read.csv('CPI_index_yearly.csv', sep = ',', header = TRUE) #inflation indexes 1700-2013

index_2014_monthly <- read.csv('CPI_index_monthly.csv', 
                               sep = ',', header = TRUE)[1014:1020,] #mesecni indeksi inflacije jan-jul 2014

index_2014 <- mean(as.numeric(as.vector(index_2014_monthly$CDKO))) #povprecje mesecnih indeksonv 2014

index <- rbind(index[index$X>1984,], c(2014,index_2014 ))         #letni ideksi 1984-2014

data_index <- merge(year_loss_1984,
                    index, 
                    by.x= "years",
                    by.y= "X")        #leto, loss, inflation ideks za leto izgube

###
# 1.3. CORRECTED_LOSS: popravljene izgube za indeks inflacije
###

corrected_loss <- data_index$gross_loss*index_2014/data_index$CDKO     #popravljenje izgube za inflacijo

loss_corrected <- data.frame(data_index, loss = corrected_loss)   #dodan stolpec popravljenje izgube

(loss_corrected[order(loss_corrected$loss, decreasing = TRUE),][1:10,])
summary(loss_corrected)

#dim(loss_corrected)

###
# 1.4. EVENT TYPES EXPLAINED : http://www.bis.org/bcbs/qis/oprdata.pdf
###

ET_sub <- sort(unique(data_full$Basel.Loss.Event))  #vsi ET, ki so v podatkih, urejeni od 1-7

ET_main <- c(rep(1,3),rep(2,2), rep(3,3), rep(4,5), 5, 6, rep(7,6),2,1) #vektor 1-7, ki podkategorijam ET doloci glavne kategorije ET

ET <- c('IF','EF', 'EPWS', 'CPBP', 'DPA', 'BDSF', 'EDPM')   #vektor s kraticami ET

ET_short <- data.frame(ET.index = 1:7, ET) #povezovalna matrika med st. ET in kratico ET

#koncna povezovalna matrika za vsak podtip ET kratica za glavni ET
ET_all <- merge(data.frame(ET_sub=ET_sub[-1], ET.index = (ET_main)), ET_short,
            by.x = 'ET.index', 
            by.y = 'ET.index')


###
# 1.5. BL
###

BL <- as.character(data_1984$Business.Line)

#Op.: Za vse BL, ki imajo vrednost n/a, se nastavi vrednost Unallocated Business Line
BL[BL=='n/a'] <- 'Unallocated Business Line'

#Op.: Za BL Insurance(life) in Insurance (non-life) se nastavi skupni BL Insurance
BL[BL=='Insurance(life)'] <- 'Insurance'
BL[BL=='Insurance (non-life)'] <- 'Insurance'


###
# 1.6. koncni podatki v matriki data
###

loss <- merge (loss_corrected,
               data.frame(ref, basel_event = data_1984$Basel.Loss.Event, BL = BL ))

data <- merge (loss,
               ET_all,
               by.x = 'basel_event',
               by.y = 'ET_sub')

summary(data)

###############################
#2. BASEL MATIRKA IN VEKTOR
###############################

###
# 2.1. Stevilo dogodkov za Basel matriko
###

BL_unique <- unique(sort(data$BL))

ET_unique <- unique(sort(data$ET))

BL_ET <- expand.grid(BL=BL_unique, ET=ET_unique) # vse kombinacije BL-ET

lev <- apply(BL_ET, 1, paste, collapse=" ")     # vsi leveli - stringi vseh kombinacij BL-ET

number_BL_ET <- sapply(split(data$loss, factor(paste(data$BL, data$ET), levels=lev)), length)

###
# 2.2. Basel matrika
###

yrs <- 1984:2014

BL_short <-c("AS","AM", "CB", "CF", "I", "PS", "RBa", "PBr", "TS", "UBL") 

n_BL <- length(BL_short)
n_ET <- length(ET_unique)
n_yrs <- length(yrs)


Basel_matrika <- matrix(number_BL_ET,ncol = n_ET , nrow=n_BL )
colnames(Basel_matrika) <- as.character(ET_unique)
rownames(Basel_matrika) <- as.character(BL_unique)

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
gross_losses_year <- sapply(split(data$loss/10^6, factor(paste(data$years), levels=yrs)), sum)

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
#3.3. Graf stevilo izgub skozi leta po BL
###

BL_years <- expand.grid(BL=BL_unique, yrs) # vse kombinacije BL-years

level_BL_years <- apply(BL_years, 1, paste, collapse=" ")     # vsi leveli - stringi vseh kombinacij BL-years

number_BL_years <- sapply(split(data$loss, factor(paste(data$BL, data$years), 
                                                       levels=level_BL_years)), length) #st. dogodkov za vsako kombinacijo BL-years

#Matrika st. dogodkov za vsako po BL za vsako leto
BL_years_M <- matrix(number_BL_years,ncol = n_yrs, nrow=n_BL )
colnames(BL_years_M) <- as.character(yrs)
rownames(BL_years_M) <- as.character(BL_unique)

#priprava za risanje
y_BL <-c(0, max(BL_years_M))  #ylim do max stevila skodnih dogodkov 
col <- rainbow(10)

par(mfrow=c(1,1))
par(mar=c(5, 5, 4, 5) + 0.1, oma = rep(0,4))


for (i in 1:n_BL){
  
  if (i==1) par(new=F, mar=c(5, 5, 4, 5) + 0.1) else  par(new=T) 
  
  plot(yrs, BL_years_M[i,], 
       ylim = y_BL, xlim = x_yrs, 
       axes = if(i==1) T else F,
       xlab = '', ylab='',
       type = "l", lty = 1, main = '', col = col[i])  
}

legend(x = "topleft",legend = BL_short, col= col, lty=1, ncol=2)

mtext(1, text='Leto', line=3)
mtext(2, text='Stevilo skodnih dogodkov', line=3)


###
#3.4 GRAF skodni dogodnik po BL skozi leta
###

#matrika leto, izguba v log od  mio GBP
BL_over_years <- data.frame(loss=log((data$loss/10^6), base = 10), years=data$years)
       

data_loceni_BL <- split(BL_over_years, factor(as.character(data$BL))) #list izgub za vsak BL 

##Priprava za risanje
y_BL <-range(BL_over_years$loss) #meja za y do najvecje izguve

layout.n_BL <- matrix(1:n_BL, ncol=2, byrow=TRUE) # razporeditv polj za risanje grafa

layout.n_BL <- rbind(layout.n_BL, c(n_BL+1, n_BL+2)) # dodano polje za napise na x osi

layout.n_BL <- cbind(c(n_BL+3,0), layout.n_BL) # dodano polje za napise na x osi

layout(layout.n_BL, widths=c(0.5,1,1), heights=rep.int(1,10)) # layout

opar <- par(mar=rep.int(0,4), oma=rep.int(3,4))

for (i in 1: n_BL){
  x <- as.data.frame(data_loceni_BL[i])[,2]       #leta
  log_loss <- as.data.frame(data_loceni_BL[i])[,1]    #izbube
  
  plot(x,log_loss ,
       xlim = x_yrs, ylim = y_BL,
       yaxt=if(i%%2==1) "s" else "n",
       xaxt=if(i==9 | i==10) "s" else "n")
  
  text(min(x_yrs)+0.05*diff(x_yrs), min(y_BL)+0.95*diff(y_BL),
       labels=BL_short[i], font=2)

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

u <- quantile(data$loss, c(0,0.1,0.2,0.3,0.4,0.5))   #vektor kvantilov 

###data_GPD so podatki v repu porazdelitve, ki so vecji od izbranega prgau u
data_GPD <- data[data$loss>u[6],]

#prestejemo vse dogodke 
number_events <- sapply(split(data_GPD$loss, factor(paste(data_GPD$BL, data_GPD$years), 
                                                       levels=level_BL_years)), length)

nrows_lambda <-n_yrs * n_BL 

num <- data.frame(years = sort(rep(yrs,n_BL)),
               BL = rep(BL_short, n_yrs),
               nb =number_events,
               row.names = seq_len(nrows_lambda))

##
(lam_glm1 <- glm(nb~1, data=num, family=poisson))

(lam_gam1 <- gam(nb~1, data=num, family=poisson))
AIC(lam_gam1)
AIC(lam_glm1)

(lam_glm2 <- glm(nb~BL-1, data=num, family=poisson))

(lam_glm3 <- glm(nb~BL+years-1, data=num, family=poisson))

lrtest(lam_glm1, lam_glm2)
lrtest(lam_glm2, lam_glm3)

##v primeru, ko dodas -1 ni INTERCEPT!

lam_gam1 <- gam(nb~ BL + years-1, data=num, family=poisson)
summary(lam_gam1)

aic <- c(AIC(lam_gam1))

for (i in 2:8){
  lam_gam <- gam(nb ~ BL + s(years, k=i+1, fx=T, bs="cr")-1, fix=T,
               data=num, family=poisson)
  
  aic[i]<- AIC(lam_gam)
}

aic
###GRAF AIC

par(mfrow=c(1,1))
plot(1:8, aic, type = 'b')


###
#ocena parametra lambda

dof <- 3

a <- 0.05
lam_gam_3 <-gam(nb ~ BL + s(years, k= dof + 1,  bs="cr"), 
                           data=num, family=poisson)



lamFit <- get.lambda.fit(lam_gam_3)

###get.lambda.fit extracts a convenient list containing unique covariate 
    #combinations and corresponding fitted values from an object returned by gam().
    #vrne torej enake vrednosti kot fitted - sort zato, da za fitted niso urejeni po atributih
    #sort(get.lambda.fit(lam_gam_3)$fit)==sort(fitted(lam_gam_3))

lamPred <- lambda.predict(lam_gam_3 , alpha=a)

###lambda.predict() computes a convenient list containing unique covariate combinations 
    #and corresponding predicted values and pointwise asymptotic confidence intervals 
    #(obtained from the estimated standard errors obtained by predict(..., se.fit=TRUE)).


#####
#graf za lambda in intervali zaupanja

y_lam <- c(min(lamPred$CI.low), max(lamPred$CI.up))
layout(layout.n_BL, widths=c(0.5,1,1), heights=rep.int(1,10)) # layout

par(mar=rep.int(0,4), oma=rep.int(3,4))

for (i in 1: n_BL){
  
  group <- lamPred$covar$BL==BL_short[i]
  
  lambda <-  lamPred$predict[group] 
  plot(yrs, lambda, type = 'b', xlim = x_yrs, ylim = y_lam,
       yaxt=if(i%%2==1) "s" else "n",
       xaxt=if(i==9 | i==10) "s" else "n")
  
  CI_up <- lamPred$CI.up[group]
  lines(yrs, CI_up, lty=2, xlim = x_yrs, ylim = y_lam)
  
  CI_low <- lamPred$CI.low[group]
  lines(yrs, CI_low, lty=2,xlim = x_yrs, ylim = y_lam)
  
} 

#TEST primerjava na roke izracunanega AIC in AIC v R
c(-2*logLik(lam_gam_3) + 2*12.18841, AIC(lam_gam_3))

plot(lam_gam_3)
gam

gpd.fit(data$loss, 10^6)





################################################################################################
################################################################################################
#ocena parametrov xi, beta

B <- 32
u_star<-u[6]
eps <- 10^(-5)
niter <- 30


bootGPD <- gamGPDboot(x=data, threshold=u_star, datvar="loss",
                      xiFrhs = ~ BL+years, # interaction
                      nuFrhs = ~ 1, # interaction
                      eps.xi=eps, eps.nu=eps)

fit <- gamGPDfit(x=data, threshold=u_star, datvar="loss",
                 xiFrhs = ~ BL+years, # interaction
                 nuFrhs = ~ 1, # interaction
                 eps.xi=eps, eps.nu=eps, niter=30)

gamGPDboot(x=data, B=B,threshold=u_star, datvar="loss")
QQplot(fit$res, reference='exp',rate=1)

fit
-2*-11517.27 +2*-11502.59


xibetaFit <- get.GPD.fit(bootGPD, alpha=a) # several s

xibetaFit$xi$fit %in% fit$xi



## compute predicted values
xibetaPred <- GPD.predict(bootGPD)
