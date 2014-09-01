#rm(list=ls())- izbrise vse podatke
##Podatki Williso skodnih dogodkih 
#data_full vsebuje 1516 podatkov o izgubah :  dim(data_full)
#podatki so od leta 1970 do leta 2014 

setwd('/Users/zala/GitHub/EVA/')

data_full <- read.csv('loss_data_full_view_popr.csv', sep = ';', header = TRUE,fill = TRUE )

summary(data_full[,2:10])

data_1984 <-data_full[data_full$Event.Date>1984,]    #podatki o izgubah od leta 1984 dalje

#dim(data_1984) :  1449   16 

#odstranjeni podatki, kjer loss=NA
ref <- data_1984$Ref
org <- data_1984$Organisation
years <- data_1984$Event.Date
gross.loss <- data_1984$Gross.Loss.GBP

year_loss_1984 <- data.frame(ref, org, years, gross.loss)   

year_loss_1984 <-na.omit(year_loss_1984) 

#odstranjeni se podatki, kjer loss==o, saj nimajo smisla
year_loss_1984 <- year_loss_1984[!year_loss_1984$gross.loss==0,]

#dim(year_loss_1984)

#INDEKS INFLACIJE - popravljene izgube za indeks inflacije

index <- read.csv('CPI_index_yearly.csv', sep = ',', header = TRUE) #inflation indexes 1700-2013

index_2014_monthly <- read.csv('CPI_index_monthly.csv', 
                               sep = ',', header = TRUE)[1014:1020,] #mesecni indeksi inflacije jan-jul 2014

index_2014 <- mean(as.numeric(as.vector(index_2014_monthly$CDKO))) #povprecje mesecnih indeksonv 2014

index <- rbind(index[index$X>1984,], c(2014,index_2014 ))         #letni ideksi 1984-2014

data_index <- merge(year_loss_1984,
                    index, 
                    by.x= "years",
                    by.y= "X")        #leto, loss, inflation ideks za leto izgube


#CORRECTED_LOSS: popravljene izgube za indeks inflacije

corrected <- data_index$gross.loss*index_2014/data_index$CDKO     #popravljenje izgube za inflacijo

loss_corrected <- data.frame(data_index, loss_corr = corrected)   #dodan stolpec popravljenje izgube

(loss_corrected[order(loss_corrected$loss_corr, decreasing = TRUE),][1:10,])
summary(loss_corrected)
#dim(loss_corrected)

#####
#EVENT TYPES EXPLAINED : http://www.bis.org/bcbs/qis/oprdata.pdf
#####

ET_sub <- sort(unique(data_full$Basel.Loss.Event))

ET_main <- c(rep(1,3),rep(2,2), rep(3,3), rep(4,5), 5, 6, rep(7,6),2,1) #

ET <- c('IF','EF', 'EPWS', 'CPBP', 'DPA', 'BDSF', 'EDPM')
ET_short <- data.frame(ET.index = 1:7, ET)

ET_all <- merge(data.frame(ET_sub=ET_sub[-1], ET.index = (ET_main)), ET_short,
            by.x = 'ET.index', 
            by.y = 'ET.index')


######
# potrebni podatki
#####
BL <- as.character(data_1984$Business.Line)

#Op.: Za vse BL, ki imajo vrednost n/a, se nastavi vrednost Unallocated Business Line
BL[BL=='n/a'] <- 'Unallocated Business Line'

#Op.: Za BL Insurance(life) in Insurance (non-life) se nastavi skupni BL Insurance
BL[BL=='Insurance(life)'] <- 'Insurance'
BL[BL=='Insurance (non-life)'] <- 'Insurance'

loss <- merge (loss_corrected,
               data.frame(ref, basel_event = data_1984$Basel.Loss.Event, BL = BL ))

data <- merge (loss,
               ET_all,
               by.x = 'basel_event',
               by.y = 'ET_sub')

summary(data)

####
#Stevilo dogodkov za Basel matriko
####
BL_unique <- unique(sort(data$BL))
ET_unique <- unique(sort(data$ET))

BL_ET <- expand.grid(BL=BL_unique, ET=ET_unique) # vse kombinacije BL-ET

lev <- apply(BL_ET, 1, paste, collapse=" ")     # vsi leveli - stringi vseh kombinacij BL-ET

number_BL_ET <- sapply(split(data$loss_corr, factor(paste(data$BL, data$ET), levels=lev)), length)

###
#Basel matrika, basel vector
###
n_BL <- length(BL_short)
n_ET <- length(ET_unique)
n_yrs <- length(years)

##Matrika
Basel_matrika <- matrix(number_BL_ET,ncol = n_ET , nrow=n_BL )
colnames(Basel_matrika) <- as.character(ET_unique)
rownames(Basel_matrika) <- as.character(BL_unique)

##Vektor
Basel_vector <- rowSums(Basel_matrika)

###
#st. skodnih dogodkov
###
years <- 1984:2014

number_events_year <- sapply(split(data$loss_corr, factor(paste(data$years), levels=years)), length)
gross_losses_year <- sapply(split(data$loss_corr/10^6, factor(paste(data$years), levels=years)), sum)

#Stevilo izgub
x_years <- c(min(years) ,max(years))       #xlim za leta

par(mar=c(5, 5, 4, 5) + 0.1)
plot(years, number_events_year, 
     ylim = c(0, max(number_events_year)), xlim = x_years, 
     xlab = '', ylab='',
     type = "l", lty = 1, col=4,
     main = '')

mtext(2, text='Stevilo skodnih dogodkov z znano bruto izgubo', line=3)

#Bruto izgube
par(new=TRUE)
plot(years, gross_losses_year,
     ylim = c(0, max(gross_losses_year)), xlim = x_years, axes = F, 
     xlab = '', ylab='',
     type = "l", lty = 1, ,col = 3,
     main = '')

axis(4, ylim = c(0, max(gross_losses_year) ), lwd = 1, col=1) 

mtext(4, text='Skupa znana bruto izguba v mio GBP',line = 3)

legend(x = 'topleft',legend = c('Stevilo skodnih dogodkov','Bruto izgube v mio GBP'), 
       lty = 1, col = c(4,3))

###
#Graf stevilo izgub skozi leta po BL
###

BL_years <- expand.grid(BL=BL_unique, years) # vse kombinacije BL-years

level_BL_years <- apply(BL_years, 1, paste, collapse=" ")     # vsi leveli - stringi vseh kombinacij BL-ET

number_BL_years <- sapply(split(data$loss_corr, factor(paste(data$BL, data$years), 
                                                       levels=level_BL_years)), length)

BL_years_M <- matrix(number_BL_years,ncol = n_yrs, nrow=n_BL )

#meje za graf
y_BL <-c(0, max(BL_years_M))  #ylim do max stevila skodnih dogodkov 

#graf

for (i in 1:n_BL){
  
  if (i==1) par(new=F, mar=c(5, 5, 4, 5) + 0.1) else  par(new=T) 
  
  plot(years, BL_years_M[i,], 
         ylim = y_BL, xlim = x_years, 
        axes = if(i==1) T else F,
         xlab = '', ylab='',
         type = "l", lty = 1, main = '', col = 7*i)  
}


BL_short <-c("AS","AM", "CB", "CF", "I", "PS", "RBa", "PBr", "TS", "UBL") 

legend(x = "topleft",legend = BL_short, col= 7*(1: n_BL), lty=1 )


####
#skodni dogodnik po BL skozi leta
####

BL_over_years <- data.frame(loss=log((data$loss_corr/10^6), base = 10), years=data$years)
        #matrika leto, izguba v log od  mio GBP

data_loceni_BL <- split(BL_over_years, factor(as.character(data$BL))) #list izgub za vsak BL 

##Priprava za risanje
y_BL <-c(0, max(BL_over_years$loss)) #meja za y do najvecje izguve

layout.n_BL <- matrix(1:n_BL, ncol=2, byrow=TRUE) # razporeditv polj za risanje grafa

layout.n_BL <- rbind(layout.n_BL, c(n_BL+1, n_BL+2)) # dodano polje za napise na x osi

layout.n_BL <- cbind(c(n_BL+3,0), layout.n_BL) # dodano polje za napise na x osi

layout(layout.n_BL, widths=c(0.5,1,1), heights=rep.int(1,10)) # layout

opar <- par(mar=rep.int(0,4), oma=rep.int(3,4))

for (i in 1: n_BL){
  years <- as.data.frame(data_loceni_BL[i])[,2]       #leta
  log_loss <- as.data.frame(data_loceni_BL[i])[,1]    #izbube
  
  plot(years,log_loss ,
       xlim = x_years, ylim = y_BL,
       yaxt=if(i%%2==1) "s" else "n",
       xaxt=if(i==9 | i==10) "s" else "n")
  
  text(min(x_years)+0.05*diff(x_years), min(y_BL)+0.95*diff(y_BL),
       labels=BL_short[i], font=2)
}

##Napisi na X osi
plot.new()

text(0.1, 0.1, labels="Year")

plot.new()

text(0.3,0.1, labels = "Skodni dogodki")
points(0,0.1)

###!!!!!!!! Popravi se napise na X osi in Y osi - X os je 10^4, Y os se napisi ne smejo prikirvati


##Napisi na Y osi
## y axis label
plot.new()

text(0.5, 0.5, srt=90,labels="TUKAJ PRIDE NAPIS NA Y OSI")




###########################
#2. OCENA PARAMETROV
###########################




