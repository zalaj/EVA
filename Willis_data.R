##Podatki Williso skodnih dogodkih 
#data_full vsebuje 1516 podatkov o izgubah :  dim(data_full)
#podatki so od leta 1970 do leta 2014 

setwd('/Users/zala/Documents/Magisterska/')

data_full <- read.csv('loss_data_full_view.csv', sep = ';', header = TRUE,fill = TRUE )

summary(data_full[,2:10])

data_1984 <-data_order[data_full$Event.Date>1984,]    #podatki o izgubah od leta 1984 dalje

#dim(data_full[data_full$Event.Date>1984,]) :  1449   16 

#odstranjeni podatki, ki loss=NA
ref <- data_1984$Ref
years <- data_1984$Event.Date
gross.loss <- data_1984$Gross.Loss.GBP

year_loss <- data.frame(ref, years, gross.loss)

year_loss <- year_loss[order(years, decreasing = TRUE),]

year_loss_1984 <-na.omit(year_loss)        

summary(year_loss_1984)
dim(year_loss_1984)

#INDEKS INFLACIJE - popravljene izgube za indeks inflacije

index <- read.csv('CPI_index_yearly.csv', sep = ',', header = TRUE) #inflation indexes 1700-2013

index_2014_monthly <- read.csv('CPI_index_monthly.csv', sep = ',', header = TRUE)[1014:1020,] 
                                                            #mesecni indeksi inflacije jan-jul 2014

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
BL <- data_1984$Business.Line

loss <- merge (loss_corrected,
               data.frame(ref, basel_event = data_1984$Basel.Loss.Event, BL = BL ))

data <- merge (loss,
               ET_all,
               by.x = 'basel_event',
               by.y = 'ET_sub')

data[order(data$loss_corr, decreasing = TRUE),][1:10,]
summary(data)

####
#Basel matrika
####
BL <- unique(na.omit(BL))

grid <- expand.grid(BL=BL, ET=ET) # *all* variable combinations GROUP - YEAR

lvls <- apply(grid, 1, paste, collapse=" ") # naredi vektor stringov vseh kombinacij GROUP-YEAR

nm <- sapply(split(data$loss_corr, factor(paste(data$ET, x$BL), levels=lvls)), length)
