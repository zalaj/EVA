a <- 0.05
B <- 30
eps <- 10^(-3)
niter <- 128

####
#LRtest

lr_test <- function(logl0, logl1, df, a){
  #H_0 : model M_0 je pravi, H_1: model M_1 je pravi
  #LogLR test izracuna D = 2* (l(M_1) - l(M_0)). H_0 zavrnemo, 
  
  D <- (2*(logl1 - logl0))
  c <-  qchisq(1-a, df)
  
  p <- pchisq(D, df, lower.tail=F)
  
  #return (list(D,c,p))
  #D je asimpotoicno chisq(alpha,df) H_0 zavrnemo, ce je  D < c, c=chisq(alpha, df), df = df1-df0 
  
  if(D<c) return(paste('Ne zavrnemo H0. P-vrednost je:',round(p,digit=4), 'D=',D,"c=", c))
  else return(paste('Zavrnemo H0. P-vrednost je:',round(p,digit=4), "D=", D,"c=", c))
}

#
#Model selection


u_kvant <- quantile(data$loss, c(0,0.1,0.2,0.3,0.4,0.5)) 

model_xi_nu <- function(u){
  
  data_GPD <- data[data$loss > u,]
  
  f_xi <- c('~1', '~PP - 1', '~PP + years - 1') #vektor vseh funkcij za fit xi
  
  glm_xi <- numeric(0)
  edf_xi <- numeric(0)
  
  for (i in 1: length(f_xi)){
      
    #gamGPDdit ti v vsakem koraku izpise povprecno relativno razliko in ko je manjsa od eps, konca
    fit_1 <- gamGPDfit(x=data_GPD, threshold=u, datvar="loss",
                     xiFrhs = formula(f_xi[i]), 
                     nuFrhs = ~ 1, 
                     eps.xi=eps, eps.nu=eps, niter=niter, progress = F)

      
    glm_xi[i]<- fit_1$logL
    edf_xi[i] <- sum(fit_1$xiObj$edf) 
    
  }
  
  
  #Model 2 proti 1
  lr_xi21 <- lr_test(glm_xi[1],glm_xi[2], 1, a)
  cat('H0: Model 1 je pravi, H1: Model 2 je pravi', '\n', lr_xi21, '\n' )
  
  #Model 3 proti 2
  lr_xi32 <- lr_test(glm_xi[2],glm_xi[3], edf_xi[3]- edf_xi[2], a)
  cat('H0: Model 2 je pravi, H1: Model 3 je pravi', '\n', lr_xi32, '\n' )
  
  #model za xi
  tmenu <- paste("Model za xi:", f_xi)
  pick_xi <- menu(tmenu, title = "\n Izberi najboljsi model (or 0 to exit):")
  
  if (pick_xi %in% 1:length(f_xi)){
    
    model_xi <- f_xi[pick_xi]
      
    #########
    #model za nu
    f_nu <- c('~1','~PP-1 ', '~PP + years - 1', "~PP + s(years, bs='cr') - 1")
      
    glm_nu <- numeric(0)
    edf_nu <- numeric(0)
    
    for (j in 1: length(f_nu)){ 
        
      #gamGPDdit ti v vsakem koraku izpise povprecno relativno razliko in ko je manjsa od eps, konca
      fit_2 <- gamGPDfit(x=data_GPD, threshold=u, datvar="loss",
                       xiFrhs = formula(model_xi), # interaction
                       nuFrhs = formula(f_nu[j]), # interactin
                       eps.xi=eps, eps.nu=eps, niter=niter, progress = F)
    
        
      glm_nu[j]<- fit_2$logL
      edf_nu[j] <- round((sum(fit_2$nuObj$edf)),4)
      }
    
    #LRtest
    #Model 2 proti 1
    lr_nu21 <- lr_test(glm_nu[1],glm_nu[2], 1, a)
    cat('H0: Model 1 je pravi, H1: Model 2 je pravi','\n',lr_nu21, '\n' )
    
    #Model 3 proti 2
    lr_nu32 <- lr_test(glm_nu[2],glm_nu[3], 1, a)
    cat('H0: Model 2 je pravi, H1: Model 3 je pravi','\n',lr_nu32, '\n' )
    
    #Model 4 proti 3
    lr_nu43 <- lr_test(glm_nu[3], glm_nu[4], 1, a)
    cat('H0: Model 3 je pravi, H1: Model 4 je pravi','\n',lr_nu43, '\n' )  
    
    #model za nu
    tmenu <- paste("Model za nu:", f_nu)
    pick_nu <- menu(tmenu, title = "\n Izberi najboljsi model (ali 0 za izhod):")
    
    if (pick_nu %in% 1: length(f_nu)){model_nu <- formula(f_nu[pick_nu])}
    }
  
  f <- list(xi = model_xi , nu = model_nu)
  f
}


#dolocitev ustreznih modelov za vsako vrednost u

loss_severity <- list()
for (i in 1:6){
  
  u <- as.numeric(u_kvant[i])
  
  kvantil <- paste((i-1)*10,'% kvantil')
  
  l<- model_xi_nu(u)
  
  loss_severity[[kvantil]] <- l   
}



###*************
#QQ_plot
###*************
QQ_plot_exp <- function(x,a=0.5){
  
  n <- length(x)
  plot.points <- ppoints(n, a)
  xp <- qexp(plot.points)
  y <- sort(x)
  plot(xp, y, xlab ="Teoreti\u010Dni kvantili", ylab = "Residuali", xlim = c(0,8), ylim = c(0,7))
  
  #premica
  y_p <- qexp(ppoints(x))
  abline(lsfit(y_p, sort(x)), col= 4  )
  
}

###
#QQ plot residualov glede na izbrane formule za xi in nu
qq_res <- function(model_xi, model_nu, u, k){ 
  #priprava za risanje
  
  #podatki v odvisnosti od 
  data_GPD <- data[data$loss > u,]
  
  #residuali za izbrani formuli xi in nu
  res <- gamGPDfit(x=data_GPD, threshold=u, datvar="loss",
                   xiFrhs = model_xi, # interaction
                   nuFrhs = model_nu, # interaction
                   eps.xi=eps, eps.nu=eps, niter=30, progress = F)$res
  
  #QQplot x=qexp, y=sort(res)
  QQ_plot_exp(res)
 
  #napisi
  if (k == 0) kvant<- "0"
  else kvant <- paste("0,",k, sep='')
  
  kvantil <- paste(kvant, '- kvantil')
  text(1, 6, labels = kvantil)
  #text(1, 4.5, labels = substitute("u=" ~u., list (u.=u)), font=2 )
}

#qqplot za vsako vrednost u in za izbrane modele
par(mfrow=c(2,3))
par(mar=c(5, 5, 2, 2) + 0.1, oma = rep(0,4))
for (i in 1:6){
  u <- as.numeric(u_kvant[i])
  
  l <- loss_severity[[i]]
  
  model_xi <- formula(l$xi)
  model_nu <- formula(l$nu)
  
  qq_res(model_xi, model_nu,u, (i-1)*10)
  
}


#residuali
resi <- mapply(function(data_GPD$loss, 0.888, 5) if (!is.na(beta)) 
  -log1p(-pGPD(data_GPD$loss, xi = xi, beta = beta))
  
  rlogL  
  fit$logL
  xi.formula <- update(interaction[2], Newton.xi ~.) 
}

#reparametriziran loglik in navadni log lik data iste vrednosti
sum(-log(fit$beta)- (1+1/fit$xi)*log(1+fit$xi*fit$y/fit$beta))
sum(log(1+fit$xi)-fit$nu-(1+1/fit$xi)*log(1+ fit$xi*(1+fit$xi)*exp(-fit$nu)*fit$y))  
glm_xi  

#st. preseganj
for (i in 1:6){
  u <- as.numeric(u_kvant[i])
  print(length(data[data$loss>u,]$loss))
}


u. <- as.numeric(u_kvant[6])
data_GPD. <- data_GPD[data_GPD$loss>u.,]

g <- gamGPDfit(x=data_GPD., threshold=u., datvar="loss",
          xiFrhs =~PP-1, # interaction
          nuFrhs = ~PP+years-1, # interaction
          eps.xi=10^(-3), eps.nu=10^(-3), niter=niter,
          include.updates=F)


  