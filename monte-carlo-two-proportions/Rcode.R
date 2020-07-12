#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Rcode
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list=ls())
library(ggplot2)
library(shiny) 
require(LearnBayes)
library(tidyverse)
library(rstan)
library(DT)
require(rms) # freq logistic regression


options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
options(max.print=1000000)
fig.width <- 400
fig.height <- 300
fig.width2 <- 1400
fig.height2 <- 300#50
fig.width3 <- 1300  
fig.height3 <- 350#400
fig.width4 <- 1380
fig.height4 <- 300
p1 <- function(x) {formatC(x, format="f", digits=1)}
p2 <- function(x) {formatC(x, format="f", digits=2)}
options(width=200)
set.seed(12345) # reproducible

is.even <- function(x){ x %% 2 == 0 } # function to id. odd maybe useful

 
                                  
                                  
                                  vec3 <- c(.5, .5)
                                  
                                  vec4<- c(11, 41)
                                  
                                  n1y1 <-c(50, 15)
                                  
                                  n2y2 <- c(50, 10)
                                  
                                  
 
  # --------------------------------------------------------------------------
  # ---------------------------------------------------------------------------
  # --------------------------------------------------------------------------
  # This is where a new sample is instigated 
   
    trt <-  vec3
    ctr <- vec4
    
    n1y1 <- n1y1
    n2y2 <- n2y2
 
          trt.alpha=trt[1]
      trt.beta=trt[2]
      ctr.alpha=ctr[1]
      ctr.beta=ctr[2]
      n1=n1y1[1]
      y1=n1y1[2]
      n2=n2y2[1]
      y2=n2y2[2]
  
    

  
 
    a  <- trt.alpha 
    b  <- trt.beta
    a1 <- ctr.alpha
    b1 <- ctr.beta
    
  
    
    I = 500000                               # simulations
    
    theta1 = rbeta(I, y1+a, n1-y1+b)        # incorp. prior for trt
    theta2 = rbeta(I, y2+a1, n2-y2+b1)      # incorp. prior for placebo
    diff =   theta1-theta2                    # simulated differences
    ratio =  theta1/theta2 
    or <-   (theta1/ (1-theta1)) / (theta2/(1-theta2))
    ptrt <- mean( theta1>theta2)  
    prob.ratio <- (theta1/theta2 )>1
    prob.or <-  ((theta1/ (1-theta1)) / (theta2/(1-theta2))) > 2
    
    f <- data.frame(cbind( diff, ratio, or ,ptrt, prob.ratio, prob.or , theta1, theta2))
    
    rnamez <- c( "risk difference trt-ctrl","relative risk trt/ctrl", "odds ratio trt:ctrl", 
                 "p(trt > ctrl)", "pr(risk ratio > 1)", "pr(odds ratio > 1)",
                 "p(y=1|trt)","p(y=1|ctrl)" )
    
    f <- data.frame(cbind( diff, ratio, or ,ptrt,  theta1, theta2))
    
    rnamez <- c( "risk difference trt-ctrl","relative risk trt/ctrl", "odds ratio trt:ctrl", 
                 "p(trt > ctrl)", #"pr(risk ratio > 1)", "pr(odds ratio > 1)",
                 "p(y=1|trt)","p(y=1|ctrl)" )
    
    cnamez <- c("Mean","2.5%","25%","50%","75%","97.5%")
    
    clean <- function(x) { c(mean(x), quantile(x,c(.025, 0.25, 0.5, 0.75, 0.975))) }
    
    x <- apply(f, 2, clean)
    
    z <- t(x)
    
    rownames(z) <- rnamez
    colnames(z) <- cnamez
    
    
    
     f1= z 
     diff = diff
     theta1 = theta1
     
                theta2 = theta2  
                ratio = ratio  
                or = or 
                ptrt = theta1>theta2    
    
 
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
 
    z <-  diff
    
    q <- quantile(z,c(.025, 0.25, 0.5, 0.75, 0.975))
    par(bg = 'lightgoldenrodyellow') 
    par(mfrow=c(1,3))
    plot(density(z),
         xlab="risk differnece trt - ctrl",
         ylab="p(trt - ctrl | y, n)",
         main="",
         ylim=c(0,max(density(z)$y)),
         frame.plot=FALSE,cex.lab=1.5,lwd=3,yaxt="no")
    abline(v=q[1], col="blue") #95% credible interval
    abline(v=q[5], col="blue")
    abline(v=0, col="red", lty='dashed')
    
    z <-  ratio 
    
    q <- quantile(z,c(.025, 0.25, 0.5, 0.75, 0.975))
    
    plot(density(z), log="x",
         xlab="relative risk trt / ctrl",
         ylab="p(trt / ctrl | y, n)",
         main="",
         ylim=c(0, max(density(z)$y)),##
         frame.plot=FALSE,cex.lab=1.5,lwd=3,yaxt="no")
    abline(v=q[1], col="blue") #95% credible interval
    abline(v=q[5], col="blue")
    abline(v=1, col="red", lty='dashed')
    
    z <-  or
    
    q <- quantile(z,c(.025, 0.25, 0.5, 0.75, 0.975))
    
    plot(density(z),   log="x",
         xlab="odds ratio",
         ylab="p(odds trt / odds ctrl | y, n)",
         main="",
         ylim=c(0, max(density(z)$y)),
         frame.plot=FALSE,cex.lab=1.5,lwd=3,yaxt="no")
    abline(v=q[1], col="blue") #95% credible interval
    abline(v=q[5], col="blue")
    abline(v=1, col="red", lty='dashed')
    captio=("xxxx")
    
    
    
    par(mfrow=c(1,1))
    
 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
    
    x<- seq(0.001,.999, length.out=10000)
    
 
    
    
    tmp1 <- max(c(dbeta(x, trt.alpha, trt.beta)  ) )
    tmp2 <- max(c(dbeta(x, ctr.alpha, ctr.beta)))
    tmp <- max(tmp1, tmp2)
    
    par(bg = 'lightgoldenrodyellow')
    
    
    A <- curve(dbeta(x, trt.alpha, trt.beta),col = "blue", xlab = c("Probabiity"), 
               main=paste0("The Beta distribution for treatment in blue with shape parameters (",p2(trt.alpha),", ",p2(trt.beta),") and control in black (",p2(ctr.alpha),", ",p2(ctr.beta),")             "  
               ),
               ylab = "Density", xlim=c(0.0,1),  ylim=c(0, (tmp)*1.1) #ylim=c(0, max(
    )
    B <- curve(dbeta(x, ctr.alpha, ctr.beta),col = "black", xlab = c("Probabiity"), 
               
               ylab = "Density",  add=TRUE
    )
    
    mytext1 <- function(n) paste('Beta(', n, ')', sep = '')
    
    A$y[is.infinite(A$y)] <- 10
    B$y[is.infinite(B$y)] <- 10
    
    text(x=which.max( B$y )/100+.03, max(B$y)*1.05, mytext1(paste0(ctr.alpha, ",", ctr.beta)),  col='black')
    text(x=which.max( A$y )/100+.03, max(A$y)*1.05, mytext1(paste0(trt.alpha, ",", trt.beta)),  col='blue')
    
 
   
    tmp1 <- max(c(dbeta(x,y1+a,  n1-y1+b)  ) )
    tmp2 <- max(c(dbeta(x,y2+a1, n2-y2+b1)))
    tmp <- max(tmp1, tmp2)
    
    par(bg = 'lightgoldenrodyellow')
    
    
    A <-  curve(dbeta(x, y1+a, n1-y1+b),col = "blue", xlab = c("Probabiity"), 
                main=paste0("The Beta distribution for treatment in blue with shape parameters (",p2(y1+a),", ",p2(n1-y1+b),") and control in black (",p2(y2+a1),", ",p2(n2-y2+b1),")"  
                ),
                ylab = "Density", xlim=c(0.0,1),  ylim=c(0, (tmp)*1.1) #ylim=c(0, max(tmp1)),
                
    )
    B <- curve(dbeta(x, y2+a1,  n2-y2+b1),col = "black", xlab = c("Probabiity"), 
               
               ylab = "Density" , add=TRUE
               
    )
    
    mytext1 <- function(n) paste('Beta(', n, ')', sep = '')
    
    A$y[is.infinite(A$y)] <- 10
    B$y[is.infinite(B$y)] <- 10
    
    text(x=which.max( A$y )/100+.03, max(A$y)*1.05, mytext1(paste0(y1+a, ",", n1-y1+b)),  col='blue')
    text(x=which.max( B$y )/100+.03, max(B$y)*1.05, mytext1(paste0(y2+a1, ",",  n2-y2+b1)),  col='black')
    
    
 
    z <- diff

    q <- quantile(z,c(.025, 0.25, 0.5, 0.75, 0.975))
    par(bg = 'lightgoldenrodyellow') 
    par(mfrow=c(1,3))
    plot(density(z),
         xlab="risk differnece trt - ctrl",
         ylab="p(trt - ctrl | y, n)",
         main="",
         ylim=c(0,max(density(z)$y)),
         frame.plot=FALSE,cex.lab=1.5,lwd=3,yaxt="no")
    abline(v=q[1], col="blue") #95% credible interval
    abline(v=q[5], col="blue")
    abline(v=0, col="red", lty='dashed')
    
    z <- ratio
    
    q <- quantile(z,c(.025, 0.25, 0.5, 0.75, 0.975))
    
    plot(density(z), log="x",
         xlab="relative risk trt / ctrl",
         ylab="p(trt / ctrl | y, n)",
         main="",
         ylim=c(0, max(density(z)$y)),##
         frame.plot=FALSE,cex.lab=1.5,lwd=3,yaxt="no")
    abline(v=q[1], col="blue") #95% credible interval
    abline(v=q[5], col="blue")
    abline(v=1, col="red", lty='dashed')
    
    z <- or
    
    q <- quantile(z,c(.025, 0.25, 0.5, 0.75, 0.975))
    
    plot(density(z),   log="x",
         xlab="odds ratio",
         ylab="p(odds trt / odds ctrl | y, n)",
         main="",
         ylim=c(0, max(density(z)$y)),
         frame.plot=FALSE,cex.lab=1.5,lwd=3,yaxt="no")
    abline(v=q[1], col="blue") #95% credible interval
    abline(v=q[5], col="blue")
    abline(v=1, col="red", lty='dashed')
    captio=("xxxx")
    
    
    
    par(mfrow=c(1,1))
    
 
    mc <- f1
    # s1 <- stan()$f
    # s2 <- stan2()$res
    
    A <- (mc)
    # B <- (s1)
    # C <- (s2)
    
    A <- data.frame(A)
    # B <- data.frame(B)
    # C <- data.frame(C)
    
    A$model <- "Bayesian Monte Carlo"
    # B$model <- "Bayes proportions"
    # C$model <- "Bayes logistic reg."
    # 
    A$parameter = rownames(A)
    # B$parameter = rownames(B)
    # C$parameter = rownames(C)
    
    x <-A# rbind(A,B,C)
    #x$parameter = rownames(x)
    
    names(x) <- c("Mean","p2.5","p25","p50","p75","p975","Model","parameter")
    x <- x[,c("Model","parameter","Mean","p2.5","p25","p50","p75","p975")]
    
    
    rownames(x) <- NULL
    x$Model <- NULL
    foo <- x
    datatable(x,
              
              rownames = FALSE,
              
              options = list(
                searching = TRUE,
                #pageLength = 20,
                paging=FALSE,
                lengthMenu = FALSE ,
                lengthChange = FALSE,
                autoWidth = FALSE
                # colReorder = TRUE,
                # deferRender = TRUE,
                # scrollY = 200,
                # scroller = T
              ))  %>% 
      formatRound(
        columns= c("Mean","p2.5","p25","p50","p75","p975"), digits=4  )
    
    
 
  # --------------------------------------------------------------------------
  
    # n1 <- sample$n1  
    # y1 <- sample$y1
    # n2 <- sample$n2
    # y2 <- sample$y2
    # 
    # n1 <- 50
    # y1 <- 15
    # n2 <- 50
    # y2 <- 10
    # 
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~
    data <- matrix(c(y1, n1-y1,y2,n2-y2),nr=2,dimnames=list(c("response","nonresponse"), c("trt","ctrl")))
    f <- fisher.test(data)
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~
    res <- prop.test(x = c(y1, y2), n = c(n1, n2), correct = FALSE)
    pr <- res 
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~
    # # logistic regression
    
    Table <- t( matrix(c(y1,y2,n1-y1,n2-y2), nc=2))
    f99 <- glm(as.table(Table) ~ c(1,0), family=binomial)
    f100 <- exp(coef(f99))[2][[1]]
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    d = as.data.frame(as.table(as.matrix(data)))
    
    # from stack exchange
    countsToCases <- function(x, countcol = "Freq") {
      # Get the row indices to pull from x
      idx <- rep.int(seq_len(nrow(x)), x[[countcol]])
      # Drop count column
      x[[countcol]] <- NULL
      # Get the rows from x
      x[idx, ]
    }
    
    
    dd <- countsToCases(d)
    rownames(dd)<-NULL
    
    
    dd$y <- ifelse(dd$Var1 %in% "response",1,0)
    dd$x <- ifelse(dd$Var2 %in% "trt",1,0)
    dd$Var1 <- dd$Var2 <- NULL
    
    #y <- dd$y; x <- dd$x
    # summary(glm(y ~ x, family=binomial))
    
    ddd <<- datadist(dd)
    options( datadist = "ddd" )
    harrell <- summary(lrm(y~x,dd))
    
    f
    pr
    harrell
   