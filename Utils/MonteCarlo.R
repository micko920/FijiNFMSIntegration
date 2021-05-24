# Required R packages
library(nlme)
library(data.table)
library(survey)
library(VGAM)
options(digits=8)
source(file = "./Funcs/UncertaintyFunc.R")

CalcMonteCarlo <- function(
  est, # CO2e estimate
  calc, # Function to calculate CO2e from inputs
  calcArgs, # function to create #MC runs of CO2e possibilities using uncertainties
  iterations = MCRuns,
  Confidence = CI, #  confidence interval
  previous = vector()
)
{
  result <- previous
  while (length(result)< iterations){
    result[length(result)+1] <- do.call(calc,calcArgs())
  }
  return(list(value=EstimateWithBounds(est, result), MCresults=result))
}




CalcStableMonteCarlo<- function(
  title,
  est,
  calc,
  calcArgs,
  start = 11,
  limit = 1e+8,
  tol = 10^(-min(round(log10(abs(est))),3)),
  c = CI, #  confidence interval
  rw = 5,
  dbgLvl = 1
)
{

  base <- start - 1 - rw
  runs <- base + 1

  mcSD <-  matrix(ncol=2,nrow=0,dimnames = list(c(),c("Runs","v")))
  mcLCI <-matrix(ncol=2,nrow=0,dimnames = list(c(),c("Runs","v")))
  mcUCI <- matrix(ncol=2,nrow=0,dimnames = list(c(),c("Runs","v")))

  mcResults <- vector()
  result <- matrix(ncol=3,nrow=0,dimnames = list(c(),c("LCI","UCI","DIFF")))


  trend <- 1
  while ((length(mcSD) < rw) | (trend > tol) & (runs < limit)) {
    if (length(mcSD) > rw) {
      trend <- sd(tail(na.omit(mcSD[,2]), rw))/mean(tail(na.omit(mcSD[,2]), rw))
      step <- round((1+log(trend/tol)) * (0.1^log10(tol)))
    } else {
      trend <- 1
      step <- 1
    }
    runs <- min(runs + step,limit)
    #mc <- vector()
    mc <- do.call(CalcMonteCarlo, list(est,calc, calcArgs,runs,c,mcResults))
    mcResults <- mc$MCresults
    result <- rbind(result, c(mc$value["LCI"],mc$value["UCI"],mc$value["UCI"]-mc$value["LCI"]))
    mcSD <- rbind(mcSD,c(runs,sd(result[,"DIFF"])))
    mcLCI <- rbind(mcLCI,c(runs,mean(result[,"LCI"])))
    mcUCI <- rbind(mcUCI,c(runs,mean(result[,"UCI"])))
    infoStableMC(rw,result,mcResults,mcSD,runs,step,trend,dbgLvl)
  }
  o <-list(v=CalcMonteCarlo(est,calc, calcArgs,runs,c,mcResults),total_runs=runs,samples=mcSD,lci_samples=mcLCI,uci_samples=mcUCI)

  resultsStableMC(title,o,dbgLvl)

  return(o)
}

resultsStableMC <- function(title,o,dbgLvl = 2) {
  if (dbgLvl > 0) {
    cat("Total Runs [",title,"] ",length(o$v$MCresults),"\n")
    print(o$v$value)
    print(cbind("RUNS"=tail(o$lci_samples[,1],5),
                "LCI"=tail(o$lci_samples[,2],5),
                "UCI"=tail(o$uci_samples[,2],5),
                "SD"=tail(o$samples[,2],5)))
  }
}

plotStableMC <-function(o){
  par(mfrow=c(3,1))

  result <- list(hist(o$v$MCresults,col="gray40", breaks=20, main ="",
                      xlim=c(min(o$v$MCresults),1.2*max(o$v$MCresults)),
                      xlab="Stable MC Estimates"),
                 plot(o$samples,ylab="SD of each set of MC estimates", xlab="Iterations", log="x"),
                 plot(NULL,
                       xlim=c(10,o$total_runs),
                       ylim=c(min(o$lci_samples[,2]),max(o$uci_samples[,2])),
                       xlab="Iterations", ylab="Qunatiles", log="x"))

  abline(h=o$v$value["Estimate"], untf = FALSE, col = "gray60")
  points(o$uci_samples,type="o", col="blue", pch="o")
  lines(o$uci_samples,col="blue",lty=1)
  points(o$lci_samples,col="red", pch="*")
  lines(o$lci_samples,col="red",lty=2)
  abline(v=o$total_runs, untf = FALSE, col = "green",)
  text(o$total_runs*(7/8), max(o$lci_samples)+((max(o$uci_samples) - max(o$lci_samples))/2), paste0("total runs = ",o$total_runs), col = "black", )

  return (result)
}

infoStableMC <- function(rw,result,mcResults,mcSD,runs,step,trend,dbgLvl = 2) {
  if (dbgLvl > 1) {
    cat(" runs=",runs)
    cat(" steps=", step)
    cat(" rolling=",trend)
    debugStableMC(rw,result,mcResults,mcSD,runs,step,trend,dbgLvl)
    cat("\n")
  }
}

debugStableMC <- function(rw,result,mcResults,mcSD,runs,step,trend,dbgLvl=3) {
  if (dbgLvl > 2) {
    cat(" LCI=",mean(result[,"LCI"]))
    cat(" Est=",mean(mcResults))
    cat(" UCI=",mean(result[,"UCI"]))
    cat(" Mean=",mean(result[,"DIFF"]))
    cat(" SD=",mcSD[length(mcSD)])
    cat(" abs=", abs(mcSD[length(mcSD)-1]-mcSD[length(mcSD)]))
    cat(" mean=", mean(tail(na.omit(mcSD[,2]), rw)))
    cat(" median=", median(tail(na.omit(mcSD[,2]), rw)))
  }
}


debugASCBRMC <- function(u,l,total,mu,x,j,z,zeta,dbgLvl=3) {
  if (dbgLvl > 2) {
    cat(" u=",u)
    cat(" l=",l)
    #cat(" total=",total)
    cat(" mu=",mu)
    #cat(" x=",x)
    cat(" j=",j)
    cat(" z-zeta=",z-zeta)
    cat("\n")
  }
}



CalcASCBRMonteCarlo<- function(
  a,b,err,zeta,
  title,
  est,
  calc,
  calcArgs,
  start = 11,
  limit = 1e+8,
  tol = 10^(-min(round(log10(abs(est))),3)),
  c = CI, #  confidence interval
  rw = 5,
  dbgLvl = 1
)
{

  u <- a
  l <- b
  total <- 0
  j <- 0
  z <- 0

  # base <- start - 1 - rw
  # runs <- base + 1
  #
  mcSD <-  matrix(ncol=2,nrow=0,dimnames = list(c(),c("Runs","v")))
  mcLCI <-matrix(ncol=2,nrow=0,dimnames = list(c(),c("Runs","v")))
  mcUCI <- matrix(ncol=2,nrow=0,dimnames = list(c(),c("Runs","v")))

  mcResults <- vector()
  result <- matrix(ncol=3,nrow=0,dimnames = list(c(),c("LCI","UCI","DIFF")))


  trend <- 1
  #while ((length(mcSD) < rw) | (trend > tol) & (runs < limit)) {
  while ((j <100) | ((z != zeta) & (j < limit))) {

    # if (length(mcSD) > rw) {
    #   trend <- sd(tail(na.omit(mcSD[,2]), rw))/mean(tail(na.omit(mcSD[,2]), rw))
    #   step <- round((1+log(trend/tol)) * (0.1^log10(tol)))
    # } else {
    #   trend <- 1
    #   step <- 1
    # }
    # runs <- min(runs + step,limit)
    #mc <- vector()
    j <- j + 1
    #mc <- do.call(CalcMonteCarlo, list(est,calc, calcArgs,j,c,mcResults))
    mcResults[length(result)+1] <- do.call(calc,calcArgs())
    x <-tail(mcResults,1)
    total <- total + x
    mu <- total / j
    if ((l<=x) & (x<=u)) { # sig = 0
      u <- u
      l <- l
      z <- z + 1
    } else { # sig = 1
      u <- mu + err
      l <- mu - err
      z <- 0
    }

    runs <- j
    step <- 1
    #result <- rbind(result, c(mc$value["LCI"],mc$value["UCI"],mc$value["UCI"]-mc$value["LCI"]))
    #mcSD <- rbind(mcSD,c(runs,sd(result[,"DIFF"])))
    #mcLCI <- rbind(mcLCI,c(runs,mean(result[,"LCI"])))
    #mcUCI <- rbind(mcUCI,c(runs,mean(result[,"UCI"])))
    if (((j %% 10000) == 0) | (z > 1)) {
      debugASCBRMC(u,l,total,mu,x,j,z,zeta,dbgLvl=3)
    }
    #infoStableMC(rw,result,mcResults,mcSD,runs,step,trend,dbgLvl)
  }
  o <-list(v=CalcMonteCarlo(est,calc, calcArgs,runs,c,mcResults),total_runs=runs,samples=mcSD,lci_samples=mcLCI,uci_samples=mcUCI)

  resultsStableMC(title,o,dbgLvl)

  return(o)
}
