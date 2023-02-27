#####  Empirically-based macroevolution model for Scale
{#### Get data for regression
  load("./Data/TableData.Rdata")
  AggrDat <- ImpSCDat
  
  ### Calculate averaged ImpScale
  ImpScaleRepl <- read.table('./Data/ImpScaleRepl.csv', sep=",", header=TRUE, stringsAsFactors = 	FALSE)
  ImpScale <- ImpScaleRepl[ImpScaleRepl$n == 1,]
  for(j in 4:6){
    for(i in 1:nrow(ImpScale)){
      ImpScale[i,j] <- mean(ImpScaleRepl[i + 1494*0:19,j])
    }
  }
  
  ### Construct Scale (scaled)
  AggrDat$Scale <- NA
  dat <- subset(ImpScale, select = c(Pop, Terr, Cap))
  dat <- dat[complete.cases(dat),]
  reslt <- prcomp(dat, scale=TRUE)
  dat$Scale <- predict(reslt)[,1]
  RegrDat <- subset(dat, select = c(Scale,Pop))
  res <- summary(fit <- lm(RegrDat))
  dat$Scale <- (dat$Scale - fit$coefficients[1])/fit$coefficients[2] ### SPC1 is scaled as log Polity Population
  AggrDat$Scale[!is.na(ImpScale$Pop)] <- dat$Scale
  
  ### Add predictors
  AggrDat <- cbind(AggrDat, subset(AggrSCWarAgriRelig, select = c(Metal,Project,Weapon,Animal,Armor,Defense,
                                                                  Cavalry,Iron,Agri,AgriLag,Grain,MSP,HS)))
  AggrDat <- AggrDat[AggrDat$Dupl == "n",]
  AggrDat <- AggrDat[is.na(AggrDat$Agri) == FALSE,]  ### This also drops Jomon before 10k BCE
  
  ### Construct MilTech
  AggrDat$MilTech <- apply(subset(AggrDat, select = c(Metal, Project, Weapon, Animal, Armor, Defense)),1,sum)
  AggrDat$IronCav <- AggrDat$Iron + AggrDat$Cavalry
  
  ### Form regression matrix
  TableDat <- subset(AggrDat, select = c(NGA, PolID, Time, Scale, Agri, AgriLag, IronCav))
  
  dpar <- 1000*1   ### d determines how rapidly geographic influence declines with distance
  source("./Data/fRegrDat.R")  
  NGARegrDat <- NonStRegrDat   ### Don't standardize variables to mean=0 and SD=1. 
  rm( list = setdiff(ls(),c("NGARegrDat", "AggrDat") ) )
}

######################################################################
{########  Estimate parameters of the regression model
  RegrDat <- subset(NGARegrDat, select = -c(NGA, PolID, Time, Space, T, Lag2, Phylogeny) ) ## Drop non-sign autocorr terms
  RegrDat <- RegrDat[complete.cases(RegrDat),]
  print(summary(fit <- lm(RegrDat)))
  out <- summary(fit <- glm(RegrDat))
  write.csv(out$coefficients, file="./outputs/4DynModel_output.csv")
  #write.table(out$coefficients, "clipboard", sep="\t")
  p <- fit$coefficients
  SD <- sd(fit$residuals)
}  

##############################  For Figure S4
{#### Dynamics without noise
  t_max <- 60
  plot(x=c(0, t_max), y=c(3, 8), ty="n", xlab="Time (centuries)", ylab="Social Scale")
  abline(v=seq(0, 200, by=10),  h=seq(0,10,by=1), col="grey")
  
  State <- data.frame(time=1:t_max, Scale=3, Agri=0.5, AgriLag=1:t_max, IronCav=c(rep(0, t_max/2),rep(2, t_max/2)))
  for(t in 2:t_max){
    State$Scale[t] <- p[1] + p[2]*State$Scale[t-1] + p[3]*State$Scale[t-1]^2 + p[4]*State$Agri[t-1] +
      p[5]*100*State$AgriLag[t-1] + p[6]*State$IronCav[t-1]
  }
  lines(subset(State, select = c(time, Scale)), lwd=2, lty=1, col="blue")
  
  for(i in 1:10){
    State <- data.frame(time=1:t_max, Scale=3, Agri=0.5, AgriLag=1:t_max, IronCav=c(rep(0, t_max/2),rep(2, t_max/2)))
    for(t in 2:t_max){
      State$Scale[t] <- p[1] + p[2]*State$Scale[t-1] + p[3]*State$Scale[t-1]^2 + p[4]*State$Agri[t-1] +
        p[5]*100*State$AgriLag[t-1] + p[6]*State$IronCav[t-1] + rnorm(1,0, SD)
    }
    lines(subset(State, select = c(time, Scale)), lwd=1, lty=2, col="blue")
  }
  abline(v=t_max/2, lty=2, lwd=2)
  text(t_max/2,8, "IronCav", col="black", pos=2, srt=90)
}

##########  Compare maximum Scale in the data to that predicted by the model
{##### (Figure 4)
par(mfrow=c(1,2))
###########################################################
{#### Plot empirical Scale
  n_emp <- 3  ### Number of empires
  dat <- data.frame(t=seq(-3500, 1500, 100), Emp1=NA, Emp2=NA, Emp3=NA, 
                    Emp4=NA, Emp5=NA, Emp6=NA, meanEmp=NA, lower=NA, upper=NA)
  
  for(i in 1:nrow(dat)){
    dt <- AggrDat$Scale[AggrDat$Time == dat$t[i]]
    dt <- dt[!is.na(dt)]
    dt <- dt[order(dt, decreasing=T)]
    dat[i, 2:7] <- dt[1:6]
    #print(length(dt))
    dat$meanEmp[i] <- mean(dt[1:n_emp])
    dat$lower[i] <- mean(dt[1:n_emp]) - 1*sd(dt[1:n_emp])
    dat$upper[i] <- mean(dt[1:n_emp]) + 1*sd(dt[1:n_emp])
  }
  
  plot(x=c(dat$t[c(1,nrow(dat))]), y=c(4.7, 8.3), ty="n", xlab="Time, BCE/CE", ylab="Social Scale", yaxt="n")
  axis(2, at=seq(0,10, by=1))
  abline(v=seq(-5000, 2000, by=1000),  h=seq(0,10,by=1), col="grey")
  # for(j in 2:4){    lines(dat[,c(1,j)], lwd=1, col="blue")    }
  
  polygon(c(dat$t,rev(dat$t)),c(dat$lower,rev(dat$upper)),col="lightblue",border=NA)
  lines(dat$t, dat$meanEmp, lwd=3, col="blue")
  #lines(dat$t, dat$lower, lwd=2, lty=2, col="blue")
  #lines(dat$t, dat$upper, lwd=2, lty=2, col="blue")
  
  
  abline(v=-1000, lty=2, lwd=2)
  text(-1000,8.3, "IronCav", col="black", pos=2, srt=90)
  #text(-3500,8.1, "(b)", col="black", pos=1)
}


########################################################
{##### Simulate dynamics with noise
  n <- 35  ### Number of modeled trajectories corresponds to the number of NGAs
  n_emp <- 3   ### Number of largest states (same as in data)
  t_max <- 60  ### The overall length of simulation is 60 centuries
  
  #### Run 100 times to get at average dynamics and variation around it
  dat_res <- matrix(nrow = t_max, ncol = 3*100)
  for(iter in 1:100){
    res <- matrix(nrow=t_max, ncol=n)
    for(i in 1:n){
      State <- data.frame(time=1:t_max, Scale=3, Agri=0.5, AgriLag=1:t_max, IronCav=c(rep(0, t_max/2),rep(2, t_max/2)))
      for(t in 2:t_max){
        State$Scale[t] <- p[1] + p[2]*State$Scale[t-1] + p[3]*State$Scale[t-1]^2 + p[4]*State$Agri[t-1] +
          p[5]*100*State$AgriLag[t-1] + p[6]*State$IronCav[t-1] + 1*rnorm(1,0, SD)
      }
      res[,i] <- State$Scale
    }
    
    dat <- data.frame(t=State$time, Emp1=NA, Emp2=NA, Emp3=NA, 
                      Emp4=NA, Emp5=NA, Emp6=NA, meanEmp=NA, lower=NA, upper=NA)
    
    for(i in 1:nrow(dat)){
      dt <- res[i,]
      dt <- dt[order(dt, decreasing=T)]
      dat[i, 2:7] <- dt[1:6]
      dat$meanEmp[i] <- mean(dt[1:n_emp])
      dat$lower[i] <- mean(dt[1:n_emp]) - 1*sd(dt[1:n_emp])
      dat$upper[i] <- mean(dt[1:n_emp]) + 1*sd(dt[1:n_emp])
    }
  dat_res[,(iter*3 -c(2,1,0))] <- as.matrix(dat[,2:4]) 
  }
  gdat <- data.frame(t=1:t_max)
  gdat$mean <- apply(dat_res, 1, mean)
  gdat$sd <- apply(dat_res, 1, sd)
  gdat$lower <- gdat$mean - gdat$sd
  gdat$upper <- gdat$mean + gdat$sd
  
  plot(x=c(0, t_max), y=c(4.7, 8.3), ty="n", xlab="Time (centuries)", ylab="Social Scale", yaxt="n")
  axis(2, at=seq(0,10, by=1))
  abline(v=seq(0, 200, by=10),  h=seq(0,10,by=1), col="grey")
  polygon(c(gdat$t,rev(gdat$t)),c(gdat$lower,rev(gdat$upper)),col="lightblue",border=NA)
  lines(gdat$t, gdat$mean, lwd=3, col="blue")
  abline(v=t_max/2, lty=2, lwd=2)
  text(t_max/2,8.3, "IronCav", col="black", pos=2, srt=90)
}
par(mfrow=c(1,1))
}
