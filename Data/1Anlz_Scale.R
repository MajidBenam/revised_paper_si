#####  Dynamic Regressions with Scale as response variable
{#### Get data
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
  ### Add binary vars
  AggrDat$Irrigation <- TSDat$Irrigation
  AggrDat$Irrigation[AggrDat$Irrigation == -999] <- NA
  AggrDat$Market <- TSDat$Market
  AggrDat$Market[AggrDat$Market == -999] <- NA
  
  ### Add Class (average of 20 imputations)
  ImpClassRepl <- read.table('./Data/ImpClassRepl.csv', sep=",", header=TRUE, stringsAsFactors = 	FALSE)
  ImpClass <- ImpClassRepl[ImpClassRepl$n == 1,]
  for(i in 1:nrow(ImpClass)){
    ImpClass$Class[i] <- mean(ImpClassRepl$Class[i + 1494*0:19])
  }
  AggrDat$Class <- ImpClass$Class
  
  AggrDat <- AggrDat[AggrDat$Dupl == "n",]
  AggrDat <- AggrDat[is.na(AggrDat$Agri) == FALSE,]  ### This also drops Jomon before 10k BCE
  
  ### Construct NonScale
  reslt <- prcomp( subset(AggrDat, select = c(Gov, Infra, Info, Money)), scale=TRUE)
  AggrDat$NonScale <- predict(reslt)[,1] + 3 ### Scale to min = 0
  
  ### Construct MilTech
  AggrDat$MilTech <- apply(subset(AggrDat, select = c(Metal, Project, Weapon, Animal, Armor, Defense)),1,sum)
  
  AggrDat$IronCav <- AggrDat$Iron + AggrDat$Cavalry
  
  AggrDat$Agri_MilTech <- AggrDat$Agri * AggrDat$MilTech         ### Nonlinear terms
  AggrDat$Agri.sq <- AggrDat$Agri^2
  AggrDat$MilTech.sq <- AggrDat$MilTech^2
  
  ### Form regression matrix
  TableDat <- subset(AggrDat, select = c(NGA, PolID, Time, Scale, Agri, AgriLag, Infra, Money,
                                         Info, Class, Hier, Grain, MilTech, IronCav, MSP, HS))
  
  dpar <- 1000*1   ### d determines how rapidly geographic influence declines with distance
  source("./Data/fRegrDat.R")  
  NGARegrDat <- RegrDat
  rm( list = setdiff(ls(),c("NGARegrDat", "AggrDat") ) )
}
write.csv(NGARegrDat, file="temp.csv",  row.names=FALSE)

######################################################################
{########   
  RegrDat <- subset(NGARegrDat, select = -c(NGA, PolID, Time) )
  summary(fit <- lm(RegrDat))
  
  RegrDat <- subset(NGARegrDat, select = -c(NGA, PolID, Time, Space, T, Lag2, Phylogeny) ) ## Drop non-sign autocorr terms
  RegrDat <- RegrDat[complete.cases(RegrDat),]
  summary(fit <- lm(RegrDat))
  
  {####  Exhaustive regressions with these terms
    print(paste("Response variable =",colnames(RegrDat)[1]))
    Predictors <- 3:ncol(RegrDat)
    output <- data.frame()
    for (nPred in 1:length(Predictors)){ print(nPred)
      Preds<- combn(Predictors, nPred)
      for(j in 1:length(Preds[1,])){
        fit <- lm(RegrDat[, c(1:2, Preds[,j])])
        Pval <- summary(fit)$coefficients[,4]
        tval <- summary(fit)$coefficients[,3]
        out <- vector("numeric",length = length(RegrDat))
        out[c(1:2,Preds[,j])] <- tval
        out <- c(out,summary(fit)$r.sq)
        fit <- glm(RegrDat[, c(1:2, Preds[,j])])
        AIC <- summary(fit)$aic
        n <- length(fit$residuals)
        p <- length(fit$coefficients)  
        out <- c(out,AIC,n,p)
        output <- rbind(output,out)
      }
    }
    colnames(output) <- c(colnames(RegrDat),"R-sq","delAIC","n","p")
    output$delAIC <- output$delAIC - min(output$delAIC)
    output <- output[order(output$delAIC),]
    write.csv(output, file="./outputs/1Anlz_Scale_output_1.csv",  row.names=FALSE)
    output <- output[output$delAIC < 2,]
    write.csv(output[1:min(100,nrow(output)),], file="./outputs/1Anlz_Scale_output_2.csv",  row.names=FALSE)
    #write.table(output[1:min(100,nrow(output)),], "clipboard", sep="\t", row.names=FALSE)
  }
}

#######  Regression coefficients
RD <- subset(RegrDat, select = -c(Infra, Money, Info, Hier, Grain, HS))   ###  Drop terms not in the best-by-AIC model
summary(fit <- lm(RD))

RD <- subset(RegrDat, select = -c(Infra, Money, Info, Hier, Grain, HS, Class))   ###  Drop Class
summary(fit <- lm(RD))

#####################################################################################################
#### Check for the remaining predictors (those that substantially reduce the number of observations)

{### Irrigation: NS
  TableDat <- subset(AggrDat, select = c(NGA, PolID, Time, Scale, Agri, AgriLag, MilTech, IronCav, MSP, Irrigation))
  dpar <- 1000*1; source("./Data/fRegrDat.R")  
  NGARegrDat <- RegrDat
  RegrDat <- subset(NGARegrDat, select = -c(NGA, PolID, Time, Space, T, Lag2, Phylogeny) ) 
  summary(fit <- lm(RegrDat))
}

{### Market: NS, negative
  TableDat <- subset(AggrDat, select = c(NGA, PolID, Time, Scale, Agri, AgriLag, MilTech, IronCav, MSP, Market))
  dpar <- 1000*1; source("./Data/fRegrDat.R")  
  NGARegrDat <- RegrDat
  RegrDat <- subset(NGARegrDat, select = -c(NGA, PolID, Time, Space, T, Lag2, Phylogeny) )  
  summary(fit <- lm(RegrDat))
}

#####################################################################################################
### Check for autocorrelation terms
{### Rerun the current model
  TableDat <- subset(AggrDat, select = c(NGA, PolID, Time, Scale, Agri, AgriLag, MilTech, IronCav, MSP))
  dpar <- 1000*1; source("./Data/fRegrDat.R")  
  NGARegrDat <- RegrDat
  RegrDat <- subset(NGARegrDat, select = -c(NGA, PolID, Time, Space, T, Lag2, Phylogeny) )  
  summary(fit <- lm(RegrDat))
  RD <- RegrDat
}

### Check for Phylogeny: NS
RD1 <- cbind(RD, NGARegrDat$Phylogeny)
summary(fit <- lm(RD1))

### Check for Space: NS
RD1 <- cbind(RD, NGARegrDat$Space)
summary(fit <- lm(RD1))

### Check for Lag2: NS
RD1 <- cbind(RD, NGARegrDat$Lag2)
summary(fit <- lm(RD1))

### Check for T: NS
RD1 <- cbind(RD, NGARegrDat$Time)
summary(fit <- lm(RD1))

### Check for NGA fixed effects: delAIC = 346.6 - 344.49 = 2.11; MilTech P = 0.13
RD1 <- cbind(RD, NGARegrDat$NGA)
colnames(RD1)[ncol(RD1)] <- "NGA" 
summary(fit <- glm(RD1))
summary(fit <- glm(RD))
# Class seems to be already out at this stage
#RD1 <- subset(RD1, select = -Class)   ### Take out Class
summary(fit <- glm(RD1))   
RD <- RD1

#####################################################################################################
#### Check for nonlinear terms
{### Agri.sq: NS
  TableDat <- subset(AggrDat, select = c(NGA, PolID, Time, Scale, Agri, AgriLag, MilTech, IronCav, MSP, Agri.sq))
  dpar <- 1000*1; source("./Data/fRegrDat.R")  
  NGARegrDat <- RegrDat
  RegrDat <- subset(NGARegrDat, select = -c(NGA, PolID, Time, Space, T, Lag2, Phylogeny) ) 
  summary(fit <- lm(RegrDat))
}

{### MilTech.sq: NS
  TableDat <- subset(AggrDat, select = c(NGA, PolID, Time, Scale, Agri, AgriLag, MilTech, IronCav, MSP, MilTech.sq))
  dpar <- 1000*1; source("./Data/fRegrDat.R")  
  NGARegrDat <- RegrDat
  RegrDat <- subset(NGARegrDat, select = -c(NGA, PolID, Time, Space, T, Lag2, Phylogeny) ) 
  summary(fit <- lm(RegrDat))
}

{### Agri_MilTech: NS
  TableDat <- subset(AggrDat, select = c(NGA, PolID, Time, Scale, Agri, AgriLag, MilTech, IronCav, MSP, Agri_MilTech))
  dpar <- 1000*1; source("./Data/fRegrDat.R")  
  NGARegrDat <- RegrDat
  RegrDat <- subset(NGARegrDat, select = -c(NGA, PolID, Time, Space, T, Lag2, Phylogeny) ) 
  summary(fit <- lm(RegrDat))
}

#####################################################################################################
#### Revert to the best-supported model
{###  R2 = 0.923
  TableDat <- subset(AggrDat, select = c(NGA, PolID, Time, Scale, Agri, AgriLag, IronCav, MSP))
  dpar <- 1000*1; source("./Data/fRegrDat.R")  
  NGARegrDat <- RegrDat
  RegrDat <- subset(NGARegrDat, select = -c(NGA, PolID, Time, Space, T, Lag2, Phylogeny) )
  RegrDat <- cbind(RegrDat, NGARegrDat$NGA)
  colnames(RegrDat)[ncol(RegrDat)] <- "NGA" 
  print(summary(fit <- lm(RegrDat)))
  out <- summary(fit <- glm(RegrDat))
  write.csv(out$coefficients, file="./outputs/1Anlz_Scale_output_3.csv")
  #write.table(out$coefficients, "clipboard", sep="\t")
}

{### R2 without autocorrelation term; R2 = 0.575
  RD <- subset(RegrDat, select = -c(Scale, Scale.sq, NGA)) 
  print(summary(fit <- lm(RD)))
  out <- summary(fit <- glm(RD))
  write.csv(out$coefficients, file="./outputs/1Anlz_Scale_output_4.csv")

  #write.table(out$coefficients, "clipboard", sep="\t")
}

########################################################################################
{###  Diagnostics
summary(fit <- lm(RegrDat))
layout(matrix(c(1,2,3,4),2,2)) # 4 graphs/page
plot(fit)
layout(matrix(1),1,1)
# Normality of Residuals
library(MASS)
sresid <- studres(fit)
hist(sresid, breaks = seq(-10, 12, by=.25), freq=FALSE, main="Distribution of Studentized Residuals")
xfit<-seq(-10, 12, by=.25)
yfit<-dnorm(xfit)
lines(xfit, yfit) 
rm(sresid,xfit,yfit)}
####
######################################################################### END





