#####  Dynamic Regressions with Agri as response variable
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
  reslt <- prcomp(subset(AggrDat, select = c(Pop, Terr, Cap, Hier)), scale=TRUE)
  AggrDat$Scale <- predict(reslt)[,1]
  RegrDat <- subset(AggrDat, select = c(Scale,Pop))
  res <- summary(fit <- lm(RegrDat))
  AggrDat$Scale <- (AggrDat$Scale - fit$coefficients[1])/fit$coefficients[2] ### SPC1 is scaled as log Polity Population
  
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

  ### Add Pastoralism
  dat <-read.table('./Data/Pastoralism.csv', sep=",", header=TRUE, stringsAsFactors = 	FALSE)
  AggrDat$Pastor <- dat$Pastor
  AggrDat$temp <- dat$PolID
  
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
  #TableDat <- subset(AggrDat, select = c(NGA, PolID, Time, Agri, AgriLag, Grain, Pastor, Scale, Gov, Infra,
  #                                       Info, Money, Class, MilTech, IronCav))
  TableDat <- subset(AggrDat, select = c(NGA, PolID, Time, Agri, AgriLag, Grain, Pastor, Scale, Gov, Infra,
                                         Info, Money, Class, MilTech))
  dpar <- 1000*1   ### d determines how rapidly geographic influence declines with distance
  source("./Data/fRegrDat.R")  
  NGARegrDat <- RegrDat
  rm( list = setdiff(ls(),c("NGARegrDat", "AggrDat") ) )
}

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
    write.csv(output, file="./outputs/1Anlz_Agri_output_1.csv",  row.names=FALSE)
    output <- output[output$delAIC < 2,]
    write.csv(output[1:min(100,nrow(output)),], file="./outputs/1Anlz_Agri_output_2.csv",  row.names=FALSE)
    #write.table(output[1:min(100,nrow(output)),], "clipboard", sep="\t", row.names=FALSE)
  }
}

#######  Regression coefficients
RD <- subset(RegrDat, select = -c(AgriLag, Scale, Infra, Money, MilTech))   ###  Drop terms not in the best-by-AIC model
summary(fit <- lm(RD))

### Check for Phylogeny: NS
RD1 <- cbind(RD, NGARegrDat$Phylogeny)
summary(fit <- lm(RD1))

### Check for Lag2: NS
RD1 <- cbind(RD, NGARegrDat$Lag2)
summary(fit <- lm(RD1))

### Check for Space: NS
RD1 <- cbind(RD, NGARegrDat$Space)
summary(fit <- lm(RD1))

### Check for T: P = 0.005; add Time.
RD1 <- cbind(RD, NGARegrDat$Time)
colnames(RD1)[ncol(RD1)] <- "Time"
summary(fit <- lm(RD1))
RD1 <- subset(RD1, select = -c(Info, Class))
summary(fit <- lm(RD1))
RD <- RD1

### Check for NGA fixed effects: results in worse AIC 
RD1 <- cbind(RD, NGARegrDat$NGA)
colnames(RD1)[ncol(RD1)] <- "NGA" 
summary(fit <- glm(RD1))
summary(fit <- glm(RD))

#####################################################################################################
#### Revert to the best-fitting model
{###  R2 = 0.957
  RegrDat <- RD
  summary(fit <- lm(RegrDat))
  out <- summary(fit <- glm(RegrDat))
  write.csv(out, file="./outputs/1Anlz_Agri_output_3.csv")
  #write.table(out$coefficients, "clipboard", sep="\t")
}

{### R2 without autocorrelation term; R2 = 0.672
  RD <- subset(RegrDat, select = -c(Agri, Agri.sq)) 
  print(summary(fit <- lm(RD)))
  out <- summary(fit <- glm(RD1))
  write.csv(out, file="./outputs/1Anlz_Agri_output_4.csv")
  #write.table(out$coefficients, "clipboard", sep="\t")
}

