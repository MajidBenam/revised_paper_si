#####  Comparing model performance with Functional versus warfare predictors
{#### Scale = response
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
  
  ### Construct NonScale
  reslt <- prcomp( subset(AggrDat, select = c(Gov, Infra, Info, Money)), scale=TRUE)
  AggrDat$NonScale <- predict(reslt)[,1] + 3 ### Scale to min = 0
  
  ### Construct MilTech
  AggrDat$MilTech <- apply(subset(AggrDat, select = c(Metal, Project, Weapon, Animal, Armor, Defense)),1,sum)
  AggrDat$IronCav <- AggrDat$Iron + AggrDat$Cavalry
  
  ### Form regression matrix
  TableDat <- subset(AggrDat, select = c(NGA, PolID, Time, Scale, Agri, AgriLag, Infra, Money,
                                      Info, NonScale, MilTech, IronCav))
  dpar <- 1000*1   ### d determines how rapidly geographic influence declines with distance
  source("./Data/fRegrDat.R")  
  NGARegrDat <- RegrDat
  rm( list = setdiff(ls(),c("NGARegrDat", "AggrDat") ) )

  ### Only agri and warfare terms
  RegrDat <- subset(NGARegrDat, select = -c(NGA, PolID, Time, Space, T, Lag2, Phylogeny,
                                            Infra, Money, Info, NonScale) ) 
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
    output <- output[order(output$delAIC),]
    write.csv(output, file="./outputs/4Comp_Func_War_1.csv",  row.names=FALSE)
  }
  
  ### Only agri and functional terms
  RegrDat <- subset(NGARegrDat, select = -c(NGA, PolID, Time, Space, T, Lag2, Phylogeny,
                                            MilTech, IronCav) ) 
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
    output <- output[order(output$delAIC),]
    write.csv(output, file="./outputs/4Comp_Func_War_2.csv",  row.names=FALSE)
  }
  #### Best functional model
  RegrDat <- subset(NGARegrDat, select = -c(NGA, PolID, Time, Space, T, Lag2, Phylogeny,
                                            Info, NonScale, MilTech, IronCav) ) 
  print(summary(fit <- glm(RegrDat)))
}

######################################################################################
######################################################################################
{#### Hier = response
  load("./Data/TableData.Rdata")
  AggrDat <- ImpSCDat
  
  ### Construct Scale
  AggrDat$Scale <- NA
  dat <- subset(AggrDat, select = c(Pop, Terr, Cap))
  reslt <- prcomp(dat, scale=TRUE)
  AggrDat$Scale <- predict(reslt)[,1]
  
  ### Add predictors
  AggrDat <- cbind(AggrDat, subset(AggrSCWarAgriRelig, select = c(Metal,Project,Weapon,Animal,Armor,Defense,
                                                                  Cavalry,Iron,Agri,AgriLag,Grain,MSP,HS)))
  AggrDat <- AggrDat[AggrDat$Dupl == "n",]
  AggrDat <- AggrDat[is.na(AggrDat$Agri) == FALSE,]  ### This also drops Jomon before 10k BCE
  
  ### Construct NonScale
  reslt <- prcomp( subset(AggrDat, select = c(Gov, Infra, Info, Money)), scale=TRUE)
  AggrDat$NonScale <- predict(reslt)[,1] + 3 ### Scale to min = 0
  
  ### Construct MilTech
  AggrDat$MilTech <- apply(subset(AggrDat, select = c(Metal, Project, Weapon, Animal, Armor, Defense)),1,sum)
  AggrDat$IronCav <- AggrDat$Iron + AggrDat$Cavalry
  
  ### Form regression matrix
  TableDat <- subset(AggrDat, select = c(NGA, PolID, Time, Hier, Agri, AgriLag, Infra, Money,
                                         Info, NonScale, MilTech, IronCav))
  dpar <- 1000*1   ### d determines how rapidly geographic influence declines with distance
  source("./Data/fRegrDat.R")  
  NGARegrDat <- RegrDat
  rm( list = setdiff(ls(),c("NGARegrDat", "AggrDat") ) )
  
  ### Only agri and warfare terms
  RegrDat <- subset(NGARegrDat, select = -c(NGA, PolID, Time, Space, T, Lag2, Phylogeny,
                                            Infra, Money, Info, NonScale) ) 
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
    output <- output[order(output$delAIC),]
    write.csv(output, file="./outputs/4Comp_Func_War_3.csv",  row.names=FALSE)
  }
  
  ### Only agri and functional terms
  RegrDat <- subset(NGARegrDat, select = -c(NGA, PolID, Time, Space, T, Lag2, Phylogeny,
                                            MilTech, IronCav) ) 
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
    output <- output[order(output$delAIC),]
    write.csv(output, file="./outputs/4Comp_Func_War_4.csv",  row.names=FALSE)
  }
  #### Best functional model
  RegrDat <- subset(NGARegrDat, select = -c(NGA, PolID, Time, Space, T, Lag2, Phylogeny,
                                            Infra, MilTech, IronCav) ) 
  print(summary(fit <- glm(RegrDat)))
}

######################################################################################
######################################################################################
{#### Gov = response
  load("./Data/TableData.Rdata")
  AggrDat <- ImpSCDat
  
  ### Construct Scale
  AggrDat$Scale <- NA
  dat <- subset(AggrDat, select = c(Pop, Terr, Cap))
  reslt <- prcomp(dat, scale=TRUE)
  AggrDat$Scale <- predict(reslt)[,1]
  
  ### Add predictors
  AggrDat <- cbind(AggrDat, subset(AggrSCWarAgriRelig, select = c(Metal,Project,Weapon,Animal,Armor,Defense,
                                                                  Cavalry,Iron,Agri,AgriLag,Grain,MSP,HS)))
  AggrDat <- AggrDat[AggrDat$Dupl == "n",]
  AggrDat <- AggrDat[is.na(AggrDat$Agri) == FALSE,]  ### This also drops Jomon before 10k BCE
  
  ### Construct NonScale
  reslt <- prcomp( subset(AggrDat, select = c(Infra, Info, Money)), scale=TRUE)
  AggrDat$NonScale <- predict(reslt)[,1] + 3 ### Scale to min = 0
  
  ### Construct MilTech
  AggrDat$MilTech <- apply(subset(AggrDat, select = c(Metal, Project, Weapon, Animal, Armor, Defense)),1,sum)
  AggrDat$IronCav <- AggrDat$Iron + AggrDat$Cavalry
  
  ### Form regression matrix
  TableDat <- subset(AggrDat, select = c(NGA, PolID, Time, Gov, Agri, AgriLag, Infra, Money,
                                         Info, NonScale, MilTech, IronCav))
  dpar <- 1000*1   ### d determines how rapidly geographic influence declines with distance
  source("./Data/fRegrDat.R")  
  NGARegrDat <- RegrDat
  rm( list = setdiff(ls(),c("NGARegrDat", "AggrDat") ) )
  
  ### Only agri and warfare terms
  RegrDat <- subset(NGARegrDat, select = -c(NGA, PolID, Time, Space, T, Lag2, Phylogeny,
                                            Infra, Money, Info, NonScale) ) 
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
    output <- output[order(output$delAIC),]
    write.csv(output, file="./outputs/4Comp_Func_War_5.csv",  row.names=FALSE)
  }
  
  ### Only agri and functional terms
  RegrDat <- subset(NGARegrDat, select = -c(NGA, PolID, Time, Space, T, Lag2, Phylogeny,
                                            NonScale, MilTech, IronCav) ) 
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
    output <- output[order(output$delAIC),]
    write.csv(output, file="./outputs/4Comp_Func_War_6.csv",  row.names=FALSE)
  }
  #### Best functional model
  RegrDat <- subset(NGARegrDat, select = -c(NGA, PolID, Time, Space, T, Lag2, Phylogeny,
                                            Infra, NonScale, MilTech, IronCav) ) 
  print(summary(fit <- glm(RegrDat)))
}

######### Done
