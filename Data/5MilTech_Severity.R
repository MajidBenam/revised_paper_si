#####  Correlation between War Consquence variables and MilTech
{#### Get data
  load("./Data/TableData.Rdata")
  load("./Data/PolsVars.Rdata")
  AggrDat <- subset(AggrSCWarAgriRelig, select = c(Metal, Project, Weapon, Animal, Armor, Defense, Dupl, uniq))
  AggrDat$MilTech <- apply(subset(AggrDat, select = c(Metal, Project, Weapon, Animal, Armor, Defense)),1,sum)
  AggrDat <- AggrDat[AggrDat$Dupl == "n" & AggrDat$uniq == "y",]
  
  ### Get WarIntensity variables
  Vars <- variables[variables$Category == "WarIntensity" | variables$Category == "ArchWar",]
  WI <- TSDat[TSDat$uniq == "y" & TSDat$Dupl == "n",c(1:3, 3+Vars$VarNumber)]
  WI$Intens <- NA
  WI$coded <- NA
  
  for(i in 1:nrow(WI)){
    WI$coded[i] <- sum(WI[i,4:20] != -999) 
    dt <- WI[i, 4:20]
    dt <- dt[dt != -999]
    if(length(dt) > 0){
      WI$Intens[i] <- mean(dt)
    }
  }
  ### Histogram of the number 
  hist(WI$coded)
  
  {### Correlation between coded and Intens
    RD <- subset(WI, select = c(Intens, coded))
    print( res <- summary(fit <- lm(RD)) )
    plot(RD[2:1], pch=16)
    abline(fit, lty=2, lwd=2)
    legend("topleft", paste("R-sq = ", round(res$r.squared, digits=2) ), bty="n")
  }
  
  gdat <- RD[,2:1]
  smoothScatter(gdat, nbin = 128, bandwidth = c(0.5,0.1), nrpoints = 0,
                colramp = colorRampPalette(c("blue","green","yellow","orange", "red")),
                xlab=colnames(gdat)[1], ylab=colnames(gdat)[2])
  points(gdat, pch=16, col="brown", cex=.5)
  
  WI$MilTech <- AggrDat$MilTech
  WIDat <- subset(WI, select = c(Intens, MilTech, coded))
  rm( list = setdiff(ls(),c("WI","WIDat", "Vars") ) )
}

{#### Regressions
  out <- data.frame()
  nn <- data.frame()
  for(iVar in 1:nrow(Vars)){
    gdat <- data.frame(MilTech = WI$MilTech, RespVar = WI[,colnames(WI) == Vars$ShortName[iVar]])
    gdat <- gdat[gdat$RespVar != -999,]
    gdat$RespVar <- round(gdat$RespVar)
    res <- summary(fit <- glm(gdat$RespVar ~ gdat$MilTech, family=binomial(link='logit')))
    nn <- rbind(nn, nrow(gdat))
    out <- rbind(out, res$coefficients[2,])
  }
  out <- cbind(Vars$ShortName,out, nn)  
  colnames(out) <- c("Response Variable", "Estimate", "Std. Error", "z value",  "Pr(>|z|)", "nn" )
  print(out)
  write.csv(out, file="./outputs/5MilTech_Severity_output_1.csv", row.names=FALSE)
  #write.table(out, "clipboard", sep="\t", row.names=FALSE)
}

{### Correlations of Intens and Miltech
  cutoff <- 9
  RD <- subset(WIDat, select = c(Intens, MilTech))
  RD <- RD[WIDat$coded >= cutoff,]
  print( res <- summary(fit <- lm(RD)) )
  plot(RD[2:1], pch=16)
  abline(fit, lty=2, lwd=2)
  legend("topleft", paste("R-sq = ", round(res$r.squared, digits=2) ), bty="n")
}  
{    
  gdat <- RD[,2:1]
  smoothScatter(gdat, nbin = 128, bandwidth = c(2,0.1), nrpoints = 0,
                  colramp = colorRampPalette(c("blue","green","yellow","orange", "red")),
                  xlab=colnames(gdat)[1], ylab=colnames(gdat)[2])
  points(gdat, pch=16, col="brown", cex=.5)
    
  abline(fit, lty=2, lwd=2, col="brown")
  legend("topleft", paste("R-sq = ", round(res$r.squared, digits=2) ), bty="n")
  text(x=0, y=0.9, paste("n = ", nrow(RD) ), pos=4)
} 

{### Correlations of Intens and Miltech, as a function of missingness
  out <- data.frame()
  for(cutoff in 1:17){
    RD <- subset(WIDat, select = c(Intens, MilTech))
    RD <- RD[WIDat$coded >= cutoff,]
    res <- summary(fit <- lm(RD))
    out <- rbind(out, c(res$coefficients[2,], res$r.squared, nrow(RD)))
  }
  colnames(out) <- c("Estimate","SE", "t", "P", "R.sq", "n")
  print(out)
  write.csv(out, file="./outputs/5MilTech_Severity_output_2.csv", row.names=FALSE)
  #write.table(out, "clipboard", sep="\t", row.names=FALSE)
}  

{### Figure S7: Intens and Miltech
  cutoff <- 9
  RD <- subset(WIDat, select = c(Intens, MilTech))
  RD <- RD[WIDat$coded >= cutoff,]
  print( res <- summary(fit <- lm(RD)) )
  plot(RD[2:1], pch=16)
  abline(fit, lty=2, lwd=2)
  
  cutoff <- 15
  RD <- subset(WIDat, select = c(Intens, MilTech))
  RD <- RD[WIDat$coded >= cutoff,]
  print( res <- summary(fit <- lm(RD)) )
  points(RD[2:1], pch=16, col="red")
  abline(fit, lty=2, lwd=2, col="red")
}  


a=WI[WI$coded >= cutoff,]
unique(a$NGA)

