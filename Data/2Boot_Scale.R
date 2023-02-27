#####  Non-parametric bootstrap
###    Model to be fitted is defined in line 32 and autoregressive terms to keep in line 43
nRep <- 20
nBoot <- 500
load("./Data/TableData.Rdata")
ImpSCDatRepl <- read.table('./Data/ImpSCDatRepl0.csv', sep=",", stringsAsFactors = 	FALSE, header=TRUE)
ImpScaleRepl <- read.table('./Data/ImpScaleRepl.csv', sep=",", stringsAsFactors = 	FALSE, header=TRUE)
ImpClassRepl <- read.table('./Data/ImpClassRepl.csv', sep=",", stringsAsFactors = 	FALSE, header=TRUE)
out <- data.frame()

for(iRep in 1:nRep){
  {#### Form data matrix
    ImpSCDat <- ImpSCDatRepl[ImpSCDatRepl$n == iRep,]
    ImpScale <- ImpScaleRepl[ImpScaleRepl$n == iRep,]
    ImpClass <- ImpClassRepl[ImpClassRepl$n == iRep,]
    AggrDat <- cbind(ImpSCDat[,c(1:3,7:11)], AggrSCWarAgriRelig[,13:ncol(AggrSCWarAgriRelig)])  ### Use imputed NonScale data
    AggrDat$Class <- ImpClass$Class
    AggrDat$Pop <- ImpScale$Pop
    AggrDat$Terr <- ImpScale$Terr
    AggrDat$Cap <- ImpScale$Cap
    
    AggrDat <- AggrDat[AggrDat$Dupl == "n",]
    AggrDat <- AggrDat[is.na(AggrDat$Agri) == FALSE,]  ### Drops Jomon before 10k BCE
    AggrDat$MilTech <- apply(subset(AggrDat, select = c(Metal, Project, Weapon, Animal, Armor)),1,sum)
    AggrDat$IronCav <- AggrDat$Iron + AggrDat$Cavalry
    AggrDat <- AggrDat[!is.na(AggrDat$Pop),]
    
    reslt <- prcomp(subset(AggrDat, select = c(Pop, Terr, Cap)), scale=TRUE)
    AggrDat$Scale <- predict(reslt)[,1]
    
    ### Define the model 
    AggrDat <- subset(AggrDat, select = c(NGA, PolID, Time, Scale, Agri, AgriLag, MilTech, IronCav)) #******* define model here
    
    ### Form regression matrix
    TableDat <- AggrDat
    dpar <- 1000*1   ### d determines how rapidly geographic influence declines with distance
    source("./Data/fRegrDat.R")  
    NGARegrDat <- RegrDat
  }
  
  ################################################################
  {### Run Bootstrap
    NGARegrDat <- subset(NGARegrDat, select = -c(Space, Lag2, T, Phylogeny))  ### Autoregressive terms to remove *************
    NGAs <- unique(NGARegrDat$NGA)
    nNGA <- length(NGAs)
    nVar <- ncol(NGARegrDat) - 3
    
    for(iBoot in 1:nBoot){    
      RD <- data.frame()
      for(iNGA in 1:nNGA){
        iNGA <- sample(NGAs,1)
        dat <- NGARegrDat[NGARegrDat$NGA==iNGA,]
        RD <- rbind(RD, dat)
      }
      RD$NGA1 <- RD$NGA
      RD <- subset(RD, select = -c(NGA, PolID, Time) )
      fit <- lm(RD)
      out <- rbind(out, fit$coefficients[1:nVar])
      print(c(iRep, iBoot, nrow(RD)))
    }
    colnames(out) <- colnames(RD[1:nVar])
    # write.csv(out, file="BootResult.csv",  row.names=FALSE)
  }
}
write.csv(out, file="./outputs/2Boot_Scale_output_1.csv",  row.names=FALSE)

{#########   Results
  out <- read.table("./outputs/2Boot_Scale_output_1.csv", sep=",", header=TRUE, stringsAsFactors = 	FALSE)
  n <- nrow(out)
  BootSum <- data.frame(Var = colnames(out), P1 = NA, P2 = NA, Min=NA, Max=NA )
  for(i in 1:ncol(out)){
    BootSum$P1[i] <- sum(out[,i] < 0)/n
    BootSum$P2[i] <- sum(out[,i] > 0)/n
    stat <- out[,i]
    stat <- stat[order(stat)]
    BootSum$Min[i] <- stat[1+n*0.025]
    BootSum$Max[i] <- stat[n*0.975]
  }
  write.csv(BootSum, file="./outputs/2Boot_Scale_output_2.csv", row.names=FALSE)
  #write.table(BootSum, "clipboard", sep="\t", row.names=FALSE)
}  

#hist(out$Agri)

