#####  Non-parametric bootstrap: Hier
###    Model to be fitted is defined in line 23 and autoregressive terms to keep in line 34
nRep <- 20
nBoot <- 500
load("./Data/TableData.Rdata")
ImpSCDatRepl <- read.table('./Data/ImpSCDatRepl4.csv', sep=",", stringsAsFactors = 	FALSE, header=TRUE)
ImpClassRepl <- read.table('./Data/ImpClassRepl.csv', sep=",", stringsAsFactors = 	FALSE, header=TRUE)
out <- data.frame()

for(iRep in 1:nRep){
  {#### Form data matrix
    ImpSCDat <- ImpSCDatRepl[ImpSCDatRepl$n == iRep,]
    ImpClass <- ImpClassRepl[ImpClassRepl$n == iRep,]
    AggrDat <- cbind(ImpSCDat[,1:10], AggrSCWarAgriRelig[,13:ncol(AggrSCWarAgriRelig)])  ### Use imputed SC data
    AggrDat$Class <- ImpClass$Class
    AggrDat$Hier <- AggrSCWarAgriRelig$Hier                        ### Non-imputed for response variable
    AggrDat <- AggrDat[AggrDat$Dupl == "n",]
    AggrDat <- AggrDat[is.na(AggrDat$Agri) == FALSE,]  ### Drops Jomon before 10k BCE
    AggrDat$MilTech <- apply(subset(AggrDat, select = c(Metal, Project, Weapon, Animal, Armor)),1,sum)
    AggrDat$IronCav <- AggrDat$Iron + AggrDat$Cavalry
    
    ### Define the model 
    AggrDat <- subset(AggrDat, select = c(NGA, PolID, Time, Hier, Agri, AgriLag, MilTech, IronCav)) #******* define model here
    
    ### Form regression matrix
    TableDat <- AggrDat
    dpar <- 1000*1   ### d determines how rapidly geographic influence declines with distance
    source("./Data/fRegrDat.R")  
    NGARegrDat <- RegrDat
  }
  
  ################################################################
  {### Run Bootstrap
    NGARegrDat <- subset(NGARegrDat, select = -c(Space, Lag2, Phylogeny, T))  ### Autoregressive terms to remove *************
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
    #write.csv(out, file="BootResult_Hier.csv",  row.names=FALSE)
  }
}
write.csv(out, file="./outputs/2Boot_Hier_output_1.csv",  row.names=FALSE)

{#########   Results
  out <- read.table("./outputs/2Boot_Hier_output_1.csv", sep=",", header=TRUE, stringsAsFactors = 	FALSE)
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
  write.csv(BootSum, file="./outputs/2Boot_Hier_output_2.csv", row.names=FALSE)
  #write.table(BootSum, "clipboard", sep="\t", row.names=FALSE)
}  
