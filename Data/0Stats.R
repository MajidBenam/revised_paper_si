#####  Summary Statistics
library(modelsummary)
library(gt)
#library(flextable)

{#### Get data
  load("./Data/TableData.Rdata")
  AggrDat <- AggrSCWarAgriRelig
  
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

  ### Construct MilTech and IronCav
  AggrDat$MilTech <- apply(subset(AggrDat, select = c(Metal, Project, Weapon, Animal, Armor, Defense)),1,sum)
  AggrDat$IronCav <- AggrDat$Iron + AggrDat$Cavalry
  rm( list = setdiff(ls(),c("AggrDat", "AggrSCWarAgriRelig") ) )

AggrDat <- AggrDat[AggrDat$Dupl == "n",]
AggrDat <- AggrDat[AggrDat$uniq == "y",]

AggrDat <- subset(AggrDat, select = c(Scale, Hier, Gov, Agri, AgriLag, Infra, Irrigation, Cap, Market,
                  Money, Info, Pop, Terr, Class, Grain, MSP, HS, MilTech, IronCav ))
AggrDat$AgriLag <- AggrDat$AgriLag/1000 ### AgriLag in millennia
}

#datasummary_skim(AggrDat)
#datasummary_skim(AggrDat, output = "table.docx")

