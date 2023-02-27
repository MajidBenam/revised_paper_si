#######  Seemingly Unrelated Regressions
library("readxl")
library(systemfit)
SC_data <- read_excel("DynReg_results.xlsx", sheet = "MultiVar_data")
#SC_data <- read_excel("to_share.xlsx", sheet = "MultiVar_data")

#########################################################################
### Replicating results of best-fitting models (Table 1 in the main article)

### Define regression models
regrScale <- Scale_t1 ~ Scale + Scale.sq + Agri + AgriLag + IronCav
regrHier  <- Hier_t1 ~ Hier + Hier.sq + Agri + AgriLag + MilTech + IronCav
regrGov <- Gov_t1 ~ Gov + Gov.sq + Agri + MilTech + IronCav

### Run regressions for each response separately
fit <- systemfit(formula = regrScale, data=SC_data)
summary(fit)

fit <- systemfit(formula = regrHier, data=SC_data)
summary(fit)

fit <- systemfit(formula = regrGov, data=SC_data)
summary(fit)

### Now run the three regressions together, using SUR
fit <- systemfit(formula = list(regrScale,regrHier,regrGov), method = "SUR", data=SC_data)
summary(fit)