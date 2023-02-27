#####  Calculate time lags between onset of Agri and MacroState

dat <- read.table("./Data/FirstAppearance.csv", sep=",", header=TRUE, stringsAsFactors = FALSE)
dat <- dat[dat$Agri < 1000,]

dat$Lag <- dat$FirstYear10 - dat$Agri
dat$Lag[dat$FirstYear10 >= 1500] <- 9000
hist(dat$Lag, breaks = seq(-2500, 10000, by=1000), main="", xlab="Lag in years")
text(8700, 2, "No Macrostates before 1500", pos=4, srt=90)
sum(dat$Lag > 8000)
sum(dat$Lag > 8000)/nrow(dat)

