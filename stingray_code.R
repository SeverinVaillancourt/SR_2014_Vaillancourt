SRmain <- read.csv(file = "sting_master.csv", header = TRUE)
SRres <- read.csv(file = "sting_residency.csv", header = TRUE)
SRioa <- read.csv(file = "sting_ioa.csv", header = TRUE)
SRquick <- read.csv(file = "Quick_Sting_master_n16_2014.csv", header = TRUE)
SRmain$fSurvey <- factor(SRmain$Survey)
SRmain$fID <- factor(SRmain$ID)


#test 
SRmain1m <- SRmain[SRmain$Survey == 1 & SRmain$Sex == "m" , ]
M1 <- boxplot(HAS ~ Treatment, data = SRmain)
#test boxplot
boxplot(TL ~ fSurvey * Sex, data = SRmain)
#just females from SRmain
SRmFe <- subset(SRmain, SRmain$Sex == "f")
SRmFe
#just males from SRmain
SRmMa <- subset(SRmain, SRmain$Sex == "m")
SRmMa
library("plyr")

reg1 <- lm(SRmain$TL ~ SRmain$IndResscore)
reg1
par(cex = .8)
plot(SRmain$Area, SRmain$IndResscore)
abline(reg1)

hist(SRres$HAS)asf
