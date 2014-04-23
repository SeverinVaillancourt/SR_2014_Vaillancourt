SRmain <- read.csv(file = "sting_master.csv", header = TRUE)
SRres <- read.csv(file = "sting_residency.csv", header = TRUE)
SRioa <- read.csv(file = "sting_ioa.csv", header = TRUE)
SRquick <- read.csv(file = "Quick_Sting_master_n16_2014.csv", header = TRUE)
SRmain$fSurvey <- factor(SRmain$Survey)
SRmain$fID <- factor(SRmain$ID)

#Area vs. Count 
reg1Area <- lm(SRquick$Count ~ SRquick$Area)
reg1Area
plot(SRquick$Count ~ SRquick$Area)
abline(reg1Area)
summary(reg1Area)
#Area vs. Male Count
reg1AreaM <- lm(SRquick$CountM ~ SRquick$Area)
reg1AreaM
plot(SRquick$CountM ~ SRquick$Area)
abline(reg1AreaM)
summary(reg1AreaM)
#Area vs. Female Count
reg1AreaF <- lm(SRquick$CountF ~ SRquick$Area)
reg1AreaF
plot(SRquick$CountF ~ SRquick$Area)
abline(reg1AreaF)
summary(reg1AreaF)


#Coral Cover vs. Count 
reg1Coralcover <- lm(SRquick$Count ~ SRquick$Coralcover)
reg1Coralcover
plot(SRquick$Count ~ SRquick$Coralcover)
abline(reg1Coralcover)
summary(reg1Coralcover)
#Coral Cover vs. Male Count
reg1CoralcoverM <- lm(SRquick$CountM ~ SRquick$Coralcover)
reg1CoralcoverM
plot(SRquick$CountM ~ SRquick$Coralcover)
abline(reg1CoralcoverM)
summary(reg1CoralcoverM)
#Coral Cover vs. Female Count
reg1CoralcoverF <- lm(SRquick$CountF ~ SRquick$Coralcover)
reg1CoralcoverF
plot(SRquick$CountF ~ SRquick$Coralcover)
abline(reg1CoralcoverF)
summary(reg1CoralcoverF)

#Rugosity vs. Count 
reg1Rugosity <- lm(SRquick$Count ~ SRquick$Rugosity)
reg1Rugosity
plot(SRquick$Count ~ SRquick$Rugosity)
abline(reg1Rugosity)
summary(reg1Rugosity)
#Rugosity vs. Male Count
reg1RugosityM <- lm(SRquick$CountM ~ SRquick$Rugosity)
reg1RugosityM
plot(SRquick$CountM ~ SRquick$Rugosity)
abline(reg1RugosityM)
summary(reg1RugosityM)
#Rugosity vs. Female Count
reg1RugosityF <- lm(SRquick$CountF ~ SRquick$Rugosity)
reg1RugosityF
plot(SRquick$CountF ~ SRquick$Rugosity)
abline(reg1RugosityF)
summary(reg1RugosityF)

#Lionfish vs. Count 
reg1Lionfish <- lm(SRquick$Count ~ SRquick$Lionfish)
reg1Lionfish
plot(SRquick$Count ~ SRquick$Lionfish)
abline(reg1Lionfish)
summary(reg1Lionfish)
#Lionfish vs. Male Count
reg1LionfishM <- lm(SRquick$CountM ~ SRquick$Lionfish)
reg1LionfishM
plot(SRquick$CountM ~ SRquick$Lionfish)
abline(reg1LionfishM)
summary(reg1LionfishM)
#Lionfish vs. Female Count
reg1LionfishF <- lm(SRquick$CountF ~ SRquick$Lionfish)
reg1LionfishF
plot(SRquick$CountF ~ SRquick$Lionfish)
abline(reg1LionfishF)
summary(reg1LionfishF)

#SiteRes.score2 vs. Count 
reg1SiteRes.score <- lm(SRquick$Count ~ SRquick$SiteRes.score2)
reg1SiteRes.score
plot(SRquick$Count ~ SRquick$SiteRes.score2)
abline(reg1SiteRes.score)
summary(reg1SiteRes.score)

#Area vs. SiteRes.score2 
reg2Area <- lm(SRquick$SiteRes.score2  ~ SRquick$Area)
reg2Area
plot(SRquick$SiteRes.score2  ~ SRquick$Area)
abline(reg2Area)
summary(reg2Area)


#Coral Cover vs. SiteRes.score2  
reg2Coralcover <- lm(SRquick$SiteRes.score2  ~ SRquick$Coralcover)
reg2Coralcover
plot(SRquick$SiteRes.score2  ~ SRquick$Coralcover)
abline(reg2Coralcover)
summary(reg2Coralcover)

#Rugosity vs. SiteRes.score2  
reg2Rugosity <- lm(SRquick$SiteRes.score2  ~ SRquick$Rugosity)
reg2Rugosity
plot(SRquick$SiteRes.score2  ~ SRquick$Rugosity)
abline(reg2Rugosity)
summary(reg2Rugosity)

#Lionfish vs. SiteRes.score2 
reg2Lionfish <- lm(SRquick$SiteRes.score2  ~ SRquick$Lionfish)
reg2Lionfish
plot(SRquick$SiteRes.score2  ~ SRquick$Lionfish)
abline(reg2Lionfish)
summary(reg2Lionish)

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
()
reg1 <- lm(SRmain$TL ~ SRmain$IndResscore)
reg1
par(cex = .8)
plot(SRmain$Area, SRmain$IndResscore)
abline(reg1)

hist(SRres$HAS)asf
