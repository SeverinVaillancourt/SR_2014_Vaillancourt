SRmain <- read.csv(file = "sting_master.csv", header = TRUE)
SRres <- read.csv(file = "sting_residency.csv", header = TRUE)
SRioa <- read.csv(file = "sting_ioa.csv", header = TRUE)
SRmain1m <- SRmain[SRmain$Survey == 1 & SRmain$Sex == "m" , ]
SRmain1m
M1 <- boxplot(TL ~ Sex, data = SRmain)
