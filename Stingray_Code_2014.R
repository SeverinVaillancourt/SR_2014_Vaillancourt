setwd(dir="~/Documents/School//SP14/498//R_stingray_project_2014/")  
STRmain <- read.csv(file= "tidy_sting_master.csv", header = TRUE)
STRmain
names(STRmain)
str(STRmain)
STRres <- read.csv(file= "tidy_sting_residency.csv")
STRres
str(STRres)
