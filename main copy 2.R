library(readxl)
library(car)
library(lmtest)
library(dplyr)



file_path = "/Users/bashkoro/Downloads/Fatalities.xls"
fatalities <- read_excel(file_path)
fatalities


#Korelasi
korelasi <- cor(fatalities)
korelasi

#Anova
anov <- aov(fatalities$Drivers~fatalities$Passengers)
anov
summary(anov)

#Regresi sederhana
fatalitiesReg <- lm(fatalities$`All Fatalities`~fatalities$Passengers, data = fatalities)
fatalitiesReg
summary(fatalitiesReg)


#regresi ganda
fatalitiesReg2 <- lm(fatalities$`All Fatalities`~fatalities$Drivers + fatalities$Passengers + fatalities$Pedestrians + fatalities$Motorcyclists + fatalities$Bicyclists + fatalities$`Other/Unknown`, data = fatalities)
fatalitiesReg2
summary(fatalitiesReg2)

shapiro.test(residuals(fatalitiesReg2))
bptest(fatalitiesReg2)
dwtest(fatalitiesReg2)
vif(fatalitiesReg2)

