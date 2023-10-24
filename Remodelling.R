oao<-read.csv("NCD_RisC_Lancet_2017_BMI_age_standardised_world_adapted.csv")
getwd()
setwd(C:\Users\Admin\OneDrive\BBE\5th semeester\Thesis)

men <- subset(oao, oao$Sex == "Men")
plot(men$Year, men$Mean.BMI)
?plot
View(men)
model<-lm(men$Mean.BMI ~ men$Year)
abline(model)
coefficients(model)%*%c(1,2050)
