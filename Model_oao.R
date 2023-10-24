#direct medical costs:

df<-read.csv("NCD_RisC_Lancet_2017_BMI_age_standardised_country_adapted.csv")

#project obesity of indivual countries:
#combine male&female:
colnames(df)[8] <- "Obesity_Prevalence"
colnames(df) [23] <- "Overweight_Prevalence"
#aggregate function:

combined<-aggregate(cbind(Obesity_Prevalence, Overweight_Prevalence) ~ Country.Region.World + Year + ISO, df, mean)
combined["oao"]<-combined["Obesity_Prevalence"]+ combined["Overweight_Prevalence"]

#reduce to relevant countries:

countries<-c("EST", "FRA", "PER", "IND", "LVA", "COL", "IDN", "JPN", "LTU", "HUN", "SVN", "POL", "HRV", "RUS", "SVK", "CRI", "ROU", "CZE", "ZAF", "KOR", "CHE", "CHN", "NZL", "ISR", "AUT", "GBR", "LUX", "BGR", "ISL", "AUS", "CHL", "BRA", "MEX", "ITA", "IRL", "IRL", "SWE", "GRC", "DNK", "FIN", "BEL", "ESP", "NOR", "ARG", "PRT", "CYP", "MLT", "CAN", "DEU", "NLD", "TUR", "SAU", "USA")

result_countries<-subset(combined, ISO %in% countries)

#predict year 2019:
# Fit a linear regression model
model <- lm(oao ~ Year + ISO, data = result_countries)
# Create a data frame with all combinations of ISO and Year for 2019
new_data <- expand.grid(Year = 2019, ISO = unique(result_countries$ISO))
# Make predictions for 2019
predicted_data <- data.frame(new_data, oao = predict(model, newdata = new_data))

# Fit a linear regression model
model <- lm(oao ~ Year, data = result_countries)


# Predict the obesity prevalence for the years 2020 to 2050
years_to_predict <- data.frame(Year = 2020:2050)
predictions <- data.frame(
  Year = years_to_predict$Year,
  oao = predict(model, newdata = years_to_predict)
)


# Calculate the average obesity prevalence for the years 2020 to 2050
average_prevalence <- mean(predictions$oao)

average_prevalence

#prediction of every country:
projection <- data.frame()

for (country in unique(result_countries$Country.Region.World)){
  country_df<-result_countries[result_countries$Country.Region.World == country, ]
  
  model <- lm(oao ~ Year, data = country_df)
  years_to_predict <- data.frame(Year = 2020:2050)
  predictions <- data.frame(
    Year = years_to_predict$Year,
    oao = predict(model, newdata = years_to_predict)
  )
  
  # Calculate the average obesity prevalence for the years 2020 to 2050
  average_prevalence <- mean(predictions$oao)
  
  # Store the results in the 'results' data frame
  projection <- rbind(projection, data.frame(Country = country, Average_Prevalence = average_prevalence))
}

# Print the results
print(projection)
#expand the model/regress it so we can always plug in a number for obesity prevelance and then get oaf from that:
projection<-projection[order(projection$Average_Prevalence),]
print(projection)
projection["oafs"]=c(5.5834, 6.1217, 7.5428, 7.7690, 6.0866, 4.6819, 9.1332, 9.0287, 5.5180, 6.4475, 8.2022, 9.090, 6.906, 6.676, 11.337, 7.669, 4.878, 9.393, 10.065, 6.008, 8.536, 10.733, 6.449, 6.960, 6.022, 8.376, 8.691, 9.354, 9.743, 10.124, 9.714, 6.648, 7.063, 8.470, 6.246, 9.140, 6.195, 8.072, 9.033, 6.956, 7.534, 10.345, 8.613, 8.356, 9.910, 8.586, 8.934, 10.628, 7.920, 11.576, 13.531, 12.652)
projection

final<-lm(projection$Average_Prevalence~projection$oafs)
final
final$coefficients%*%c(1,13)
summary(final)

#should be correct:
final<-lm(projection$oafs~projection$Average_Prevalence)
final
final$coefficients%*%c(1,0.5)
summary(final)
#why is my coefficient different to the one in the paper?
plot(projection$oaf~projection$Average_Prevalence)
abline(final$coefficients)



#direct non-medical costs:
#travel costs:
gas<-read.csv("e2373858-926a-4755-8350-78bb920f6ee9_Data.csv")
View(gas)
gas <- gas[, c(1,2,3,4,50,51,52,53,54,55,56,57,58, 59, 60, 61)]
gas$gas2016<-gas$X2016..YR2016.
#replace missing values by mean:
gas$gas2016[gas$gas2016 == ".."] <- NA
m <- mean(as.numeric(gas$gas2016), na.rm = TRUE)
gas$gas2016[is.na(gas$gas2016)] <- m

#average outpatient visits per capitaoutpatient<-5.42
#gas price per l * km * l/km:
atc<-gas$gas2016*5*0.06
#average inpatient visits per capita
inpatient<-0.10

outpatient_travel_cost<-atc*outpatient*population
