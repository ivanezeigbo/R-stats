setwd('~/Downloads/')
data <- read.csv('ANT_mass_changes_Watkins053116.csv')
plot(data$TIME..year.decimal., data$Greenland.mass..Gt., type = 'l', xlab = 'Time (Years)', ylab = 'Land Ice Mass (Gt)', main = 'Change in Greenland and Antarctica Ice Mass', col = 'dark green')
lines(data$TIME..year.decimal., data$Antarctica.mass..Gt., col = 'red')
legend(2011, 1500, c("Greenland", "Antarctica"), lty = c(1, 1), lwd = c(1.5, 1.5), col = c("dark green", "red"))
plot(data$TIME..year.decimal., data$Ocean.mass..mm., col = 'blue', xlab = 'Time (Years)', ylab = 'Ocean Mass (mm)', main = 'Change in Ocean Mass with Time', col.main = 'brown')

#Carbon Dioxide
new_data = read.table("co2_mm_mlo.txt")
copy_co2 = new_data[- which(new_data$V4 == -99.99), ]
plot(copy_co2$V3, copy_co2$V4, xlab = 'Time (Years)', ylab = 'Mole Fraction of CO2 (ppm)', main = 'Change in amount of CO2 with time', col = 'black')

#Global Temperature
globaltemp_data = read.table("647_Global_Temperature_Data_File.txt")
plot(globaltemp_data$V1, globaltemp_data$V2, xlab = 'Time (Years)', ylab = 'Temperature Anomaly (C)', main = 'Global Land-Ocean Temperature Index', col = 'blue')
lines(globaltemp_data$V1, globaltemp_data$V3, lwd = 2)

#Sea Level Change
sealevel_data = read.table("sealevel.txt")
plot(sealevel_data$V3, sealevel_data$V8, xlab = 'Time (Years)', ylab = 'Global Mean Sea Level (mm)', main = 'Global Land-Ocean Temperature Index', col = 'light blue', col.main = 'blue')

#Carbon Dioxide and Temperature
carbon_dat = read.table('co2_annmean_mlo.txt')
temp_dat = read.table("647_Global_Temperature_Data_File.txt")
copy_temp = temp_dat[- which(temp_dat$V1 < 1959), ]
plot(carbon_dat$V2, copy_temp$V3, type = 'l', xlab = 'Mole Fraction of Carbon Dioxide (ppm)', ylab = 'Temperature Anomaly (C)', main = 'Change in Global Temperature with CO2', col = 'red')

#Calculating Correlation of Carbon Dioxide Rise and Temperature
cor(carbon_dat$V2, copy_temp$V3)
