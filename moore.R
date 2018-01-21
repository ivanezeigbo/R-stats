setwd('~/Downloads/')
datafile <- read.csv('benchmarks.txt')
set.seed(1234)
data <- datafile[which(datafile$benchName == '101.tomcatv'), ]
date <- strsplit(as.character(data$testID), '-')
date <- as.matrix(sapply(date, "[", 2))
colnames(date) <- "Date"
Date <- na.omit(cbind(data, date))
Date$base <- log10(Date$base)
plot(Date$Date, Date$base, main = "Semi-Log Plot to Prove Moore's Law", xlab = "Date", ylab = "Base")
fit <- lm(Date$base ~ Date$Date)
abline(fit, col = 'red')
