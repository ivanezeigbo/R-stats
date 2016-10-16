library(Matching)
library(foreign)
library(rbounds)
x <- read.dta("basic.dta")
#covariates
x <- na.omit(x) #Leaving  out, specifically, the missing notwot values
set.seed(14432)
#x$region <- as.numeric(x$region) #Perhaps their region affects their believes too. So converting the numbers on the region to numeric/integer values for matching
treat <- x$anygirls
#Did not pick x$perf because x$female should be more important in my reasoning #x$aauw, x$rtl, x$rgroup, x$statalph
Xi <- cbind(x$female, x$white, x$repub, x$age, I(x$age ^2), x$srvlng, I(x$srvlng^2),  x$demvote)
BalanceMat <- cbind(x$female, x$white, x$repub, x$age, I(x$age ^2), x$srvlng, I(x$srvlng^2), x$demvote)
treat <- x$anygirls
Y <- x$nowtot
genout <- GenMatch(Tr=treat, X=Xi, BalanceMatrix=BalanceMat, estimand="ATT", M=1,
                   pop.size=500, max.generations=100, wait.generations=15, cluster = c("localhost", "localhost"))
mout <- Match(Y=Y, Tr=treat, X=Xi, estimand="ATT", Weight.matrix=genout)
summary(mout)
summary(X)
#Check Balance
mb <- MatchBalance(treat~x$female + x$white + x$repub + x$age + I(x$age ^2) + x$srvlng + I(x$srvlng^2) + x$demvote,
                   match.out=mout, nboots=500)
#Run linear regression
#basemod <- lm(nowtot~x$female + x$white + x$repub + x$age + I(x$age ^2) + x$srvlng + I(x$srvlng^2) + x$demvote, x)
parcoord(BalanceMat, col = c(1, 2), lty = 1, var.label = TRUE)

par(mfrow = c(1,2))
plot(density(rnorm(1000)))
hist(rnorm(1000))
plot(density(x$age[treat==1], bw = 2), lwd = 3, col = "red")
lines(density(x$age[treat==0], bw = 2), lwd = 3, col = "blue")
plot(density(x$demvote[treat==1], bw = 2), lwd = 3, col = "red")
lines(density(x$demvote[treat==0], bw = 2), lwd = 3, col = "blue")
#After
plot(density(x$age[mout$index.treated], bw = 2), lwd = 3, col = "red")
lines(density(x$age[mout$index.control], bw = 2), lwd = 3, col = "blue")
plot(density(x$demvote[mout$index.treated], bw = 2), lwd = 3, col = "red")
lines(density(x$demvote[mout$index.control], bw = 2), lwd = 3, col = "blue")
