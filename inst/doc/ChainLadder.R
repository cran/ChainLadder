### R code from vignette source 'ChainLadder.Rnw'

###################################################
### code chunk number 1: options
###################################################
options(prompt = "R> ", digits = 4, show.signif.stars = FALSE)


###################################################
### code chunk number 2: ChainLadder.Rnw:192-193 (eval = FALSE)
###################################################
## demo(package="ChainLadder")


###################################################
### code chunk number 3: ChainLadder.Rnw:196-198 (eval = FALSE)
###################################################
## library(ChainLadder)
## demo("demo name")


###################################################
### code chunk number 4: ChainLadder.Rnw:207-208 (eval = FALSE)
###################################################
## install.packages('ChainLadder') 


###################################################
### code chunk number 5: ChainLadder.Rnw:213-214
###################################################
library(ChainLadder)


###################################################
### code chunk number 6: ChainLadder.Rnw:216-217 (eval = FALSE)
###################################################
## library(ChainLadder)


###################################################
### code chunk number 7: ChainLadder.Rnw:219-220
###################################################
cat(ChainLadder:::chainladderWelcomeMessage())


###################################################
### code chunk number 8: ChainLadder.Rnw:236-238 (eval = FALSE)
###################################################
## require(ChainLadder)
## data(package="ChainLadder")


###################################################
### code chunk number 9: ChainLadder.Rnw:243-245
###################################################
## Sample triangle
RAA


###################################################
### code chunk number 10: ChainLadder.Rnw:272-273 (eval = FALSE)
###################################################
## plot(RAA)


###################################################
### code chunk number 11: TrianglePlot1
###################################################
plot(RAA)


###################################################
### code chunk number 12: ChainLadder.Rnw:292-293 (eval = FALSE)
###################################################
## plot(RAA, lattice=TRUE)


###################################################
### code chunk number 13: TrianglePlot2
###################################################
plot(RAA, lattice=TRUE)


###################################################
### code chunk number 14: ChainLadder.Rnw:313-314 (eval = FALSE)
###################################################
## ?plot.triangle


###################################################
### code chunk number 15: ChainLadder.Rnw:322-328
###################################################
raa.inc <- cum2incr(RAA)
## Show first origin period and its incremental development
raa.inc[1,]
raa.cum <- incr2cum(raa.inc)
## Show first origin period and its cumulative development
raa.cum[1,]


###################################################
### code chunk number 16: ChainLadder.Rnw:338-339 (eval = FALSE)
###################################################
## demo(DatabaseExamples)


###################################################
### code chunk number 17: ChainLadder.Rnw:354-360
###################################################
filename <-  file.path(system.file("Database", 
                                   package="ChainLadder"), 
                       "TestData.csv")
myData <- read.csv(filename)
head(myData)
summary(myData)


###################################################
### code chunk number 18: ChainLadder.Rnw:363-365
###################################################
raa <- subset(myData, lob %in% "RAA")
head(raa)


###################################################
### code chunk number 19: ChainLadder.Rnw:371-376
###################################################
raa.tri <- as.triangle(raa, 
                       origin="origin", 
                       dev="dev", 
                       value="value")
raa.tri


###################################################
### code chunk number 20: ChainLadder.Rnw:383-385
###################################################
raa.df <- as.data.frame(raa.tri, na.rm=TRUE)
head(raa.df)


###################################################
### code chunk number 21: ChainLadder.Rnw:404-405 (eval = FALSE)
###################################################
## x <- read.table(file="clipboard", sep="\t", na.strings="")


###################################################
### code chunk number 22: ChainLadder.Rnw:410-411 (eval = FALSE)
###################################################
## write.table(RAA, file="clipboard", sep="\t", na="")


###################################################
### code chunk number 23: ChainLadder.Rnw:431-438
###################################################
n <- 10
f <- sapply(1:(n-1), 
             function(i){
               sum(RAA[c(1:(n-i)),i+1])/sum(RAA[c(1:(n-i)),i])
             }
             )
f


###################################################
### code chunk number 24: ChainLadder.Rnw:444-453
###################################################
dev.period <- 1:(n-1)
plot(log(f-1) ~ dev.period, main="Log-linear extrapolation of age-to-age factors")
tail.model <- lm(log(f-1) ~ dev.period) 
abline(tail.model)
co <- coef(tail.model)
## extrapolate another 100 dev. period
tail <- exp(co[1] + c((n + 1):(n + 100)) * co[2]) + 1
f.tail <- prod(tail)
f.tail      


###################################################
### code chunk number 25: ChainLadder.Rnw:458-461
###################################################
plot(100*(rev(1/cumprod(rev(c(f, tail[tail>1.0001]))))), t="b",
     main="Expected claims development pattern",
     xlab="Dev. period", ylab="Development % of ultimate loss") 


###################################################
### code chunk number 26: ChainLadder.Rnw:470-476
###################################################
f <- c(f, f.tail)
fullRAA <- cbind(RAA, Ult = rep(0, 10))
for(k in 1:n){ 
  fullRAA[(n-k+1):n, k+1] <- fullRAA[(n-k+1):n,k]*f[k]
}
round(fullRAA)


###################################################
### code chunk number 27: ChainLadder.Rnw:480-481
###################################################
sum(fullRAA[ ,11] - getLatestCumulative(RAA))


###################################################
### code chunk number 28: ChainLadder.Rnw:504-520
###################################################
linkratios <- c(attr(ata(RAA), "vwtd"), tail = 1.05)
round(linkratios, 3) # display to only three decimal places
LDF <- rev(cumprod(rev(linkratios)))
names(LDF) <- colnames(RAA) # so the display matches the triangle
round(LDF, 3)
currentEval <- getLatestCumulative(RAA)
# Reverse the LDFs so the first, least mature factor [1] 
#	is applied to the last origin year (1990)
EstdUlt <- currentEval * rev(LDF) #
# Start with the body of the exhibit
Exhibit <- data.frame(currentEval, LDF = round(rev(LDF), 3), EstdUlt)
# Tack on a Total row
Exhibit <- rbind(Exhibit, 
data.frame(currentEval=sum(currentEval), LDF=NA, EstdUlt=sum(EstdUlt),
           row.names = "Total"))
Exhibit


###################################################
### code chunk number 29: ChainLadder.Rnw:529-534
###################################################
lmCL <- function(i, Triangle){
  lm(y~x+0, weights=1/Triangle[,i],
     data=data.frame(x=Triangle[,i], y=Triangle[,i+1]))
} 
sapply(lapply(c(1:(n-1)), lmCL, RAA), coef)


###################################################
### code chunk number 30: ChainLadder.Rnw:574-576
###################################################
mack <- MackChainLadder(RAA, est.sigma="Mack")
mack


###################################################
### code chunk number 31: ChainLadder.Rnw:579-581
###################################################
mack$f
mack$FullTriangle


###################################################
### code chunk number 32: MackPlot1
###################################################
plot(mack)


###################################################
### code chunk number 33: MackPlot2
###################################################
plot(mack, lattice=TRUE)


###################################################
### code chunk number 34: ChainLadder.Rnw:616-620
###################################################
## See also the example in section 8 of England & Verrall (2002) 
## on page 55.
B <- BootChainLadder(RAA, R=999, process.distr="gamma")
B


###################################################
### code chunk number 35: ChainLadder.Rnw:624-625
###################################################
plot(B)


###################################################
### code chunk number 36: ChainLadder.Rnw:631-632
###################################################
quantile(B, c(0.75,0.95,0.99, 0.995))


###################################################
### code chunk number 37: ChainLadder.Rnw:636-644
###################################################
## fit a distribution to the IBNR
library(MASS)
plot(ecdf(B$IBNR.Totals))
## fit a log-normal distribution 
fit <- fitdistr(B$IBNR.Totals[B$IBNR.Totals>0], "lognormal")
fit
curve(plnorm(x,fit$estimate["meanlog"], fit$estimate["sdlog"]),
      col="red", add=TRUE)  


###################################################
### code chunk number 38: ChainLadder.Rnw:651-662
###################################################
MCLpaid
MCLincurred
op <- par(mfrow=c(1,2))
plot(MCLpaid)
plot(MCLincurred)
par(op)

# Following the example in Quarg's (2004) paper:
MCL <- MunichChainLadder(MCLpaid, MCLincurred, est.sigmaP=0.1, est.sigmaI=0.1)
MCL
plot(MCL)


###################################################
### code chunk number 39: ChainLadder.Rnw:720-721
###################################################
str(liab)


###################################################
### code chunk number 40: ChainLadder.Rnw:724-726
###################################################
liab2 <- as(liab, "triangles")
class(liab2)


###################################################
### code chunk number 41: ChainLadder.Rnw:729-730 (eval = FALSE)
###################################################
## showMethods(classes = "triangles")


###################################################
### code chunk number 42: ChainLadder.Rnw:734-736
###################################################
# use drop = TRUE to remove rows that are all NA's
liab2[, 12:14, drop = TRUE]


###################################################
### code chunk number 43: ChainLadder.Rnw:739-740
###################################################
cbind2(liab2[1:3, 12])


###################################################
### code chunk number 44: ChainLadder.Rnw:753-755
###################################################
fit1 <- MultiChainLadder(liab, fit.method = "OLS")
lapply(summary(fit1)$report.summary, "[", 15, )


###################################################
### code chunk number 45: ChainLadder.Rnw:769-772
###################################################
fit <- lapply(liab, MackChainLadder, est.sigma = "Mack")
# the same as the first triangle above
lapply(fit, function(x) t(summary(x)$Totals)) 


###################################################
### code chunk number 46: ChainLadder.Rnw:784-786
###################################################
(B1 <- MultiChainLadder(list(GenIns), fit.method = "OLS", 
    mse.method = "Independence"))


###################################################
### code chunk number 47: ChainLadder.Rnw:795-797
###################################################
fit2 <- MultiChainLadder(liab, fit.method = "SUR")
lapply(summary(fit2)$report.summary, "[", 15, )


###################################################
### code chunk number 48: ChainLadder.Rnw:804-805
###################################################
round(unlist(residCor(fit2)), 3)


###################################################
### code chunk number 49: ChainLadder.Rnw:812-813
###################################################
do.call("rbind", coef(fit2))


###################################################
### code chunk number 50: ChainLadder.Rnw:821-825 (eval = FALSE)
###################################################
## parold <- par(mfrow = c(4, 2), mar = c(4, 4, 2, 1), 
##     mgp = c(1.3, 0.3, 0), tck = -0.02)
## plot(fit2, which.triangle = 1:2, which.plot = 1:4)
## par(parold)


###################################################
### code chunk number 51: MultiPlot
###################################################
parold <- par(mfrow = c(4, 2), mar = c(4, 4, 2, 1), 
    mgp = c(1.3, 0.3, 0), tck = -0.02)
plot(fit2, which.triangle = 1:2, which.plot = 1:4)
par(parold)


###################################################
### code chunk number 52: ChainLadder.Rnw:887-890
###################################################
W1 <- MultiChainLadder2(liab, mse.method = "Independence", 
      	control = systemfit.control(methodResidCov = "Theil"))
lapply(summary(W1)$report.summary, "[", 15, )


###################################################
### code chunk number 53: ChainLadder.Rnw:896-903
###################################################
for (i in 1:5){
  W2 <- MultiChainLadder2(liab, mse.method = "Independence", 
      control = systemfit.control(methodResidCov = "Theil", maxiter = i))
  print(format(summary(W2)@report.summary[[3]][15, 4:5], 
          digits = 6, big.mark = ","))    
}
lapply(summary(W2)$report.summary, "[", 15, )


###################################################
### code chunk number 54: ChainLadder.Rnw:922-923
###################################################
str(auto)


###################################################
### code chunk number 55: ChainLadder.Rnw:927-931
###################################################
f0 <- MultiChainLadder2(auto, type = "MCL")
# show correlation- the last three columns have zero correlation
# because separate chain ladders are used
print(do.call(cbind, residCor(f0)), digits = 3)


###################################################
### code chunk number 56: ChainLadder.Rnw:940-941
###################################################
f1 <- MultiChainLadder2(auto, type = "MCL+int")


###################################################
### code chunk number 57: Multi_resid
###################################################
parold <- par(mfrow = c(2, 3), mar = c(3, 3, 2, 1))
mt <- list(c("Personal Paid", "Personal Incured", "Commercial Paid"))
plot(f0, which.plot = 3, main = mt)
plot(f1, which.plot = 3, main = mt)
par(parold)


###################################################
### code chunk number 58: ChainLadder.Rnw:967-968
###################################################
lapply(summary(f1, portfolio = "1+3")@report.summary, "[", 11, )


###################################################
### code chunk number 59: ChainLadder.Rnw:977-979
###################################################
ult <- summary(f1)$Ultimate
print(ult[, 1] /ult[, 2], 3)


###################################################
### code chunk number 60: ChainLadder.Rnw:990-1006
###################################################
da <- auto[1:2]
# MCL with diagonal development
M0 <- MultiChainLadder(da)
# non-diagonal development matrix with no intercepts
M1 <- MultiChainLadder2(da, type = "GMCL-int")
# Munich Chain Ladder
M2 <- MunichChainLadder(da[[1]], da[[2]])
# compile results and compare projected paid to incured ratios
r1 <- lapply(list(M0, M1), function(x){
          ult <- summary(x)@Ultimate
          ult[, 1] / ult[, 2]
      })
names(r1) <- c("MCL", "GMCL")
r2 <- summary(M2)[[1]][, 6]
r2 <- c(r2, summary(M2)[[2]][2, 3])
print(do.call(cbind, c(r1, list(MuCl = r2))) * 100, digits = 4)


###################################################
### code chunk number 61: ChainLadder.Rnw:1058-1059
###################################################
ClarkLDF(RAA)


###################################################
### code chunk number 62: ChainLadder.Rnw:1069-1070
###################################################
ClarkLDF(RAA, maxage = 20)


###################################################
### code chunk number 63: ChainLadder.Rnw:1074-1075
###################################################
ClarkLDF(RAA, G="weibull")


###################################################
### code chunk number 64: LDFweibull
###################################################
plot(ClarkLDF(RAA, G="weibull"))


###################################################
### code chunk number 65: ChainLadder.Rnw:1103-1104
###################################################
ClarkCapeCod(RAA, Premium = 40000, G = "weibull")


###################################################
### code chunk number 66: CapeCod
###################################################
plot(ClarkCapeCod(RAA, Premium = 40000, G = "weibull"))


###################################################
### code chunk number 67: ChainLadder.Rnw:1161-1166
###################################################
# load data 
data(GenIns)
GenIns <- GenIns / 1000
# fit Poisson GLM
(fit1 <- glmReserve(GenIns))


###################################################
### code chunk number 68: ChainLadder.Rnw:1170-1171
###################################################
summary(fit1, type = "model")


###################################################
### code chunk number 69: ChainLadder.Rnw:1176-1180
###################################################
# Gamma GLM
(fit2 <- glmReserve(GenIns, var.power = 2))
# compound Poisson GLM (variance function estimated from the data):
#(fit3 <- glmReserve(GenIns, var.power = NULL))


###################################################
### code chunk number 70: ChainLadder.Rnw:1188-1190
###################################################
set.seed(11)
(fit5 <- glmReserve(GenIns, mse.method = "boot"))


###################################################
### code chunk number 71: ChainLadder.Rnw:1197-1198
###################################################
names(fit5)


###################################################
### code chunk number 72: ChainLadder.Rnw:1203-1207
###################################################
pr <- as.data.frame(fit5$sims.reserve.pred)
qv <- c(0.025, 0.25, 0.5, 0.75, 0.975)
res.q <- t(apply(pr, 2, quantile, qv))
print(format(round(res.q), big.mark = ","), quote = FALSE)


###################################################
### code chunk number 73: ChainLadder.Rnw:1213-1222 (eval = FALSE)
###################################################
## library(ggplot2)
## library(reshape2)
## prm <- melt(pr)
## names(prm) <- c("year", "reserve")
## gg <- ggplot(prm, aes(reserve))
## gg <- gg + geom_density(aes(fill = year), alpha = 0.3) + 
##         facet_wrap(~year, nrow = 2, scales = "free")  + 
##          theme(legend.position = "none") 
## print(gg)


###################################################
### code chunk number 74: ChainLadder.Rnw:1239-1240 (eval = FALSE)
###################################################
## system.file("Excel", package="ChainLadder") 


###################################################
### code chunk number 75: ChainLadder.Rnw:1248-1249 (eval = FALSE)
###################################################
## system.file("SWord", package="ChainLadder") 


###################################################
### code chunk number 76: ChainLadder.Rnw:1254-1255 (eval = FALSE)
###################################################
## demo(package="ChainLadder")


