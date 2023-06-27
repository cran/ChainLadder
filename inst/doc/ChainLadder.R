## ----options, echo=FALSE------------------------------------------------------
library(knitr)
library(lattice)
options(prompt = "R> ", digits = 4, show.signif.stars = TRUE)
options(continue="   ")
opts_chunk$set(
  comment = NA,
  message = FALSE,
  warning = FALSE,
  fig.asp = 0.8,
  fig.width = 6,
  out.width = "60%",
  fig.align = "center"
)
suppressPackageStartupMessages(library(ChainLadder))

## ---- echo=FALSE--------------------------------------------------------------
print(citation("ChainLadder"), bibtex=FALSE)

## ----eval=FALSE---------------------------------------------------------------
#  demo(package="ChainLadder")

## ----eval=FALSE---------------------------------------------------------------
#  install.packages('ChainLadder')

## ----eval=FALSE---------------------------------------------------------------
#  library(ChainLadder)
#  data(package="ChainLadder")

## ----RAAdata------------------------------------------------------------------
RAA

## ----RAAplot, fig.cap = "Claims development chart of the RAA triangle, with one line per origin period."----
plot(RAA/1000,  main = "Claims development by origin year")

## ----RAAplot2, fig.cap = "Claims development chart of the RAA triangle, with individual panels for each origin period"----
plot(RAA/1000, lattice=TRUE, main = "Claims development by origin year")

## -----------------------------------------------------------------------------
raa.inc <- cum2incr(RAA)
## Show first origin period and its incremental development
raa.inc[1,]
raa.cum <- incr2cum(raa.inc)
## Show first origin period and its cumulative development
raa.cum[1,]

## ----ExcelTriangle, echo=FALSE, fig.cap="Screen shot of a triangle in a spreadsheet software."----
knitr::include_graphics("SpreadsheetTriangle.png")

## ----eval=FALSE---------------------------------------------------------------
#  myCSVfile <- "path/to/folder/with/triangle.csv"
#  ## Use the R command:
#  # myCSVfile <- file.choose() to select the file interactively
#  tri <- read.csv(file=myCSVfile, header = FALSE)
#  ## Use read.csv2 if semicolons are used as a separator likely
#  ## to be the case if you are in continental Europe
#  library(ChainLadder)
#  ## Convert to triangle
#  tri <- as.triangle(as.matrix(tri))
#  # Job done.

## ----eval=FALSE---------------------------------------------------------------
#  tri <- read.table(file="clipboard", sep="\t", na.strings="")

## ----eval=FALSE---------------------------------------------------------------
#  demo(DatabaseExamples)

## -----------------------------------------------------------------------------
filename <-  file.path(system.file("Database",
                                   package="ChainLadder"),
                       "TestData.csv")
myData <- read.csv(filename)
head(myData)
summary(myData)

## -----------------------------------------------------------------------------
raa <- subset(myData, lob %in% "RAA")
head(raa)

## -----------------------------------------------------------------------------
raa.tri <- as.triangle(raa,
                       origin="origin",
                       dev="dev",
                       value="value")
raa.tri

## -----------------------------------------------------------------------------
raa.df <- as.data.frame(raa.tri, na.rm=TRUE)
head(raa.df)

## ----database, fig.cap="Flow chart of data between R and data bases", echo=FALSE----
knitr::include_graphics("RandDatabases.png")

## -----------------------------------------------------------------------------
as.triangle(matrix(c(100, 150, 175, 180, 200,
                     110, 168, 192, 205, NA,
                     115, 169, 202, NA,  NA,
                     125, 185, NA,  NA,  NA,
                     150, NA,  NA,  NA,  NA),
                   nrow = 5, byrow = TRUE))

## -----------------------------------------------------------------------------
triangle(c(100, 150, 175, 180, 200),
         c(110, 168, 192, 205),
         c(115, 169, 202),
         c(125, 185),
         150)

## -----------------------------------------------------------------------------
# Calculate age-to-age factors for RAA triangle
n <- 10
f <- sapply(1:(n-1),
            function(i){
              sum(RAA[c(1:(n-i)),i+1])/sum(RAA[c(1:(n-i)),i])
            }
)
f

## ----fig=TRUE-----------------------------------------------------------------
dev.period <- 1:(n-1)
plot(log(f-1) ~ dev.period, 
     main="Log-linear extrapolation of age-to-age factors")
tail.model <- lm(log(f-1) ~ dev.period)
abline(tail.model)
co <- coef(tail.model)
## extrapolate another 100 dev. period
tail <- exp(co[1] + c(n:(n + 100)) * co[2]) + 1
f.tail <- prod(tail)
f.tail

## ----fig=TRUE-----------------------------------------------------------------
plot(100*(rev(1/cumprod(rev(c(f, tail[tail>1.0001]))))), t="b",
     main="Expected claims development pattern",
     xlab="Dev. period", ylab="Development % of ultimate loss")

## -----------------------------------------------------------------------------
f <- c(f, f.tail)
fullRAA <- cbind(RAA, Ult = rep(0, 10))
for(k in 1:n){
  fullRAA[(n-k+1):n, k+1] <- fullRAA[(n-k+1):n,k]*f[k]
}
round(fullRAA)

## -----------------------------------------------------------------------------
sum(fullRAA[ ,11] - getLatestCumulative(RAA))

## -----------------------------------------------------------------------------
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

## -----------------------------------------------------------------------------
lmCL <- function(i, Triangle){
  lm(y~x+0, weights=1/Triangle[,i],
     data=data.frame(x=Triangle[,i], y=Triangle[,i+1]))
}
sapply(lapply(c(1:(n-1)), lmCL, RAA), coef)

## -----------------------------------------------------------------------------
mack <- MackChainLadder(RAA, est.sigma="Mack")
mack # same as summary(mack) 

## -----------------------------------------------------------------------------
mack$f
mack$FullTriangle

## -----------------------------------------------------------------------------
mack_smmry <- summary(mack) # See also ?summary.MackChainLadder
mack_smmry$ByOrigin
mack_smmry$Totals

## ----fig=TRUE, label=MackPlot1, fig.asp=1.5, fig.cap="Some residual show clear trends, indicating that the Mack assumptions are not well met"----
plot(mack)

## ----fig=TRUE, label=MackPlot2, fig.asp=1.1-----------------------------------
plot(mack, lattice=TRUE)

## -----------------------------------------------------------------------------
calPeriods <- (row(RAA) + col(RAA) - 1)
(weights <- ifelse(calPeriods <= 5, 0, ifelse(calPeriods > 10, NA, 1)))
MackChainLadder(RAA, weights=weights, est.sigma = "Mack")

## ----fig=TRUE, fig.width = 6.5------------------------------------------------
MCLpaid
MCLincurred
par(mfrow=c(1,2))
plot(MCLpaid)
plot(MCLincurred)
par(mfrow=c(1,1))

## -----------------------------------------------------------------------------
# Following the example in Quarg's (2004) paper:
MCL <- MunichChainLadder(MCLpaid, MCLincurred, est.sigmaP=0.1, est.sigmaI=0.1)
MCL

## ----fig=TRUE, fig.asp=1.5----------------------------------------------------
plot(MCL)

## -----------------------------------------------------------------------------
## See also the example in section 8 of England & Verrall (2002)
## on page 55.
B <- BootChainLadder(RAA, R=999, process.distr="gamma")
B

## ----fig=TRUE, fig.asp=1.5----------------------------------------------------
plot(B)

## -----------------------------------------------------------------------------
quantile(B, c(0.75,0.95,0.99, 0.995))

## ----fig=TRUE-----------------------------------------------------------------
## fit a distribution to the IBNR
library(MASS)
plot(ecdf(B$IBNR.Totals))
## fit a log-normal distribution
fit <- fitdistr(B$IBNR.Totals[B$IBNR.Totals>0], "lognormal")
fit
curve(plnorm(x,fit$estimate["meanlog"], fit$estimate["sdlog"]),
      col="red", add=TRUE)

## -----------------------------------------------------------------------------
str(liab)

## -----------------------------------------------------------------------------
liab2 <- as(liab, "triangles")
class(liab2)

## ----eval = FALSE-------------------------------------------------------------
#  showMethods(classes = "triangles")

## -----------------------------------------------------------------------------
# use drop = TRUE to remove rows that are all NA's
liab2[, 12:14, drop = TRUE]

## -----------------------------------------------------------------------------
cbind2(liab2[1:3, 12])

## -----------------------------------------------------------------------------
fit1 <- MultiChainLadder(liab, fit.method = "OLS")
lapply(summary(fit1)$report.summary, "[", 15, )

## -----------------------------------------------------------------------------
fit <- lapply(liab, MackChainLadder, est.sigma = "Mack")
# the same as the first triangle above
lapply(fit, function(x) t(summary(x)$Totals))

## -----------------------------------------------------------------------------
(B1 <- MultiChainLadder(list(GenIns), fit.method = "OLS",
    mse.method = "Independence"))

## -----------------------------------------------------------------------------
fit2 <- MultiChainLadder(liab, fit.method = "SUR")
lapply(summary(fit2)$report.summary, "[", 15, )

## -----------------------------------------------------------------------------
round(unlist(residCor(fit2)), 3)

## -----------------------------------------------------------------------------
do.call("rbind", coef(fit2))

## ----multi, fig = TRUE, echo = FALSE, fig.asp = 1.5, fig.cap="Summary and diagnostic plots from a `MultiChainLadder` object"----
parold <- par(mfrow = c(4, 2), mar = c(4, 4, 2, 1),
    mgp = c(1.3, 0.3, 0), tck = -0.02)
plot(fit2, which.triangle = 1:2, which.plot = 1:4)
par(parold)

## -----------------------------------------------------------------------------
require(systemfit)
W1 <- MultiChainLadder2(liab, mse.method = "Independence",
      	control = systemfit.control(methodResidCov = "Theil"))
lapply(summary(W1)$report.summary, "[", 15, )

## -----------------------------------------------------------------------------
for (i in 1:5){
  W2 <- MultiChainLadder2(liab, mse.method = "Independence",
      control = systemfit.control(methodResidCov = "Theil", maxiter = i))
  print(format(summary(W2)@report.summary[[3]][15, 4:5],
          digits = 6, big.mark = ","))
}
lapply(summary(W2)$report.summary, "[", 15, )

## -----------------------------------------------------------------------------
str(auto)

## -----------------------------------------------------------------------------
f0 <- MultiChainLadder2(auto, type = "MCL")
# show correlation- the last three columns have zero correlation
# because separate chain-ladders are used
print(do.call(cbind, residCor(f0)), digits = 3)

## -----------------------------------------------------------------------------
f1 <- MultiChainLadder2(auto, type = "MCL+int")

## ----fig = TRUE, fig.cap="Residual plots for the MCL model (first row) and the GMCL (MCL+int) model (second row) for the auto data", echo = FALSE, fig.asp = 1.2, eval = FALSE----
#  parold <- par(mfrow = c(2, 3), mar = c(3, 3, 2, 1))
#  mt <- list(c("Personal Paid", "Personal Incured", "Commercial Paid"))
#  plot(f0, which.plot = 3, main = mt)
#  plot(f1, which.plot = 3, main = mt)
#  par(parold)

## ----eval = FALSE-------------------------------------------------------------
#  lapply(summary(f1, portfolio = "1+3")@report.summary, "[", 11, )

## ----eval = FALSE-------------------------------------------------------------
#  ult <- summary(f1)$Ultimate
#  print(ult[, 1] /ult[, 2], 3)

## ----eval = FALSE-------------------------------------------------------------
#  da <- auto[1:2]
#  # MCL with diagonal development
#  M0 <- MultiChainLadder(da)
#  # non-diagonal development matrix with no intercepts
#  M1 <- MultiChainLadder2(da, type = "GMCL-int")
#  # Munich chain-ladder
#  M2 <- MunichChainLadder(da[[1]], da[[2]])
#  # compile results and compare projected paid to incured ratios
#  r1 <- lapply(list(M0, M1), function(x){
#            ult <- summary(x)@Ultimate
#            ult[, 1] / ult[, 2]
#        })
#  names(r1) <- c("MCL", "GMCL")
#  r2 <- summary(M2)[[1]][, 6]
#  r2 <- c(r2, summary(M2)[[2]][2, 3])
#  print(do.call(cbind, c(r1, list(MuCl = r2))) * 100, digits = 4)

## -----------------------------------------------------------------------------
ClarkLDF(RAA)

## -----------------------------------------------------------------------------
ClarkLDF(RAA, maxage = 20)

## -----------------------------------------------------------------------------
ClarkLDF(RAA, G="weibull")

## ----fig = TRUE, label = LDFweibull, fig.asp=1.4------------------------------
plot(ClarkLDF(RAA, G="weibull"))

## -----------------------------------------------------------------------------
ClarkCapeCod(RAA, Premium = 40000, G = "weibull")

## ----fig=TRUE, label=CapeCod, fig.asp=1.4-------------------------------------
plot(ClarkCapeCod(RAA, Premium = 40000, G = "weibull"))

## -----------------------------------------------------------------------------
# load data
data(GenIns)
GenIns <- GenIns / 1000
# fit Poisson GLM
(fit1 <- glmReserve(GenIns))

## -----------------------------------------------------------------------------
summary(fit1, type = "model")

## -----------------------------------------------------------------------------
# Gamma GLM
(fit2 <- glmReserve(GenIns, var.power = 2))
# compound Poisson GLM (variance function estimated from the data):
# (fit3 <- glmReserve(GenIns, var.power = NULL))

## -----------------------------------------------------------------------------
set.seed(11)
(fit5 <- glmReserve(GenIns, mse.method = "boot"))

## -----------------------------------------------------------------------------
names(fit5)

## -----------------------------------------------------------------------------
pr <- as.data.frame(fit5$sims.reserve.pred)
qv <- c(0.025, 0.25, 0.5, 0.75, 0.975)
res.q <- t(apply(pr, 2, quantile, qv))
print(format(round(res.q), big.mark = ","), quote = FALSE)

## ----eval=FALSE---------------------------------------------------------------
#  library(ggplot2)
#  prm <- reshape(pr, varying=list(names(pr)), v.names = "reserve",
#                 timevar = "year", direction="long")
#  gg <- ggplot(prm, aes(reserve))
#  gg <- gg + geom_density(aes(fill = year), alpha = 0.3) +
#          facet_wrap(~year, nrow = 2, scales = "free")  +
#           theme(legend.position = "none")
#  print(gg)

## ----fig.cap="The predictive distribution of loss reserves for each year based on bootstrapping", echo=FALSE----
knitr::include_graphics("glmReservePlot.png")

## -----------------------------------------------------------------------------
PIC <- PaidIncurredChain(USAApaid, USAAincurred)
PIC

## -----------------------------------------------------------------------------
PIC$Res.Origin

## -----------------------------------------------------------------------------
PIC$Res.Tot

## -----------------------------------------------------------------------------
M <- MackChainLadder(MW2014, est.sigma="Mack")
cdrM <- CDR(M)
round(cdrM, 1)

## -----------------------------------------------------------------------------
cdrAll <- CDR(M,dev="all")
round(cdrAll, 1)

## ----tweedieReserve, eval=FALSE-----------------------------------------------
#   p_profile <- tweedieReserve(MW2008, p.optim=TRUE,
#     p.check=c(0,1.1,1.2,1.3,1.4,1.5,2,3),
#     design.type=c(0,1,1),
#     rereserving=FALSE,
#     bootstrap=0,
#     progressBar=FALSE)
#  # 0 1.1 1.2 1.3 1.4 1.5 2 3
#  # ........Done.
#  # MLE of p is between 0 and 1, which is impossible.
#  # Instead, the MLE of p has been set to NA .
#  # Please check your data and the call to tweedie.profile().
#  # Error in if ((xi.max == xi.vec[1]) | (xi.max == xi.vec[length(xi.vec)])) { :
#  # missing value where TRUE/FALSE needed

## ----echo=FALSE---------------------------------------------------------------
knitr::include_graphics("tweedieReserve.png")

