getwd()
setwd("../Desktop/Duke/2017Spring/Bayes/Project/GSS")
setwd("GSS")

installed.packages()
install.packages("readstata13")
install.packages("dplyr")
install.packages("R2jags")
install.packages('gjam')
require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)

library(readstata13)
library(R2jags)
library(gjam)
library(plyr)

saveRDS(Predictor, file="Predictor")
saveRDS(Behavior, file="Behavior")
saveRDS(GSS, file="GSS new")

# GSS <- readRDS(file="GSS new")

GSS94 <- read.dta13("GSS1994.dta")
GSS00 <- read.dta13("GSS2000.dta")
GSS10 <- read.dta13("GSS2010.dta")

# Slice out variables of interest, adding yeara of survey as v14
data94 <- cbind.data.frame(GSS94$grnprice, GSS94$grntaxes, GSS94$grnsol, GSS94$ihlpgrn, GSS94$scigrn, GSS94$grnecon, GSS94$grnprog, GSS94$grwthelp, GSS94$toodifme, GSS94$sex, GSS94$cohort, GSS94$rincome, GSS94$educ, GSS94$wtssall, GSS94$partyid, 1994)
colnames(data94) <- c("WTPprice", "WTPtaxes", "WTPcut", "WTPmoney", "NEAscience", "NEAjobs", "NEAharm", "NEAecon", "NEAnotme", "sex", "cohort", "income", "educ", "weight", "party", "survey")

data00 <- cbind.data.frame(GSS00$grnprice, GSS00$grntaxes, GSS00$grnsol, GSS00$ihlpgrn, GSS00$scigrn, GSS00$grnecon, GSS00$grnprog, GSS00$grwthelp, GSS00$toodifme, GSS00$sex, GSS00$cohort, GSS00$rincome, GSS00$educ, GSS00$wtssall, GSS00$partyid, 2000)
colnames(data00) <- c("WTPprice", "WTPtaxes", "WTPcut", "WTPmoney", "NEAscience", "NEAjobs", "NEAharm", "NEAecon", "NEAnotme", "sex", "cohort", "income", "educ", "weight", "party", "survey")

data10 <- cbind.data.frame(GSS10$grnprice, GSS10$grntaxes, GSS10$grnsol, GSS10$ihlpgrn, GSS10$scigrn, GSS10$grnecon, GSS10$grnprog, GSS10$grwthelp, GSS10$toodifme, GSS10$sex, GSS10$cohort, GSS10$rincome, GSS10$educ, GSS10$wtssall, GSS10$partyid, 2010)
colnames(data10) <- c("WTPprice", "WTPtaxes", "WTPcut", "WTPmoney", "NEAscience", "NEAjobs", "NEAharm", "NEAecon", "NEAnotme", "sex", "cohort", "income", "educ", "weight", "party", "survey")


# Append data
GSS <- rbind.data.frame(data94, data00, data10)

# Create new varicbindable Att by adding up the five variables
nea <-  as.numeric(GSS[, 5]) + as.numeric(GSS[, 6])+as.numeric(GSS[, 7]) + as.numeric(GSS[, 8])+as.numeric(GSS[, 9]) 
GSSnea <- cbind.data.frame(GSS, nea)

# rescaled as 5-25; 5: least like to react for env, 20: highest, pro-env
#NEA <- t(new)
# Create new variable matrix
#Predictor <- cbind(NEA, GSS[, 10:15])
#colnames(Predictor)[1] <- "NEA"
#Predictor <- edit(Predictor)
# summary(Predictor[,1])

#WTP <- t(GSS[, 1]) + t(GSS[, 2]) + t(GSS[, 3]) + t(GSS[, 4])
#B <- 5-newb # Convert pro-env to pro-econ from 5-1 to 0-4, so the econVsenv can be in positive relation
#Behavior <-t(B)

# Descriptive Data
fprice<-count(GSS$WTPprice)
par(mfrow=c(2,2),bty='n', mar=c(4,4,1,1))
plot(GSS$cohort, GSS$WTPprice, las=2)
plot(GSS$WTPcut, las=2)
plot(GSS$WTPtaxes, las=2)
plot(GSS$WTPmoney, las=2)
plot(GSSnea$cohort,GSSnea$nea,las=2)

# Non-gjam method
WTP <-  as.numeric(GSS[, 1]) + as.numeric(GSS[, 2])+as.numeric(GSS[, 3]) + as.numeric(GSS[, 4]) 
as.factor(WTP)
GSSneawtp <- cbind.data.frame(GSSnea, WTP)

n <- nrow(GSSneawtp)
cohorts <-0
#Generate new variable cohorts:
for(j in 1:n){
  if(is.na(GSSneawtp$cohort[j])){
    cohorts[j] <- NA
  }else if(GSSneawtp$cohort[j]<=1925){
    cohorts[j] <- 1
  }else if(GSSneawtp$cohort[j]<=1945 & GSSneawtp$cohort[j]>1925){
    cohorts[j] <- 2 
  }else if(GSSneawtp$cohort[j] <= 1965 & GSSneawtp$cohort[j] > 1945){
    cohorts[j] <- 3
  }else if(GSSneawtp$cohort[j] <= 1992 & GSSneawtp$cohort[j] > 1965){
    cohorts[j] <- 4
  }
  
}

GSSnongjam <- cbind.data.frame(GSSneawtp, cohorts)

ynames <- c("WTPprice", "WTPtaxes", "WTPcut", "WTPmoney")

ydata <- GSSnongjam[,ynames]
for(j in 1:ncol(ydata)){
  if(!is.factor(ydata[,j]))next
  yj <- as.numeric(ydata[,j])
  yj[is.na(ydata[,j])] <- NA
  ydata[,j] <- yj - 1
}

aq <- transform(airquality, Month = factor(Month, labels = month.abb[5:9]))
aq <- subset(aq, Month != "Jul")
aq
table(aq$Month)
table(droplevels(aq)$Month)

GSSnongjam <- subset(GSSnongjam, income != "incomerefused" | income != "incomedk" | income != "incomena" | income != "incomeiap")
droplevels(GSSnongjam)
income <- droplevels(GSSnongjam)$income
summary(income)

library(dplyr)
df %>%
  mutate(GSS$income = recode(GSS$income, "c('income$25000 OR MORE', 'incomerefused', 'incomedk', 'incomena', 'incomeiap')='2500 OR MORE'"))

modelList <- list(ng = 1000, burnin = 10, typeNames = 'OC')
#formula <- as.formula(~ as.numeric(sex) + as.numeric(cohort) + as.numeric(educ)+as.numeric(income))
#formula <- as.formula(~ sex+cohort+educ+nea)
#formula <- as.formula(~ cohort+sex*educ+cohort*educ+educ+nea)
#formula <- as.formula(~ cohort)
#formula <- as.formula(~ educ)
formula <- as.formula(~ educ+cohorts+nea)
formula2 <- as.formula(~ educ+cohorts+nea+party)
formula <- as.formula(~ educ+income+cohorts+nea)

options(warn=-1)
out     <- gjam(formula, xdata = GSS, ydata, modelList = modelList)
summary(out)
plotPars <- list(SMALLPLOTS=F, GRIDPLOTS=T)
gjamPlot(out,plotPars)

help("droplevels")

options(warn=-1)
out2     <- gjam(formula2, xdata = GSS, ydata, modelList = modelList)
summary(out2)
plotPars <- list(SMALLPLOTS=F, GRIDPLOTS=T)
gjamPlot(out2,plotPars)

help(list)

is.data.frame(GSSnongjam)
summary(GSSnongjam)

ftable(xtabs(~ sex + WTPprice + cohorts, data = GSSneawtp))
m <- polr(as.factor(WTP) ~ sex + nea + cohorts + educ, data = GSSneawtp, Hess=TRUE)
summary(m)
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
is.factor(GSSnea$income)
## store table
## combined table
(ctable <- coef(summary(m)))
(ctable <- cbind(ctable, "p value" = p))
help(polr)
m <- polr(as.factor(WTP) ~ cohorts + educ, data = GSSneawtp, Hess=TRUE)
summary(m)
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## store table
## combined table
(ctable <- coef(summary(m)))
(ctable <- cbind(ctable, "p value" = p))
help(polr)
m <- polr(as.factor(WTP) ~ cohorts + nea + educ, data = GSSneawtp, Hess=TRUE)
summary(m)
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## store table
## combined table
(ctable <- coef(summary(m)))
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(m)) 



#graph
newdat <- data.frame(
  cohorts = rep(1:4, 2100),
  educ = rep(0:20, each = 400),
  nea = rep(seq(from = 5, to = 25, length.out = 100), 4))

newdat <- cbind(newdat, predict(m, newdat, type = "probs"))

##show first few rows
head(newdat)

lnewdat <- melt(newdat, id.vars = c("cohorts", "educ", "nea"),
                variable.name = "Level", value.name="Probability")
## view first few rows
head(lnewdat)
ggplot(lnewdat, aes(x = cohorts, y = Probability, colour = Level)) +
  geom_line() + facet_grid(nea ~ educ, labeller="label_both")
