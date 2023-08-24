library(foreign)
library(car)
library(gplots)
library(apsrtable)
library(plm)
library(tseries)
library(lmtest)
library(readxl)
library(broom)
library(texreg)


#Unfortunately i can't provide access to this data because they are commercial
DataKPI <- read_excel("Path.xlsx", sheet="SheetKPI")

#y's dynamics by year across countries on different grafs
coplot(Rentabel ~ Year|branch, type="l", data=DataKPI)
coplot(Rentabel ~ Year|branch, type="b", data=DataKPI)

#y's dynamics by year on alone graf
scatterplot(Rentabel~Year|branch, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=DataKPI)

#heterogeneity across countries ("plotmeans" draw a 95% confidence interval around the means)
plotmeans(Rentabel~branch, main="Heterogeineity across branches", data=DataKPI)
detach("package:gplots")

#heterogeneity across years
plotmeans(Rentabel~Year, main="Heterogeineity across months", data=DataKPI)
detach("package:gplots")

#regular OLS
regularOLS <- lm(Rentabel ~ GrossProduction_t+Marketability+Farm_capacity+Culling+YealdPerCow+FoodPerkg+OperationCostsPerkg+Cow_repear+Amortisation+Credit_procent+Price_instandartweight+Subsidy+IOFC, data=DataKPI)
summary(regularOLS)
write.csv(tidy(regularOLS) , "regularOLS.csv" )
sink("regularOLS.txt")
print(summary(lm(Rentabel ~ GrossProduction_t+Marketability+Farm_capacity+Culling+YealdPerCow+FoodPerkg+OperationCostsPerkg+Cow_repear+Amortisation+Credit_procent+Price_inrealweight+Price_instandartweight+Subsidy+IOFC, data=DataKPI)))
sink()
y_hat <- regularOLS$fitted
plot(DataKPI$Culling, DataKPI$Rentabel, pch=19, xlab="Culling", ylab="Rentabel")
abline(lm(DataKPI$Rentabel~DataKPI$Culling), lwd=3, col="red")
plot(DataKPI$Price_instandartweight, DataKPI$Rentabel, pch=19, xlab="Price_instandartweight", ylab="Rentabel")
abline(lm(DataKPI$Rentabel~DataKPI$Price_instandartweight), lwd=3, col="red")
plot(DataKPI$Subsidy, DataKPI$Rentabel, pch=19, xlab="Subsidy", ylab="Rentabel")
abline(lm(DataKPI$Rentabel~DataKPI$Subsidy), lwd=3, col="red")
plot(DataKPI$YealdPerCow, DataKPI$Rentabel, pch=19, xlab="YealdPerCow", ylab="Rentabel")
abline(lm(DataKPI$Rentabel~DataKPI$YealdPerCow), lwd=3, col="red")
plot(DataKPI$Marketability, DataKPI$Rentabel, pch=19, xlab="Marketability", ylab="Rentabel")
abline(lm(DataKPI$Rentabel~DataKPI$Marketability), lwd=3, col="red")
plot(DataKPI$GrossProduction_t, DataKPI$Rentabel, pch=19, xlab="GrossProduction_t", ylab="Rentabel")
abline(lm(DataKPI$Rentabel~DataKPI$GrossProduction_t), lwd=3, col="red")

#FE LSDV(least square dummy variable)
fixed.dum <- lm(Rentabel ~ GrossProduction_t+Marketability+Farm_capacity+Culling+YealdPerCow+FoodPerkg+OperationCostsPerkg+Cow_repear+Amortisation+Credit_procent+Price_inrealweight+Price_instandartweight+Subsidy+IOFC+factor(branch)-1, data=DataKPI)
summary(fixed.dum)
y_hatFE <- fixed.dum$fitted
scatterplot(y_hatFE~Dataset$x1|Dataset$country, boxplot=FALSE, xlab="x1", ylab="y_hatFE", smooth=FALSE)
abline(lm(Dataset$y~Dataset$x1), lwd=3, col="red")
#Create and save comparison table
apsrtable(regularOLS, fixed.dum, model.names = c("regularOLS", "OLS_LSDV"))
cat(apsrtable(regularOLS,fixed.dum, model.names = c("regularOLS", "OLS_LSDV"), Sweave=F), file="ols_fixed1.txt")

#FE
fixed <- plm(Rentabel ~ GrossProduction_t+Marketability+Farm_capacity+Culling+YealdPerCow+FoodPerkg+OperationCostsPerkg+Cow_repear+Amortisation+Credit_procent+Price_instandartweight+Subsidy+IOFC, data=DataKPI, index=c("branch", "Year"), model="within")
summary(fixed)
write.csv(tidy(fixed) , "fixed.csv" )
# Display the fixed effects (constants for each country)
fixef(fixed)
# Testing for fixed effects, null: OLS better than fixed
pFtest(fixed, regularOLS)

#RE ("swar")
random <- plm(Rentabel ~ GrossProduction_t+Marketability+Farm_capacity+Culling+
                YealdPerCow+FoodPerkg+OperationCostsPerkg+Cow_repear+Amortisation+
                Credit_procent+Price_instandartweight+Subsidy+IOFC,
              data=DataKPI, index=c("branch", "Year"), model = "random", random.method = "walhus")
summary(random)
write.csv(tidy(random), "random.csv")
# Setting as panel data (an alternative way to run the above model)
DataKPI.set <- plm.data(DataKPI, index=c("branch", "Year"))
random.set <- plm(Rentabel~GrossProduction_t+Fat+Protein+Marketability+Farm_capacity+Culling+
                    YealdPerCow+FoodPerkg+OperationCostsPerkg+Cow_repear+Amortisation+
                    Credit_procent+Price_inrealweight+Price_instandartweight+Subsidy+IOFC,
                  data=DataKPI.set, model="random")
summary(random.set)

#Choice between RE and FE (Hausman test)
phtest(fixed, random)

#Time-FE
fixed.time <- plm(Rentabel ~ GrossProduction_t+Marketability+Farm_capacity+Culling+
                    YealdPerCow+FoodPerkg+OperationCostsPerkg+Cow_repear+Amortisation+
                    Credit_procent+Price_instandartweight+Subsidy+
                    IOFC + factor(Year), data=DataKPI, index=c("branch", "Year"), model="within")
summary(fixed.time)
write.csv(tidy(fixed.time), "fixed_time.csv")
#Choice time-FE and FE
pFtest(fixed.time, fixed)
plmtest(fixed, c("time"), type=("bp"))

#Pool
pool <- plm(Rentabel ~ GrossProduction_t+Marketability+Farm_capacity+Culling+
              YealdPerCow+FoodPerkg+OperationCostsPerkg+Cow_repear+Amortisation+
              Credit_procent+Price_instandartweight+Subsidy+IOFC,
            data=DataKPI, index=c("branch", "Year"), model="pooling")
summary(pool)
pFtest(fixed, pool)
# Breusch-Pagan Lagrange Multiplier for random effects. Null is no panel effect (i.e. OLS better)
plmtest(pool, type=c("bp"))

#Testing for cross-sectional dependence/contemporaneous correlation: using Breusch-Pagan LM test of independence and Pasaran CD test
pcdtest(fixed, test=c("lm"))
pcdtest(fixed, test=c("cd"))

#Driscoll and Kraay (1998) Robust Covariance Matrix Estimator
summary(pool, vcov = vcovSCC)
summary(pool, vcov = function(x) vcovSCC(x, method="arellano", type="HC1"))
summary(fixed, vcov = vcovSCC)
summary(fixed, vcov = function(x) vcovSCC(x, method="arellano", type="HC1"))
sink("fixedRoBUST.txt")
print(summary(fixed, vcov = function(x) vcovSCC(x, method="arellano", type="HC1")))
sink()

coeftest(fixed)
coeftest(fixed, vcov.=vcovSCC)
coeftest(fixed, vcov.=function(x) vcovSCC(x, type="HC1", maxlag=4))

#FE without subcidy and culling
fixed1 <- plm(Rentabel ~ Marketability+Farm_capacity+YealdPerCow+
                FoodPerkg+OperationCostsPerkg+Cow_repear+Amortisation+Credit_procent+
                Price_inrealweight+Price_instandartweight+IOFC, data=DataKPI, index=c("branch", "Year"), model="within")
summary(fixed1)
summary(fixed1, vcov = function(x) vcovSCC(x, method="arellano", type="HC1"))

#Testing for serial correlation
pbgtest(fixed)
pbgtest(fixed.time)

#Stationarity test Dickey-fuller
adf.test(DataKPI.set$Rentabel, k=2)

#Heteroskedastisity
bptest(Rentabel ~ Marketability+Farm_capacity+Culling+YealdPerCow+
         FoodPerkg+OperationCostsPerkg+Cow_repear+Amortisation+Credit_procent+
         Price_inrealweight+Price_instandartweight+Subsidy+IOFC+factor(branch), data=DataKPI, studentize=F)

# Original coefficients
coeftest(random)
#Heteroscedasticity consistent coefficient
coeftest(random, vcovHC)
# Heteroskedasticityconsistent coefficients, type 3
coeftest(random, vcovHC(random, type="HC3"))
# The following shows the HC standard errors of the coefficients
t(sapply(c("HC0","HC1","HC2","HC3","HC4"), function(x) sqrt(diag(vcovHC(random,type=x)))))

coeftest(fixed)
coeftest(fixed, vcovHC)
coeftest(fixed, vcovHC(fixed, method="arellano"))
coeftest(fixed, vcovHC(fixed, type="HC3"))
t(sapply(c("HC0","HC1","HC2","HC3","HC4"), function(x) sqrt(diag(vcovHC(fixed, type=x)))))