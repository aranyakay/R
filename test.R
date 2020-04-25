### Basic LM model
data("data_SRHS_long")
SRHS <- data_SRHS_long[1:2400,]
# Categories rescaled to vary from 1 (“poor”) to 5 (“excellent”)
SRHS$srhs <- 5 - SRHS$srhs
out <- lmest(responsesFormula = srhs ~ NULL,
             index = c("id","t"),
             data = SRHS,
             k = 3,
             start = 1,
             modBasic = 1,
             seed = 123)
out
summary(out)
## Not run:
## Basic LM model with model selection using BIC
out1 <- lmest(responsesFormula = srhs ~ NULL,
              index = c("id","t"),
              data = SRHS,
              k = 1:5,
              tol = 1e-8,
              modBasic = 1,
              seed = 123)
out1
out1$Bic
# Basic LM model with model selection using AIC
out2 <- lmest(responsesFormula = srhs ~ NULL,
              index = c("id","t"),
              data = SRHS,
              k = 1:5,
              tol = 1e-8,
              modBasic = 1,
              modSel = "AIC",
              seed = 123)
out2
out2$Aic
# Criminal data
data(data_criminal_sim)
data_criminal_sim = data.frame(data_criminal_sim)
responsesFormula <- lmestFormula(data = data_criminal_sim,
                                 response = "y")$responsesFormula
out3 <- lmest(responsesFormula = responsesFormula,
              index = c("id","time"),
              data = data_criminal_sim,
              k = 1:7,modBasic = 1,tol = 10^-4)
out3
# Example of drug consumption data
data("data_drug")
long <- data_drug[,-6]-1
long <- data.frame(id = 1:nrow(long),long)
long <- reshape(long,direction = "long",
                idvar = "id",
                varying = list(2:ncol(long)))
out4 <- lmest(index = c("id","time"),k = 3, data = long,
              weights = data_drug[,6],modBasic = 1)
out4
summary(out4)
### LM model with covariates in the latent model
# Covariates: gender, race, educational level (2 columns), age and age^2
out5 <- lmest(responsesFormula = srhs ~ NULL,
              latentFormula = ~
                I(gender - 1) +
                I( 0 + (race == 2) + (race == 3)) +
                I(0 + (education == 4)) +
                I(0 + (education == 5)) +
                I(age - 50) + I((age-50)^2/100),
              index = c("id","t"),
              data = SRHS,
              k = 2,
              paramLatent = "multilogit",
              start = 0)
out5
summary(out5)
### LM model with the above covariates in the measurement model
out6 <- lmest(responsesFormula = srhs ~ -1 +
                I(gender - 1) +
                I( 0 + (race == 2) + (race == 3)) +
                I(0 + (education == 4)) +
                I(0 + (education == 5)) + I(age - 50) +
                I((age-50)^2/100),
              index = c("id","t"),
              data = SRHS,
              k = 2,
              modManifest = "LM",
              out_se = TRUE,
              tol = 1e-8,
              start = 1,
              seed = 123)
out6
summary(out6)
## End(Not run)



require(mmm)
data(multiLongGaussian)
str(multiLongGaussian)
## Data with continous resposes
dt <- lmestData(data = multiLongGaussian, id = "ID")
str(dt)
## Summary of each variable and for each time
summary(dt)
## Summary of each variable
summary(dt, type = "cross")
## Summary of each variable by time
summary(dt, type = "year")
plot(dt)
plot(dt, typePlot = "sh")


data("data_criminal_sim")
dt1 <- lmestData(data = data_criminal_sim, id = "id", time = "time")
str(dt1)
summary(dt1, varType = rep("d",ncol(dt1$Y)))
