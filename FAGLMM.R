# Fluster Activity GLMM-models

library("lubridate")
library("tidyverse")
library("ggplot2")
library("glmmTMB")
library("DHARMa")     # For residuals (predictions)
library("parameters") # For confidence intervals of parameter estimates
library("ggeffects")  # For confidence intervals that take random effects and zi into consideration
library("car")
library("effects")    # For evaluation of effects

# Load data:
load("SuperData.RData")

# Transform data if issues with original data
SupDat <- transform(SuperData, tempAvg=scale(tempAvg, center=F), PAR=scale(PAR, center=F))

# Create vector with informatation about what time of day the flight occured.
TimeOfDay <- rep("0",nrow(SuperData))

TimeOfDay[hour(as.POSIXct(SuperData$time,tz = "UTC",format="%d-%m-%Y %H:%M")) >= 0 & 
          hour(as.POSIXct(SuperData$time,tz = "UTC",format="%d-%m-%Y %H:%M")) <= 2] <- "zero"
TimeOfDay[hour(as.POSIXct(SuperData$time,tz = "UTC",format="%d-%m-%Y %H:%M")) >= 3 & 
            hour(as.POSIXct(SuperData$time,tz = "UTC",format="%d-%m-%Y %H:%M")) <= 5] <- "three"
TimeOfDay[hour(as.POSIXct(SuperData$time,tz = "UTC",format="%d-%m-%Y %H:%M")) >= 6 & 
            hour(as.POSIXct(SuperData$time,tz = "UTC",format="%d-%m-%Y %H:%M")) <= 8] <- "six"
TimeOfDay[hour(as.POSIXct(SuperData$time,tz = "UTC",format="%d-%m-%Y %H:%M")) >= 9 & 
            hour(as.POSIXct(SuperData$time,tz = "UTC",format="%d-%m-%Y %H:%M")) <= 11] <- "nine"
TimeOfDay[hour(as.POSIXct(SuperData$time,tz = "UTC",format="%d-%m-%Y %H:%M")) >= 12 & 
            hour(as.POSIXct(SuperData$time,tz = "UTC",format="%d-%m-%Y %H:%M")) <= 14] <- "twelve"
TimeOfDay[hour(as.POSIXct(SuperData$time,tz = "UTC",format="%d-%m-%Y %H:%M")) >= 15 & 
            hour(as.POSIXct(SuperData$time,tz = "UTC",format="%d-%m-%Y %H:%M")) <= 17] <- "fifteen"
TimeOfDay[hour(as.POSIXct(SuperData$time,tz = "UTC",format="%d-%m-%Y %H:%M")) >= 18 & 
            hour(as.POSIXct(SuperData$time,tz = "UTC",format="%d-%m-%Y %H:%M")) <= 20] <- "eighteen"
TimeOfDay[hour(as.POSIXct(SuperData$time,tz = "UTC",format="%d-%m-%Y %H:%M")) >= 21 & 
            hour(as.POSIXct(SuperData$time,tz = "UTC",format="%d-%m-%Y %H:%M")) <= 23] <- "twentyone"

SuperData$TimeOfDay <- TimeOfDay
SupDat$TimeOfDay <- TimeOfDay

TimeOfDayModel1 <- glmmTMB(Counts ~ Ssp:age + Ssp:tempAvg + Ssp:PAR + Ssp:wind_speedAvg + Year + season + TimeOfDay - 1 + (1|locality/Col), data = SuperData, ziformula = ~antennae -1, family = nbinom1)
TimeOfDayModel2 <- glmmTMB(Counts ~ Ssp:age + Ssp:tempAvg + Ssp:PAR + Ssp:wind_speedAvg + Year + season + TimeOfDay - 1 + (1|locality/Col), data = SupDat, ziformula = ~antennae -1, family = nbinom1)
TimeOfDayModel3 <- glmmTMB(Counts ~ Ssp:age + Ssp:tempAvg + Ssp:PAR + Ssp:wind_speedAvg + Year + season + Ssp:TimeOfDay - 1 + (1|locality/Col), data = SuperData, ziformula = ~antennae -1, family = nbinom1)  
TimeOfDayModel4 <- glmmTMB(Counts ~ Ssp:age + Ssp:tempAvg + Ssp:PAR + Ssp:wind_speedAvg + Year + season + Ssp:TimeOfDay - 1 + (1|locality/Col), data = SupDat, ziformula = ~antennae -1, family = nbinom1)

AIC(TimeOfDayModel2, TimeOfDayModel4)
chosenModel <- TimeOfDayModel4

Anova(chosenModel) # Check type II Anova (from Car package). If check for type III Anova, contrasts are needed. See: Post-model-fitting procedures with glmmTMB

# Effects
plot(allEffects(chosenModel))

# Confidence intervals
test <- model_parameters(chosenModel, effects = "all")
ggpredict(chosenModel, "Ssp")
ggp <- ggpredict(chosenModel, terms = "Ssp")
plot(ggp)

# Residual plots
SimResid <- simulateResiduals(fittedModel = chosenModel, plot = F)
plot(SimResid)
