### SEM - Path Analysis Code for: 
### Increased lead and glucocorticoid concentrations 
### reduce reproductive success in House Sparrows 
### along an urban gradient 
########################################################

# Packages
library(lavaan)
library(semPlot)
library(ggplot2)
library(lmerTest)

# Change to proper location
data <- read.csv("urban_lead_cort_data.csv")

str(data)
head(data)


#################################################################
#### Clean up data ###
data$nest <-  as.factor(data$Nest)
data$noise <- as.numeric(data$NoiseValue)
data$light <- as.numeric(data$NightLightReflect)
data$urban <- as.numeric(data$UrbanDensity*100)
data$cort <- as.numeric(data$cort)
data$lead <- as.numeric(data$lead)
data$avgyoungmass <- as.numeric(data$nest.average.weight)
data$broodsize <- as.numeric(data$X..nestlings)
data$sex <- as.factor(data$sex)
data$mass <- as.numeric(data$weight)
data$tarsus <- as.numeric(data$tarsus)

# Remove columns
data$Nest <- NULL
data$NoiseValue <- NULL
data$NightLightReflect <- NULL
data$UrbanDensity <- NULL
data$nest.average.weight <- NULL
data$X..nestlings <- NULL

# Rearrange Columns 
dat <- subset(data, select=c("nest", "noise", "light", "urban", "cort", "lead", "avgyoungmass", "broodsize", "sex", "mass", "tarsus"))


# Look at  correlations
cor(dat[,c(2:7,10)], use="pairwise", method="spearman")      



###############################################
# Adult Mass and Urbanization 
ggplot(dat, aes(x = urban, y = mass)) +
geom_point()

mass.mod <- lm(mass ~ urban, dat)
summary(mass.mod)
anova(mass.mod)
  # no relationship between urban density and adult mass
qqnorm(resid(mass.mod))
qqline(resid(mass.mod))
plot(density(resid(mass.mod)))
  # residuals look normal



#############################################
##### Scale data for SEM
dat$avgyoungmass <- as.numeric(scale(dat$avgyoungmass))
dat$cort <- as.numeric(scale(dat$cort))
dat$lead <- as.numeric(scale(dat$lead))
dat$noise <- as.numeric(scale(dat$noise))
dat$light <- as.numeric(scale(dat$light))
dat$urban <- as.numeric(scale(dat$urban))



###################################################
########## SEM - Path Analysis ####################

model <- '
cort ~ light + urban + lead + noise
lead ~ light + urban + noise
avgyoungmass ~  cort + lead
'

fit<-sem(model, data=dat)
summary(fit, standardized=T, fit.measures=T, rsq=T)   
semPaths(fit,'std',layout='tree2')

