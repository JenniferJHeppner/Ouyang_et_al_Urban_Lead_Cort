### Figures Code for: 
### Increased lead and glucocorticoid concentrations 
### reduce reproductive success in House Sparrows 
### along an urban gradient 
########################################################

library(ggplot2)

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





##################################
### Figures

# Urban Density and Cort
a <- ggplot(dat, aes(x = urban, y = cort)) +
  geom_point(size = 4)+
  geom_smooth(method = lm, se= FALSE, fullrange=FALSE, level=0.95, color = "black", size = 1) +
  theme_classic() +
  labs(x="Urban Density", y = "Corticosterone (ng/mL)") +
  theme(axis.title.x = element_text(size = 18)) +
  theme(axis.title.y = element_text(size = 18)) +
  theme(axis.text = element_text(size = 13))
a

# Urban Density and Lead
b <- ggplot(dat, aes(x = urban, y = lead)) +
  geom_point(size = 4) +
  geom_smooth(method = lm, se= FALSE, fullrange=FALSE, level=0.95, color = "black", size = 1) +
  theme_classic() +
  labs(x="Urban Density", y = "Plasma Lead (ng/dL)") +
  theme(axis.title.x = element_text(size = 18)) +
  theme(axis.title.y = element_text(size = 18)) +
  theme(axis.text = element_text(size = 13))
b

# Cort and Lead
c <- ggplot(dat, aes(x = lead, y = cort)) +
  geom_point(size = 4) +
  geom_smooth(method = lm, se= FALSE, fullrange=FALSE, level=0.95, color = "black", size = 1) +
  theme_classic() +
  labs(x="Plasma Lead (ng/dL)", y = "Corticosterone (ng/mL)") +
  theme(axis.title.x = element_text(size = 18)) +
  theme(axis.title.y = element_text(size = 18)) +
  theme(axis.text = element_text(size = 13))
c


# Cort and Offspring Mass
d <- ggplot(dat, aes(x = cort, y = avgyoungmass)) +
  geom_point(size = 4) +
  geom_smooth(method = lm, se= FALSE, fullrange=FALSE, level=0.95, color = "black", size = 1) +
  theme_classic() +
  labs(x="Corticosterone (ng/mL)", y = "Average Nestling Mass (g)") +
  theme(axis.title.x = element_text(size = 18)) +
  theme(axis.title.y = element_text(size = 18)) +
  theme(axis.text = element_text(size = 13))
d


# Combining all graphs into one
library(ggpubr)
theme_set(theme_pubr())
figure1 <- ggarrange(a, c, b, d,
                     ncol = 2, nrow = 2)
figure1



