rm(list=ls())
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggcorrplot)
library(patchwork)
library(caret)

setwd("C:/Users/epark/Desktop/CPLN 505/Assignments/Assignment3")
#setwd("/Users/LauraFrances_1/Library/CloudStorage/GoogleDrive-lauramuellersoppart@gmail.com/My Drive/Penn/Courses/CPLN5050_PlanningbyNumbers/Datasets")

### ** Modeling Land Use Change"

#### 1. Read the data into R. Run descriptive statistics as necessary.

chester <- read.csv("Chester_Urban_Growth.csv") # read csv
head(chester)

summary(chester)

##### Distance in meters to a water feature
# d <- density(chester$DIST_WATER, na.rm = TRUE)
# density_water <- ggplot(data.frame(x = d$x, y = d$y), aes(x, y)) +
#   geom_line() +
#   labs(title = "DIST_WATER", x = "Distance", y = "Density")
boxplot <- ggplot(chester, aes(x = "", y = DIST_WATER)) +
  geom_boxplot() +
  labs(title = "DIST_WATER", x = "", y = "Distance")
histogram <- ggplot(chester, aes(x = DIST_WATER)) +
  geom_histogram(binwidth = 0.5, color = "black", fill = "white") +
  labs(title = "DIST_WATER", x = "Distance", y = "Frequency")

combined_plot <- boxplot + coord_flip() + histogram +
  plot_layout(ncol = 1, heights = c(1, 2))
print(combined_plot)

###### Log Scale
boxplot <- ggplot(chester, aes(x = "", y = log(DIST_WATER))) +
  geom_boxplot() +
  labs(title = "Log(DIST_WATER)", x = "", y = "Distance")
histogram <- ggplot(chester, aes(x = log(DIST_WATER))) +
  geom_histogram(binwidth = 0.5, color = "black", fill = "white") +
  labs(title = "Log(DIST_WATER)", x = "Distance", y = "Frequency")

combined_plot <- boxplot + coord_flip() + histogram +
  plot_layout(ncol = 1, heights = c(1, 2))
print(combined_plot)

##### Distance in meters to a regional rail station
boxplot <- ggplot(chester, aes(x = "", y = DIST_RAILS)) +
  geom_boxplot() +
  labs(title = "DIST_RAILS", x = "", y = "Distance")
histogram <- ggplot(chester, aes(x = DIST_RAILS)) +
  geom_histogram(binwidth = 0.5, color = "black", fill = "white") +
  labs(title = "DIST_RAILS", x = "Distance", y = "Frequency")

combined_plot <- boxplot + coord_flip() + histogram +
  plot_layout(ncol = 1, heights = c(1, 2))
print(combined_plot)

###### Log Scale
boxplot <- ggplot(chester, aes(x = "", y = log(DIST_RAILS))) +
  geom_boxplot() +
  labs(title = "Log(DIST_RAILS)", x = "", y = "Distance")
histogram <- ggplot(chester, aes(x = log(DIST_RAILS))) +
  geom_histogram(binwidth = 0.5, color = "black", fill = "white") +
  labs(title = "Log(DIST_RAILS)", x = "Distance", y = "Frequency")

combined_plot <- boxplot + coord_flip() + histogram +
  plot_layout(ncol = 1, heights = c(1, 2))
print(combined_plot)

##### Distance in meters to a regional rail line
boxplot <- ggplot(chester, aes(x = "", y = DIST_REGRA)) +
  geom_boxplot() +
  labs(title = "DIST_REGRA", x = "", y = "Distance")
histogram<- ggplot(chester, aes(x = DIST_REGRA)) +
  geom_histogram(binwidth = 0.5, color = "black", fill = "white") +
  labs(title = "DIST_REGRA", x = "Distance", y = "Frequency")

ccombined_plot <- boxplot + coord_flip() + histogram +
  plot_layout(ncol = 1, heights = c(1, 2))
print(combined_plot)

###### Log Scale
boxplot <- ggplot(chester, aes(x = "", y = log(DIST_REGRA))) +
  geom_boxplot() +
  labs(title = "Log(DIST_REGRA)", x = "", y = "Distance")
histogram <- ggplot(chester, aes(x = log(DIST_REGRA))) +
  geom_histogram(binwidth = 0.5, color = "black", fill = "white") +
  labs(title = "Log(DIST_REGRA)", x = "Distance", y = "Frequency")

combined_plot <- boxplot + coord_flip() + histogram +
  plot_layout(ncol = 1, heights = c(1, 2))
print(combined_plot)

##### Distance in meters to a passenger rail line
boxplot <- ggplot(chester, aes(x = "", y = DIST_PASSR)) +
  geom_boxplot() +
  labs(title = "DIST_PASSR", x = "", y = "Distance")
histogram <- ggplot(chester, aes(x = DIST_PASSR)) +
  geom_histogram(binwidth = 0.5, color = "black", fill = "white") +
  labs(title = "DIST_PASSR", x = "Distance", y = "Frequency")

combined_plot <- boxplot + coord_flip() + histogram +
  plot_layout(ncol = 1, heights = c(1, 2))
print(combined_plot)

###### Log Scale
boxplot <- ggplot(chester, aes(x = "", y = log(DIST_PASSR))) +
  geom_boxplot() +
  labs(title = "Log(DIST_PASSR)", x = "", y = "Distance")
histogram <- ggplot(chester, aes(x = log(DIST_PASSR))) +
  geom_histogram(binwidth = 0.5, color = "black", fill = "white") +
  labs(title = "Log(DIST_PASSR)", x = "Distance", y = "Frequency")

combined_plot <- boxplot + coord_flip() + histogram +
  plot_layout(ncol = 1, heights = c(1, 2))
print(combined_plot)

##### Distance in meters to a  4-lane highway
boxplot <- ggplot(chester, aes(x = "", y = DIST_4LNE_)) +
  geom_boxplot() +
  labs(title = "DIST_4LNE_", x = "", y = "Distance")
histogram <- ggplot(chester, aes(x = DIST_4LNE_)) +
  geom_histogram(binwidth = 0.5, color = "black", fill = "white") +
  labs(title = "DIST_4LNE_", x = "Distance", y = "Frequency")

combined_plot <- boxplot + coord_flip() + histogram +
  plot_layout(ncol = 1, heights = c(1, 2))
print(combined_plot)

###### Log Scale
boxplot <- ggplot(chester, aes(x = "", y = log(DIST_4LNE_))) +
  geom_boxplot() +
  labs(title = "Log(DIST_4LNE_)", x = "", y = "Distance")
histogram <- ggplot(chester, aes(x = log(DIST_4LNE_))) +
  geom_histogram(binwidth = 0.5, color = "black", fill = "white") +
  labs(title = "Log(DIST_4LNE_)", x = "Distance", y = "Frequency")

combined_plot <- boxplot + coord_flip() + histogram +
  plot_layout(ncol = 1, heights = c(1, 2))
print(combined_plot)

##### Distance in meters to an interstate freeway
boxplot <- ggplot(chester, aes(x = "", y = DIST_INTER)) +
  geom_boxplot() +
  labs(title = "DIST_INTER", x = "", y = "Distance")
hisrogram <- ggplot(chester, aes(x = DIST_INTER)) +
  geom_histogram(binwidth = 0.5, color = "black", fill = "white") +
  labs(title = "DIST_INTER", x = "Distance", y = "Frequency")

combined_plot <- boxplot + coord_flip() + histogram +
  plot_layout(ncol = 1, heights = c(1, 2))
print(combined_plot)

###### Log Scale
boxplot <- ggplot(chester, aes(x = "", y = log(DIST_INTER))) +
  geom_boxplot() +
  labs(title = "Log(DIST_INTER)", x = "", y = "Distance")
histogram <- ggplot(chester, aes(x = log(DIST_INTER))) +
  geom_histogram(binwidth = 0.5, color = "black", fill = "white") +
  labs(title = "Log(DIST_INTER)", x = "Distance", y = "Frequency")

combined_plot <- boxplot + coord_flip() + histogram +
  plot_layout(ncol = 1, heights = c(1, 2))
print(combined_plot)

##### Distance in meters to the closest park
boxplot <- ggplot(chester, aes(x = "", y = DIST_PARKS)) +
  geom_boxplot() +
  labs(title = "DIST_PARKS", x = "", y = "Distance")
histogram <- ggplot(chester, aes(x = DIST_PARKS)) +
  geom_histogram(binwidth = 0.5, color = "black", fill = "white") +
  labs(title = "DIST_PARKS", x = "Distance", y = "Frequency")

combined_plot <- boxplot + coord_flip() + histogram +
  plot_layout(ncol = 1, heights = c(1, 2))
print(combined_plot)

###### Log Scale
boxplot <- ggplot(chester, aes(x = "", y = log(DIST_PARKS))) +
  geom_boxplot() +
  labs(title = "Log(DIST_PARKS)", x = "", y = "Distance")
histogram <- ggplot(chester, aes(x = log(DIST_PARKS))) +
  geom_histogram(binwidth = 0.5, color = "black", fill = "white") +
  labs(title = "Log(DIST_PARKS)", x = "Distance", y = "Frequency")

combined_plot <- boxplot + coord_flip() + histogram +
  plot_layout(ncol = 1, heights = c(1, 2))
print(combined_plot)

##### Bar plots/density of binary variables and 92 urban status

chester92_variables <- 
  chester %>%
  as.data.frame() %>%
  select(URBAN92, FOURLNE300, INTERST800, REGRAIL300, PARKS500M, WATER100, CITBORO_10) %>%
  gather(variable, value, -URBAN92)

ggplot(chester92_variables %>%
         group_by(URBAN92, variable) %>%
         summarize(sum = sum(value))) + 
  geom_bar(aes(as.factor(URBAN92), 
               sum, 
               fill=as.factor(URBAN92)),
           stat="identity") + 
  facet_wrap(~variable, scales = "free") +
  scale_fill_manual(values = c("light blue", "dark blue"),
                    labels = c("Not Urbanized","Urbanized"),
                    name = "") +
  ggtitle("Urbanized vs. Not Urbanized Raster Cell by Characteristic (1992)")+
  xlab("Urbanized vs Not Urbanized") + ylab("Count of Raster Cell Characteristic")
labs(x="Urbanized", y="Value")

##### Bar plots/density of binary variables and 01 urban status

chester01_variables <- 
  chester %>%
  as.data.frame() %>%
  select(URBAN01, FOURLNE300, INTERST800, REGRAIL300, PARKS500M, WATER100, CITBORO_10) %>%
  gather(variable, value, -URBAN01)

ggplot(chester01_variables %>%
         group_by(URBAN01, variable) %>%
         summarize(sum = sum(value))) + 
  geom_bar(aes(as.factor(URBAN01), 
               sum, 
               fill=as.factor(URBAN01)),
           stat="identity") + 
  facet_wrap(~variable, scales = "fixed") +
  scale_fill_manual(values = c("light blue", "dark blue"),
                    labels = c("Not Urbanized","Urbanized"),
                    name = "") +
  ggtitle("Urbanized vs. Not Urbanized Raster Cell by Characteristic (2001)")+
  xlab("Urbanized vs Not Urbanized") + ylab("Count of Raster Cell Characteristic")
labs(x="Urbanized", y="Value")

#### 2. Create a new binary (0/1) variable, [CHNG_URB] indicating those 
#### raster cells that were either farmland or pasture land or forest in 1992, and which converted to urban uses by
#### 2001. Indicate these cells with a 1. All other cells should be coded to 0.

chester$CHNG_URB <- ifelse(chester$FARM92 == 1 | chester$PASTURE92 == 1 | chester$FOREST92 == 1 & chester$URBAN01 == 1, 1, 0)

#### 3. Build a best “lean & mean” binomial logit model that identifies the 
#### determinants of agricultural/pasture/forest land-to-urban land use change 
#### between 1992 and 2001 (This is the 0/1 variable created in step 2). 
#### Use whatever independent variables you think appropriate, and be sure to explain 
#### your logic and model development steps.

##### Correlation Matrix btw CHNG_URB and demographic variables
chest_demo<-select(chester, CHNG_URB,PCT_WHITE_, PCT_SFHOME, PCT_COLGRD, POPDEN90)
chest_demo <- as.data.frame(sapply(chest_demo, as.numeric))
chest_demo_cor <- cor(chest_demo)
ggcorrplot(chest_demo_cor, method = "circle", type = "lower", title = "Correlation Matrix",lab = T, lab_size = 2.5)

#Percent College Graduates seems to be the strongest indicator of urban change of the demographic variables.

##### Correlation Matrix btw CHNG_URB and infrastructure variables

chest_infra<-select(chester, CHNG_URB, FOURLNE300, INTERST800, REGRAIL300,RAILSTN100,CITBORO_10)
chest_infra <- as.data.frame(sapply(chest_infra, as.numeric))
chest_infra_cor <- cor(chest_infra)
ggcorrplot(chest_infra_cor, method = "circle", type = "lower", title = "Correlation Matrix", lab = T, lab_size = 2.5)

#Within 800m of an interstate and within 300m of regional rail seem to be the strongest indicators of urban change of the infrastructure variables.

##### Correlation Matrix btw CHNG_URB and distance variables
chest_dist<-select(chester, CHNG_URB, DIST_WATER, DIST_RAILS, DIST_REGRA, DIST_PASSR, DIST_4LNE_, DIST_INTER, DIST_PARKS)
chest_dist <- as.data.frame(sapply(chest_dist, as.numeric))
chest_dist_cor <- cor(chest_dist)
ggcorrplot(chest_dist_cor, method = "circle", type = "lower", title = "Correlation Matrix", lab = T, lab_size = 2.5)

#Distance to rail station and distance to regional rail line are perfectly correlated in this model because you cannot have a station without a line. We'll choose presence of a regional rail station between the two rail variables.
#Distance to interstate and distance to regional rail station are highly correlated in this model - since being located relatively close to a regional rail station is likely a better catalyst of urban growth than the presence of an interstate, we'll remove distance interstate from the model.
#Of the remaining variables, distance to rail station and distance to park are the strongest indicators of urban change.

chest_variables <- select(chester, CHNG_URB,PCT_COLGRD,INTERST800, REGRAIL300, DIST_RAILS, DIST_PARKS )
chest_variables <- as.data.frame(sapply(chest_variables, as.numeric))
chest_variables_cor <- cor(chest_variables)
ggcorrplot(chest_variables_cor, method = "circle", type = "lower", title = "Correlation Matrix", lab = T, lab_size = 2.5)

bptest(CHNG_URB ~ REGRAIL300 + DIST_RAILS, data = chester, studentize = F)
# p-value is less than 5%, therefore we reject the null hypothesis. There is heteroscedasticity. 
bptest(CHNG_URB ~ INTERST800 + DIST_RAILS, data = chester, studentize = F)
# p-value is less than 5%, therefore we reject the null hypothesis. There is heteroscedasticity

#The independent variables with the strongest positive relationships to urban change are distance to rail station and distance to parks. We'll build our binomial logit model starting with these variables.

mod <- glm (CHNG_URB ~ DIST_RAILS+DIST_PARKS, data=chester, family = binomial)
summary(mod)

#The p-values are both low, suggesting that the variables are significant in this model.

#Let's add in the variable for within 800m of an interstate.
mod1 <- glm (CHNG_URB ~ DIST_RAILS+DIST_PARKS+INTERST800, data=chester, family = binomial)
summary(mod1)

#The p-values are high for the interstate variable, suggesting that being within 800m of an interstate is not significant in this model. We'll remove it.
#Let's add in the variable for within 300m of a rail line.
mod2 <- glm (CHNG_URB ~ DIST_RAILS+DIST_PARKS+REGRAIL300, data=chester, family = binomial)
summary(mod2)

#The p-values are low for the rail line variable, suggesting that being within 300m of a rail line is significant. Being within 300 m of a rail line decreases a cell's probability of being urbanized by 58%.

#Let's add in the variable for percent college graduates.
mod3 <- glm (CHNG_URB ~ DIST_RAILS+DIST_PARKS+REGRAIL300+PCT_COLGRD, data=chester, family = binomial)
summary(mod3)

#The p-values are high for the college graduate variable, suggesting that percent of college graduates is not significant in this model. We'll remove it.

#Mod2 is our leanest and meanest model. 

exp(coef(mod2)) 
#percent change in the odds ratio 
100 * (exp(coef(mod2))-1)

#For every 1m increase in distance to rail station, there is a 0.006% increase in the odds ratio of having urban change.
#For every 1m increase in distance to a park, there is a 0.014% increase in the odds ratio of having urban change.
#If a cell is within 300m of a regional rail line, there is a 44.007% decrease in the odds ratio of having urban change.

## DEVIANCE
#Null deviance shows how well the response is predicted by a model 
##with nothing but an intercept (grand mean). 

pred <- as.data.frame(fitted(mod2))
pred <- rename(pred, "prob" = "fitted(mod2)")
pred <- mutate(pred, "binary" = ifelse(prob < 0.5, 0, 1))

chester$urbanbinary <- pred$binary
head(chester)

caret::confusionMatrix(reference = as.factor(chester$CHNG_URB), 
                       data = as.factor(chester$urbanbinary), 
                       positive = "1")
#The model has a 64% accuracy rate. 
# Its specificity (ability to predict true negatives) is 61%.
# Its sensitivity (ability to predict true positives) is 68%.

exp(confint.default(mod2)) 

#### 4. Explain the results of your best model in a few paragraphs for non-statisticians. 
#### How well does your model fit the data? Which factors are most important in explaining land
#### use change in Chester County from 1992-2001? How do you know? Do you have any ideas for improving the performance of your model?

#### 5. Include a few plots or tables that show the probabilities versus changes 
#### in key variables in your model.

#Distance to rail stations
chester_gg<-data.frame(matrix(ncol = 3, nrow = nrow(chest_variables)))
colnames(chester_gg)<- c("DIST_RAILS", "DIST_PARKS", "REGRAIL300")
chester_gg$DIST_RAILS <- chest_variables$DIST_RAILS
chester_gg$DIST_PARKS <- mean(chest_variables$DIST_PARKS)
chester_gg$REGRAIL300 <- 1

chester_gg1<-data.frame(matrix(ncol = 3, nrow = nrow(chest_variables)))
colnames(chester_gg1)<- c("DIST_RAILS", "DIST_PARKS", "REGRAIL300")
chester_gg1$DIST_RAILS <- chest_variables$DIST_RAILS
chester_gg1$DIST_PARKS <- mean(chest_variables$DIST_PARKS)
chester_gg1$REGRAIL300 <- 0

pred_dat<- data.frame(matrix(ncol = 3, nrow = nrow(chest_variables)))
colnames(pred_dat)<- c("DIST_RAILS", "Pred_RegRail300_Park5161m", "Pred_NORegRail300_Park5161m")
pred_dat$DIST_RAILS<-chest_variables$DIST_RAILS
pred_dat$Pred_RegRail300_Park5161m<- predict(mod2, chester_gg, type="response")
pred_dat$Pred_NORegRail300_Park5161m<- predict(mod2, chester_gg1, type="response")

dat <- gather(pred_dat, -DIST_RAILS, key = "Scenario", value = "value")

ggplot(dat, aes(x = DIST_RAILS, y = value, colour = Scenario)) + 
  geom_line() + ylim(0,1) +
  xlab("Distance to Rail Stations") + ylab("Predicted Probability of Urbanization")

plot(chest_variables$DIST_RAILS, chest_variables$CHNG_URB)
plot(jitter(chest_variables$CHNG_URB, factor=.8) ~ chest_variables$DIST_RAILS,
     pch= 8, cex = 0.4,col = "light blue",
     ylab="Urbanized",
     xlab="Distance to Rail Station",  main = "Probability of Urbanization vs Distance to Rail Stations")

fitted (mod2)
points (chest_variables$DIST_RAILS, fitted (mod2), col="dark blue")

#Distance to parks
chester_gg<-data.frame(matrix(ncol = 3, nrow = nrow(chest_variables)))
colnames(chester_gg)<- c("DIST_RAILS", "DIST_PARKS", "REGRAIL300")
chester_gg$DIST_RAILS <- mean(chest_variables$DIST_RAILS)
chester_gg$DIST_PARKS <- (chest_variables$DIST_PARKS)
chester_gg$REGRAIL300 <- 1

chester_gg1<-data.frame(matrix(ncol = 3, nrow = nrow(chest_variables)))
colnames(chester_gg1)<- c("DIST_RAILS", "DIST_PARKS", "REGRAIL300")
chester_gg1$DIST_RAILS <- mean(chest_variables$DIST_RAILS)
chester_gg1$DIST_PARKS <- (chest_variables$DIST_PARKS)
chester_gg1$REGRAIL300 <- 0

pred_dat<- data.frame(matrix(ncol = 3, nrow = nrow(chest_variables)))
colnames(pred_dat)<- c("DIST_RAILS", "Pred_RegRail300_Rail13233m", "Pred_NORegRail300_Rail13233m")
pred_dat$DIST_PARKS<-chest_variables$DIST_PARKS
pred_dat$Pred_RegRail300_Rail13233m<- predict(mod2, chester_gg, type="response")
pred_dat$Pred_NORegRail300_Rail13233m<- predict(mod2, chester_gg1, type="response")

dat <- gather(pred_dat, -DIST_PARKS, key = "Scenario", value = "value")

ggplot(dat, aes(x = DIST_PARKS, y = value, colour = Scenario)) + 
  geom_line() + ylim(0,1) +
  xlab("Distance to Parks") + ylab("Predicted Probability of Urbanization")

plot(chest_variables$DIST_PARKS, chest_variables$CHNG_URB)
plot(jitter(chest_variables$CHNG_URB, factor=.8) ~ chest_variables$DIST_PARKS,
     pch= 8, cex = 0.4,col = "light blue",
     ylab="Urbanized",
     xlab="Distance to Parks", main = "Probability of Urbanization vs Distance to Parks")

fitted (mod2)
points (chest_variables$DIST_PARKS, fitted (mod2), col="dark blue")

