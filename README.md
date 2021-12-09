# IndependentProject
```r
library(tidyverse)
library(skimr)
library(gapminder)
library(Stat2Data)
leaf = read.csv("~/desktop/LeafWidth.csv")
## 1.15
leaf %>%
  summarize(mean_Width_avg = mean(Width))

ggplot(leaf, aes(x = Year, y = Width)) + 
  geom_point() +
  stat_smooth(method = lm, se = FALSE)

fm = lm(Width ~ Year, data = leaf)
summary(fm)

## 1.16
data("GlowWorms")
View(GlowWorms)

regressionWorm = lm(Eggs ~ Lantern, data = GlowWorms)
summary(regressionWorm)
ggplot(GlowWorms, aes(x = Lantern, y = Eggs)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE)

## 1.20
data("GrinnellHouses")
View(GrinnellHouses)
house = GrinnellHouses
View(house)

ggplot(house, aes(x = ListPrice, y = SalePrice)) +
  geom_point()

regressionHouse = lm(SalePrice ~ ListPrice, data = house)
summary(regressionHouse)
# residual plot
resHouse = resid(regressionHouse)
plot(fitted(regressionHouse), resHouse)
abline(0,0)

## 1.24
data("Sparrows")
bird = Sparrows
View(bird)
ggplot(bird, aes(x = Weight, y = WingLength)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE)

birdModel = lm(WingLength ~ Weight, data = bird)
summary(birdModel)
# histogram of residuals
ggplot(bird, aes(x = birdModel$residuals)) +
  geom_histogram()
# normal probability plot of residuals
bird.stdres = rstandard(birdModel)
qqnorm(bird.stdres)

## 1.26
data("RailsTrails")
train = RailsTrails
ggplot(train, aes(x = SquareFeet, y = Adj2007)) + 
  geom_point()
# linear regression model
regressionTrain = lm(Adj2007 ~ SquareFeet, data = train)
summary(regressionTrain)
ggplot(train, aes(x = SquareFeet, y = Adj2007)) + 
  geom_point() +
  stat_smooth(method = lm, se = FALSE)
# residual plot
resTrain = resid(regressionTrain)
plot(fitted(regressionTrain), resTrain)
abline(0,0)
# normal probability plot of residuals
train.stdres = rstandard(regressionTrain)
qqnorm(train.stdres)
qqline(train.stdres, col = "steelblue", lwd = 2)
# histogram of residuals
ggplot(train, aes(x = regressionTrain$residuals)) +
  geom_histogram()

## 1.29
data("Caterpillars")
bug = Caterpillars
View(bug)
# scatterplot of WetFrass on Mass
bugModel = lm(WetFrass ~ Mass, data = bug)
summary(bugModel)
ggplot(data = bug, aes(x = Mass, y = WetFrass)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE)
# Log graph
ggplot(data = bug) +
  geom_point(aes(x = log(Mass), y = log(WetFrass)))
# predictor
bugModel = lm(LogWetFrass ~ LogMass, data = bug)
ggplot(bugModel, aes(x = bug$LogMass, y = bug$LogWetFrass)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE)
# add grouping variable
ggplot(bugModel, aes(x = bug$LogMass, y = bug$LogWetFrass, col = Instar)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE)
ggplot(bug) +
  geom_point(aes(x = bug$LogMass, y = bug$LogWetFrass, col = Fgp))

## 1.33
stamp = read.csv("~/downloads/USStamp.csv")
stampLM = lm(Price ~ Year, data = stamp)
ggplot(stampLM, aes(x = Year, y = Price)) +
  geom_point()
# remove first four observations
stampNew = stamp %>%
  slice(5:45)
stampLM1 = lm(Price ~ Year, data = stampNew)
ggplot(stampLM1, aes(x = Year, y = Price)) +
  geom_point() + stat_smooth(method = lm, se = FALSE)
summary(stampLM1)
# plot residuals
stampRes = resid(stampLM1)
plot(fitted(stampLM1), stampRes)
abline(0,0)

## 1.44
text = TextPrices
View(text)
ggplot(text, aes(x = Pages, y = Price)) +
  geom_point()
# regression line
textRes = lm(Price ~ Pages, text)
summary(textRes)
ggplot(text, aes(x = Pages, y = Price)) +
  geom_point() + stat_smooth(method = lm, se = FALSE)
plot(textRes)
# Residual plot
textResid = resid(textRes)
plot(fitted(textRes), textResid)
abline(0,0)
hist(textResid)
```
