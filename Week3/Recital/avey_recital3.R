#################################################################################
## Foundation to Strategic Business Analytics                                  ##
## Module 2 - Predicting and Forecasting                                       ##
#################################################################################
library(tidyverse)
library(aveytoolkit)
library(survival)

options(stringsAsFactors = FALSE)

#################################################################################
## Credit Scoring 2                                                            ##
#################################################################################

credit <- read.csv("DATA_3.01_CREDIT.csv")
credit2 <- read.csv("DATA_4.01_CREDIT2.csv")

## Build model to predict Credit Rating
lmod <- lm(Rating ~ ., credit)

credit <- mutate(credit, Rating_fitted = predict(lmod, newdata = credit))
credit2 <- mutate(credit2, Rating_fitted = predict(lmod, newdata = credit2))

## Original Dataset
lab <- paste0("Corr: ", round(cor(credit$Rating, credit$Rating_fitted), 2))
p1 <- ggplot(data = credit, aes(x = Rating, y = Rating_fitted)) +
  geom_point() +
  geom_smooth(color = "blue", method = lm, se = FALSE) +
  geom_text(data = data.frame(Rating = 250, Rating_fitted = 800),
            label = lab, color = "blue", size = 5) +
  ggtitle("Fitted") +
  getBaseTheme()

## New Dataset
lab <- paste0("Corr: ", round(cor(credit2$Rating, credit2$Rating_fitted), 2))
p2 <- ggplot(data = credit2, aes(x = Rating, y = Rating_fitted)) +
  geom_point() +
  geom_smooth(color = "blue", method = lm, se = FALSE) +
  geom_text(data = data.frame(Rating = 250, Rating_fitted = 800),
            label = lab, color = "blue", size = 5) +
  ggtitle("Out-of-Sample") +
  getBaseTheme()

Multiplot(p1, p2)

#################################################################################
## HR Analytics 3                                                              ##
#################################################################################

hr2 <- read.csv("DATA_3.02_HR2.csv")
hr3 <- read.csv("DATA_4.02_HR3.csv")

str(hr2)
str(hr3)

## Fit logistic regressio model
logmod <- glm(left ~ ., family = binomial(logit), data = hr2)

hr3 <- hr3 %>%
  mutate(probaToLeave = predict(logmod, newdata = hr3),
         performance = LPE)

## Prioritize who to try to retain (high performance, high probability to leave)
hr3 <- hr3 %>%
  tbl_df() %>%
  mutate(priority = performance * probaToLeave) %>%
  arrange(-priority)

## Plot probability to leave vs performance
p3 <- ggplot(data = hr3, aes(x = probaToLeave, y = performance)) +
  geom_point() +
  xlab("Probability to Leave\n(low -> high)") +
  ylab("Performance (LPE)") +
  getBaseTheme()
plot(p3)


## Add priority to the plot
p4 <- p3 +
  geom_point(aes(color = priority))
plot(p4)

## Add segments to the plot
pdf("HR_Analytics_Segmented.pdf")
p5 <- p3 +
  annotate("rect", xmin = -1.25, xmax = 2.5, ymin = 0.6, ymax = 1,
           fill = "red", alpha = 0.2) +
  annotate("rect", xmin = -7, xmax = -1.25, ymin = 0.6, ymax = 1,
           fill = "green", alpha = 0.2) +
  annotate("rect", xmin = -7, xmax = 2.5, ymin = 0.35, ymax = 0.6,
           fill = "orange", alpha = 0.2) +
  annotate("text", x = -6, y = 0.8, label = "Maintain", size = 7) +
  annotate("text", x = -6, y = 0.45, label = "Up or Out", size = 7) +
  annotate("text", x = 1.25, y = 0.98, label = "Retain", size = 7)
plot(p5)
dev.off()

#################################################################################
## Predictive Maintenance                                                      ##
#################################################################################

mnt <- read.csv("DATA_4.03_MNT.csv") %>%
  mutate_if(is.character, as.factor)
str(mnt)

lmod <- lm(lifetime ~ . -broken, data = mnt)
summary(lmod)

response <- Surv(time = mnt$lifetime, event = mnt$broken)
survmod <- survreg(response ~ pressureInd + moistureInd +
                     temperatureInd + team + provider,
                   dist = "gaussian", data = mnt)
summary(survmod)

forecast <- mnt %>%
  mutate(expectedLifetime = predict(survmod, newdata = mnt,
             type = "quantile", p = 0.5)) %>%
  mutate(RemainingLT = expectedLifetime - lifetime) %>%
  filter(broken == 0) %>%
  arrange(RemainingLT)

ggplot(data = forecast) +
  geom_point(aes(x = lifetime, y = expectedLifetime)) +
  getBaseTheme()
  
  


survfit(response ~ pressureInd + moistureInd + temperatureInd + team + provider,
        data = mnt)
