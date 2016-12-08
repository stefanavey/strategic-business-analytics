#################################################################################
## Foundation to Strategic Business Analytics                                  ##
## Module 3 - Factors leading to events                                        ##
#################################################################################
library(tidyverse)
library(aveytoolkit)
library(GGally)
library(pROC)

options(stringsAsFactors = FALSE)
###################################################################################
## Creit Scoring Data                                                            ##
###################################################################################
credit <- read.csv("DATA_3.01_CREDIT.csv")
str(credit)

## What is the distribution of Credit Ratings?
ggplot(data = credit) +
    geom_histogram(aes(x = Rating), bins = 50) +
    getBaseTheme()

## What are the correlations between each column and Credit Rating?
credit %>%
    tbl_df() %>%
    select_if(is.numeric) %>%
    cor(credit$Rating, .)

## Fit a linear regression to determine what factors are important to predict Rating
lmod <- lm(Rating ~ ., data = credit)

credit2 <- data.frame(credit, Rating_fitted = lmod$fitted.values)

## Rating vs fitted Rating
lab <- paste0("Corr: ", round(cor(credit2$Rating, credit2$Rating_fitted), 2))
ggplot(data = credit2, aes(x = Rating, y = Rating_fitted)) +
    geom_point() +
    geom_smooth(color = "blue", method = lm, se = FALSE) +
    geom_text(data = data.frame(Rating = 250, Rating_fitted = 800),
              label = lab, color = "blue", size = 5) +
    getBaseTheme()

## Balance vs Rating
lab <- paste0("Corr: ", round(cor(credit2$Rating, credit2$Balance), 2))
ggplot(data = credit2, aes(x = Balance, y = Rating)) +
    geom_point() +
    geom_smooth(color = "blue", method = lm, se = FALSE) +
    geom_text(data = data.frame(Rating = 250, Balance = 1500),
              label = lab, color = "blue", size = 5) +
    getBaseTheme()

## Income vs Rating
lab <- paste0("Corr: ", round(cor(credit2$Rating, credit2$Income), 2))
ggplot(data = credit2, aes(x = Income, y = Rating)) +
    geom_point() +
    geom_smooth(color = "blue", method = lm, se = FALSE) +
    geom_text(data = data.frame(Rating = 250, Income = 100),
              label = lab, color = "blue", size = 5) +
    getBaseTheme()

###################################################################################
## HR Analytics 2                                                                ##
###################################################################################

hr <- read.csv("DATA_3.02_HR2.csv")
str(hr)

## What factors correlate with leaving the company?
hr %>%
  tbl_df() %>%
  cor(hr$left, .)
## No single factor has a strong correlation

## Together can they explain leaving the company?
logmod <- glm(left ~ ., data = hr, family = binomial(logit))
summary(logmod)
## Everything is "significant" because we have a large sample size and the model
## is likely overfitting

## Compare fitted and actual values
cor(logmod$fitted.values, hr$left)

## Calculate the AUC
preds <- predict.glm(logmod, newdata = hr, type = "response")
rc <- roc(response = hr$left, predictor = preds)
plot(rc)
## AUC: 0.85

## ggpairs(hr)                             # ggpairs plot is not too useful here

## Most important drivers: TIC (Time In Company), S (Satistifaction)

## Plot TIC vs attrition
ggplot(data = hr, aes(x = TIC, y = left)) +
  geom_point()

## Can't see anything because they are both discrete variables so:

## Group by TIC
plotDat <- hr %>%
  group_by(TIC) %>%
  summarize(attrition = mean(left))
ggplot(data = plotDat, aes(x = TIC, y = attrition)) +
  geom_bar(stat = "identity") +
  getBaseTheme()

## Look at distribution of satisfaction (S)
ggplot(data = hr, aes(x = S)) +
  geom_histogram()

## Specify the number of equally sized bins
bins <- 20
breaks <- seq(0, 1, length.out = bins + 1)
plotDat <- hr %>%
  ## mutate(Satisfaction = as.numeric(cut(S, bins))) %>%
  mutate(Satisfaction = as.numeric(cut(S, breaks = breaks))) %>%  
  group_by(Satisfaction) %>%
  summarize(attrition = mean(left), num.employees = n())
ggplot(data = plotDat, aes(x = Satisfaction, y = attrition)) +
  geom_count(aes(size = num.employees)) +
  scale_size_area() +
  scale_x_continuous(limits = c(1, bins),
                     breaks = c(1, round(bins/2), bins),
                     labels = c("low", "moderate", "high")) +
  guides(size = guide_legend(title = "# Employees")) +
  xlab("Satisfaction\n(low -> high)") +
  ylab("Average Attrition") +
  getBaseTheme()


