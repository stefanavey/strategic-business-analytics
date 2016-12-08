############
## Quiz 1 ##
############

##############
## Packages ##
##############
library(tidyverse)

fname <- "PASTPURCHASE.csv"
dat <- read.csv(fname)

## What is the correct mean and standard deviation of the quantity of pasta purchased by time unit by household?
mean(dat$PASTA)
sd(dat$PASTA)

## In which area are located (i) the poorest household and (ii) the wealthiest household?
dat %>%
  filter(INCOME == max(INCOME)) %>%
  select(AREA) %>%
  distinct()

dat %>%
  filter(INCOME == min(INCOME)) %>%
  select(AREA) %>%
  distinct()

## What is the maximum pasta quantity a household has bought over the whole time period? (Sum the quantity of pasta by household over time and indicate the maximum)
dat %>%
  group_by(HHID) %>%
  summarize(total = sum(PASTA)) %>%
  ungroup() %>%
  top_n(n = 1, wt = total)

## What is the average income of households living in area 4?
dat %>%
  filter(AREA == 4) %>%
  summarize(avgInc = mean(INCOME))

## How many households live in area 2, earn more than 20k, and have purchased more than 30 units of pasta over the whole time period?
dat %>%
  filter(AREA == 2, INCOME > 20000) %>%
  group_by(HHID) %>%
  summarize(total = sum(PASTA)) %>%
  ungroup() %>%
  filter(total > 30) %>%
  nrow

## What is the correlation between the purchases of pasta and the exposures?
cor(dat$PASTA, dat$EXPOS)


## Which of the following graphs reports the correct histogram by household of the total purchase of pasta made by the household over the whole period? (Sum the purchases by household and make a histogram.)
plotDat <- dat %>%
  group_by(HHID) %>%
  summarize(total = sum(PASTA))
ggplot(data = plotDat, aes(x = total)) +
  geom_histogram()

## Which of the following graphs reports the correct time series of the overall total purchase of pasta? (Sum the purchases by time units and plot the quantity by time unit.)
plotDat <- dat %>%
  group_by(TIME) %>%
  summarize(PastaPerTimeUnit = sum(PASTA))
ggplot(data = plotDat, aes(x = TIME, y = PastaPerTimeUnit)) +
  geom_point()


