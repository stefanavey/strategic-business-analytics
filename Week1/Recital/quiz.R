##########
## Quiz ##
##########
library(tidyverse)
library(aveytoolkit)

## 1
sku <- read.csv("DATA_2.01_SKU.csv")
mean(sku$CV)
median(sku$CV)

## 2
scaledat <- scale(sku)
d <- dist(scaledat, method = "euclidean")
hcward <- hclust(d, method = "ward.D")
sku$groups <- cutree(hcward, k = 2)

## Plot the groups
gg <- ggplot(data = sku, mapping = aes(x = CV, y = ADS, color = factor(groups))) +
  geom_point() +
  geom_hline(yintercept = 4, color = "red") +
  geom_vline(xintercept = 0.2, color = "red") +
  scale_color_discrete(guide = guide_legend(title = "Group")) +
  xlab("Sales Volatility (CV)") +
  ylab("Sales Volume (ADS)") +
  getBaseTheme()
plot(gg)

## 3
hr <- read.csv("DATA_2.02_HR.csv")

ggplot(data = hr, mapping = aes(y = NP, x = LPE)) +
  geom_point()

## 4
dat2 <- hr %>% select(-Newborn)
datn <- scale(dat2)

d <- dist(datn, method = "euclidean")
hcward <- hclust(d, method = "ward.D")

plot(hcward)                            # look at dendrogram

dat2$groups <- cutree(hcward, k = 2)

## Summarize each variable by the mean
dat2 %>%
group_by(groups) %>%
summarize_each(funs(median))

## 5
dat <- read.csv("DATA_2.03_Telco.csv")

## Find groups using hierarhical clustering
datn <- scale(dat)
d <- dist(datn, method = "euclidean")
hcward <- hclust(d, method = "ward.D")
dat$groups <- cutree(hcward, k = 5) # assign our points to our 5 clusters

## What percent of data is in each cluster?
aggdat <- dat %>%
group_by(groups) %>%
mutate(Proportion = n() / nrow(dat)) %>%
summarize_each(funs(mean))

show(aggdat)

