#################################################################################
## Foundation to Strategic Business Analytics                                  ##
## Module 2 - Finding groups within data                                       ##
#################################################################################
library(tidyverse)
library(lattice)
library(aveytoolkit)
library(png)
library(grid)

########################
## Stock Keeping Unit ##
########################
dat <- read.csv("DATA_2.01_SKU.csv")
str(dat)
summary(dat)

## Plot the data to look for groups
gg1 <- ggplot(data = dat, mapping = aes(x = CV, y = ADS)) +
  geom_point() +
  geom_hline(yintercept = 4, color = "red") +
  geom_vline(xintercept = 0.2, color = "red") +
  xlab("Sales Volatility (CV)") +
  ylab("Sales Volume (ADS)") +
  getBaseTheme()
plot(gg1)

## Add in segment images
cricket <- readPNG("cricket.png")
g_cricket <- rasterGrob(cricket, interpolate = TRUE)
horse <- readPNG("horse.png")
g_horse <- rasterGrob(horse, interpolate = TRUE)
bull <- readPNG("bull.png")
g_bull <- rasterGrob(bull, interpolate = TRUE)

pdf("SKU-labeled-groups.pdf")
gg2 <- gg1 +
  geom_text(data = data.frame(CV = 0.1, ADS = 9.5), aes(label = "Horses")) +
  annotation_custom(g_horse, xmin = 0, xmax = 0.2, ymin = 7, ymax = 9) +
  geom_text(data = data.frame(CV = 0.6, ADS = 9.5), aes(label = "Wild Bulls")) +
  annotation_custom(g_bull, xmin = 0.5, xmax = 0.8, ymin = 7, ymax = 9) +
  geom_text(data = data.frame(CV = 0.85, ADS = 3), aes(label = "Crickets")) +
  annotation_custom(g_cricket, xmin = 0.7, xmax = 1, ymin = 0, ymax = 4)
plot(gg2)
dev.off()

## Find groups using hierarhical clustering
testdat <- scale(dat)

d <- dist(testdat, method = "euclidean")
hcward <- hclust(d, method = "ward.D")
dat$groups <- cutree(hcward, k = 3)

## Plot the groups
pdf("SKU-hclust.pdf")
gg3 <- ggplot(data = dat, mapping = aes(x = CV, y = ADS, color = factor(groups))) +
  geom_point() +
  geom_hline(yintercept = 4, color = "red") +
  geom_vline(xintercept = 0.2, color = "red") +
  scale_color_discrete(guide = guide_legend(title = "Group")) +
  xlab("Sales Volatility (CV)") +
  ylab("Sales Volume (ADS)") +
  getBaseTheme()
plot(gg3)
dev.off()

###############################
## Human Resources Analytics ##
###############################
dat <- read.csv("DATA_2.02_HR.csv")
str(dat)
summary(dat)

dat2 <- dat %>% select(-Newborn)
datn <- scale(dat2)

d <- dist(datn, method = "euclidean")
hcward <- hclust(d, method = "ward.D")

plot(hcward)                            # look at dendrogram

dat2$groups <- cutree(hcward, k = 4)

## Summarize each variable by the mean
dat2 %>%
group_by(groups) %>%
summarize_each(funs(mean))

## What percent of data is in each cluster?
dat2 %>%
group_by(groups) %>%
mutate(Proportion = n() / nrow(dat2)) %>%
summarize_each(funs(mean))

## Rather than only looking at the means, I'd want to see the full distribution
## of each variable compared in each of the groups as a boxplot
pdf("HR-group-comparison.pdf")
plotDat <- dat2 %>%
gather(key = "Variable", value = "Value", -groups) %>%
mutate(groups = factor(groups))

ggplot(data = plotDat) +
geom_boxplot(aes(x = groups, y = Value, color = groups)) +
scale_color_discrete(guide = guide_legend(title = "Group")) +
facet_grid(Variable ~ ., scales = "free_y")
dev.off()

########################
## Telecommunications ##
########################
dat <- read.csv("DATA_2.03_Telco.csv")
str(dat)
summary(dat)

## Find groups using hierarhical clustering
datn <- scale(dat)

d <- dist(datn, method = "euclidean")
hcward <- hclust(d, method = "ward.D")

dat$groups <- cutree(hcward, k = 8) # assign our points to our k=8 clusters

## Summarize each variable by the mean
dat %>%
group_by(groups) %>%
summarize_each(funs(mean))

## What percent of data is in each cluster?
dat %>%
group_by(groups) %>%
mutate(Proportion = n() / nrow(dat)) %>%
summarize_each(funs(mean))

## Let's try again with 5 segments
dat$groups <- cutree(hcward, k = 5) # assign our points to our 5 clusters

## What percent of data is in each cluster?
aggdat <- dat %>%
group_by(groups) %>%
mutate(Proportion = n() / nrow(dat)) %>%
summarize_each(funs(mean))

show(aggdat)

## Let's draw the radar chart with the function stars()
palette(rainbow(12, s = 0.6, v = 0.75)) # Select the colors to use
stars(aggdat[,2:(ncol(dat))], len = 0.6, key.loc = c(11, 6),xlim=c(2,12),main = "Segments", draw.segments = TRUE,nrow = 2, cex = .75,labels=aggdat$groups)

## I think this is a terrible plot, instead what about a bar chart to start?

pdf("Telco-group-comparison.pdf")

plotDat <- dat %>%
group_by(groups) %>%
summarize_each(funs(mean)) %>%
arrange(Age) %>%
mutate(Group = LETTERS[1:5]) %>%
gather(key = "Variable", value = "Value", 2:6)

ggplot(data = plotDat) +
geom_bar(stat = "identity", aes(x = Group, y = Value, fill = Group)) +
scale_fill_discrete(guide = FALSE) +
facet_grid(Variable ~ ., scales = "free_y")
dev.off()

