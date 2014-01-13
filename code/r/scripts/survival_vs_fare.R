# install.packages("devtools")
# library("devtools")
# install.packages("microbenchmark")
# install.packages("Lahman")
# install.packages('hflights')
# install.packages('RSQLite')
# install.packages('RSQLite.extfuns')
# devtools::install_github("dplyr")
# vignette("introduction", package = "dplyr")
#
# devtools::install_github("plotly/R-api")

library(dplyr)
library(data.table)
library(ggplot2)
library(reshape2)
source('accounts.R')

d <- data.table(read.csv('../../data/train.csv'))

ggplot(d, aes(Fare, color=Sex)) +
  geom_histogram(binwidth=0.05) +
  scale_x_log10()

ggplot(d, aes(Fare)) +
  geom_histogram(binwidth=0.05) +
  scale_x_log10()

################
dm <- d[Fare > 4]

breaks <- unique(c(
  seq(4, 10, by=1),
  seq(10, 30, by=1),
  seq(30, 40, by=0.5),
  seq(40, 70, by=0.5),
  seq(70, 80, by=1),
  seq(80, 100, by=2),
  seq(100, 200, by=0.5),
  seq(200, 515, by=5)
))

#breaks <- seq(floor(min(d$Fare)), ceiling(max(d$Fare)), by=0.9)
#breaks <- ceiling(length(as.integer(dm$Fare))/5)

# cut and summarize
dm$bin <- cut(dm$Fare, breaks=breaks, labels=seq(2, length(breaks)))
ds <- mutate(summarise(group_by(dm, Sex, bin), total=n(), survived=sum(Survived)), survived.frac=survived/total)

ds$bin <- as.integer(as.character(ds$bin))
#ds$bin <- as.numeric(as.factor(ds$bin))

ggplot(ds, aes(bin, survived.frac, color=Sex)) +
  geom_point(data=ds[Sex == 'male'],
             aes(bin,
                 loess(survived.frac ~ bin, span=0.6,
                       family='gaussian')$fitted)) +
  geom_point(data=ds[Sex == 'female'], 
             aes(bin,
                 loess(survived.frac ~ bin, span=0.6,
                       family='gaussian')$fitted)) +
  scale_x_continuous(trans='log10')

# plotly
library(plotly)

py <- do.call(plotly, plotly_info)

data0 <- list(
  name = 'male',
  x = ds$bin,
  y = with(ds[Sex == 'male'], loess(survived.frac ~ bin, span=0.6,
                                      family='gaussian')$fitted),
  marker = list(
    #symbol = 'cross',
    color = 'green'
  ),
  type = 'scatter',
  mode = 'markers'
)
data1 <- list(
  name = 'female',
  x = ds$bin,
  y = with(ds[Sex == 'female'], loess(survived.frac ~ bin, span=0.6,
                                      family='gaussian')$fitted),
  marker = list(
    #symbol = 'cross',
    color = 'red'
  ),
  type = 'scatter',
  mode = 'markers'
)

layout <- list(
  title = 'Fare vs Survival rate',
  xaxis = list(
    type = 'log'
  )
)

response = py$plotly(data0, data1, kwargs=list(layout=layout))
browseURL(response$url)
