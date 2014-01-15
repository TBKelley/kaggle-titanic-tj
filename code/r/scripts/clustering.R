library(data.table)
library(ggplot2)
library(dplyr)
library(reshape2)
#source('scripts/myplclust.R')
source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R")

#d <- read.csv('../../data/original/train.csv')
d <- as.data.table(read.csv('../../data/original/train.csv'))
d <- d[, list(PassengerId, Survived, Pclass, Sex, SibSp, Parch, Age, Fare)]

ggplot(d, aes(Age, Fare, color=as.factor(Survived))) +
  geom_point() +
  facet_wrap(~Sex + Pclass, scales='free_y') 

ggplot(d, aes(Age, color=as.factor(Survived))) +
  geom_histogram(, binwidth=4) +
  facet_wrap(~Pclass + Sex)





breaks <- c(4, 10, 30, 40, 70, 80, 100, 200, 515)
#breaks <- with(d, seq(floor(min(Fare)), ceiling(max(Fare)), by=1))
d$fare.b <- cut(d$Fare, breaks, labels=breaks[2:length(breaks)])
age.breaks <- c(0, 16, 20, seq(25, ceiling(max(d$Age, na.rm=T)) + 10, by=10))
d$age.b <- cut(d$Age, age.breaks, labels=breaks[2:length(age.breaks)])

# clustering based on average fare of males
#distanceMatrix <- dist(d[d$Sex == 'male', c('Survived', 'Pclass', 'fare.b')])

x <- d[Sex == 'male', list(Survived, age.b, Pclass, fare.b)]
x <- as.data.table(dcast(x, fare.b + Pclass + age.b ~ Survived))
x$fare.b <- as.integer(as.character(x$fare.b))
x$age.b <- as.integer(as.character(x$age.b))
#x$Sex <- as.integer(as.character(x$Sex))

x <- x[!is.na(fare.b)]
x <- x[!is.na(age.b)]
x <- as.data.table(melt(x, id.vars=c('fare.b', 'age.b', 'Pclass')))
x$variable <- as.integer(as.character(x$variable))
x <- x[value != 0]
#install.packages('gplots')
#install.packages('made4',type='source')
#require(made4)
require(gplots)
heatmap.2(as.matrix(x[,fare.b, age.b]))



d.male <- d[Sex == 'male']
plot(t(table(d.male$Survived, d.male$Pclass)))

ggplot(d.male, aes(Age, Fare, color=as.factor(Survived))) +
  geom_point() +
  facet_wrap(~Pclass,scales='free_y')

ggplot(d.male[!(Parch %in% c(3,4,5))], aes(Age, Fare, color=as.factor(Survived))) +
  geom_density2d() +
  facet_wrap(~Pclass + Parch, scales='free_y')

distxy <- dist(d.male)
hClustering <- hclust(distxy)
plot(hClustering)