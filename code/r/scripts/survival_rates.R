library(dplyr)
library(data.table)
library(ggplot2)
library(reshape2)

d <- data.table(read.csv('../../data/original/train.csv'))

## by age, 5 year bins
breaks <- with(d, seq(floor(min(Age, na.rm=T)), ceiling(max(Age, na.rm=T)), by=5))
d$age.bin <- cut(d$Age, breaks=breaks, labels=breaks[1:length(breaks)-1])
d <- d[!is.na(age.bin)]

# overall
ds <- mutate(summarise(group_by(d, age.bin), total=n(), survived=sum(Survived)), survived.frac=survived/total)
ds$bin <- as.integer(as.character(ds$age.bin))

ggplot(ds, aes(age.bin, survived.frac, group=1)) +
  geom_point(alpha=0.5) +
  geom_smooth(method=loess, se=F)

# by sex/age
ds <- mutate(summarise(group_by(d, Sex, age.bin), total=n(), survived=sum(Survived)), survived.frac=survived/total)
ds$bin <- as.integer(as.character(ds$age.bin))

ggplot(ds, aes(age.bin, survived.frac, color=Sex, group=Sex)) +
  geom_point(alpha=0.5) +
  geom_smooth(method=loess, se=F)

# by sex/age/Pclass
ds <- mutate(summarise(group_by(d, Sex, Pclass, age.bin), total=n(), survived=sum(Survived)), survived.frac=survived/total)
ds$bin <- as.integer(as.character(ds$age.bin))

ggplot(ds[!is.na(age.bin)], aes(age.bin, survived.frac, color=as.factor(Pclass), group=Pclass)) +
  geom_point(alpha=0.5) +
  facet_wrap(~Sex) +
  geom_smooth(method=loess, se=F)

# by sex/age/Perch
ds <- mutate(summarise(group_by(d, Sex, Parch, age.bin), total=n(), survived=sum(Survived)), survived.frac=survived/total)
ds$bin <- as.integer(as.character(ds$age.bin))

ggplot(ds[Parch %in% c(0, 1, 2)], aes(age.bin, survived.frac, color=as.factor(Sex), group=Sex)) +
  geom_point(alpha=0.5) +
  facet_wrap(~Parch) +
  geom_smooth(method=loess, se=F)

# by sex/age/SibSp
breaks <- with(d, seq(floor(min(Age, na.rm=T)), ceiling(max(Age, na.rm=T)), by=1))
d$age.bin <- cut(d$Age, breaks=breaks, labels=breaks[1:length(breaks)-1])
d <- d[!is.na(age.bin)]

ds <- mutate(summarise(group_by(d, Sex, SibSp, age.bin), total=n(), survived=sum(Survived)), survived.frac=survived/total)
ds$bin <- as.integer(as.character(ds$age.bin))

ggplot(ds[SibSp %in% c(0, 1, 2)], aes(age.bin, survived.frac, color=as.factor(Sex), group=Sex)) +
  geom_point(alpha=0.5) +
  facet_wrap(~SibSp,ncol=1) +
  geom_smooth(method=loess, span=1, se=F)





# FARE
## by fare, 5 year bins
breaks <- with(d, seq(floor(min(Fare, na.rm=T)), ceiling(max(Fare, na.rm=T)), by=5))
d$fare.bin <- cut(d$Fare, breaks=breaks, labels=breaks[1:length(breaks)-1])
d <- d[!is.na(fare.bin)]
# overall fare
ds <- mutate(summarise(group_by(d, fare.bin), total=n(), survived=sum(Survived)), survived.frac=survived/total)

ggplot(ds, aes(fare.bin, survived.frac, group=1)) +
  geom_point(alpha=0.5) +
  geom_smooth(method=loess)

# by sex/fare
ds <- mutate(summarise(group_by(d, Sex, fare.bin), total=n(), survived=sum(Survived)), survived.frac=survived/total)
ds$bin <- as.integer(as.character(ds$fare.bin))

ggplot(ds, aes(fare.bin, survived.frac, color=Sex, group=Sex)) +
  geom_point(alpha=0.5) +
  geom_smooth(method=loess, se=F)

# by sex/fare/Pclass
ds <- mutate(summarise(group_by(d, Sex, Pclass, fare.bin), total=n(), survived=sum(Survived)), survived.frac=survived/total)
ds$bin <- as.integer(as.character(ds$fare.bin))

ggplot(ds[!is.na(fare.bin)], aes(fare.bin, survived.frac, color=as.factor(Pclass), group=Pclass)) +
  geom_point(alpha=0.5) +
  facet_wrap(~Sex) +
  geom_smooth(method=loess, se=F)

# by sex/fare/Perch
ds <- mutate(summarise(group_by(d, Sex, Parch, fare.bin), total=n(), survived=sum(Survived)), survived.frac=survived/total)
ds$bin <- as.integer(as.character(ds$fare.bin))

ggplot(ds[Parch %in% c(0, 1, 2)], aes(fare.bin, survived.frac, color=as.factor(Sex), group=Sex)) +
  geom_point(alpha=0.5) +
  facet_wrap(~Parch) +
  geom_smooth(method=loess, se=F)

# by sex/fare/SibSp
breaks <- with(d, seq(floor(min(Fare, na.rm=T)), ceiling(max(Fare, na.rm=T)), by=1))
d$fare.bin <- cut(d$Fare, breaks=breaks, labels=breaks[1:length(breaks)-1])
d <- d[!is.na(fare.bin)]

ds <- mutate(summarise(group_by(d, Sex, SibSp, fare.bin), total=n(), survived=sum(Survived)), survived.frac=survived/total)
ds$bin <- as.integer(as.character(ds$fare.bin))

ggplot(ds[SibSp %in% c(0, 1, 2)], aes(fare.bin, survived.frac, color=as.factor(Sex), group=Sex)) +
  geom_point(alpha=0.5) +
  facet_wrap(~SibSp,ncol=1) +
  geom_smooth(method=loess, span=1, se=F)


breaks <- with(d, seq(floor(min(Age, na.rm=T)), ceiling(max(Age, na.rm=T)), by=5))
d$age.bin <- cut(d$Age, breaks=breaks, labels=breaks[1:length(breaks)-1])
breaks <- with(d, seq(floor(min(Fare, na.rm=T)), ceiling(max(Fare, na.rm=T)), by=5))
d$fare.bin <- cut(d$Fare, breaks=breaks, labels=breaks[1:length(breaks)-1])

#library(hexbin)
ggplot(d[!is.na(age.bin) & !is.na(fare.bin)], aes(fare.bin, age.bin)) +
  geom_bin2d() +
  facet_wrap(~Sex)