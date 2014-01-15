Visualizations
==============

Load requirements.  

```r
library(dplyr)
library(data.table)
library(ggplot2)
library(reshape2)

d <- data.table(read.csv("../../../data/original/train.csv"))
```


Survival rate based visualizations
----------------------------------

### By age, bin width = 5 years


```r
breaks <- with(d, seq(floor(min(Age, na.rm = T)), ceiling(max(Age, na.rm = T)), 
    by = 5))
d$age.bin <- cut(d$Age, breaks = breaks, labels = breaks[1:length(breaks) - 
    1])
d <- d[!is.na(age.bin)]

ds <- mutate(summarise(group_by(d, age.bin), total = n(), survived = sum(Survived)), 
    survived.frac = survived/total)

ggplot(ds, aes(age.bin, survived.frac, group = 1)) + geom_point(alpha = 0.5) + 
    geom_smooth(method = loess, se = F) + xlab("Age") + ylab("Survival rate") + 
    ggtitle("Overall Survival Rate by Age")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1.png) 


```r
ds <- mutate(summarise(group_by(d, Sex, age.bin), total = n(), survived = sum(Survived)), 
    survived.frac = survived/total)
ds$bin <- as.integer(as.character(ds$age.bin))

ggplot(ds, aes(age.bin, survived.frac, color = Sex, group = Sex)) + geom_point(alpha = 0.5) + 
    geom_smooth(method = loess, se = F) + xlab("Age") + ylab("Survival rate") + 
    ggtitle("Survival Rate- male/female")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 



```r
ds <- mutate(summarise(group_by(d, Sex, Pclass, age.bin), total = n(), survived = sum(Survived)), 
    survived.frac = survived/total)
ds$bin <- as.integer(as.character(ds$age.bin))

ggplot(ds[!is.na(age.bin)], aes(age.bin, survived.frac, color = as.factor(Pclass), 
    group = Pclass)) + geom_point(alpha = 0.5) + facet_wrap(~Sex) + geom_smooth(method = loess, 
    se = F) + xlab("Age") + ylab("Survival Rate") + ggtitle("Survival Rate by Age by Pclass")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 



```r
ds <- mutate(summarise(group_by(d, Sex, Parch, age.bin), total = n(), survived = sum(Survived)), 
    survived.frac = survived/total)
ds$bin <- as.integer(as.character(ds$age.bin))

ggplot(ds[Parch %in% c(0, 1, 2)], aes(age.bin, survived.frac, color = as.factor(Sex), 
    group = Sex)) + geom_point(alpha = 0.5) + facet_wrap(~Parch, ncol = 1) + 
    geom_smooth(method = loess, se = F) + xlab("Age") + ylab("Survival Rate") + 
    ggtitle("Survival Rate by Age by Parch")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 



```r
ds <- mutate(summarise(group_by(d, Sex, SibSp, age.bin), total = n(), survived = sum(Survived)), 
    survived.frac = survived/total)
ds$bin <- as.integer(as.character(ds$age.bin))

ggplot(ds[SibSp %in% c(0, 1, 2)], aes(age.bin, survived.frac, color = as.factor(Sex), 
    group = Sex)) + geom_point(alpha = 0.5) + facet_wrap(~SibSp, ncol = 1) + 
    geom_smooth(method = loess, span = 1, se = F) + xlab("Age") + ylab("Survival Rate") + 
    ggtitle("Survival Rate by Age by SibSp")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 


### By Fare, bin width = 5

```r
breaks <- with(d, seq(floor(min(Fare, na.rm = T)), ceiling(max(Fare, na.rm = T)), 
    by = 5))
d$fare.bin <- cut(d$Fare, breaks = breaks, labels = breaks[1:length(breaks) - 
    1])
d <- d[!is.na(fare.bin)]

ds <- mutate(summarise(group_by(d, fare.bin), total = n(), survived = sum(Survived)), 
    survived.frac = survived/total)

ggplot(ds, aes(fare.bin, survived.frac, group = 1)) + geom_point(alpha = 0.5) + 
    geom_smooth(method = loess) + xlab("Fare") + ylab("Survival Rate") + ggtitle("Overall Survival Rate by Fare")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 



```r
ds <- mutate(summarise(group_by(d, Sex, fare.bin), total = n(), survived = sum(Survived)), 
    survived.frac = survived/total)

ggplot(ds, aes(fare.bin, survived.frac, color = Sex, group = Sex)) + geom_point(alpha = 0.5) + 
    geom_smooth(method = loess, se = F) + xlab("Fare") + ylab("Survival Rate") + 
    ggtitle("Survival Rate by Fare, Sex")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 



```r
ds <- mutate(summarise(group_by(d, Sex, Pclass, fare.bin), total = n(), survived = sum(Survived)), 
    survived.frac = survived/total)
ds$bin <- as.integer(as.character(ds$fare.bin))

ggplot(ds[!is.na(fare.bin)], aes(fare.bin, survived.frac, color = as.factor(Pclass), 
    group = Pclass)) + geom_point(alpha = 0.5) + facet_wrap(~Sex, ncol = 1) + 
    geom_smooth(method = loess, se = F) + xlab("Fare") + ylab("Survival Rate") + 
    ggtitle("Survival Rate by Fare, Sex, Pclass")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 



```r
ds <- mutate(summarise(group_by(d, Sex, Parch, fare.bin), total = n(), survived = sum(Survived)), 
    survived.frac = survived/total)
ds$bin <- as.integer(as.character(ds$fare.bin))

ggplot(ds[Parch %in% c(0, 1, 2)], aes(fare.bin, survived.frac, color = as.factor(Sex), 
    group = Sex)) + geom_point(alpha = 0.5) + facet_wrap(~Parch, ncol = 1) + 
    geom_smooth(method = loess, se = F) + xlab("Fare") + ylab("Survival Rate") + 
    ggtitle("Survival Rate by Fare, Sex, Parch")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 



```r
ds <- mutate(summarise(group_by(d, Sex, SibSp, fare.bin), total = n(), survived = sum(Survived)), 
    survived.frac = survived/total)
ds$bin <- as.integer(as.character(ds$fare.bin))

ggplot(ds[SibSp %in% c(0, 1, 2)], aes(fare.bin, survived.frac, color = as.factor(Sex), 
    group = Sex)) + geom_point(alpha = 0.5) + facet_wrap(~SibSp, ncol = 1) + 
    geom_smooth(method = loess, span = 1, se = F) + xlab("Fare") + ylab("Survival Rate") + 
    ggtitle("Survival Rate by Fare, Sex, SibSp")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 


### by Age and Fare bins

```r
breaks <- with(d, seq(floor(min(Age, na.rm = T)), ceiling(max(Age, na.rm = T)), 
    by = 5))
d$age.bin <- cut(d$Age, breaks = breaks, labels = breaks[1:length(breaks) - 
    1])
breaks <- with(d, seq(floor(min(Fare, na.rm = T)), ceiling(max(Fare, na.rm = T)), 
    by = 5))
d$fare.bin <- cut(d$Fare, breaks = breaks, labels = breaks[1:length(breaks) - 
    1])

ggplot(d[!is.na(age.bin) & !is.na(fare.bin)], aes(fare.bin, age.bin)) + geom_bin2d() + 
    facet_wrap(~Sex) + xlab("Fare bin") + ylab("Age bin") + ggtitle("By Age and Fare bins, color=count")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 




