## Assignment IDA 25/26 reading in NHANES data

# set the working directory to the location where the data file is

dat<-dget(file="NHANESvIDA.rda")
str(dat) # concise info on size and variables
dat$id <- NULL # delete observation id as not required
head(dat) # first six observations
summary(dat) # summary for all variables 

# start exploring missing values ...
sum(is.na(dat))
colSums(is.na(dat))
round(colMeans(is.na(dat)),3) ## proportion missing

sum(complete.cases(dat))
mean(complete.cases(dat))

##install.packages("mice")
library(mice)
md.pattern(dat)

##install.packages("VIM")
library(VIM)
aggr(dat,
     numbers=TRUE,
     sortVars=TRUE,
     cex.axis=.7,
     gap=3)

matrixplot(dat)

library(naniar)
vis_miss(dat)

# EDA (summary statistics and graphics)
str(dat)
summary(dat)

## Summary statistics for numeric variables
summary(dat[,c("age","height","bmi","waist",
               "cholesterol","apoliprot",
               "cpm","num_mvpa_bouts")])

## Frequency tables for categorical variables
table(dat$gender)
table(dat$ethnic)
table(dat$educ)
table(dat$marital)
table(dat$income)

## Histograms
par(mfrow=c(2,4))

hist(dat$age,
     main="Distribution of Age",
     xlab="Age")

hist(dat$height,
     main="Distribution of Standing Height",
     xlab="Height (cm)")

hist(dat$bmi,
     main="Distribution of BMI",
     xlab="BMI")

hist(dat$waist,
     main="Distribution of Waist Circumference",
     xlab="Waist (cm)")

hist(dat$cholesterol,
     main="Distribution of LDL Cholesterol",
     xlab="LDL Cholesterol")

hist(dat$apoliprot,
     main="Distribution of Apolipoprotein B",
     xlab="Apolipoprotein B")

hist(dat$cpm,
     main="Distribution of Physical Activity (CPM)",
     xlab="Counts per minute")

hist(dat$num_mvpa_bouts,
     main="MVPA bouts per day",
     xlab="Number of bouts")

## Boxplots by gender
par(mfrow=c(1,1))
boxplot(cholesterol ~ gender,
        data=dat,
        main="Cholesterol by Gender",
        ylab="LDL Cholesterol")

boxplot(bmi ~ gender,
        data=dat,
        main="BMI by Gender",
        ylab="BMI")

## Scatter plots for relationships
plot(dat$bmi, dat$cholesterol,
     xlab="BMI",
     ylab="LDL Cholesterol",
     main="BMI vs Cholesterol")

plot(dat$cpm, dat$cholesterol,
     xlab="Physical Activity (CPM)",
     ylab="LDL Cholesterol",
     main="Physical Activity vs Cholesterol")

plot(dat$waist, dat$cholesterol,
     xlab="Waist Circumference",
     ylab="LDL Cholesterol",
     main="Waist vs Cholesterol")

