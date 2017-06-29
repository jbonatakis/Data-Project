# Clear the work environment
rm(list=ls())

setwd("/home/jack/data")
# Read in the csv file from active working directory
Loan_Stats <- read.csv("~/data/LoanStats3c.csv", header=TRUE, skip = 1)

# Check read in data frame
head(Loan_Stats)

# Cut unnecessary fields
Loan_2014 <- Loan_Stats[c("grade", "sub_grade", "zip_code", "term", "loan_amnt", "annual_inc", "verification_status", "purpose", "tax_liens",
                          "pct_tl_nvr_dlq", "int_rate", "loan_status")]

# Check cut output of cut down data frame
head(Loan_2014)
summary(Loan_2014)

# Check class of data frame objects
sapply(Loan_2014, class)

# Coerce interest rate into more appropriate classes
Loan_2014["int_rate"] <- as.numeric(Loan_2014$int_rate)

# Check coercion of interest rate
sapply(Loan_2014, class)
summary(Loan_2014)

plot(Loan_2014$annual_inc)


Loan_2014$Agg <- paste(Loan_2014$zip_code, Loan_2014$verification_status)
Loan_2014_Veri_Zip <- split(Loan_2014, Loan_2014$Agg)

install.packages("data.table")
library(data.table)

Loan_2014_Veri_Zip <- as.data.table(Loan_2014_Veri_Zip)
head(Loan_2014_Veri_Zip)

# Part B

# Read in IRS data
dat <- read.csv("14zpallnoagi.csv")

# Select only relevant variables
zipdata <- dat[c("ZIPCODE", "N02650", "A02650")]
head(zipdata)

# Removes the last two numbers from ZIPCODE to be consistent with Lending Club Data
# install.packages("stringr")
library(stringr)
zipdata$ZIPCODE <- str_sub(zipdata$ZIPCODE, 1, str_length(zipdata$ZIPCODE)-2)
head(zipdata)

# Sum N02650 and A02650 by first 3 numbers of each ZIP
library(plyr)
agg_zipdata <- ddply(zipdata, .(ZIPCODE), summarise, tax_returns = sum(N02650), total_income = sum(A02650))
head(agg_zipdata)

# Finds average income by ZIP
agg_zipdata <- transform(agg_zipdata, avg_income = total_income / tax_returns)
tail(agg_zipdata)

# Gives the 6 points breakdown on avg_income
summary(agg_zipdata$avg_income)
hist(agg_zipdata$avg_income, breaks=30, labels=TRUE, xlab="Average Income", main="Average Income Histogram")

# Sums the number of tax returns. 290,077,690 observations
sum(agg_zipdata$tax_returns)
