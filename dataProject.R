# Clear the work environment
rm(list=ls())

# Read in the csv file from active workinng directory
Loan_Stats <- read.csv("LoanStats3C.csv", header=TRUE, skip = 1)

# Check read in dat frame
head(Loan_Stats)

# Cut uneccessary fields
Loan_2014 <- Loan_Stats[c("grade", "sub_grade", "zip_code", "term", "loan_amnt", "annual_inc", "verification_status", "purpose", "tax_liens",
                        "pct_tl_nvr_dlq", "int_rate", "loan_status", "home_ownership")]

# Check cut output of cut down data frame
head(Loan_2014)
summary(Loan_2014)

# Check classs of data frame objects
lapply(Loan_2014, class)

# Coerce interest rate into more appropriate classes
Loan_2014$int_rate <- gsub(" ", "", Loan_2014$int_rate)
head(Loan_2014$int_rate)
Loan_2014$int_rate <- gsub("%", "", Loan_2014$int_rate)
head(Loan_2014$int_rate)
Loan_2014$int_rate <- as.numeric(Loan_2014$int_rate)/100
head(Loan_2014$int_rate)

# Check coercion of interest rate
lapply(Loan_2014, class)
summary(Loan_2014)

plot(Loan_2014$annual_inc)

# Removal of outliers
meanX = mean(Loan_2014$annual_inc)
meanX
sdX = sd(Loan_2014$annual_inc)
sdX





## Loan_2014$Agg <- paste(Loan_2014$zip_code, Loan_2014$verification_status)
## Loan_2014_Veri_Zip <- split(Loan_2014, Loan_2014$Agg)


library(data.table)

Loan_2014 <- as.data.table(Loan_2014)
setkey(Loan_2014, zip_code, verification_status)
head(Loan_2014)
Loan_VeriZip <- aggregate(Loan_2014, by=list(Loan_2014$zip_code, 
                                             Loan_2014$verification_status), FUN=mean, na.rm = TRUE)
head(Loan_VeriZip)
tail(Loan_VeriZip)


############ VERI ZIP

Loan_VeriZip <- Loan_2014[,list(avgLoan = mean(loan_amnt),
                                avgInc = mean(annual_inc),
                                currentLoan = sum(loan_status == "Current")/.N,
                                paidLoan = sum(loan_status == "Fully Paid")/.N,
                                debtConsolidation = sum(purpose == "debt_consolidation")/.N,
                                avgLiens = mean(tax_liens),
                                avgDel = mean(pct_tl_nvr_dlq),
                                avgIntRat = mean(int_rate),
                                homeOwner = sum(home_ownership == "MORTGAGE", home_ownership == "OWN")/.N), by='zip_code,verification_status']
head(Loan_VeriZip)
tail(Loan_VeriZip)

############ VZST
## Some guidance on how to aggregate within each group. 
##For the continuous variables, compute the average within each group.  
##For loan_status, compute two fractions: thefraction of loans that are current, and the fraction of loans that are fully paid. 
#For home_ownership, compute the fraction of loans with a homeowner.  For purpose, compute fraction of debt consolidations


Loan_VZST <- Loan_2014[,list(avgLoan = mean(loan_amnt),
                             avgInc = mean(annual_inc),
                             currentLoan = sum(loan_status == "Current")/.N,
                             paidLoan = sum(loan_status == "Fully Paid")/.N,
                             ChargeOff = sum(loan_status == "Charged Off")/.N,
                             graceLoan = sum(loan_status == "In Grace Period")/.N,
                             lateLoanS = sum(loan_status == "Late (16-30 days)")/.N,
                             lateLoanL = sum(loan_status == "Late (31-120 days)")/.N,
                             debtConsolidation = sum(purpose == "debt_consolidation")/.N,
                             avgLiens = mean(tax_liens),
                             avgDel = mean(pct_tl_nvr_dlq),
                             avgIntRat = sum(int_rate)/.N,
                             homeOwner = sum(home_ownership == "MORTGAGE", home_ownership == "OWN")/.N)
                       , by='zip_code,verification_status,sub_grade,term']
head(Loan_VZST)
tail(Loan_VZST)


######################################   5   ###################################################

# Avg Income Verified
summary(Loan_VZST$avgInc[Loan_VZST$verification_status == "Verified"], na.rm = TRUE)

# Avg Inc Source
summary(Loan_VZST$avgInc[Loan_VZST$verification_status == "Source Verified"], na.rm = TRUE)

# Avg Inc Not
summary(Loan_VZST$avgInc[Loan_VZST$verification_status == "Not Verified"], na.rm = TRUE)

# Verified: Income vs Interest Rate
par(mfrow = c(2,3))

plot(Loan_VZST$avgIntRat[Loan_VZST$verification_status == "Verified"], Loan_VZST$avgInc[Loan_VZST$verification_status == "Verified"],
     ylim=c(0,300000), main = "Verified")

plot(Loan_VZST$avgIntRat[Loan_VZST$verification_status == "Source Verified"], Loan_VZST$avgInc[Loan_VZST$verification_status == "Source Verified"],
     ylim=c(0,300000), main="Source Verified")

plot(Loan_VZST$avgIntRat[Loan_VZST$verification_status == "Not Verified"], Loan_VZST$avgInc[Loan_VZST$verification_status == "Not Verified"],
     ylim=c(0,300000), main= "Not Verified")

plot(Loan_VZST$avgLoan[Loan_VZST$verification_status == "Verified"], Loan_VZST$avgInc[Loan_VZST$verification_status == "Verified"],
     ylim=c(0,300000), main = "Verified")

plot(Loan_VZST$avgLoan[Loan_VZST$verification_status == "Source Verified"], Loan_VZST$avgInc[Loan_VZST$verification_status == "Source Verified"],
     ylim=c(0,300000), main="Source Verified")

plot(Loan_VZST$avgLoan[Loan_VZST$verification_status == "Not Verified"], Loan_VZST$avgInc[Loan_VZST$verification_status == "Not Verified"],
     ylim=c(0,300000), main= "Not Verified")

################## 5B Answer: As a general rule those with verified incomes have higher incomes and recieved larger loans
################# Regardless of verification status those with higher incomes received higher loans

################################## 5C #####################################################


#### 5A
summary(Loan_VZST$avgIn[Loan_VZST$verification_status == "Not Verified"], na.rm = TRUE)


summary(Loan_VZST)
hist(Loan_VZST$ChargeOff[Loan_VZST$verification_status == "Verified"])
hist(Loan_VZST$currentLoan[Loan_VZST$term == 36])

##### 5B

plot(Loan_VZST$avgInc[Loan_VZST$verification_status == "Not Verified"], na.rm = TRUE)

plot(Loan_VZST$avgIntRat, Loan_VZST$avgInc)

plot(Loan_VZST$avgInc, Loan_VZST$avgLoan)

par(mfrow=c(1,2))

Loan36 <- Loan_VZST$avgLoan[Loan_VZST$term == 36]
head(Loan36)
hist(Loan36, breaks=2)
hist(Loan_VZST$term)

#######################################################




####################################################### Part B

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
