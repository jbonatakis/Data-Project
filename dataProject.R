#### Clear the work environment
rm(list=ls())

## Read in the csv file from active workinng directory
Loan_Stats <- read.csv("LoanStats3c.csv", header=TRUE, skip = 1, na.strings=c("", "NA"))

## Check read in dat frame
head(Loan_Stats)

### Cut uneccessary fields
Loan_2014 <- Loan_Stats[c("grade", "sub_grade", "zip_code", "term", "loan_amnt", "annual_inc",
                          "verification_status", "purpose", "tax_liens",
                        "pct_tl_nvr_dlq", "int_rate", "loan_status", "home_ownership")]

## Check cut output of cut down data frame
head(Loan_2014)
summary(Loan_2014)

### Check classs of data frame objects
lapply(Loan_2014, class)

## Coerce interest rate into more appropriate classes: purpose as other
Loan_2014$int_rate <- gsub(" ", "", Loan_2014$int_rate)
head(Loan_2014$int_rate)
Loan_2014$int_rate <- gsub("%", "", Loan_2014$int_rate)
head(Loan_2014$int_rate)
Loan_2014$int_rate <- as.numeric(Loan_2014$int_rate)/100
head(Loan_2014$int_rate)



## Check coercion of interest rate
lapply(Loan_2014, class)
summary(Loan_2014)

plot(Loan_2014$annual_inc)

######################################################
### Removal of outliers ------ Currently none removed
meanX = mean(Loan_2014$annual_inc)
meanX
sdX = sd(Loan_2014$annual_inc)
sdX
#####################################################

## Loan_2014$Agg <- paste(Loan_2014$zip_code, Loan_2014$verification_status)
## Loan_2014_Veri_Zip <- split(Loan_2014, Loan_2014$Agg)



###### 3. Aggregate data

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
                             #graceLoan = sum(loan_status == "In Grace Period")/.N,
                             #lateLoanS = sum(loan_status == "Late (16-30 days)")/.N,
                             #lateLoanL = sum(loan_status == "Late (31-120 days)")/.N,
                             debtConsolidation = sum(purpose == "debt_consolidation")/.N,
                             avgLiens = mean(tax_liens),
                             avgPctND = mean(pct_tl_nvr_dlq),
                             avgIntRat = sum(int_rate)/.N,
                             homeOwner = sum(home_ownership == "MORTGAGE", home_ownership == "OWN")/.N)
                       , by='zip_code,verification_status,sub_grade,term']
head(Loan_VZST)
tail(Loan_VZST)


######################################   5A   ####################################################################################

#A. term of the loan predict the status of the loan?  Think about what this comparison tells you about the structure of the data
########  36 month loans are much more likely to be paid off, also leading the inverse to be true:
########  60 month loans are more likely to be current
lm_ts= lm(paidLoan~term, data=Loan_VZST)
summary(lm_ts)

lm_tc= lm(currentLoan~term, data=Loan_VZST)
summary(lm_tc)

lm_tch= lm(ChargeOff~term, data=Loan_VZST)
summary(lm_tch)
######### You are more likely to receive the full investment back on a shorter 36 month loan
###################################################################################################################################


# Avg Income Verified
summary(Loan_VZST$avgInc[Loan_VZST$verification_status == "Verified"], na.rm = TRUE)

# Avg Inc Source
summary(Loan_VZST$avgInc[Loan_VZST$verification_status == "Source Verified"], na.rm = TRUE)

# Avg Inc Not
summary(Loan_VZST$avgInc[Loan_VZST$verification_status == "Not Verified"], na.rm = TRUE)


######################################################################################## 5B #######################################
# annual  income  matter  for  the  loan  amount  and/or  its  interest  rate?
#  Is  the relationship any different if the income is verified, source verified, or not verified\
par(mfrow = c(2,3))

################### Verified Income/Int
plot(Loan_VZST$avgInc[Loan_VZST$verification_status == "Verified"], Loan_VZST$avgIntRat[Loan_VZST$verification_status == "Verified"],
     xlim=c(0,200000),
     xlab = "Average Income",
     ylab = "Average Interest Rate",
     main = "Verified: Income vs Interest Rate")

lm_veriInt = lm(avgIntRat~avgInc, data=Loan_VZST[Loan_VZST$verification_status == "Verified"])
abline(lm_veriInt, col="red")

##################### Source Verified Income/Int

plot(Loan_VZST$avgInc[Loan_VZST$verification_status == "Source Verified"], Loan_VZST$avgIntRat[Loan_VZST$verification_status == "Source Verified"],
     xlim=c(0,200000),
     xlab = "Average Income",
     ylab = "Average Interest Rate",
     main = "Source Verified: Income vs Interest Rate")

lm_svInt = lm(avgIntRat~avgInc, data=Loan_VZST[Loan_VZST$verification_status == "Source Verified"])
abline(lm_svInt, col="red")

#################### Not Verified Income/Int
plot(Loan_VZST$avgInc[Loan_VZST$verification_status == "Not Verified"], Loan_VZST$avgIntRat[Loan_VZST$verification_status == "Not Verified"],
     xlim=c(0,200000),
     xlab = "Average Income",
     ylab = "Average Interest Rate",
     main = "Not Verified: Income vs Interest Rate")

lm_nvInt = lm(avgIntRat~avgInc, data=Loan_VZST[Loan_VZST$verification_status == "Not Verified"])
abline(lm_nvInt, col="red")

#################### Verified Income/Amnt
plot(Loan_VZST$avgInc[Loan_VZST$verification_status == "Verified"], Loan_VZST$avgLoan[Loan_VZST$verification_status == "Verified"],
     xlim=c(0,200000),
     xlab = "Average Income",
     ylab = "Average Loan Amount",
     main = "Verified: Income vs Loan Amount")

lm_vLoan = lm(avgLoan~avgInc, data=Loan_VZST[Loan_VZST$verification_status == "Verified"])
abline(lm_vLoan, col="red")

#################### Source Verified Income/Amnt
plot(Loan_VZST$avgInc[Loan_VZST$verification_status == "Source Verified"], Loan_VZST$avgLoan[Loan_VZST$verification_status == "Source Verified"],
     xlim=c(0,200000),
     xlab = "Average Income",
     ylab = "Average Loan Amount",
     main = "Source Verified: Income vs Loan Amount")

lm_svLoan = lm(avgLoan~avgInc, data=Loan_VZST[Loan_VZST$verification_status == "Source Verified"])
abline(lm_svLoan, col="red")

###################### Not Verified Income/Amnt
plot(Loan_VZST$avgInc[Loan_VZST$verification_status == "Not Verified"], Loan_VZST$avgLoan[Loan_VZST$verification_status == "Not Verified"],
     xlim=c(0,200000),
     xlab = "Average Income",
     ylab = "Average Loan Amount",
     main = "Not Verified: Income vs Loan Amount")

lm_nvLoan = lm(avgLoan~avgInc, data=Loan_VZST[Loan_VZST$verification_status == "Not Verified"])
abline(lm_nvLoan, col="red")
################## 5B Answer: As a general rule those with verified incomes have higher incomes and recieved larger loans
################# Regardless of verification status those with higher incomes received higher loans


#######################################################################################################################################
## NEEDS LABELS ##
################################## 5C #####################################################
# c.Is the subgrade assigned to the loan useful for predicting the likelihood of defaulting on
# the  loan("Charged  Off"  means  the  borrower  defaulted)?    
# As  you  think  about  this question, think about whether examining 36-month term versus
# 60-month term would tell you different information

par(mfrow=c(1,2))

plot(Loan_VZST$sub_grade, Loan_VZST$ChargeOff, notch=TRUE)

plot(Loan_VZST$term, Loan_VZST$ChargeOff, notch=TRUE)


####################################################################################################################################

################################################## 5D ####################################

# d.Conditional on the loan's subgrade, are borrower credit characteristics (i.e., tax liens, previous delinquencies)
# important for whether the borrower will default on the loan? As you think about this question,
# think about whether examining 36-month term versus 60-month term would tell you different information.

# ChargeOff
# tax_liens
# sub_grade
# avgPctND

lm_co <- lm(ChargeOff~sub_grade+avgLiens+avgPctND, data = Loan_VZST)
summary(lm_co)

lm_coNS <- lm(ChargeOff~avgLiens, data = Loan_VZST)
summary(lm_coNS)
lm_coNS <- lm(ChargeOff~avgPctND, data = Loan_VZST)
summary(lm_coNS)

lm_coNS <- lm(ChargeOff~sub_grade:avgLiens, data = Loan_VZST)
summary(lm_coNS)
lm_coNS <- lm(ChargeOff~sub_grade:avgPctND, data = Loan_VZST)
summary(lm_coNS)

########### ??? Answer?: Given the conditional availability of subgrades they provide much more information regarding 
# AvgLiens: Unsignificant: is not a good predictor of charge off likelyhood
# avgPctND: The percentage of previous delinquent loans is significant. Its slope, while very small, suggests that those with
## previosly delinquent loans are actually less likely to default on their loan than their subgrade may sugggest.



################################################## 5E ####################################
#E. Conditional on the loan's subgrade, are borrower credit characteristics
# (i.e., tax liens, previous delinquencies) important for the interest rate or the loan amount?

# INTEREST RATE
lm_int <- lm(avgIntRat~sub_grade+avgLiens+avgPctND, data = Loan_VZST)
summary(lm_int)

lm_intLiens <- lm(avgIntRat~avgLiens, data = Loan_VZST)
summary(lm_intLiens)
lm_intND <- lm(avgIntRat~avgPctND, data = Loan_VZST)
summary(lm_intND)

# Conditional on sub_grade, previous delinquency does not have a signifcant affect on Interest rates
# Liens does however have a signifiant effect, increasing your interest rate as previous liens increase

# AVG LOAN AMNT
lm_loan <- lm(avgLoan~sub_grade+avgLiens+avgPctND, data = Loan_VZST)
summary(lm_loan)

lm_loanLiens <- lm(avgLoan~avgLiens, data = Loan_VZST)
summary(lm_loanLiens)
lm_loanND <- lm(avgLoan~avgPctND, data = Loan_VZST)
summary(lm_loanND)

# Answer: Conditional upon sub_grade previous liens do not have a signifcant affect on loan ammount
## Previous delinquencies do have a significant affect on loan size: those who have never been delinquent are likely to receive
## larger loans
##############################################################################################################################

################################################# 5F ########################################
# f.Does the purpose of the loan or the borrower's homeownership status matter for the likelihood of defaulting on the loan?

Loan_Purpose <- Loan_2014[,list(avgLoan = mean(loan_amnt),
                                avgInc = mean(annual_inc),
                                currentLoan = sum(loan_status == "Current")/.N,
                                paidLoan = sum(loan_status == "Fully Paid")/.N,
                                debtConsolidation = sum(purpose == "debt_consolidation")/.N,
                                ChargeOff = sum(loan_status == "Charged Off")/.N,
                                avgLiens = mean(tax_liens),
                                avgDel = mean(pct_tl_nvr_dlq),
                                avgIntRat = mean(int_rate),
                                homeOwner = sum(home_ownership == "MORTGAGE", home_ownership == "OWN")/.N),
                          by='purpose,home_ownership']




levels(Loan_Purpose$purpose)
lm_defp <- lm(ChargeOff~purpose, data = Loan_Purpose)
summary(lm_defp)
### Though the general intercept indicating that above all approximately 12% of people will default on their loan is the most significant
## Moving and small business loans are signifcantly more likely to be defaulted on with small_business loans being the most risky by far
# Wedding loans are the only purpose that offers lower risk than your average loan: the union of two indiviuals' finances leads to an
# easier time at paying back the loan and much less possibility of defaulting


lm_defh <- lm(ChargeOff~home_ownership, data = Loan_Purpose)
summary(lm_defh)
levels(Loan_Purpose$purpose)
###### Those with a mortgage have the smallest chance of defaulting on their loan. On the contrary renters, who pay rent in excess of
## their loan every month are more likely to default on any given loan. Homeowners are almost as likely as renters to default








####################################################### Part B  ##################################################################
install.packages("stringr")

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
agg_zipdata <- transform(agg_zipdata, avg_income = (total_income / tax_returns)*1000)
tail(agg_zipdata)

# Gives the 6 points breakdown on avg_income
summary(agg_zipdata$avg_income)
hist(agg_zipdata$avg_income, breaks=30, labels=TRUE, xlab="Average Income", main="Average Income Histogram")
#### Right skew: avg income cannot pass below 0, leaving a tail of higher incomes to the right


# Merge two data.tables on ZIP
head(Loan_VeriZip)

Loan_B <- Loan_VeriZip
Loan_B$ZIPCODE <- gsub("xx", "", Loan_B$zip_code)
class(Loan_B$ZIPCODE)
class(agg_zipdata$ZIPCODE)


Loan_ZipAgg <- merge(Loan_B, agg_zipdata, by="ZIPCODE")
Loan_ZipAgg$zip_code <- NULL
head(Loan_ZipAgg)
### TWO DATA SETS MERGED

########################################################## B3 #############################################################

########  Barplot of lending club vs irs avg income
boxplot(Loan_ZipAgg$avg_income, Loan_ZipAgg$avgInc, ylim=c(0,250000), main="LC vs IRS Average Income", names=c("IRS","LC"))

## Compare lending club avgINC to IRS avg income, selected based on veri, source and not veri
par(mfrow=c(1,3))

plot(Loan_ZipAgg$avgInc[Loan_VZST$verification_status == "Verified"],
     Loan_ZipAgg$avg_income[Loan_VZST$verification_status == "Verified"],
     xlab = "Average Lending Club Income",
     ylab = "Average IRS Income",
     main = "Verified vs IRS Income",
     ylim=c(0,200000),
     xlim=c(0,200000))

lm_veriInc = lm(avg_income~avgInc, data=Loan_ZipAgg[Loan_ZipAgg$verification_status == "Verified"])
summary(lm_veriInc)
abline(lm_veriInc, col="red")


plot(Loan_ZipAgg$avgInc[Loan_VZST$verification_status == "Source Verified"],
     Loan_ZipAgg$avg_income[Loan_VZST$verification_status == "Source Verified"],
     xlab = "Average Lending Club Income",
     ylab = "Average IRS Income",
     main = "Source Verified vs IRS Income",
     ylim=c(0,200000),
     xlim=c(0,200000))
lm_svInc = lm(avg_income~avgInc, data=Loan_ZipAgg[Loan_ZipAgg$verification_status == "Source Verified"])
summary(lm_svInc)
abline(lm_svInc, col="red")

plot(Loan_ZipAgg$avgInc[Loan_VZST$verification_status == "Not Verified"],
     Loan_ZipAgg$avg_income[Loan_VZST$verification_status == "Not Verified"],
     xlab = "Average Lending Club Income",
     ylab = "Average IRS Income",
     main = "Not Verified vs IRS Income",
     ylim=c(0,200000),
     xlim=c(0,200000))
lm_nvInc = lm(avg_income~avgInc, data=Loan_ZipAgg[Loan_ZipAgg$verification_status == "Not Verified"])
summary(lm_nvInc)
abline(lm_nvInc, col="red")

#################################################### 4A ######################################################
####### Veri, SV, NV Box plots
par(mfrow=c(1,1))

boxplot(Loan_ZipAgg$avg_income, Loan_ZipAgg$avgInc[Loan_ZipAgg$verification_status == "Verified"],
        Loan_ZipAgg$avgInc[Loan_ZipAgg$verification_status == "Source Verified"],
        Loan_ZipAgg$avgInc[Loan_ZipAgg$verification_status == "Not Verified"],
        ylim=c(0,250000), main="Verification Levels vs IRS Avg Income", names=c("IRS","Verified", "Source Verified", "Not Verified"))


#lm_IRSveri <- lm(avg_income~avgInc, data=Loan_ZipAgg[Loan_ZipAgg$verification_status == "Verified"])
#summary(lm_veriInc)
#abline(lm_veriInc, col="red")



#### Does IRS or lending club average income serve as a better predictor of homeowner status?

irs_inc_vs_home <- lm(avg_income~homeOwner, data=Loan_ZipAgg)
summary(irs_inc_vs_home)
plot(Loan_ZipAgg$homeOwner,Loan_ZipAgg$avg_income)
abline(irs_inc_vs_home, col="red")


### c.Construct a binned IRS income variableto help with this one. Does IRS income exhibit a stronger relationship with LC
### income for different parts of the distribution of income (e.g., the top quartile versus the bottom quartile)
summary(Loan_ZipAgg$avg_income)
Loan_ZipAgg$inc_bin <- as.factor(cut(Loan_ZipAgg$avg_income, breaks=c(0,49572, 55117, 64818, Inf),
                             labels=c('Bottom Quartile', "Lower Quartile", "Upper Quartile", "Top Quartile")))
head(Loan_ZipAgg)
tail(Loan_ZipAgg)


# Create bins for the quartiles of the data
lm_bins <- lm(avgInc~inc_bin, data=Loan_ZipAgg)
summary(lm_bins)
plot(Loan_ZipAgg$inc_bin, Loan_ZipAgg$avgInc, main= "LC Average Approximated by IRS Quartiles",
     ylab="Average LC Income", xlab="IRS Quartiles", ylim=c(30000,150000))
abline(lm_bins, col="red")
abline(h=69132, col="blue")

# summary(Loan_ZipAgg$avgInc)
#plot(Loan_ZipAgg$avgInc, Loan_ZipAgg$inc_bin)

#### What leads LC to decide who gets their income verified
summary(Loan_ZipAgg[Loan_ZipAgg$verification_status == "Verified"])
#vs source
summary(Loan_ZipAgg[Loan_ZipAgg$verification_status == "Source Verified"])
#vs
summary(Loan_ZipAgg[Loan_ZipAgg$verification_status == "Not Verified"])


############ Verified vs Not Verified AvgInc, AvgLoan, AvgIntRate

# Verify because: previous credit, higher income, (fraudulent?) larger loans

boxplot(Loan_ZipAgg$avg_income, Loan_ZipAgg$avgInc[Loan_ZipAgg$verification_status == "Verified"],
        Loan_ZipAgg$avgInc[Loan_ZipAgg$verification_status == "Source Verified"],
        Loan_ZipAgg$avgInc[Loan_ZipAgg$verification_status == "Not Verified"],
        ylim=c(0,150000), main="Verification Levels vs IRS Avg Income", names=c("IRS","Verified", "Source Verified", "Not Verified"),
        notch=TRUE)


boxplot(Loan_ZipAgg$avgLoan, Loan_ZipAgg$avgLoan[Loan_ZipAgg$verification_status == "Verified"],
        Loan_ZipAgg$avgLoan[Loan_ZipAgg$verification_status == "Source Verified"],
        Loan_ZipAgg$avgLoan[Loan_ZipAgg$verification_status == "Not Verified"],
        ylim=c(0,40000), main="Verification Levels vs Avg Loan Amount", names=c("Average","Verified", "Source Verified", "Not Verified"),
        notch=TRUE)


boxplot(Loan_ZipAgg$avgLiens, Loan_ZipAgg$avgLiens[Loan_ZipAgg$verification_status == "Verified"],
        Loan_ZipAgg$avgLiens[Loan_ZipAgg$verification_status == "Source Verified"],
        Loan_ZipAgg$avgLiens[Loan_ZipAgg$verification_status == "Not Verified"],
        ylim=c(0,.5), main="Verification Levels vs Previous Liens", names=c("Average","Verified", "Source Verified", "Not Verified"),
        notch=TRUE)



