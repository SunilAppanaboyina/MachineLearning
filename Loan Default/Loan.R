


#--------------------------------------------------------------------------------------------------------------------------------------
# DATA UNDERSTANDING & PREPARATION
#--------------------------------------------------------------------------------------------------------------------------------------

# Read data
loan <- read.csv("loan.csv",stringsAsFactors = FALSE)

# Print dimensions
dim(loan)
# 39717 rows & 111 columns

# Print column names
colnames(loan)
# The meaning of the variables(columns) used for analysis are listed below in the comments of DATA ANLYSIS section.

# Finding percentage of NA's in each column
data.frame(colMeans(is.na(loan)))
length(which(colMeans(is.na(loan))==1))
# Conclusion: we see that there are totally 54 columns where there are 100% NA values. 

# Removing these columns
loan <- loan[,-(which(colMeans(is.na(loan))==1))]

# Checking to see if there are any columns with the same value in the whole column
data.frame(sapply(loan,function(y) length(unique(y))))
# Conclusion: we can see that 6 columns: pymnt_plan, initial_list_status, policy_code, application_type, acc_now_delinq and delinq_amnt
# have the same value in the whole column and are not useful for analysis.

# Removing these columns
loan <- loan[,-which(sapply(loan,function(y) length(unique(y)))==1)]

# Finding missing values in each column
length(which(sapply(loan,function(y) length(which(y=="")))>0))
# Conclusion: only 7 columns have missing values they will be dealt with later as and when we do the analysis

# outlier treatment
# The outliers will be treated as and when we encounter during the analysis

# From the remaining data after careful consideration 22 variables are slected for analysis. 
# After researching on the web we feel these 22 variables have a high probability of giving us insights on the profile of loan defaulters.
loan <- loan[c("id", "loan_amnt", "term", "int_rate","installment", "grade", "sub_grade", "emp_length", "home_ownership", "annual_inc", 
               "verification_status", "loan_status", "purpose", "dti", "delinq_2yrs", "inq_last_6mths", "pub_rec_bankruptcies",
               "open_acc", "pub_rec", "revol_bal", "revol_util", "total_acc")]


#-----------------------------------------------------------------------------------------------------------------------------------
# DATA CLEANING & MANIPULATION
#-----------------------------------------------------------------------------------------------------------------------------------

# Removing % from the int_rate values
loan$int_rate <- as.numeric(sub("%","",loan$int_rate))


# Checking if pub_rec_bankruptcies has NA values
sum(is.na(loan$pub_rec_bankruptcies))
# Replacing the NA values with the median
loan$pub_rec_bankruptcies[is.na(loan$pub_rec_bankruptcies)] <- median(loan$pub_rec_bankruptcies,na.rm = T)


# Removing % from the revol_util values
loan$revol_util <- as.numeric(sub("%","",loan$revol_util))
# The above command converts missing values to NA. Hence imputing NA values with median
loan$revol_util[which(is.na(loan$revol_util))] <- median(loan$revol_util,na.rm = T)


#-----------------------------------------------------------------------------------------------------------------------------------
# DATA ANALYSIS
#-----------------------------------------------------------------------------------------------------------------------------------

# Creating a new metric (new_status) in which "Current" & "Fully Paid" entries from loan_status are labeled as "Cleared" and 
# "Charged Off" are labeled as "Defaulted"
loan$new_status <- factor(ifelse(loan$loan_status %in% c("Current","Fully Paid"),"Cleared","Defaulted"))

# Loading ggplot2 library
library(ggplot2)

# Univariate Analysis
#--------------------

# Variable: loan_amnt - Loan amount
# Histogram of loan_amnt
ggplot(loan,aes(x=loan_amnt,fill=new_status))+ geom_histogram(col="black",position = "fill",breaks=seq(0,35000,by=5000)) + scale_fill_manual(name="",values = c("green","red")) + theme(legend.position = "top",plot.title = element_text(hjust = 0.5)) + labs(x="Loan Amount($)", y="Proportion",title="Proportion of borrowers by loan amount")
# Conclusion: With increase in loan amount the proportion of defaulted loans in increasing. HENCE LOAN AMOUNT IS AN IMPORTANT DRIVER VARIABLE.


# Variable: term - Term of the loan (36 or 60 months)
# Bar plot of term
ggplot(loan,aes(x=term,fill=new_status))+ geom_bar(col="black",position = "fill") + scale_fill_manual(name="",values = c("green","red")) + theme(legend.position = "top",plot.title = element_text(hjust = 0.5)) + labs(x="Loan Term", y="Proportion",title="Proportion of borrowers by term")
# Conclusion: Higher the term higher the is the proportion of defaulted loans. HENCE LOAN TERM IS AN IMPORTANT DRIVER VARIABLE.


# Variable: int_rate - Loan interest rate
# Histogram of int_rate
ggplot(loan,aes(x=int_rate,fill=new_status))+ geom_histogram(col="black",position = "fill",breaks=seq(5,25,by=5)) + scale_fill_manual(name="",values = c("green","red")) + theme(legend.position = "top",plot.title = element_text(hjust = 0.5)) + labs(x="Interest Rate(%)", y="Proportion",title="Proportion of borrowers by interest rate")
# Conclusion: With increase in interest rate the proportion of defaulted loans in increasing. HENCE INTEREST RATE IS AN IMPORTANT DRIVER VARIABLE.


# Variable: installment - Monthly installment amount
# Histogram of installment
ggplot(loan,aes(x=installment,fill=new_status))+ geom_histogram(col="black",position = "fill",breaks=seq(0,1310,by=100)) + scale_fill_manual(name="",values = c("green","red")) + theme(legend.position = "top",plot.title = element_text(hjust = 0.5)) + labs(x="Monthly Installment Amount($)", y="Proportion",title="Proportion of borrowers by monthly installment amount")
# Conclusion: The expectation was that with increase in installment amount the proportion of defaulted loans should increase.
#             But the data dosen't show any trend. HENCE INSTALLMENT IS NOT A DRIVER VARIABLE


# Variable: grade - Assigned loan grade corresponding to interest rate based on borrower's credit history
# Bar plot of grade
ggplot(loan,aes(x=grade,fill=new_status))+ geom_bar(col="black",position = "fill") + scale_fill_manual(name="",values = c("green","red")) + theme(legend.position = "top",plot.title = element_text(hjust = 0.5)) + labs(x="Grade", y="Proportion",title="Proportion of borrowers by grade")
# Conclusion: With increase in grade the proportion of defaulted loans in increasing. HENCE GRADE IS AN IMPORTANT DRIVER VARIABLE.


# Variable: sub_grade - Assigned loan sub grade corresponding to grade based on borrower's credit history
# Bar plot of sub grade
ggplot(loan,aes(x=sub_grade,fill=new_status)) + geom_bar(position = "fill") + scale_fill_manual(name="",values = c("green","red")) + theme(legend.position = "top",plot.title = element_text(hjust = 0.5)) + labs(x="Sub Grade", y="Proportion",title="Proportion of borrowers by sub grade")
# Conclusion: From A1 to F5 with increase in sub grade the proportion of defaulted loans in increasing. From G1 to G5 there is not clear trend.
#             Nevertheless proportion of defaulted is higher from G1 to G5 compared to other sub grades. 
#             HENCE SUB GRADE IS AN IMPORTANT DRIVER VARIABLE.


# Variable: emp_length - Number of years employed
# Bar plot of number of years employed
ggplot(loan,aes(x=emp_length,fill=new_status))+ geom_bar(position = "fill") + scale_fill_manual(name="",values = c("green","red")) + theme(legend.position = "top",plot.title = element_text(hjust = 0.5)) + labs(x="Employment Length", y="Proportion",title="Proportion of borrowers by length of employment")
# Conclusion: The proportion of defaulted is mostly the same for all lengths of employement. "n/a" shows the highest proportion of defaulted.
#             We are assuming "n/a" means the borrower was not interested in sharing the information and this is something to be cautious about.
#             NUMBER OF YEARS EMPLOYED IS NOT A DRIVER VARIABLE.


# Variable: home_ownership - Home ownership status of the borrower
# Bar plot of home ownership
ggplot(loan,aes(x=home_ownership,fill=new_status))+ geom_bar(col="black",position = "fill") + scale_fill_manual(name="",values = c("green","red")) + theme(legend.position = "top",plot.title = element_text(hjust = 0.5)) + labs(x="Home Ownership", y="Proportion",title="Proportion of borrowers by home ownership")
# Conclusion: No trend in the data. HENCE HOME OWNERSHIP STATUS IS NOT A DRIVER VARIABLE.


# Variable: annual_inc - Annual income
# Outlier treatment: finding the annual income below which 98% of the values in annual_inc column fall under.
quantile(loan$annual_inc,probs = 0.98)
# Histogram of annual income representing 98% of the values
ggplot(subset(loan,annual_inc<=187000),aes(x=annual_inc,fill=new_status))+ geom_histogram(col="black",position = "fill",breaks=seq(0,187000,by=25000)) + scale_fill_manual(name="",values = c("green","red")) + theme(legend.position = "top",plot.title = element_text(hjust = 0.5)) + labs(x="Annual Income($)", y="Proportion",title="Proportion of borrowers by annual income")
# Conclusion: Lower the annual income higher the proportion of defaults. The proportion of defaults decreases as income increases.
#             HENCE ANNUAL INCOME IS AN IMPORTANT DRIVER VARIABLE.  


# Variable: verification_status - Indicates if income was verified, not verified or source verified
# Bar plot of verification_status
ggplot(loan,aes(x=verification_status,fill=new_status))+ geom_bar(col="black",position = "fill") + scale_fill_manual(name="",values = c("green","red")) + theme(legend.position = "top",plot.title = element_text(hjust = 0.5)) + labs(x="Verification Status", y="Proportion",title="Proportion of borrowers by income verification status")
# Conclusion: Surprisingly the results show opposite of what we would expect. HENCE VERIFICATION STATUS IS NOT A DRIVER VARIABLE


# Variable: purpose - Reason for borrowing money
# Bar plot of purpose
ggplot(loan,aes(x=purpose,fill=new_status))+ geom_bar(col="black",position = "fill") + scale_fill_manual(name="",values = c("green","red")) + theme(legend.position = "top",plot.title = element_text(hjust = 0.5)) + labs(x="Purpose", y="Proportion",title="Proportion of borrowers by purpose for loan") + coord_flip()
# Conclusion: Small business has the highest proportion of defaults. Followed by renewable energy and educational purpose.
#             It makes sense that if the borrower's small business is not doing well then it is difficult to repay the loan.
#             PURPOSE IS AN IMPORTANT DRIVER VARIABLE


# Variable: dti - Debt-to-income ratio
# Histogram of dti
ggplot(loan,aes(x=dti,fill=new_status))+ geom_histogram(col="black",position = "fill",breaks=seq(0,30,by=5)) + scale_fill_manual(name="",values = c("green","red")) + theme(legend.position = "top",plot.title = element_text(hjust = 0.5)) + labs(x="Debt-to-Income Ratio", y="Proportion",title="Proportion of borrowers by debt-to-income ratio")
# Conclusion: There is a steady increase in the proportion of defaulters as dti increases from 0-25%. There is a decrease at 25-30%. 
#             Maybe thats because of stricter standards being followed to approve loans to borrowers with high dti.
#             Nevertheless dti is an important factor. HENCE DEBT-TO-INCOME RATIO IS AN IMPORTANT VARIABLE.


# Variable: delinq_2yrs - The number of 30+ days past-due incidences of delinquency in the borrower's credit file for the past 2 years
# Bar plot of delinq_2yrs 
ggplot(loan,aes(x=delinq_2yrs,fill=new_status))+ geom_bar(col="black",position = "fill") + scale_fill_manual(name="",values = c("green","red")) + theme(legend.position = "top",plot.title = element_text(hjust = 0.5)) + labs(x="30+ days deliquency in past 2 years", y="Proportion",title="Proportion of borrowers by deliquency in 2 years")
# Conclusion: There is no particular trend in the data. HENCE NUMBER OF DELINQUENT INCIDENCES IS NOT A DRIVER VARIABLE.


# Variable: inq_last_6mnths - Number of inquiries made by lenders to access credit history of the borrower
# Bar plot of inq_last_6mnths
ggplot(loan,aes(x=inq_last_6mths,fill=new_status))+ geom_bar(col="black",position = "fill") + scale_fill_manual(name="",values = c("green","red")) + theme(legend.position = "top",plot.title = element_text(hjust = 0.5)) + labs(x="Credit inquiries in last 6 months", y="Proportion",title="Proportion of borrowers by credit inquires in last 6 months")
# Conclusion: There is no particular trend in the data. HENCE NUMBER OF INQUIRIES IN LAST 6 MONTHS IS NOT A DRIVER VARIABLE.


# Variable: pub_rec_bankruptcies - Number of bankruptcies on public record
# Bar plot of pub_rec_bankruptcies
ggplot(loan,aes(x=loan$pub_rec_bankruptcies,fill=new_status))+ geom_bar(col="black",position = "fill") + scale_fill_manual(name="",values = c("green","red")) + theme(legend.position = "top",plot.title = element_text(hjust = 0.5)) + labs(x="Number of public recorded bankruptcies", y="Proportion",title="Proportion of borrowers by number of bankruptcies")
# Conclusion: The proportion of defaulters increases with increasing number of bankruptices.
#             HENCE NUMBER OF PUBLIC RECORD BANKRUPTCIES IS AN IMPORTANT DRIVER VARIABLE


# Variable: open_acc - The number of open credit lines in the borrower's credit file.
# Histogram of open_acc
ggplot(loan,aes(x=open_acc,fill=new_status))+ geom_histogram(col="black",position = "fill",breaks=seq(1,44,by=4)) + scale_fill_manual(name="",values = c("green","red")) + theme(legend.position = "top",plot.title = element_text(hjust = 0.5)) + labs(x="Number of open credit lines in the borrower's credit file", y="Proportion",title="Proportion of borrowers by open accounts")
# Conclusion: There is no particular trend in the data. HENCE NUMBER OF OPEN ACCOUNTS IS NOT A DRIVER VARIABLE.


# Variable: pub_rec - Number of derogatory public records
# Bar plot of pub_rec
ggplot(loan,aes(x=pub_rec,fill=new_status))+ geom_bar(col="black",position = "fill") + scale_fill_manual(name="",values = c("green","red")) + theme(legend.position = "top",plot.title = element_text(hjust = 0.5)) + labs(x="Number of derogatory public records", y="Proportion",title="Proportion of borrowers by number of derogatory public records")
# Conclusion: There is no particular trend in the data. HENCE NUMBER OF DEROGATORY PUBLIC RECORDS IS NOT A DRIVER VARIABLE.


# Variable: revol_bal - Balance need to be paid by the borrower to accounts like credit cards
# Histogram of revol_bal
ggplot(loan,aes(x=loan$revol_bal,fill=new_status))+ geom_histogram(col="black",position = "fill",breaks=seq(0,150000,by=25000)) + scale_fill_manual(name="",values = c("green","red")) + theme(legend.position = "top",plot.title = element_text(hjust = 0.5)) + labs(x="Total credit revolving balance($)", y="Proportion",title="Proportion of borrowers by revolving balance")
# Conclusion: There is no particular trend in the data. HENCE REVOLVING BALANCE IS NOT A DRIVER VARIABLE.


# Variable: revol_util - Amount of credit the borrower is using relative to all available revolving credit.
# Histogram of revol_util
ggplot(loan,aes(x=loan$revol_util,fill=new_status))+ geom_histogram(col="black",position = "fill",by=seq(0,100,by=20)) + scale_fill_manual(name="",values = c("green","red")) + theme(legend.position = "top",plot.title = element_text(hjust = 0.5)) + labs(x="Revolving credit utilization(%)", y="Proportion",title="Proportion of borrowers by revolving credit utilization")
# Conclusion: The proportion of defaulters increases with increasing revolving credit utilization.
#             HENCE REVOLVING CREDIT UTILIZATION IS AN IMPORTANT DRIVER VARIABLE


# Variable: total_acc -  Total number of credit lines (open+close) currently in the borrower's credit file
# Histogram of total_acc
ggplot(loan,aes(x=loan$total_acc,fill=new_status))+ geom_histogram(col="black",position = "fill",breaks=seq(0,90,by=15)) + scale_fill_manual(name="",values = c("green","red")) + theme(legend.position = "top",plot.title = element_text(hjust = 0.5)) + labs(x="Total number of credit lines currently in the borrower's credit file", y="Proportion",title="Proportion of borrowers by total accounts")
# Conclusion: There is no particular trend in the data. HENCE TOTAL ACCOUNTS IS NOT A DRIVER VARIABLE.


# Derived metric: Loan amount to annual income ratio
loan$loan_amnt_by_annual_inc <- round(loan$loan_amnt/loan$annual_inc, digits = 2)
# Histogram of loan_amnt_by_annual_inc
ggplot(loan,aes(x=loan$loan_amnt_by_annual_inc,fill=new_status))+ geom_histogram(col="black",position = "fill",breaks=seq(0,1,by=0.2)) + scale_fill_manual(name="",values = c("green","red")) + theme(legend.position = "top",plot.title = element_text(hjust = 0.5)) + labs(x="Ratio of loan amount to annual income", y="Proportion",title="Proportion of borrowers by loan amount to annual income ratio")
# Conclusion: Higher the ratio, higher is the proportion of defaulters. HENCE LOAN AMOUNT TO ANNUAL INCOME IS AN IMPORTANT DRIVER VARIABLE.



# RESULTS FROM UNIVARIATE ANALYSIS 
#---------------------------------
# IMPORTANT DRIVER VARIABLES:
# int_rate - Loan interest rate
# grade - Assigned loan grade corresponding to interest rate based on borrower's credit history
# sub_grade - Assigned loan sub grade corresponding to grade based on borrower's credit history
# dti - Debt-to-income ratio
# loan_amnt - Loan amount
# annual_inc - Annual income
# term - Term of the loan (36 or 60 months)
# purpose - Reason for borrowing money
# pub_rec_bankruptcies - Number of bankruptcies on public record
# revol_util - Amount of credit the borrower is using relative to all available revolving credit.
# loan_amount_by_annual_inc - Loan amount to annual income ratio
# ---------------------------------





# Bivariate Analysis
#-----------------------

# Box plot: purpose vs loan_amnt
ggplot(loan,aes(x=purpose,y=loan_amnt)) + geom_boxplot(aes(fill=new_status)) + scale_fill_manual(name="",values = c("green","red")) + theme(legend.position = "right",plot.title = element_text(hjust = 0.5)) + labs(x="Purpose", y="Loan Amount($)",title="Purpose vs Loan Amount") + coord_flip()
# Conclusion: Small business had the highest median loan amount for defaulters. For most of the reasons (purpose) the median loan amount
#             was the same or higher for defaulted compared to cleared loans.



# Box plot: int_rate vs term
ggplot(loan,aes(x=term,y=int_rate)) + geom_boxplot(aes(fill=new_status)) + scale_fill_manual(name="",values = c("green","red")) + theme(legend.position = "top",plot.title = element_text(hjust = 0.5)) + labs(x="Term", y="Interest Rate(%)",title="Interest Rate vs Term")
# Conclusion: The median interest rate was higher for 60 months duration loans compared to 36 months. And the median interest rate was
#             higher for defaulted loans compared to cleared loans.



# Box plot: int_rate vs grade
ggplot(loan,aes(x=grade,y=int_rate)) + geom_boxplot(aes(fill=new_status)) + scale_fill_manual(name="",values = c("green","red")) + theme(legend.position = "top",plot.title = element_text(hjust = 0.5)) + labs(x="Grade", y="Interest Rate(%)",title="Interest Rate vs Grade")
# Conclusion: The rate of interest increases as grade increases. For each grade the median interest rate for defaulted loans was higher
#             compared to cleared loans.



# Box plot: int_rate vs revol_util
loan$revol_util_bins <- cut(loan$revol_util,c(0,20,40,60,80,100),include.lowest = T)
ggplot(loan,aes(x=revol_util_bins,y=int_rate)) + geom_boxplot(aes(fill=new_status)) + scale_fill_manual(name="",values = c("green","red")) + theme(legend.position = "top",plot.title = element_text(hjust = 0.5)) + labs(x="Revolving Utilization(%)", y="Interest Rate(%)",title="Interest Rate vs Revolving Utilization")
# Conclusion: The interest rate increases as revolving utilization increases. The median interest rate for defaulted loans was higher 
#             compared to cleared loans for all the bins.



# Box plot: annual_inc vs grade
# Annual income of $187000 covers 98% of the data
ggplot(subset(loan,annual_inc<=187000),aes(x=grade,y=annual_inc)) + geom_boxplot(aes(fill=new_status)) + scale_fill_manual(name="",values = c("green","red")) + theme(legend.position = "top",plot.title = element_text(hjust = 0.5)) + labs(x="Grade", y="Annual Income($)",title="Annual Income vs Grade")
# Conclusion: For every grade the defaulters had lower median annual income compared to those who cleared their loans. 


# Box plot: annual_inc vs dti
loan$dti_bins <- cut(loan$dti,c(0,5,10,15,20,25,30),include.lowest = T)
ggplot(subset(loan,annual_inc<=187000),aes(x=dti_bins,y=annual_inc)) + geom_boxplot(aes(fill=new_status)) + scale_fill_manual(name="",values = c("green","red")) + theme(legend.position = "top",plot.title = element_text(hjust = 0.5)) + labs(x="Debt-to-Income Ratio(%)", y="Annual Income($)",title="Annual Income vs Debt-to-Income Ratio")
# Conclusion: The median income of defaulters is lower compared to that of cleared loans.


# RESULTS FROM BIVARIATE ANALYSIS
#----------------------------------
# Interest rate (or grade) and annual income variables provide the most predictive power for determining potential defaulters.
#----------------------------------


