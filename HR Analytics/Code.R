#-----------------------------------------------------------------------------------------------------------------------------------------------------------
# BUSINESS UNDERSTANDING
# DATA UNDERSTANDING
# EDA
# DATA PREPARATION
# MODEL BUILDING
# MODEL EVALUATION
# RECOMMENDATIONS
#------------------------------------------------------------------------------------------------------------------------------------------------------------

#--------------------------------------------BUSINESS UNDERSTANDING------------------------------------------------------------------------------------------

# A company XYZ, at a given point of time has 4000 employees. Every year around 15% leave the company. This level of attrition is very bad for the company.

# Goal: To model the probability of attrition using logistic regression. It will be used to understand what factors the company should focus on, 
# in order to curb attrition. Also, which of these variables are most important and needs to be addressed right away.

#-----------------------------------------------------------------------------------------------------------------------------------------------------------

#---------------------------------------------DATA UNDERSTANDING-------------------------------------------------------------------------------------------
#Install and Load the required packages
#install.packages("MASS")
#install.packages("car")
#install.packages("e1071")
#install.packages("caret", dependencies = c("Depends", "Suggests"))
#install.packages("cowplot")
#install.packages("GGally")
#install.packages("caTools")
#install.packages("ROCR")
library(ROCR)
library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(GGally)
library(dplyr)

# Loading Input files
general_data<- read.csv("general_data.csv", stringsAsFactors = F)
employee_survey_data<- read.csv("employee_survey_data.csv", stringsAsFactors = F)
manager_survey_data<- read.csv("manager_survey_data.csv", stringsAsFactors = F)
out_time <- read.csv("out_time.csv", stringsAsFactors = F)
in_time <- read.csv("in_time.csv", stringsAsFactors = F)

#DATA cLEANING

#Before Merging different datasets, lets see if we can drop unwanted columns.

# Duplicate columns in General_data

# We need to find columns which is having same value.

general_data_dummy_cols <- as.vector(which(sapply(general_data, function(x) length(unique(x)) ) == 1))

# from above command , we can see we have "EmployeeCount", "StandardHours", "Over18" varaibles have same values and will not be
# useful in modeling.

general_data <- general_data[,-general_data_dummy_cols] 

# Duplicate columns in employee_survey_data

as.vector(which(sapply(employee_survey_data, function(x) length(unique(x)) ) == 1))
str(employee_survey_data)

# we dont have duplicate columns.

# Duplicate columns in manager_survey_data
as.vector(which(sapply(manager_survey_data, function(x) length(unique(x)) ) == 1))
str(manager_survey_data)

# we dont have duplicate columns.

# In_time and Out_time will have all rows NA for the Public Holidays columns which can be truncated.

In_time_dummy_cols <- as.vector(which(sapply(in_time, function(x) length(unique(x)) ) == 1))
In_time_dummy_cols
length(In_time_dummy_cols)

#there are 12 Public Holiday in in_time, Let me first check the out_time whether it matches the in_time.

out_time_dummy_cols <- as.vector(which(sapply(out_time, function(x) length(unique(x)) ) == 1))
out_time_dummy_cols
length(out_time_dummy_cols)
#it does match, so we can delete these columns from intime and outtime.

in_time <- in_time[,-In_time_dummy_cols]
out_time <- out_time[,-out_time_dummy_cols]

# Lets check if we have same employee id values for all 5 data sets. to see we can merge without losing any rows.

setdiff(general_data$EmployeeID,employee_survey_data$EmployeeID)
setdiff(general_data$EmployeeID,manager_survey_data$EmployeeID)
# Both intime and out_time as X column which represents employee id.
setdiff(general_data$EmployeeID,in_time$X)
setdiff(general_data$EmployeeID,out_time$X)

# All set for merging, but intime and out_time has 250 columns with in and out timestamp,
# lets do a feature engineering in from these two datasets to get "Avg_Working_Hrs" and "No_of_Leave"

# Method, 

# 1. Convert all the values in in_time and out_time to convert as datatime and extract only hour part.
# 2. Calcultate the Average in time and outtime hr for each row in the dataframe.
# 3. Count of NA for each row which represents the No of Vacations.
# 4. Merger the intime and outtime data frame with X column(employee id) and then difference Avg_out_hr - Avg_in_hr to get
#    avg_working_hr.


# 1. Convert all cols as as.date in intime and out_time

Extract_hour <- function(df){ 
                          df[,2:ncol(df)] <- as.data.frame(sapply(df[, 2:ncol(df)],  function(x){
                            format(as.POSIXct(strptime(x,"%Y-%m-%d %H:%M:%S")) ,format = "%H")
                          }
                          ))
  return(df)
}



in_time_hr <- Extract_hour(in_time)

out_time_hr <- Extract_hour(out_time)

# Lets convert all the columns value to numeric type.

in_time_hr <- as.data.frame(lapply(in_time_hr, function(x) as.numeric(as.character(x))))
str(in_time_hr)
#Lets calcualte the Avg In hr and Avg Out hr for each employee from the extracted hr dataframe



in_time_hr['Avg_in_hr'] <- rowMeans(in_time_hr[,2:ncol(in_time_hr)], na.rm = TRUE)
summary(in_time_hr['Avg_in_hr'])

# For Outtime:
out_time_hr <- as.data.frame(lapply(out_time_hr, function(x) as.numeric(as.character(x))))
str(out_time_hr)
#Lets calcualte the Avg In hr and Avg Out hr for each employee from the extracted hr dataframe

out_time_hr['Avg_out_hr'] <- rowMeans(out_time_hr[,2:ncol(out_time_hr)], na.rm = TRUE)
summary(out_time_hr['Avg_out_hr'])

in_time_hr['Avg_working_hrs'] <- out_time_hr['Avg_out_hr'] - in_time_hr['Avg_in_hr']
summary(in_time_hr['Avg_working_hrs'])

#Lets check no of vacations taken by each employee.

in_time_hr['No_of_Vacations'] <- rowSums(is.na(in_time_hr))

out_time_hr['No_of_Vacations'] <- rowSums(is.na(out_time_hr))

# This will check if in_time data is matching with out_time data, so that there is no issue in data.
sum(in_time_hr['No_of_Vacations'] == out_time_hr['No_of_Vacations'])
# Count matched to 4410. All set.

time_derived_measures <- data.frame("EmployeeID" = in_time_hr$X,
                                    "Avg_working_hrs" = in_time_hr$Avg_working_hrs,
                                    "No_of_Vacations" = in_time_hr$No_of_Vacations) 

#We have "Avg_working_hrs" and "No_of_Vacations" is time_derived_measures dataframe which we can merge in to general_data.

General_data_time <-
  merge(general_data, time_derived_measures, by = 'EmployeeID', all = F)

# Lets merge the employee and manager survey data into above dataframe.


Merged_data <-
  merge(General_data_time, employee_survey_data, by = 'EmployeeID', all = F)

Final_data <-
  merge(Merged_data, manager_survey_data, by = 'EmployeeID', all = F)


# Lets start Working on missing data from final_data

sum(is.na(Final_data))
#we have total 111 Missing values.

#Lets check the number of records which has missing values.

nrow(Final_data[!complete.cases(Final_data),])

#We have 110 rows with NA values, so it is about 110/4410 = 2.4 %, so we can omit the records.

Final_data <- na.omit(Final_data)

#Lets remove Employee id columns, as this is not useful in modeling.

Final_data$EmployeeID <- NULL

#--------------------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------EDA-----------------------------------------------------------------------------------


# Lets start the EDA, we can convert the columns into Factors and Numerics types before EDA.

str(Final_data)

#Convert all Char items to Factor:
Fact_variables <- sapply(Final_data, is.character)
Final_data[Fact_variables] <- lapply(Final_data[Fact_variables], as.factor)

# Barcharts for categorical features with stacked Attrition information
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")

plot_grid(ggplot(Final_data, aes(x=BusinessTravel,fill=Attrition))+ geom_bar(position = position_fill())+ labs(y="Fraction"), 
          ggplot(Final_data, aes(x=MaritalStatus,fill=Attrition))+ geom_bar(position = position_fill())+ labs(y="Fraction") +bar_theme1,
          align = "h")  

plot_grid(ggplot(Final_data, aes(x=factor(Education),fill=Attrition))+ geom_bar(position = position_fill())+ labs(y="Fraction") +
            scale_x_discrete(breaks = 1:5, labels = c("Below College","College","Bachelor","Master","Doctor")), 
          ggplot(Final_data, aes(x=Gender,fill=Attrition))+ geom_bar(position = position_fill())+ labs(y="Fraction")+bar_theme1,
          align = "h")  

plot_grid(ggplot(Final_data, aes(x=JobRole,fill=Attrition))+ geom_bar(position = position_fill())+ labs(y="Fraction") +bar_theme1, 
          ggplot(Final_data, aes(x=EducationField,fill=Attrition))+ geom_bar(position = position_fill())+ labs(y="Fraction") 
          + bar_theme1,
          align = "h")  

plot_grid(ggplot(Final_data, aes(x=Department,fill=Attrition))+ geom_bar(position = position_fill())+ labs(y="Fraction"), 
          ggplot(Final_data, aes(x=factor(PerformanceRating),fill=Attrition))+ geom_bar(position = position_fill())+ labs(y="Fraction") + 
            scale_x_discrete(breaks = 1:4, labels = c("Low","Good","Excellent","Outstanding")) 
          +bar_theme1,
          align = "h") 


plot_grid(ggplot(Final_data, aes(x=factor(WorkLifeBalance),fill=Attrition))+ geom_bar(position = position_fill())+ labs(y="Fraction") + 
            scale_x_discrete(breaks = 1:4, labels = c("Low","Medium","High","Very High")) ,
          ggplot(Final_data, aes(x=factor(JobSatisfaction),fill=Attrition))+ geom_bar(position = position_fill())+ labs(y="Fraction") + 
            scale_x_discrete(breaks = 1:4, labels = c("Low","Medium","High","Very High")) 
          +bar_theme1,
          align = "h")  

# Boxplots for numeric variables 
box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")



plot_grid(ggplot(Final_data, aes(x=Attrition,y=NumCompaniesWorked, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(Final_data, aes(x=Attrition,y=Age, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(Final_data, aes(x=Attrition,y=TotalWorkingYears, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)


plot_grid(ggplot(Final_data, aes(x=Attrition,y=PercentSalaryHike, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(Final_data, aes(x=Attrition,y=MonthlyIncome, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(Final_data, aes(x=Attrition,y=DistanceFromHome, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)


plot_grid(ggplot(Final_data, aes(x=Attrition,y=TrainingTimesLastYear, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(Final_data, aes(x=Attrition,y=YearsAtCompany, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(Final_data, aes(x=Attrition,y=No_of_Vacations,fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)


plot_grid(ggplot(Final_data, aes(x=Attrition,y=Avg_working_hrs, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(Final_data, aes(x=Attrition,y=YearsSinceLastPromotion, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(Final_data, aes(x=Attrition,y=YearsWithCurrManager, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)

ggplot(Final_data,aes(x=YearsSinceLastPromotion,fill=Attrition))+ geom_histogram(col="black",breaks=seq(0,15,by=3),position = position_fill())


str(Final_data)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------

#-------------------------------------------------------------DATA PREPARATION----------------------------------------------------------------------------------

#There are still int varaibles which are factor like Education, JobLevel which needs to be converted to factors.

Int_to_Fact <- c("Education","JobLevel","StockOptionLevel","EnvironmentSatisfaction", "JobSatisfaction",
                 "WorkLifeBalance","JobInvolvement","PerformanceRating")
Final_data[Int_to_Fact] <- lapply(Final_data[Int_to_Fact], as.factor)

Int_Num_variables <- sapply(Final_data, is.numeric) | sapply(Final_data, is.integer)

#Check of outliers in integer and numeric values:
sapply(Final_data[Int_Num_variables], function (x) {boxplot.stats(x)$out } )
# CONCLUSION: These outliers are not errors in measurements. They are valid values. So no outlier treatment is done.

#Correlation Plot against all Numerical features.

ggpairs(Final_data[Int_Num_variables])

#As expected, years in the company & years with the current manager is highly correlated


# Lets scale all the integer and numeric varaibles.

Final_data[Int_Num_variables] <- lapply(Final_data[Int_Num_variables], scale)

#Lets create Dummies for all the Fact variables.

Fact_variables <- sapply(Final_data, is.factor)
Fact_Dataframe <- Final_data[Fact_variables] 
Non_Fact_Dataframe <-Final_data[Int_Num_variables]

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(Fact_Dataframe, 
                            function(x) data.frame(model.matrix(~x-1,data =Fact_Dataframe))[,-1]))

#Lets create a combined dataframe for the model
HR_Analytics_Model<- cbind(Non_Fact_Dataframe,dummies)

#--------------------------------------------------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------MODEL BUILDING--------------------------------------------------------------------
########################################################################
# splitting the data between train and test
set.seed(100)

indices = sample.split(HR_Analytics_Model$Attrition, SplitRatio = 0.7)

train = HR_Analytics_Model[indices,]

test = HR_Analytics_Model[!(indices),]

########################################################################
# Logistic Regression: 

#Initial model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) 

# Stepwise selection
model_2<- stepAIC(model_1, direction="both")

summary(model_2)
# Removing multicollinearity through VIF check

sort(vif(model_2))

#EducationField.xLife.Sciences Removed.
model_3<-  glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Avg_working_hrs + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 Education.x5 + EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobLevel.x2 + JobLevel.x5 + JobRole.xHuman.Resources + JobRole.xManager + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
               data = train)

summary(model_3)

# Removing multicollinearity through VIF check
sort(vif(model_3))


#BusinessTravel.xTravel_Rarely Removed.

model_4<-  glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Avg_working_hrs + BusinessTravel.xTravel_Frequently +
                 Education.x5 + EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobLevel.x2 + JobLevel.x5 + JobRole.xHuman.Resources + JobRole.xManager + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
               data = train)

summary(model_4)

# Removing multicollinearity through VIF check

sort(vif(model_4))

#EducationField.xMarketing Removed

model_5<-  glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Avg_working_hrs + BusinessTravel.xTravel_Frequently +
                 Education.x5 +
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobLevel.x2 + JobLevel.x5 + JobRole.xHuman.Resources + JobRole.xManager + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
               data = train)

summary(model_5)

# Removing multicollinearity through VIF check

sort(vif(model_5))

#EducationField.xMedical removed.

model_6<-  glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Avg_working_hrs + BusinessTravel.xTravel_Frequently +
                 Education.x5 +
                 EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobLevel.x2 + JobLevel.x5 + JobRole.xHuman.Resources + JobRole.xManager + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
               data = train)

summary(model_6)

# Removing multicollinearity through VIF check

sort(vif(model_6))

#EducationField.xTechnical.Degree Removed.

model_7<-  glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Avg_working_hrs + BusinessTravel.xTravel_Frequently +
                 Education.x5 +
                 EducationField.xOther + 
                 JobLevel.x2 + JobLevel.x5 + JobRole.xHuman.Resources + JobRole.xManager + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
               data = train)

summary(model_7)

# Removing multicollinearity through VIF check

sort(vif(model_7))

#EducationField.xOther Removed.

model_7<-  glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Avg_working_hrs + BusinessTravel.xTravel_Frequently +
                 Education.x5 +
                 JobLevel.x2 + JobLevel.x5 + JobRole.xHuman.Resources + JobRole.xManager + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
               data = train)

summary(model_7)

# Removing multicollinearity through VIF check

sort(vif(model_7))


#JobLevel.x5 removed.


model_8<-  glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Avg_working_hrs + BusinessTravel.xTravel_Frequently +
                 Education.x5 +
                 JobLevel.x2 + JobRole.xHuman.Resources + JobRole.xManager + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                 StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
               data = train)

summary(model_8)

# Removing multicollinearity through VIF check

sort(vif(model_8))


#MaritalStatus.xMarried removed.

model_9<-  glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Avg_working_hrs + BusinessTravel.xTravel_Frequently +
                 Education.x5 +
                 JobLevel.x2 + JobRole.xHuman.Resources + JobRole.xManager + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive +  MaritalStatus.xSingle + 
                 StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
               data = train)

summary(model_9)

# Removing multicollinearity through VIF check

sort(vif(model_9))

#JobRole.xHuman.Resources removed.

model_10<-  glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Avg_working_hrs + BusinessTravel.xTravel_Frequently +
                 Education.x5 +
                 JobLevel.x2 + JobRole.xManager + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive +  MaritalStatus.xSingle + 
                 StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
               data = train)

summary(model_10)

# Removing multicollinearity through VIF check

sort(vif(model_10))

#Education.x5 removed.

model_11<-  glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Avg_working_hrs + BusinessTravel.xTravel_Frequently +
                  JobLevel.x2 + JobRole.xManager + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive +  MaritalStatus.xSingle + 
                  StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
                data = train)

summary(model_11)

# Removing multicollinearity through VIF check

sort(vif(model_11))

#JobRole.xManager

model_12<-  glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Avg_working_hrs + BusinessTravel.xTravel_Frequently +
                  JobLevel.x2 + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive +  MaritalStatus.xSingle + 
                  StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
                data = train)

summary(model_12)

# Removing multicollinearity through VIF check

sort(vif(model_12))

#StockOptionLevel.x1  removed.

model_13<-  glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Avg_working_hrs + BusinessTravel.xTravel_Frequently +
                  JobLevel.x2 + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive +  MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
                data = train)

summary(model_13)

# Removing multicollinearity through VIF check

sort(vif(model_13))


#JobLevel.x2 removed.

model_14<-  glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Avg_working_hrs + BusinessTravel.xTravel_Frequently +
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive +  MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
                data = train)

summary(model_14)

# Removing multicollinearity through VIF check

sort(vif(model_14))


#JobInvolvement.x3 Removed.

model_15<-  glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Avg_working_hrs + BusinessTravel.xTravel_Frequently +
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive +  MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 , family = "binomial", 
                data = train)

summary(model_15)

# Removing multicollinearity through VIF check

sort(vif(model_15))

#JobRole.xSales.Executive removed
model_16<-  glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Avg_working_hrs + BusinessTravel.xTravel_Frequently +
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 , family = "binomial", 
                data = train)

summary(model_16)

# Removing multicollinearity through VIF check

sort(vif(model_16))

#JobRole.xResearch.Director Removed
model_17<-  glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Avg_working_hrs + BusinessTravel.xTravel_Frequently +
                  JobRole.xManufacturing.Director +
                  MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 , family = "binomial", 
                data = train)

summary(model_17)

# Removing multicollinearity through VIF check

sort(vif(model_17))

# we have all the significant variables in the model, but we have 19 features which will be difficult to explain to business.
# Lets reduce further variables based on p values.

# TrainingTimesLastYear removed.
model_18<-  glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Avg_working_hrs + BusinessTravel.xTravel_Frequently +
                  JobRole.xManufacturing.Director +
                  MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 , family = "binomial", 
                data = train)

summary(model_18)

# Removing multicollinearity through VIF check

sort(vif(model_18))

#JobSatisfaction.x2 removed.
model_19<-  glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Avg_working_hrs + BusinessTravel.xTravel_Frequently +
                  JobRole.xManufacturing.Director +
                  MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 , family = "binomial", 
                data = train)

summary(model_19)

# Removing multicollinearity through VIF check

sort(vif(model_19))


#JobSatisfaction.x3 removed.
model_20<-  glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Avg_working_hrs + BusinessTravel.xTravel_Frequently +
                  JobRole.xManufacturing.Director +
                  MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 +
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 , family = "binomial", 
                data = train)

summary(model_20)

# Removing multicollinearity through VIF check

sort(vif(model_20))

#EnvironmentSatisfaction.x2 removed.
model_21<-  glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Avg_working_hrs + BusinessTravel.xTravel_Frequently +
                  JobRole.xManufacturing.Director +
                  MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 +
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 , family = "binomial", 
                data = train)

summary(model_21)

#JobRole.xManufacturing.Director removed.

model_22<-  glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Avg_working_hrs + BusinessTravel.xTravel_Frequently +
                  MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 +
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 , family = "binomial", 
                data = train)

summary(model_22)

sort(vif(model_22))

#EnvironmentSatisfaction.x3 removed.
model_23<-  glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Avg_working_hrs + BusinessTravel.xTravel_Frequently +
                  MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.x4 +
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 , family = "binomial", 
                data = train)

summary(model_23)

sort(vif(model_23))

#EnvironmentSatisfaction.x4 removed
model_24<-  glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Avg_working_hrs + BusinessTravel.xTravel_Frequently +
                  MaritalStatus.xSingle + 
                  JobSatisfaction.x4 
                  + WorkLifeBalance.x2 + WorkLifeBalance.x3 
                  + WorkLifeBalance.x4 
                , family = "binomial", 
                data = train)

summary(model_24)

sort(vif(model_24))

#WorkLifeBalance.x2 removed.
model_25<-  glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Avg_working_hrs + BusinessTravel.xTravel_Frequently +
                  MaritalStatus.xSingle + 
                  JobSatisfaction.x4 
                + WorkLifeBalance.x3 
                + WorkLifeBalance.x4 
                , family = "binomial", 
                data = train)

summary(model_25)

sort(vif(model_25))

#WorkLifeBalance.x4 removed.
model_26<-  glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                  TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Avg_working_hrs + BusinessTravel.xTravel_Frequently +
                  MaritalStatus.xSingle + 
                  JobSatisfaction.x4 
                + WorkLifeBalance.x3 
                , family = "binomial", 
                data = train)

summary(model_26)

sort(vif(model_26))

########################################################################
# With ALL significant variables in the model, lets treat model_26

final_model<- model_26

#######################################################################

#--------------------------------------------------------------------------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------MODEL EVALUATION--------------------------------------------------------------------


### Test Data ####

#predicted probabilities of Attrition 1 for test data

test_pred = predict(final_model, type = "response", 
                    newdata = test)


# Let's see the summary 

summary(test_pred)

test$prob <- test_pred
View(test)
# Let's use the probability cutoff of 50%.

test_pred_Attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_Attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))


table(test_actual_Attrition,test_pred_Attrition)


#######################################################################
test_pred_Attrition <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))

test_conf <- confusionMatrix(test_pred_Attrition, test_actual_Attrition, positive = "Yes")
test_conf
#######################################################################

# Sensitivity is very low (0.354) compared to Specificity, so lets use a cutoff function for getting tradeoff.


#Cutoff functions which will calculate the sensitivity, specificity and accuracy for given range(0.1 to 0.8) of cutoff
perform_fn <- function(cutoff) 
{
  predicted_Attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_Attrition, test_actual_Attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}


summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]
cutoff
# Let's choose a cutoff value of 0.1776 for final model

test_cutoff_Attrition <- factor(ifelse(test_pred >=0.1776, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_Attrition, test_actual_Attrition, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

View(test)
##################################################################################################
### KS -statistic - Test Data ######

test_cutoff_Attrition <- ifelse(test_cutoff_Attrition=="Yes",1,0)
test_actual_Attrition <- ifelse(test_actual_Attrition=="Yes",1,0)

#on testing  data
pred_object_test<- prediction(test_cutoff_Attrition, test_actual_Attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)



####################################################################
# Lift & Gain Chart 

# plotting the lift chart


lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

#Creating decile dataframe using lift function.
Attrition_decile = lift(test_actual_Attrition, test_pred, groups = 10)

#Column for Base lift of 1.
Attrition_decile$BaseLift <- rep.int(1, 10)

# Theme for Lift and Gain charts
lift_gain_theme <- theme(
  panel.background = element_rect(fill = NA),
  panel.grid.major = element_line(colour = "grey50"),
  panel.ontop = TRUE,
  axis.text.x = element_text(face="bold", color="#993333", 
                             size=14, angle=45),
  axis.text.y = element_text(face="bold", color="#993333", 
                             size=14, angle=45),
  plot.title = element_text(lineheight=.8, face="bold")
    
)


#Lift chart, we can see 2 lift at 4rd decile.

ggplot(data=Attrition_decile, aes(x=bucket)) +
  geom_line(aes(y=Cumlift, colour = "Cumlift"))+
  geom_point(aes(y=Cumlift)) +
  geom_text(aes(y=Cumlift,label=format.default(Cumlift, digits=2)),hjust=0, vjust=0) +
  geom_line(aes(y=BaseLift, colour = "Baseline Lift")) +
  lift_gain_theme +
  ggtitle("Lift Chart") +
  scale_y_continuous(breaks=seq(0,100,10)) +
  scale_x_continuous(breaks=seq(0,10,1)) 
  

#Random_Gain column with increment of 10% from 10 to 100
Attrition_decile$Random_Gain <- as.vector(seq(from=10,to=100,by=10))

#Gain chart, we can see 79% gain in 4rd Decile which is good.
ggplot(data=Attrition_decile, aes(x=bucket)) +
  geom_line(aes(y=Gain, colour = "Gain"))+
  geom_point(aes(y=Gain)) +
  geom_text(aes(y=Gain,label=format.default(Gain, digits=2)),hjust=0, vjust=0) +
  geom_line(aes(y=Random_Gain, colour = "Random Gain")) +
  lift_gain_theme +
  ggtitle("Gain Chart") +
  scale_y_continuous(breaks=seq(0,100,10)) +
  scale_x_continuous(breaks=seq(0,10,1))
   
# At 4th decile
# Gain: 79%
# Lift : 2
#--------------------------------------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------RECOMMENDATIONS------------------------------------------------------------------------------

# Varibales in the final model and their contribution to probability of attrition based on estimate(Beta):
# Age - Younger the age more the attrition rate so we need to address the issues faced by Junior employees.
# NumofCompaniesWorked - Higher the companies the candidate has chnages, more possiblility of employee leaving the organisation.
# TotalWorkingyears - Higher working experiance leads to less chances of attrition.
# Avg_working_hrs - higher the number of hours worked higher the chances of attrition
# Yearssincelastpromotion - Higher the time since employee got promotion, higher the chance of attrition.
# Yearswithcurrmanager - Higher the time spent with current manager, lesses the chance of attrition.
# Worklifebalance.high - Higher the satisfaction lower the attrition
# JobSatisfaction.VeryHigh - higer the satisfaction lower the attrition
# BusinessTravel.Travel_Frequently - higher business travel leads to higher attrition
# MaritalStatus.Single - high estimate value indicating stronger relation to attrition, Single employee have higher chances of attrition

# Care should be taken that employees are not unhappy with the worklifebalance and Jobsatisfaction environment. Business travel frequency should be discussed with the employee and
# kept to an acceptable limit. Also its the Manager's responsibility to see that the employee is not overworked.

