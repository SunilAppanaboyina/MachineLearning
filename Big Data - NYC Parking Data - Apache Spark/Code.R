# load SparkR
library(SparkR)
library(ggplot2)

# initiating the spark session
sparkR.session(master='local')

# Reading data into Spark Data Frame
#2015
df_2015<- read.df("s3://bigdata-sappanab-s3-oregon/NYCParkingTickets/FY_2015.csv",source = "csv", inferSchema = "true", header = "true")
# 2016
df_2016<- read.df("s3://bigdata-sappanab-s3-oregon/NYCParkingTickets/FY_2016.csv",source = "csv", inferSchema = "true", header = "true")
# 2017
df_2017<- read.df("s3://bigdata-sappanab-s3-oregon/NYCParkingTickets/FY_2017.csv",source = "csv", inferSchema = "true", header = "true")


# 2015
dim(df_2015)
str(df_2015)
printSchema(df_2015)
getNumPartitions(df_2015)
# 2016
dim(df_2016)
str(df_2016)
printSchema(df_2016)
getNumPartitions(df_2016)
# 2017
dim(df_2017)
str(df_2017)
printSchema(df_2017)
getNumPartitions(df_2017)


# converting 'Issue Date' column from CHAR to DATE data type
# 2015
df_2015 <- withColumn(df_2015,"Issue Date",to_date(df_2015$`Issue Date`,"MM/dd/yyyy"))
# 2016
df_2016 <- withColumn(df_2016,"Issue Date",to_date(df_2016$`Issue Date`,"MM/dd/yyyy"))
# 2017
df_2017 <- withColumn(df_2017,"Issue Date",to_date(df_2017$`Issue Date`,"MM/dd/yyyy"))

# checking if the data has records from other years also
# 2015
year_chk_2015 <- summarize(groupBy(df_2015, year(df_2015$`Issue Date`)),count = n(df_2015$`Issue Date`))
head(arrange(year_chk_2015, year_chk_2015$`year(Issue Date)`),100)
# 2016
year_chk_2016 <- summarize(groupBy(df_2016, year(df_2016$`Issue Date`)),count = n(df_2016$`Issue Date`))
head(arrange(year_chk_2016, year_chk_2016$`year(Issue Date)`),100)
# 2017
year_chk_2017 <- summarize(groupBy(df_2017, year(df_2017$`Issue Date`)),count = n(df_2017$`Issue Date`))
head(arrange(year_chk_2017, year_chk_2017$`year(Issue Date)`),100)
# CONCLUSION: Data from all the three years has records from other years as well

# FURTHUR ANALYSIS WILL BE USING DATA FROM CALENDAR YEARS 2015, 2016 & 2017
# 2015
df_2015 <- where(df_2015,year(df_2015$`Issue Date`)==2015)
# 2016
df_2016 <- where(df_2016,year(df_2016$`Issue Date`)==2016)
# 2017
df_2017 <- where(df_2017,year(df_2017$`Issue Date`)==2017)

# new dimension after above step
# 2015
dim(df_2015)
# 2016
dim(df_2016)
# 2017
dim(df_2017)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# EXAMINE THE DATA
#----------------------

# QUESTION 1. FIND TOTAL NUMBER OF TICKETS FOR EACH YEAR

# checking for Null and missing values
# 2015
count(where(df_2015, isNull(df_2015$`Summons Number`)))
count(where(df_2015, df_2015$`Summons Number`==""))
# 2016
count(where(df_2016, isNull(df_2016$`Summons Number`)))
count(where(df_2016, df_2016$`Summons Number`==""))
# 2017
count(where(df_2017, isNull(df_2017$`Summons Number`)))
count(where(df_2017, df_2017$`Summons Number`==""))
# CONCLUSION: No Null or missing values in the column for all three years

# checking for duplicates
# 2015
gp_sn_2015 <- summarize(groupBy(df_2015, df_2015$`Summons Number`),count = n(df_2015$`Summons Number`))
count(where(gp_sn_2015,gp_sn_2015$count>1))
# 2016
gp_sn_2016 <- summarize(groupBy(df_2016, df_2016$`Summons Number`),count = n(df_2016$`Summons Number`))
count(where(gp_sn_2016,gp_sn_2016$count>1))
# 2017
gp_sn_2017 <- summarize(groupBy(df_2017, df_2017$`Summons Number`),count = n(df_2017$`Summons Number`))
count(where(gp_sn_2017,gp_sn_2017$count>1))
# CONCLUSION: the column in all the three years has duplicates 

# removing duplicates. henceforth this data frame will be used for analysis
# 2015
df_2015_nodup <- dropDuplicates(df_2015,"Summons Number")
# 2016
df_2016_nodup <- dropDuplicates(df_2016,"Summons Number")
# 2017
df_2017_nodup <- dropDuplicates(df_2017,"Summons Number")

cache(df_2015_nodup)
cache(df_2016_nodup)
cache(df_2017_nodup)


# After clean up finally answering the question, how many tickets each year
# 2015
count_2015 <- head(select(df_2015_nodup, count(df_2015_nodup$`Summons Number`)))
# 2016
count_2016 <- head(select(df_2016_nodup, count(df_2016_nodup$`Summons Number`)))
# 2017
count_2017 <- head(select(df_2017_nodup, count(df_2017_nodup$`Summons Number`)))

# dimensions of the new data frame with data only from the respective year and no duplicates
# 2015
dim(df_2015_nodup)
# 2016
dim(df_2016_nodup)
# 2017
dim(df_2017_nodup)

str(count_2017)
### PLOT###

count_2015$year <- '2015'
count_2016$year <- '2016'
count_2017$year <- '2017'
count_df <- rbind(count_2015,count_2016,count_2017)
ggplot(count_df, aes(x = year, y = `count(Summons Number)`, fill = year )) +geom_bar(stat = "identity") + scale_y_continuous(name="Number of Tickets", labels = scales::comma)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------

# QUESTION 2. FIND HOW MANY UNIQUE STATES THE CARS WHICH GOT PARKING TICKETS CAME FROM.

# 2015
unique_states_2015 <- summarize(groupBy(df_2015_nodup, df_2015_nodup$`Registration State`),count = n(df_2015_nodup$`Registration State`))      
head(unique_states_2015,100) # the list shows '99' as a state. Not changing it an considering it as a state
head(count(unique_states_2015)) # the list has states from Canada as well

# 2016
unique_states_2016 <- summarize(groupBy(df_2016_nodup, df_2016_nodup$`Registration State`),count = n(df_2016_nodup$`Registration State`))      
head(unique_states_2016,100) # the list shows '99' as a state. Not changing it an considering it as a state
head(count(unique_states_2016)) # the list has states from Canada as well

# 2017
unique_states_2017 <- summarize(groupBy(df_2017_nodup, df_2017_nodup$`Registration State`),count = n(df_2017_nodup$`Registration State`))      
head(unique_states_2017,100) # the list shows '99' as a state. Not changing it an considering it as a state
head(count(unique_states_2017)) # the list has states from Canada as well


#---------------------------------------------------------------------------------------------------------------------------------------------------------------

# QUESTION 3. SOME PARKING TICKETS DONT HAVE ADDRESSES ON THEM. FIND OUT HOW MANY TICKETS SUCH THERE ARE.

# Assuming address here means the location where the violation took place, finding the missing values in `Violation Location` column.

# 2015
count(where(df_2015_nodup, isNull(df_2015$`Violation Location`)))
# 2016
count(where(df_2016_nodup, isNull(df_2016$`Violation Location`)))
# 2017
count(where(df_2017_nodup, isNull(df_2017$`Violation Location`)))

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# AGGREGATION TASKS
#---------------------------------

# QUESTION 1. HOW OFTEN DOES EACH VIOLATION CODE OCCUR? (FREQUENCY OF VIOLATION CODES - FIND THE TOP 5)

# 2015
all_viola_2015 <- summarize(groupBy(df_2015_nodup, df_2015_nodup$`Violation Code`),count = n(df_2015_nodup$`Violation Code`))
Violation_2015 <-head(arrange(all_viola_2015,desc(all_viola_2015$count)),5) 
# 2016
all_viola_2016 <- summarize(groupBy(df_2016_nodup, df_2016_nodup$`Violation Code`),count = n(df_2016_nodup$`Violation Code`))
Violation_2016 <- head(arrange(all_viola_2016,desc(all_viola_2016$count)),5) 
# 2017
all_viola_2017 <- summarize(groupBy(df_2017_nodup, df_2017_nodup$`Violation Code`),count = n(df_2017_nodup$`Violation Code`))
Violation_2017 <- head(arrange(all_viola_2017,desc(all_viola_2017$count)),5) 


require(gridExtra)

p1 <- ggplot(Violation_2015, aes(x = as.factor(`Violation Code`), y = count, fill = as.factor(`Violation Code`) ) ) +geom_bar(stat = "identity") + scale_y_continuous(name="Ticket count", labels = scales::comma)+ ggtitle("2015 data")
p2 <- ggplot(Violation_2016, aes(x = as.factor(`Violation Code`), y = count, fill = as.factor(`Violation Code`) )) +geom_bar(stat = "identity") + scale_y_continuous(name="Ticket count", labels = scales::comma)+ ggtitle("2016 data")
p3 <- ggplot(Violation_2017, aes(x = as.factor(`Violation Code`), y = count,fill = as.factor(`Violation Code`) )) +geom_bar(stat = "identity") + scale_y_continuous(name="Ticket count", labels = scales::comma)+ ggtitle("2017 data")
grid.arrange(plot1, plot2,plot3, ncol=3)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------

# QUESTION 2. HOW OFTEN DOES EACH VEHICLE BODY GET PARKING TICKET ? (FIND THE TOP 5)

# 2015
vehicle_body_2015 <- summarize(groupBy(df_2015_nodup, df_2015_nodup$`Vehicle Body Type`),count = n(df_2015_nodup$`Vehicle Body Type`))
head(arrange(vehicle_body_2015,desc(vehicle_body_2015$count)),5)
# 2016
vehicle_body_2016 <- summarize(groupBy(df_2016_nodup, df_2016_nodup$`Vehicle Body Type`),count = n(df_2016_nodup$`Vehicle Body Type`))
head(arrange(vehicle_body_2016,desc(vehicle_body_2016$count)),5)
# 2017
vehicle_body_2017 <- summarize(groupBy(df_2017_nodup, df_2017_nodup$`Vehicle Body Type`),count = n(df_2017_nodup$`Vehicle Body Type`))
head(arrange(vehicle_body_2017,desc(vehicle_body_2017$count)),5)

# HOW ABOUT VEHICLE MAKE ? (FIND THE TOP 5)

# 2015
vehicle_make_2015 <- summarize(groupBy(df_2015_nodup, df_2015_nodup$`Vehicle Make`),count = n(df_2015_nodup$`Vehicle Make`))
vehicleType_2015  <- head(arrange(vehicle_make_2015,desc(vehicle_make_2015$count)),5)
# 2016
vehicle_make_2016 <- summarize(groupBy(df_2016_nodup, df_2016_nodup$`Vehicle Make`),count = n(df_2016_nodup$`Vehicle Make`))
vehicleType_2016  <- head(arrange(vehicle_make_2016,desc(vehicle_make_2016$count)),5)
# 2017
vehicle_make_2017 <- summarize(groupBy(df_2017_nodup, df_2017_nodup$`Vehicle Make`),count = n(df_2017_nodup$`Vehicle Make`))
vehicleType_2017  <-head(arrange(vehicle_make_2017,desc(vehicle_make_2017$count)),5)


p1 <- ggplot(vehicleType_2015, aes(x = as.factor(`Vehicle Make`), y = count, fill = as.factor(`Vehicle Make`) ) ) +geom_bar(stat = "identity") + scale_y_continuous(name="Ticket count", labels = scales::comma)+ ggtitle("2015 data")
p2 <- ggplot(vehicleType_2016, aes(x = as.factor(`Vehicle Make`), y = count, fill = as.factor(`Vehicle Make`) )) +geom_bar(stat = "identity") + scale_y_continuous(name="Ticket count", labels = scales::comma)+ ggtitle("2016 data")
p3 <- ggplot(vehicleType_2017, aes(x = as.factor(`Vehicle Make`), y = count, fill = as.factor(`Vehicle Make`) )) +geom_bar(stat = "identity") + scale_y_continuous(name="Ticket count", labels = scales::comma)+ ggtitle("2017 data")
grid.arrange(p1, p2,p3, ncol=3)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------

# QUESTION 3. FIND THE (5 HIGHEST) FREQUENCIES OF:

#  VIOLATING PRECINCTS
# 2015
viola_precinct_2015 <- summarize(groupBy(df_2015_nodup, df_2015_nodup$`Violation Precinct`),count = n(df_2015_nodup$`Violation Precinct`))
head(arrange(viola_precinct_2015,desc(viola_precinct_2015$count)),5)
# 2016
viola_precinct_2016 <- summarize(groupBy(df_2016_nodup, df_2016_nodup$`Violation Precinct`),count = n(df_2016_nodup$`Violation Precinct`))
head(arrange(viola_precinct_2016,desc(viola_precinct_2016$count)),5)
# 2017
viola_precinct_2017 <- summarize(groupBy(df_2017_nodup, df_2017_nodup$`Violation Precinct`),count = n(df_2017_nodup$`Violation Precinct`))
head(arrange(viola_precinct_2017,desc(viola_precinct_2017$count)),5)


# ISSUING PRECINCTS
# 2015
issuer_precinct_2015 <- summarize(groupBy(df_2015_nodup, df_2015_nodup$`Issuer Precinct`),count = n(df_2015_nodup$`Issuer Precinct`))
precinct_2015 <- head(arrange(issuer_precinct_2015,desc(issuer_precinct_2015$count)),5)
# 2016
issuer_precinct_2016 <- summarize(groupBy(df_2016_nodup, df_2016_nodup$`Issuer Precinct`),count = n(df_2016_nodup$`Issuer Precinct`))
precinct_2016 <- head(arrange(issuer_precinct_2016,desc(issuer_precinct_2016$count)),5)
# 2017
issuer_precinct_2017 <- summarize(groupBy(df_2017_nodup, df_2017_nodup$`Issuer Precinct`),count = n(df_2017_nodup$`Issuer Precinct`))
precinct_2017 <- head(arrange(issuer_precinct_2017,desc(issuer_precinct_2017$count)),5)

p1 <- ggplot(precinct_2015, aes(x = as.factor(`Issuer Precinct`), y = count, fill = as.factor(`Issuer Precinct`) ) ) +geom_bar(stat = "identity") + scale_y_continuous(name="Ticket count", labels = scales::comma)+ ggtitle("2015 data")
p2 <- ggplot(precinct_2016, aes(x = as.factor(`Issuer Precinct`), y = count, fill = as.factor(`Issuer Precinct`) )) +geom_bar(stat = "identity") + scale_y_continuous(name="Ticket count", labels = scales::comma)+ ggtitle("2016 data")
p3 <- ggplot(precinct_2017, aes(x = as.factor(`Issuer Precinct`), y = count, fill = as.factor(`Issuer Precinct`) )) +geom_bar(stat = "identity") + scale_y_continuous(name="Ticket count", labels = scales::comma)+ ggtitle("2017 data")
grid.arrange(p1, p2,p3, ncol=3)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------

# QUESTION 4. FIND THE VIOLATION CODE FREQUENCY ACROSS 3 PRECINCTS WHICH HAVE ISSUED THE MOST NUMBER OF TICKETS - 

# 2015
# Issuer Precincts which issued most are 0, 19 and 14
# Issuer Precinct : 0
df_2015_nodup_0isp <- filter(df_2015_nodup,df_2015_nodup$`Issuer Precinct`==0)
isp0_2015_vc <- summarize(groupBy(df_2015_nodup_0isp, df_2015_nodup_0isp$`Violation Code`),count = n(df_2015_nodup_0isp$`Violation Code`))
head(arrange(isp0_2015_vc,desc(isp0_2015_vc$count)),5)
# Issuer Precinct : 19
df_2015_nodup_19isp <- filter(df_2015_nodup,df_2015_nodup$`Issuer Precinct`==19)
isp19_2015_vc <- summarize(groupBy(df_2015_nodup_19isp, df_2015_nodup_19isp$`Violation Code`),count = n(df_2015_nodup_19isp$`Violation Code`))
head(arrange(isp19_2015_vc,desc(isp19_2015_vc$count)),5)
# Issuer Precinct : 14
df_2015_nodup_14isp <- filter(df_2015_nodup,df_2015_nodup$`Issuer Precinct`==14)
isp14_2015_vc <- summarize(groupBy(df_2015_nodup_14isp, df_2015_nodup_14isp$`Violation Code`),count = n(df_2015_nodup_14isp$`Violation Code`))
head(arrange(isp14_2015_vc,desc(isp14_2015_vc$count)),5)

# 2016
# Issuer Precincts which issued most are 0, 19 and 13
# Issuer Precinct : 0
df_2016_nodup_0isp <- filter(df_2016_nodup,df_2016_nodup$`Issuer Precinct`==0)
isp0_2016_vc <- summarize(groupBy(df_2016_nodup_0isp, df_2016_nodup_0isp$`Violation Code`),count = n(df_2016_nodup_0isp$`Violation Code`))
head(arrange(isp0_2016_vc,desc(isp0_2016_vc$count)),5)
# Issuer Precinct : 19
df_2016_nodup_19isp <- filter(df_2016_nodup,df_2016_nodup$`Issuer Precinct`==19)
isp19_2016_vc <- summarize(groupBy(df_2016_nodup_19isp, df_2016_nodup_19isp$`Violation Code`),count = n(df_2016_nodup_19isp$`Violation Code`))
head(arrange(isp19_2016_vc,desc(isp19_2016_vc$count)),5)
# Issuer Precinct : 13
df_2016_nodup_13isp <- filter(df_2016_nodup,df_2016_nodup$`Issuer Precinct`==13)
isp13_2016_vc <- summarize(groupBy(df_2016_nodup_13isp, df_2016_nodup_13isp$`Violation Code`),count = n(df_2016_nodup_13isp$`Violation Code`))
head(arrange(isp13_2016_vc,desc(isp13_2016_vc$count)),5)


# 2017
# Issuer Precincts which issued most are 0, 19 and 14
# Issuer Precinct : 0
df_2017_nodup_0isp <- filter(df_2017_nodup,df_2017_nodup$`Issuer Precinct`==0)
isp0_2017_vc <- summarize(groupBy(df_2017_nodup_0isp, df_2017_nodup_0isp$`Violation Code`),count = n(df_2017_nodup_0isp$`Violation Code`))
head(arrange(isp0_2017_vc,desc(isp0_2017_vc$count)),5)
# Issuer Precinct : 19
df_2017_nodup_19isp <- filter(df_2017_nodup,df_2017_nodup$`Issuer Precinct`==19)
isp19_2017_vc <- summarize(groupBy(df_2017_nodup_19isp, df_2017_nodup_19isp$`Violation Code`),count = n(df_2017_nodup_19isp$`Violation Code`))
head(arrange(isp19_2017_vc,desc(isp19_2017_vc$count)),5)
# Issuer Precinct : 14
df_2017_nodup_14isp <- filter(df_2017_nodup,df_2017_nodup$`Issuer Precinct`==14)
isp14_2017_vc <- summarize(groupBy(df_2017_nodup_14isp, df_2017_nodup_14isp$`Violation Code`),count = n(df_2017_nodup_14isp$`Violation Code`))
head(arrange(isp14_2017_vc,desc(isp14_2017_vc$count)),5)


#---------------------------------------------------------------------------------------------------------------------------------------------------------------

# QUESTION 5. FIND PROPERTIES OF PARKING VIOLATIONS ACROSS DIFFERENT TIMES OF THE DAY. FIND WAY TO DEAL WITH MISSING VALUES.
# DIVIDE 24 HRS INTO EQUAL BINS AND FIND VIOLATIONS OCCURRING. ALSO FOR A PARTICULAR VIOLATION FIND COMMONLY OCCURRING TIMES.

# 2015
# count Nulls
count(where(df_2015_nodup, isNull(df_2015_nodup$`Violation Time`)))
# out of 5373971 rows only 664 rows have missing values thats 0.01%. Better to remove these rows:
df_2015_nodup_vt <- where(df_2015_nodup, isNotNull(df_2015_nodup$`Violation Time`))

vt_last_char_2015 <- summarize(groupBy(df_2015_nodup_vt, substr(df_2015_nodup_vt$`Violation Time`,6,6)),count = n(substr(df_2015_nodup_vt$`Violation Time`,6,6)))
head(arrange(vt_last_char_2015,desc(vt_last_char_2015$count)))
# from this we know that the 'Violation Time' values end with 'A' or 'P'

# converting 'Violation Time' in char format to integer format. If the 5th char is 'A' then store the first 4 chars as integer. If 'P' then add 1200 to the first 4 chars and store as integer.
df_2015_nodup_vt <- withColumn(df_2015_nodup_vt,"Violation Time Parsed",ifelse(substr(df_2015_nodup_vt$`Violation Time`,6,6)=="A",cast(substr(df_2015_nodup_vt$`Violation Time`,2,5),"integer"),cast(substr(df_2015_nodup_vt$`Violation Time`,2,5),"integer")+1200))

# checking is the above operation on 'Violation Time' column has generated Null values
count(where(df_2015_nodup_vt, isNull(df_2015_nodup_vt$`Violation Time Parsed`)))
# After viewing the data frame it was observed that some values in 'Violation Time' had strange chars due to which instead of integer it got converted to Null
# removing the Nulls from the data frame
df_2015_nodup_vt <- where(df_2015_nodup_vt, isNotNull(df_2015_nodup_vt$`Violation Time Parsed`))

# The 'Violation Time' column has error data such as "8023P". So considering only records with proper time between 0-2400 hrs.
df_2015_nodup_vt <- where(df_2015_nodup_vt,df_2015_nodup_vt$`Violation Time Parsed`>=0 & df_2015_nodup_vt$`Violation Time Parsed`<=2400)
dim(df_2015_nodup_vt) # 4894597      52

# Divided into groups
df_2015_nodup_vt$`Parts of Day` <- ifelse(df_2015_nodup_vt$`Violation Time Parsed`>=0 & df_2015_nodup_vt$`Violation Time Parsed`<=400,"Midnight",ifelse(df_2015_nodup_vt$`Violation Time Parsed`>400 & df_2015_nodup_vt$`Violation Time Parsed`<=800,"Early Morning",ifelse(df_2015_nodup_vt$`Violation Time Parsed`>800 & df_2015_nodup_vt$`Violation Time Parsed`<=1200,"Morning",ifelse(df_2015_nodup_vt$`Violation Time Parsed`>1200 & df_2015_nodup_vt$`Violation Time Parsed`<=1600,"Afternoon",ifelse(df_2015_nodup_vt$`Violation Time Parsed`>1600 & df_2015_nodup_vt$`Violation Time Parsed`<=2000,"Evening","Night")))))

# For each of these groups, find the 3 most commonly occurring violations
# For Midnight
df_2015_nodup_vt_MN <- where(df_2015_nodup_vt,df_2015_nodup_vt$`Parts of Day`=="Midnight")
df_2015_nodup_vt_MN <- summarize(groupBy(df_2015_nodup_vt_MN, df_2015_nodup_vt_MN$`Violation Code`),count = n(df_2015_nodup_vt_MN$`Violation Code`))
common_violtion_midnight <- head(arrange(df_2015_nodup_vt_MN,desc(df_2015_nodup_vt_MN$count)),3)


# For Early Morning
df_2015_nodup_vt_EM <- where(df_2015_nodup_vt,df_2015_nodup_vt$`Parts of Day`=="Early Morning")
df_2015_nodup_vt_EM <- summarize(groupBy(df_2015_nodup_vt_EM, df_2015_nodup_vt_EM$`Violation Code`),count = n(df_2015_nodup_vt_EM$`Violation Code`))
common_violtion_earlymorning <- head(arrange(df_2015_nodup_vt_EM,desc(df_2015_nodup_vt_EM$count)),3)


# For Morning
df_2015_nodup_vt_MO <- where(df_2015_nodup_vt,df_2015_nodup_vt$`Parts of Day`=="Morning")
df_2015_nodup_vt_MO <- summarize(groupBy(df_2015_nodup_vt_MO, df_2015_nodup_vt_MO$`Violation Code`),count = n(df_2015_nodup_vt_MO$`Violation Code`))
common_violtion_morning <-head(arrange(df_2015_nodup_vt_MO,desc(df_2015_nodup_vt_MO$count)),3)


# For Afternoon
df_2015_nodup_vt_AF <- where(df_2015_nodup_vt,df_2015_nodup_vt$`Parts of Day`=="Afternoon")
df_2015_nodup_vt_AF <- summarize(groupBy(df_2015_nodup_vt_AF, df_2015_nodup_vt_AF$`Violation Code`),count = n(df_2015_nodup_vt_AF$`Violation Code`))
common_violtion_afternoon <- head(arrange(df_2015_nodup_vt_AF,desc(df_2015_nodup_vt_AF$count)),3)



# For Evening
df_2015_nodup_vt_EV <- where(df_2015_nodup_vt,df_2015_nodup_vt$`Parts of Day`=="Evening")
df_2015_nodup_vt_EV <- summarize(groupBy(df_2015_nodup_vt_EV, df_2015_nodup_vt_EV$`Violation Code`),count = n(df_2015_nodup_vt_EV$`Violation Code`))
common_violtion_evening <-head(arrange(df_2015_nodup_vt_EV,desc(df_2015_nodup_vt_EV$count)),3)



# For Night
df_2015_nodup_vt_NI <- where(df_2015_nodup_vt,df_2015_nodup_vt$`Parts of Day`=="Night")
df_2015_nodup_vt_NI <- summarize(groupBy(df_2015_nodup_vt_NI, df_2015_nodup_vt_NI$`Violation Code`),count = n(df_2015_nodup_vt_NI$`Violation Code`))
common_violtion_night <- head(arrange(df_2015_nodup_vt_NI,desc(df_2015_nodup_vt_NI$count)),3)


# For the 3 most commonly occurring violation codes, find the most common times of day
# First finding the 3 most commonly occuring violation codes for this violation time cleaned data frame
df_2015_nodup_vt_viola <- summarize(groupBy(df_2015_nodup_vt, df_2015_nodup_vt$`Violation Code`),count = n(df_2015_nodup_vt$`Violation Code`))
head(arrange(df_2015_nodup_vt_viola,desc(df_2015_nodup_vt_viola$count)),3)

df_2015_nodup_vt_viola_21 <- where(df_2015_nodup_vt,df_2015_nodup_vt$`Violation Code`==21)
df_2015_nodup_vt_viola_21 <- summarize(groupBy(df_2015_nodup_vt_viola_21, df_2015_nodup_vt_viola_21$`Parts of Day`),count = n(df_2015_nodup_vt_viola_21$`Parts of Day`))
comoncode_21 <- head(arrange(df_2015_nodup_vt_viola_21,desc(df_2015_nodup_vt_viola_21$count)),3)

df_2015_nodup_vt_viola_38 <- where(df_2015_nodup_vt,df_2015_nodup_vt$`Violation Code`==38)
df_2015_nodup_vt_viola_38 <- summarize(groupBy(df_2015_nodup_vt_viola_38, df_2015_nodup_vt_viola_38$`Parts of Day`),count = n(df_2015_nodup_vt_viola_38$`Parts of Day`))
comoncode_38 <- head(arrange(df_2015_nodup_vt_viola_38,desc(df_2015_nodup_vt_viola_38$count)),3)

df_2015_nodup_vt_viola_14 <- where(df_2015_nodup_vt,df_2015_nodup_vt$`Violation Code`==14)
df_2015_nodup_vt_viola_14 <- summarize(groupBy(df_2015_nodup_vt_viola_14, df_2015_nodup_vt_viola_14$`Parts of Day`),count = n(df_2015_nodup_vt_viola_14$`Parts of Day`))
comoncode_14 <-head(arrange(df_2015_nodup_vt_viola_14,desc(df_2015_nodup_vt_viola_14$count)),3)

p1 <- ggplot(comoncode_21, aes(x = as.factor(`Parts of Day`), y = count, fill = as.factor(`Parts of Day`) ) ) +geom_bar(stat = "identity") + scale_y_continuous(name="Ticket count", labels = scales::comma)+ ggtitle("2015:code -21 data")
p2 <- ggplot(comoncode_38, aes(x = as.factor(`Parts of Day`), y = count, fill = as.factor(`Parts of Day`) )) +geom_bar(stat = "identity") + scale_y_continuous(name="Ticket count", labels = scales::comma)+ ggtitle("2015:code -38 data")
p3 <- ggplot(comoncode_14, aes(x = as.factor(`Parts of Day`), y = count, fill = as.factor(`Parts of Day`) )) +geom_bar(stat = "identity") + scale_y_continuous(name="Ticket count", labels = scales::comma)+ ggtitle("2015:code -14 data")
grid.arrange(p1, p2,p3, ncol=3)

# 2016
# count Nulls
count(where(df_2016_nodup, isNull(df_2016_nodup$`Violation Time`)))
# only 74 rows have missing values. Better to remove these rows:
df_2016_nodup_vt <- where(df_2016_nodup, isNotNull(df_2016_nodup$`Violation Time`))

vt_last_char_2016 <- summarize(groupBy(df_2016_nodup_vt, substr(df_2016_nodup_vt$`Violation Time`,6,6)),count = n(substr(df_2016_nodup_vt$`Violation Time`,6,6)))
head(arrange(vt_last_char_2016,desc(vt_last_char_2016$count)))
# from this we know that the 'Violation Time' values end with 'A' or 'P'

# converting 'Violation Time' in char format to integer format. If the 5th char is 'A' then store the first 4 chars as integer. If 'P' then add 1200 to the first 4 chars and store as integer.
df_2016_nodup_vt <- withColumn(df_2016_nodup_vt,"Violation Time Parsed",ifelse(substr(df_2016_nodup_vt$`Violation Time`,6,6)=="A",cast(substr(df_2016_nodup_vt$`Violation Time`,2,5),"integer"),cast(substr(df_2016_nodup_vt$`Violation Time`,2,5),"integer")+1200))

# checking is the above operation on 'Violation Time' column has generated Null values
count(where(df_2016_nodup_vt, isNull(df_2016_nodup_vt$`Violation Time Parsed`)))
# After viewing the data frame it was observed that some values in 'Violation Time' had strange chars due to which instead of integer it got converted to Null
# removing the Nulls from the data frame
df_2016_nodup_vt <- where(df_2016_nodup_vt, isNotNull(df_2016_nodup_vt$`Violation Time Parsed`))

# The 'Violation Time' column has error data such as "8023P". So considering only records with proper time between 0-2400 hrs.
df_2016_nodup_vt <- where(df_2016_nodup_vt,df_2016_nodup_vt$`Violation Time Parsed`>=0 & df_2016_nodup_vt$`Violation Time Parsed`<=2400)
dim(df_2016_nodup_vt) # 4430826      52

# Divided into groups
df_2016_nodup_vt$`Parts of Day` <- ifelse(df_2016_nodup_vt$`Violation Time Parsed`>=0 & df_2016_nodup_vt$`Violation Time Parsed`<=400,"Midnight",ifelse(df_2016_nodup_vt$`Violation Time Parsed`>400 & df_2016_nodup_vt$`Violation Time Parsed`<=800,"Early Morning",ifelse(df_2016_nodup_vt$`Violation Time Parsed`>800 & df_2016_nodup_vt$`Violation Time Parsed`<=1200,"Morning",ifelse(df_2016_nodup_vt$`Violation Time Parsed`>1200 & df_2016_nodup_vt$`Violation Time Parsed`<=1600,"Afternoon",ifelse(df_2016_nodup_vt$`Violation Time Parsed`>1600 & df_2016_nodup_vt$`Violation Time Parsed`<=2000,"Evening","Night")))))

# For each of these groups, find the 3 most commonly occurring violations
# For Midnight
df_2016_nodup_vt_MN <- where(df_2016_nodup_vt,df_2016_nodup_vt$`Parts of Day`=="Midnight")
df_2016_nodup_vt_MN <- summarize(groupBy(df_2016_nodup_vt_MN, df_2016_nodup_vt_MN$`Violation Code`),count = n(df_2016_nodup_vt_MN$`Violation Code`))
head(arrange(df_2016_nodup_vt_MN,desc(df_2016_nodup_vt_MN$count)),3)

# For Early Morning
df_2016_nodup_vt_EM <- where(df_2016_nodup_vt,df_2016_nodup_vt$`Parts of Day`=="Early Morning")
df_2016_nodup_vt_EM <- summarize(groupBy(df_2016_nodup_vt_EM, df_2016_nodup_vt_EM$`Violation Code`),count = n(df_2016_nodup_vt_EM$`Violation Code`))
head(arrange(df_2016_nodup_vt_EM,desc(df_2016_nodup_vt_EM$count)),3)

# For Morning
df_2016_nodup_vt_MO <- where(df_2016_nodup_vt,df_2016_nodup_vt$`Parts of Day`=="Morning")
df_2016_nodup_vt_MO <- summarize(groupBy(df_2016_nodup_vt_MO, df_2016_nodup_vt_MO$`Violation Code`),count = n(df_2016_nodup_vt_MO$`Violation Code`))
head(arrange(df_2016_nodup_vt_MO,desc(df_2016_nodup_vt_MO$count)),3)

# For Afternoon
df_2016_nodup_vt_AF <- where(df_2016_nodup_vt,df_2016_nodup_vt$`Parts of Day`=="Afternoon")
df_2016_nodup_vt_AF <- summarize(groupBy(df_2016_nodup_vt_AF, df_2016_nodup_vt_AF$`Violation Code`),count = n(df_2016_nodup_vt_AF$`Violation Code`))
head(arrange(df_2016_nodup_vt_AF,desc(df_2016_nodup_vt_AF$count)),3)

# For Evening
df_2016_nodup_vt_EV <- where(df_2016_nodup_vt,df_2016_nodup_vt$`Parts of Day`=="Evening")
df_2016_nodup_vt_EV <- summarize(groupBy(df_2016_nodup_vt_EV, df_2016_nodup_vt_EV$`Violation Code`),count = n(df_2016_nodup_vt_EV$`Violation Code`))
head(arrange(df_2016_nodup_vt_EV,desc(df_2016_nodup_vt_EV$count)),3)

# For Night
df_2016_nodup_vt_NI <- where(df_2016_nodup_vt,df_2016_nodup_vt$`Parts of Day`=="Night")
df_2016_nodup_vt_NI <- summarize(groupBy(df_2016_nodup_vt_NI, df_2016_nodup_vt_NI$`Violation Code`),count = n(df_2016_nodup_vt_NI$`Violation Code`))
head(arrange(df_2016_nodup_vt_NI,desc(df_2016_nodup_vt_NI$count)),3)

# For the 3 most commonly occurring violation codes, find the most common times of day
# First finding the 3 most commonly occuring violation codes for this violation time cleaned data frame
df_2016_nodup_vt_viola <- summarize(groupBy(df_2016_nodup_vt, df_2016_nodup_vt$`Violation Code`),count = n(df_2016_nodup_vt$`Violation Code`))
head(arrange(df_2016_nodup_vt_viola,desc(df_2016_nodup_vt_viola$count)),3)

df_2016_nodup_vt_viola_21 <- where(df_2016_nodup_vt,df_2016_nodup_vt$`Violation Code`==21)
df_2016_nodup_vt_viola_21 <- summarize(groupBy(df_2016_nodup_vt_viola_21, df_2016_nodup_vt_viola_21$`Parts of Day`),count = n(df_2016_nodup_vt_viola_21$`Parts of Day`))
comoncode_21 <- head(arrange(df_2016_nodup_vt_viola_21,desc(df_2016_nodup_vt_viola_21$count)),3)

df_2016_nodup_vt_viola_36 <- where(df_2016_nodup_vt,df_2016_nodup_vt$`Violation Code`==36)
df_2016_nodup_vt_viola_36 <- summarize(groupBy(df_2016_nodup_vt_viola_36, df_2016_nodup_vt_viola_36$`Parts of Day`),count = n(df_2016_nodup_vt_viola_36$`Parts of Day`))
comoncode_36 <-head(arrange(df_2016_nodup_vt_viola_36,desc(df_2016_nodup_vt_viola_36$count)),3)

df_2016_nodup_vt_viola_38 <- where(df_2016_nodup_vt,df_2016_nodup_vt$`Violation Code`==38)
df_2016_nodup_vt_viola_38 <- summarize(groupBy(df_2016_nodup_vt_viola_38, df_2016_nodup_vt_viola_38$`Parts of Day`),count = n(df_2016_nodup_vt_viola_38$`Parts of Day`))
comoncode_38 <-head(arrange(df_2016_nodup_vt_viola_38,desc(df_2016_nodup_vt_viola_38$count)),3)

p1 <- ggplot(comoncode_21, aes(x = as.factor(`Parts of Day`), y = count, fill = as.factor(`Parts of Day`) ) ) +geom_bar(stat = "identity") + scale_y_continuous(name="Ticket count", labels = scales::comma)+ ggtitle("2016:code -21 data")
p2 <- ggplot(comoncode_36, aes(x = as.factor(`Parts of Day`), y = count, fill = as.factor(`Parts of Day`) )) +geom_bar(stat = "identity") + scale_y_continuous(name="Ticket count", labels = scales::comma)+ ggtitle("2016:code -36 data")
p3 <- ggplot(comoncode_38, aes(x = as.factor(`Parts of Day`), y = count, fill = as.factor(`Parts of Day`) )) +geom_bar(stat = "identity") + scale_y_continuous(name="Ticket count", labels = scales::comma)+ ggtitle("2016:code -38 data")
grid.arrange(p1, p2,p3, ncol=3)


# 2017
# count Nulls
count(where(df_2017_nodup, isNull(df_2017_nodup$`Violation Time`)))
# only 16 rows have missing values. Better to remove these rows:
df_2017_nodup_vt <- where(df_2017_nodup, isNotNull(df_2017_nodup$`Violation Time`))

vt_last_char_2017 <- summarize(groupBy(df_2017_nodup_vt, substr(df_2017_nodup_vt$`Violation Time`,6,6)),count = n(substr(df_2017_nodup_vt$`Violation Time`,6,6)))
head(arrange(vt_last_char_2017,desc(vt_last_char_2017$count)))
# from this we know that the 'Violation Time' values end with 'A' or 'P'

# converting 'Violation Time' in char format to integer format. If the 5th char is 'A' then store the first 4 chars as integer. If 'P' then add 1200 to the first 4 chars and store as integer.
df_2017_nodup_vt <- withColumn(df_2017_nodup_vt,"Violation Time Parsed",ifelse(substr(df_2017_nodup_vt$`Violation Time`,6,6)=="A",cast(substr(df_2017_nodup_vt$`Violation Time`,2,5),"integer"),cast(substr(df_2017_nodup_vt$`Violation Time`,2,5),"integer")+1200))

# checking is the above operation on 'Violation Time' column has generated Null values
count(where(df_2017_nodup_vt, isNull(df_2017_nodup_vt$`Violation Time Parsed`)))
# After viewing the data frame it was observed that some values in 'Violation Time' had strange chars due to which instead of integer it got converted to Null
# removing the Nulls from the data frame
df_2017_nodup_vt <- where(df_2017_nodup_vt, isNotNull(df_2017_nodup_vt$`Violation Time Parsed`))

# The 'Violation Time' column has error data such as "8023P". So considering only records with proper time between 0-2400 hrs.
df_2017_nodup_vt <- where(df_2017_nodup_vt,df_2017_nodup_vt$`Violation Time Parsed`>=0 & df_2017_nodup_vt$`Violation Time Parsed`<=2400)
dim(df_2017_nodup_vt) # 4932007      44

# Divided into groups
df_2017_nodup_vt$`Parts of Day` <- ifelse(df_2017_nodup_vt$`Violation Time Parsed`>=0 & df_2017_nodup_vt$`Violation Time Parsed`<=400,"Midnight",ifelse(df_2017_nodup_vt$`Violation Time Parsed`>400 & df_2017_nodup_vt$`Violation Time Parsed`<=800,"Early Morning",ifelse(df_2017_nodup_vt$`Violation Time Parsed`>800 & df_2017_nodup_vt$`Violation Time Parsed`<=1200,"Morning",ifelse(df_2017_nodup_vt$`Violation Time Parsed`>1200 & df_2017_nodup_vt$`Violation Time Parsed`<=1600,"Afternoon",ifelse(df_2017_nodup_vt$`Violation Time Parsed`>1600 & df_2017_nodup_vt$`Violation Time Parsed`<=2000,"Evening","Night")))))

# For each of these groups, find the 3 most commonly occurring violations
# For Midnight
df_2017_nodup_vt_MN <- where(df_2017_nodup_vt,df_2017_nodup_vt$`Parts of Day`=="Midnight")
df_2017_nodup_vt_MN <- summarize(groupBy(df_2017_nodup_vt_MN, df_2017_nodup_vt_MN$`Violation Code`),count = n(df_2017_nodup_vt_MN$`Violation Code`))
head(arrange(df_2017_nodup_vt_MN,desc(df_2017_nodup_vt_MN$count)),3)

# For Early Morning
df_2017_nodup_vt_EM <- where(df_2017_nodup_vt,df_2017_nodup_vt$`Parts of Day`=="Early Morning")
df_2017_nodup_vt_EM <- summarize(groupBy(df_2017_nodup_vt_EM, df_2017_nodup_vt_EM$`Violation Code`),count = n(df_2017_nodup_vt_EM$`Violation Code`))
head(arrange(df_2017_nodup_vt_EM,desc(df_2017_nodup_vt_EM$count)),3)

# For Morning
df_2017_nodup_vt_MO <- where(df_2017_nodup_vt,df_2017_nodup_vt$`Parts of Day`=="Morning")
df_2017_nodup_vt_MO <- summarize(groupBy(df_2017_nodup_vt_MO, df_2017_nodup_vt_MO$`Violation Code`),count = n(df_2017_nodup_vt_MO$`Violation Code`))
head(arrange(df_2017_nodup_vt_MO,desc(df_2017_nodup_vt_MO$count)),3)

# For Afternoon
df_2017_nodup_vt_AF <- where(df_2017_nodup_vt,df_2017_nodup_vt$`Parts of Day`=="Afternoon")
df_2017_nodup_vt_AF <- summarize(groupBy(df_2017_nodup_vt_AF, df_2017_nodup_vt_AF$`Violation Code`),count = n(df_2017_nodup_vt_AF$`Violation Code`))
head(arrange(df_2017_nodup_vt_AF,desc(df_2017_nodup_vt_AF$count)),3)

# For Evening
df_2017_nodup_vt_EV <- where(df_2017_nodup_vt,df_2017_nodup_vt$`Parts of Day`=="Evening")
df_2017_nodup_vt_EV <- summarize(groupBy(df_2017_nodup_vt_EV, df_2017_nodup_vt_EV$`Violation Code`),count = n(df_2017_nodup_vt_EV$`Violation Code`))
head(arrange(df_2017_nodup_vt_EV,desc(df_2017_nodup_vt_EV$count)),3)

# For Night
df_2017_nodup_vt_NI <- where(df_2017_nodup_vt,df_2017_nodup_vt$`Parts of Day`=="Night")
df_2017_nodup_vt_NI <- summarize(groupBy(df_2017_nodup_vt_NI, df_2017_nodup_vt_NI$`Violation Code`),count = n(df_2017_nodup_vt_NI$`Violation Code`))
head(arrange(df_2017_nodup_vt_NI,desc(df_2017_nodup_vt_NI$count)),3)

# For the 3 most commonly occurring violation codes, find the most common times of day
# First finding the 3 most commonly occuring violation codes for this violation time cleaned data frame
df_2017_nodup_vt_viola <- summarize(groupBy(df_2017_nodup_vt, df_2017_nodup_vt$`Violation Code`),count = n(df_2017_nodup_vt$`Violation Code`))
head(arrange(df_2017_nodup_vt_viola,desc(df_2017_nodup_vt_viola$count)),3)

df_2017_nodup_vt_viola_21 <- where(df_2017_nodup_vt,df_2017_nodup_vt$`Violation Code`==21)
df_2017_nodup_vt_viola_21 <- summarize(groupBy(df_2017_nodup_vt_viola_21, df_2017_nodup_vt_viola_21$`Parts of Day`),count = n(df_2017_nodup_vt_viola_21$`Parts of Day`))
comoncode_21 <- head(arrange(df_2017_nodup_vt_viola_21,desc(df_2017_nodup_vt_viola_21$count)),3)

df_2017_nodup_vt_viola_36 <- where(df_2017_nodup_vt,df_2017_nodup_vt$`Violation Code`==36)
df_2017_nodup_vt_viola_36 <- summarize(groupBy(df_2017_nodup_vt_viola_36, df_2017_nodup_vt_viola_36$`Parts of Day`),count = n(df_2017_nodup_vt_viola_36$`Parts of Day`))
comoncode_36 <- head(arrange(df_2017_nodup_vt_viola_36,desc(df_2017_nodup_vt_viola_36$count)),3)

df_2017_nodup_vt_viola_38 <- where(df_2017_nodup_vt,df_2017_nodup_vt$`Violation Code`==38)
df_2017_nodup_vt_viola_38 <- summarize(groupBy(df_2017_nodup_vt_viola_38, df_2017_nodup_vt_viola_38$`Parts of Day`),count = n(df_2017_nodup_vt_viola_38$`Parts of Day`))
comoncode_38 <- head(arrange(df_2017_nodup_vt_viola_38,desc(df_2017_nodup_vt_viola_38$count)),3)

p1 <- ggplot(comoncode_21, aes(x = as.factor(`Parts of Day`), y = count, fill = as.factor(`Parts of Day`) ) ) +geom_bar(stat = "identity") + scale_y_continuous(name="Ticket count", labels = scales::comma)+ ggtitle("2017:code -21 data")
p2 <- ggplot(comoncode_36, aes(x = as.factor(`Parts of Day`), y = count, fill = as.factor(`Parts of Day`) )) +geom_bar(stat = "identity") + scale_y_continuous(name="Ticket count", labels = scales::comma)+ ggtitle("2017:code -36 data")
p3 <- ggplot(comoncode_38, aes(x = as.factor(`Parts of Day`), y = count, fill = as.factor(`Parts of Day`) )) +geom_bar(stat = "identity") + scale_y_continuous(name="Ticket count", labels = scales::comma)+ ggtitle("2017:code -38 data")
grid.arrange(p1, p2,p3, ncol=3)


#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# QUESTION 6. DIVIDE THE YEAR INTO SOME NUMBER OF SEASONS, AND FIND FREQUENCIES OF TICKETS FOR EACH SEASON. FIND 3 MOST COMMON VIOLATIONS FOR EACH SEASON

# 2015
df_2015_nodup$Seasons <- ifelse(month(df_2015_nodup$`Issue Date`)==03 | month(df_2015_nodup$`Issue Date`)==04,"Spring",ifelse(month(df_2015_nodup$`Issue Date`)>=05 & month(df_2015_nodup$`Issue Date`)<=08,"Summer",ifelse(month(df_2015_nodup$`Issue Date`)==09 | month(df_2015_nodup$`Issue Date`)==10,"Autmn","Winter")))
season_2015_tickets <- summarize(groupBy(df_2015_nodup,df_2015_nodup$Seasons),count=n(df_2015_nodup$Seasons))
head(arrange(season_2015_tickets,desc(season_2015_tickets$count))) # Surprisingly in Autum there were no tickets issued
# Winter
df_2015_nodup_winter <- where(df_2015_nodup,df_2015_nodup$Seasons=="Winter")
df_2015_nodup_winter <- summarize(groupBy(df_2015_nodup_winter, df_2015_nodup_winter$`Violation Code`),count = n(df_2015_nodup_winter$`Violation Code`))
winter_2015 <- head(arrange(df_2015_nodup_winter,desc(df_2015_nodup_winter$count)),3)
# Spring
df_2015_nodup_spring <- where(df_2015_nodup,df_2015_nodup$Seasons=="Spring")
df_2015_nodup_spring <- summarize(groupBy(df_2015_nodup_spring, df_2015_nodup_spring$`Violation Code`),count = n(df_2015_nodup_spring$`Violation Code`))
spring_2015 <- head(arrange(df_2015_nodup_spring,desc(df_2015_nodup_spring$count)),3)
# Summer
df_2015_nodup_summer <- where(df_2015_nodup,df_2015_nodup$Seasons=="Summer")
df_2015_nodup_summer <- summarize(groupBy(df_2015_nodup_summer, df_2015_nodup_summer$`Violation Code`),count = n(df_2015_nodup_summer$`Violation Code`))
summer_2015 <- head(arrange(df_2015_nodup_summer,desc(df_2015_nodup_summer$count)),3)
p1 <- ggplot(winter_2015, aes(x = as.factor(`Violation Code`), y = count, fill = as.factor(`Violation Code`) ) ) +geom_bar(stat = "identity") + scale_y_continuous(name="Ticket count", labels = scales::comma)+ ggtitle("2015 Winter")
p2 <- ggplot(spring_2015, aes(x = as.factor(`Violation Code`), y = count, fill = as.factor(`Violation Code`) )) +geom_bar(stat = "identity") + scale_y_continuous(name="Ticket count", labels = scales::comma)+ ggtitle("2015 Spring")
p3 <- ggplot(summer_2015, aes(x = as.factor(`Violation Code`), y = count, fill = as.factor(`Violation Code`) )) +geom_bar(stat = "identity") + scale_y_continuous(name="Ticket count", labels = scales::comma)+ ggtitle("2015 summer")
grid.arrange(p1, p2,p3, ncol=3)
# 2016
df_2016_nodup$Seasons <- ifelse(month(df_2016_nodup$`Issue Date`)==03 | month(df_2016_nodup$`Issue Date`)==04,"Spring",ifelse(month(df_2016_nodup$`Issue Date`)>=05 & month(df_2016_nodup$`Issue Date`)<=08,"Summer",ifelse(month(df_2016_nodup$`Issue Date`)==09 | month(df_2016_nodup$`Issue Date`)==10,"Autmn","Winter")))
season_2016_tickets <- summarize(groupBy(df_2016_nodup,df_2016_nodup$Seasons),count=n(df_2016_nodup$Seasons))
head(arrange(season_2016_tickets,desc(season_2016_tickets$count)))
# Winter
df_2016_nodup_winter <- where(df_2016_nodup,df_2016_nodup$Seasons=="Winter")
df_2016_nodup_winter <- summarize(groupBy(df_2016_nodup_winter, df_2016_nodup_winter$`Violation Code`),count = n(df_2016_nodup_winter$`Violation Code`))
winter_2016 <-head(arrange(df_2016_nodup_winter,desc(df_2016_nodup_winter$count)),3)
# Spring
df_2016_nodup_spring <- where(df_2016_nodup,df_2016_nodup$Seasons=="Spring")
df_2016_nodup_spring <- summarize(groupBy(df_2016_nodup_spring, df_2016_nodup_spring$`Violation Code`),count = n(df_2016_nodup_spring$`Violation Code`))
spring_2016 <-head(arrange(df_2016_nodup_spring,desc(df_2016_nodup_spring$count)),3)
# Summer
df_2016_nodup_summer <- where(df_2016_nodup,df_2016_nodup$Seasons=="Summer")
df_2016_nodup_summer <- summarize(groupBy(df_2016_nodup_summer, df_2016_nodup_summer$`Violation Code`),count = n(df_2016_nodup_summer$`Violation Code`))
summer_2016 <- head(arrange(df_2016_nodup_summer,desc(df_2016_nodup_summer$count)),3)
# Autmn
df_2016_nodup_autmn <- where(df_2016_nodup,df_2016_nodup$Seasons=="Autmn")
df_2016_nodup_autmn <- summarize(groupBy(df_2016_nodup_autmn, df_2016_nodup_autmn$`Violation Code`),count = n(df_2016_nodup_autmn$`Violation Code`))
auumn_2016 <-head(arrange(df_2016_nodup_autmn,desc(df_2016_nodup_autmn$count)),3)

p1 <- ggplot(winter_2016, aes(x = as.factor(`Violation Code`), y = count, fill = as.factor(`Violation Code`) ) ) +geom_bar(stat = "identity") + scale_y_continuous(name="Ticket count", labels = scales::comma)+ ggtitle("2016 Winter")
p2 <- ggplot(spring_2016, aes(x = as.factor(`Violation Code`), y = count, fill = as.factor(`Violation Code`) )) +geom_bar(stat = "identity") + scale_y_continuous(name="Ticket count", labels = scales::comma)+ ggtitle("2016 Spring")
p3 <- ggplot(summer_2016, aes(x = as.factor(`Violation Code`), y = count, fill = as.factor(`Violation Code`) )) +geom_bar(stat = "identity") + scale_y_continuous(name="Ticket count", labels = scales::comma)+ ggtitle("2016 summer")
p4 <- ggplot(auumn_2016, aes(x = as.factor(`Violation Code`), y = count, fill = as.factor(`Violation Code`) )) +geom_bar(stat = "identity") + scale_y_continuous(name="Ticket count", labels = scales::comma)+ ggtitle("2016 autumn")

grid.arrange(p1, p2,p3,p4, ncol=4)
# 2017
df_2017_nodup$Seasons <- ifelse(month(df_2017_nodup$`Issue Date`)==03 | month(df_2017_nodup$`Issue Date`)==04,"Spring",ifelse(month(df_2017_nodup$`Issue Date`)>=05 & month(df_2017_nodup$`Issue Date`)<=08,"Summer",ifelse(month(df_2017_nodup$`Issue Date`)==09 | month(df_2017_nodup$`Issue Date`)==10,"Autmn","Winter")))
season_2017_tickets <- summarize(groupBy(df_2017_nodup,df_2017_nodup$Seasons),count=n(df_2017_nodup$Seasons))
head(arrange(season_2017_tickets,desc(season_2017_tickets$count)))
# Winter
df_2017_nodup_winter <- where(df_2017_nodup,df_2017_nodup$Seasons=="Winter")
df_2017_nodup_winter <- summarize(groupBy(df_2017_nodup_winter, df_2017_nodup_winter$`Violation Code`),count = n(df_2017_nodup_winter$`Violation Code`))
winter_2017 <- head(arrange(df_2017_nodup_winter,desc(df_2017_nodup_winter$count)),3)
# Spring
df_2017_nodup_spring <- where(df_2017_nodup,df_2017_nodup$Seasons=="Spring")
df_2017_nodup_spring <- summarize(groupBy(df_2017_nodup_spring, df_2017_nodup_spring$`Violation Code`),count = n(df_2017_nodup_spring$`Violation Code`))
spring_2017 <-head(arrange(df_2017_nodup_spring,desc(df_2017_nodup_spring$count)),3)
# Summer
df_2017_nodup_summer <- where(df_2017_nodup,df_2017_nodup$Seasons=="Summer")
df_2017_nodup_summer <- summarize(groupBy(df_2017_nodup_summer, df_2017_nodup_summer$`Violation Code`),count = n(df_2017_nodup_summer$`Violation Code`))
summer_2017 <- head(arrange(df_2017_nodup_summer,desc(df_2017_nodup_summer$count)),3)
# Autmn
df_2017_nodup_autmn <- where(df_2017_nodup,df_2017_nodup$Seasons=="Autmn")
df_2017_nodup_autmn <- summarize(groupBy(df_2017_nodup_autmn, df_2017_nodup_autmn$`Violation Code`),count = n(df_2017_nodup_autmn$`Violation Code`))
autmn_2017 <- head(arrange(df_2017_nodup_autmn,desc(df_2017_nodup_autmn$count)),3)

p1 <- ggplot(winter_2017, aes(x = as.factor(`Violation Code`), y = count, fill = as.factor(`Violation Code`) ) ) +geom_bar(stat = "identity") + scale_y_continuous(name="Ticket count", labels = scales::comma)+ ggtitle("2017 Winter")
p2 <- ggplot(spring_2017, aes(x = as.factor(`Violation Code`), y = count, fill = as.factor(`Violation Code`) )) +geom_bar(stat = "identity") + scale_y_continuous(name="Ticket count", labels = scales::comma)+ ggtitle("2017 Spring")
p3 <- ggplot(summer_2017, aes(x = as.factor(`Violation Code`), y = count, fill = as.factor(`Violation Code`) )) +geom_bar(stat = "identity") + scale_y_continuous(name="Ticket count", labels = scales::comma)+ ggtitle("2017 summer")
p4 <- ggplot(autmn_2017, aes(x = as.factor(`Violation Code`), y = count, fill = as.factor(`Violation Code`) )) +geom_bar(stat = "identity") + scale_y_continuous(name="Ticket count", labels = scales::comma)+ ggtitle("2017 autumn")

grid.arrange(p1, p2,p3,p4, ncol=4)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# QUESTION 7. FIND TOTAL OCCURRENCES OF THE 3 MOST COMMON VIOLATION CODES. FIND THE TOTAL AMOUNT COLLECTED FOR ALL THE FINES. 

# From Question 1 we know that for 2015 the top Violation Codes are 21, 38 and 14. Their average fines are $55,$50 and $115 respectively.
fine_amount_2015 <- data.frame(`Violation Code`=c("21","38","14"),Count = c(720902,663904,466488))
fine_amount_2015$'Fine Amount' <- c(fine_amount_2015$Count[1]*55,fine_amount_2015$Count[2]*50,fine_amount_2015$Count[3]*115)
fine_amount_2015[order(-fine_amount_2015$`Fine Amount`),]

# From Question 1 we know that for 2016 the top Violation Codes are 21, 36 and 38. Their average fines are $55, $50 and $50 respectively.
fine_amount_2016 <- data.frame(`Violation Code`=c("21","36","38"),Count = c(664947,615242,547080))
fine_amount_2016$'Fine Amount' <- c(fine_amount_2016$Count[1]*55,fine_amount_2016$Count[2]*50,fine_amount_2016$Count[3]*50)
fine_amount_2016[order(-fine_amount_2016$`Fine Amount`),]

# From Question 1 we know that for 2017 the top Violation Codes are 21, 36 and 38. Their average fines are $55, $50 and $50 respectively.
fine_amount_2017 <- data.frame(`Violation Code`=c("21","36","38"),Count = c(768087,662765,542079))
fine_amount_2017$'Fine Amount' <- c(fine_amount_2017$Count[1]*55,fine_amount_2017$Count[2]*50,fine_amount_2017$Count[3]*50)
fine_amount_2017[order(-fine_amount_2017$`Fine Amount`),]


