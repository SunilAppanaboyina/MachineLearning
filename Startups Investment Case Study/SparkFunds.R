
library(stringr)
library(tidyr)
library(dplyr)

#CHECKPOINT 1: DATA CLEANING 1
#Table 1.1: Understand the Data Set 
#Load the companies and rounds data into two data frames and name them companies and rounds2 respectively.
companies <- read.delim("companies.txt", header=TRUE,stringsAsFactors = FALSE, sep = "\t",na.strings = TRUE)
rounds2 <- read.csv("rounds2.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = TRUE)

#I use a Linux system and RStudio had some issues encoding the original input files to its default format. So the below two steps will
#solve that issue
companies[,sapply(companies,is.character)] <- sapply(companies[,sapply(companies,is.character)],iconv,"ISO-8859-1","Shift_JIS","UTF-8")
rounds2[,sapply(rounds2,is.character)] <- sapply(rounds2[,sapply(rounds2,is.character)],iconv, "ISO-8859-1","Shift_JIS","UTF-8")

#How many unique companies are present in rounds2?
rounds2$company_permalink <- str_to_lower(rounds2$company_permalink)
length(unique(rounds2$company_permalink))

#How many unique companies are present in companies?
companies$permalink <- str_to_lower(companies$permalink)
length(unique(companies$permalink))

#Merge the two data frames so that all variables (columns) in the companies frame are added to the rounds2 data frame. 
#Name the merged frame master_frame. How many observations are present in master_frame?
master_frame <- merge(companies,rounds2,by.x = "permalink",by.y = "company_permalink")
nrow(master_frame)

#CHECKPOINT 2: FUNDING TYPE ANALYSIS
#Table 2.1: Average Values of Investments for Each of these Funding Types
#Average funding amount of venture type
mean(master_frame[which(master_frame$funding_round_type=="venture"),"raised_amount_usd"],na.rm=TRUE)

#Average funding amount of angel type
mean(master_frame[which(master_frame$funding_round_type=="angel"),"raised_amount_usd"],na.rm=TRUE)

#Average funding amount of seed type
mean(master_frame[which(master_frame$funding_round_type=="seed"),"raised_amount_usd"],na.rm=TRUE)

#Average funding amount of private equity type
mean(master_frame[which(master_frame$funding_round_type=="private_equity"),"raised_amount_usd"],na.rm=TRUE)


#CHECKPOINT 3: COUNTRY ANALYSIS
#Table 3.1: Analysing the Top 3 English-Speaking Countries
#For the chosen investment type, make a data frame named top9 with the top nine countries 
#(based on the total investment amount each country has received)
top9 <- master_frame[which(master_frame$funding_round_type=="venture"),]
top9 <- aggregate(top9$raised_amount_usd,by=list(top9$country_code),sum,na.rm=TRUE,na.action=NULL)
top9 <- top9[order(-top9$x)[1:9],]


#CHECKPOINT 4: SECTOR ANALYSIS 1
#Extract the primary sector of each category list from the category_list column
master_frame$primary_sector <- tolower(sapply(strsplit(master_frame$category_list,"|",fixed = TRUE),"[",1))

#Use the mapping file 'mapping.csv' to map each primary sector to one of the eight main sectors 
#Expected Results: Code for a merged data frame with each primary sector mapped to its main sector 
#(the primary sector should be present in a separate column).
mapping <- read.csv("mapping.csv",stringsAsFactors = FALSE,check.names = FALSE)
mapping <- gather(mapping,main_sector,val,2:10)
mapping <- mapping[!(mapping$val == 0),]
mapping <- mapping[,-3]
mapping$category_list <- tolower(str_replace_all(mapping$category_list,"0","na"))
mapping$category_list <- str_replace_all(mapping$category_list," 2.na"," 2.0")
master_frame <- merge(master_frame,mapping,by.x = "primary_sector",by.y = "category_list")
master_frame <- master_frame[,c(2:16,1,17)]


#CHECKPOINT 5: SECTOR ANALYSIS 2
#Create three separate data frames D1, D2 and D3 for each of the three countries containing the observations of funding type 
#FT falling within the 5-15 million USD range. The three data frames should contain:
D1 <- master_frame[which((master_frame$country_code=="USA") & (master_frame$funding_round_type=="venture") & (master_frame$raised_amount_usd>=5000000) & (master_frame$raised_amount_usd<=15000000)),]
D2 <- master_frame[which((master_frame$country_code=="GBR") & (master_frame$funding_round_type=="venture") & (master_frame$raised_amount_usd>=5000000) & (master_frame$raised_amount_usd<=15000000)),]
D3 <- master_frame[which((master_frame$country_code=="IND") & (master_frame$funding_round_type=="venture") & (master_frame$raised_amount_usd>=5000000) & (master_frame$raised_amount_usd<=15000000)),]

#The total number (or count) of investments for each main sector in a separate column
D1 <- D1 %>% group_by(main_sector) %>% mutate(total_num_invstmnt = NROW(main_sector))
D2 <- D2 %>% group_by(main_sector) %>% mutate(total_num_invstmnt = NROW(main_sector))
D3 <- D3 %>% group_by(main_sector) %>% mutate(total_num_invstmnt = NROW(main_sector))

#The total amount invested in each main sector in a separate column
D1 <- D1 %>% group_by(main_sector) %>% mutate(total_amt_invstmnt = sum(raised_amount_usd,na.rm=TRUE))
D2 <- D2 %>% group_by(main_sector) %>% mutate(total_amt_invstmnt = sum(raised_amount_usd,na.rm=TRUE))
D3 <- D3 %>% group_by(main_sector) %>% mutate(total_amt_invstmnt = sum(raised_amount_usd,na.rm=TRUE))

#Total number of investments (count)
# Country 1
sum(unique(D1$total_num_invstmnt))
# Country 2
sum(unique(D2$total_num_invstmnt))
# Country 3
sum(unique(D3$total_num_invstmnt))

#Total amount of investment (USD)
# Country 1
sum(D1$raised_amount_usd)
# Country 2
sum(D2$raised_amount_usd)
# Country 3
sum(D3$raised_amount_usd)

#Top sector (based on count of investments)
#Number of investments in the top sector 
# Country 1
D1_top_sector <- summary(factor(D1$main_sector))
D1_top_sector[order(-D1_top_sector)][1]
# Country 2
D2_top_sector <- summary(factor(D2$main_sector))
D2_top_sector[order(-D2_top_sector)][1]
# Country 3
D3_top_sector <- summary(factor(D3$main_sector))
D3_top_sector[order(-D3_top_sector)][1]

#Second-best sector (based on count of investments)
#Number of investments in the second-best sector 
# Country 1
D1_top_sector[order(-D1_top_sector)][2]
# Country 2
D2_top_sector[order(-D2_top_sector)][2]
# Country 3
D3_top_sector[order(-D3_top_sector)][2]

#Third-best sector (based on count of investments)
#Number of investments in the third-best sector
# Country 1
D1_top_sector[order(-D1_top_sector)][3]
# Country 2
D2_top_sector[order(-D2_top_sector)][3]
# Country 3
D3_top_sector[order(-D3_top_sector)][3]

#For the top sector count-wise (point 3), which company received the highest investment?
# Country 1
D1_first_company <- filter(D1, main_sector=="Others")
D1_first_company <- aggregate(D1_first_company$raised_amount_usd,by=list(tolower(D1_first_company$name)),sum,na.rm=TRUE,na.action=NULL)
D1_first_company[order(-D1_first_company$x)[1],]
# Country 2
D2_first_company <- filter(D2, main_sector=="Others")
D2_first_company <- aggregate(D2_first_company$raised_amount_usd,by=list(tolower(D2_first_company$name)),sum,na.rm=TRUE,na.action=NULL)
D2_first_company[order(-D2_first_company$x)[1],]
# Country 3
D3_first_company <- filter(D3, main_sector=="Others")
D3_first_company <- aggregate(D3_first_company$raised_amount_usd,by=list(tolower(D3_first_company$name)),sum,na.rm=TRUE,na.action=NULL)
D3_first_company[order(-D3_first_company$x)[1],]

#For the second-best sector count-wise (point 4), which company received the highest investment?
# Country 1
D1_second_company <- filter(D1, main_sector=="Social, Finance, Analytics, Advertising")
D1_second_company <- aggregate(D1_second_company$raised_amount_usd,by=list(tolower(D1_second_company$name)),sum,na.rm=TRUE,na.action=NULL)
D1_second_company[order(-D1_second_company$x)[1],]
# Country 2
D2_second_company <- filter(D2, main_sector=="Social, Finance, Analytics, Advertising")
D2_second_company <- aggregate(D2_second_company$raised_amount_usd,by=list(tolower(D2_second_company$name)),sum,na.rm=TRUE,na.action=NULL)
D2_second_company[order(-D2_second_company$x)[1],]
# Country 3
D3_second_company <- filter(D3, main_sector=="Social, Finance, Analytics, Advertising")
D3_second_company <- aggregate(D3_second_company$raised_amount_usd,by=list(tolower(D3_second_company$name)),sum,na.rm=TRUE,na.action=NULL)
D3_second_company[order(-D3_second_company$x)[1],]
