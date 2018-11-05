# Set the appropriate working directory where the files are placed
setwd("C:/Users/eravdiv/Desktop/Ravi/PG-Data-Science/Investment-Case-Study/Investment-Case-Study")

#Loading companies and rounds2 into a dataframe
companies <- read.delim("companies.txt",header = TRUE, sep = "\t", stringsAsFactors = FALSE)
rounds2   <- read.csv("rounds2.csv", stringsAsFactors = FALSE)
write.csv(companies, file = "Table-Companies.csv", row.names = TRUE)

#Results Expected: Table 1.1
#How many unique companies are present in rounds2?
# 1. Extract company_permalink column
# 2. Convert all content to lower case.
# 3. Find unique content
# 4. Count the unique content

unique_names <- data.frame(unique(tolower(rounds2$company_permalink)))
no_unique_companies <- nrow(unique_names)
sprintf("Number of unique companies are %s", no_unique_companies)

#How many unique companies are present in companies?
# 1. Extract names column
# 2. Find any duplicates in columsn "name" and permalink
# 3. Find unique content
# 4. Count the unique content

unique_names2 <- data.frame(unique(tolower(companies$permalink)))
no_unique_companies2 <- nrow(unique_names2)
sprintf("Number of unique companies are %s", no_unique_companies2)

# In the companies data frame, which column can be used as the unique key for each company? Write the name of the column.
# 1. Use the function anyDuplicated against column names to find no duplication.
# 2. Print the column name which can be used as the unique key

anyDuplicated(companies$name)
anyDuplicated(companies$permalink)
Duplicates <- anyDuplicated(companies$permalink)
sprintf("Column %s can be used as the unique key since it has %s duplicates", names(companies[1]), Duplicates)

#Are there any companies in the rounds2 file which are not present in companies? Answer yes or no: Y/N
# 1. Extract the columns company_permalink and permalink from the 2 tables.
# 2. Use the %in% operation in R to look for possible overlaps.
# 3. Print the results

x<-unique_names[,1]
y<-unique_names2[,1]
company_duplicates <-data.frame(x,y)
company_duplicates$x[!(x %in% y)]
sprintf("There are no companies in the rounds2 file which are not present in companies")

#Merge the two data frames so that all variables (columns) in the companies frame are added to the rounds2 data frame.
#Name the merged frame master_frame. How many observations are present in master_frame?
# 1. Rename column name so that the common column name between the 2 data frames is permalink
# 2. Convert the contents of the column to lowercase so that matching of column content can also happen.
# 3. use merge command to create new data frame master_frame

colnames(rounds2)[which(names(rounds2) == "company_permalink")] <- "permalink"
rounds2$permalink <-tolower(rounds2$permalink)
companies$permalink <-tolower(companies$permalink)

master_frame <-merge(rounds2, companies, by = "permalink")


##Results Expected: Table 2.1
#Calculate the average investment amount for each of the four funding types (venture, angel, seed, and private equity)
#and report the answers in Table 2.1
# 1. Convert all the NA values in raised_amount_usd field to 0
# 2. Use the aggregate function and group by funding_round_type to obtain the numeric average value
# 3. Use subset function to filter out specific funding round type
# 4. Print the average values.

names(master_frame)
str(master_frame)
summary(master_frame)
master_frame$raised_amount_usd <-as.numeric(master_frame$raised_amount_usd)
#master_frame$raised_amount_usd[is.na(master_frame$raised_amount_usd)] <- 0

Avg_FundingRoundType <- aggregate(master_frame$raised_amount_usd, by = list(master_frame$funding_round_type), FUN = mean, na.rm = TRUE)

Avg_VentureType <- subset(Avg_FundingRoundType, Avg_FundingRoundType$Group.1 == "venture")
Avg_AngelType <- subset(Avg_FundingRoundType, Avg_FundingRoundType$Group.1 == "angel")
Avg_SeedType <- subset(Avg_FundingRoundType, Avg_FundingRoundType$Group.1 == "seed")
Avg_PrivateEquityType <- subset(Avg_FundingRoundType, Avg_FundingRoundType$Group.1 == "private_equity")
sprintf("Average funding round type %s is %s", Avg_VentureType[1,1],Avg_VentureType[1,2])
sprintf("Average funding round type %s is %s", Avg_AngelType[1,1],Avg_AngelType[1,2])
sprintf("Average funding round type %s is %s", Avg_SeedType[1,1],Avg_SeedType[1,2])
sprintf("Average funding round type %s is %s", Avg_PrivateEquityType[1,1],Avg_PrivateEquityType[1,2])

#Based on the average investment amount calculated above, which investment type do you think 
#is the most suitable for Spark Funds?
#Considering that Spark Funds wants to invest between 5 to 15 million USD per investment round, 
#which investment type is the most suitable for it?
# Answer
# 1. Create a data frame with the Investment Type and Avergae Amount.
# 2. Name the columns appropriately
# 3. Change the Average Amount column to numeric type.
# 4. Write a for loop to find the correct investment type for Spark Funds.

InvestmentType <- rbind(c(Avg_VentureType[1,1],Avg_VentureType[1,2]), c(Avg_AngelType[1,1],Avg_AngelType[1,2]), c(Avg_SeedType[1,1],Avg_SeedType[1,2]), c(Avg_PrivateEquityType[1,1],Avg_PrivateEquityType[1,2]))  
InvestmentType <- data.frame(InvestmentType, stringsAsFactors = FALSE)
names(InvestmentType)[1]<- paste("FundingType")
names(InvestmentType)[2]<- paste("AvgAmount")
InvestmentType
#typeof(InvestmentType)
InvestmentType$AvgAmount <- as.numeric(InvestmentType$AvgAmount)
#is.numeric(InvestmentType$AvgAmount)
#InvestmentType$AvgAmount
#Type   <- InvestmentType[1, 2]
#Type
#nrow(InvestmentType)

for (row in 1:nrow(InvestmentType)) {
  sprintf("hello")
  Type   <- InvestmentType[row, 1]
  Amount <- InvestmentType[row, 2]
  if ((Amount >= 5000000) && (Amount <= 15000000)) {
    print(paste("Funding Type", Type,"of amount",Amount, "is suitable"))
  } else {
    print(paste("Funding Type", Type,"of amount",Amount, "is not suitable"))
  }
}

colnames(master_frame) <- names(master_frame)
write.csv(master_frame, file = "master_frame.csv", row.names = FALSE)

#Checkpoint 3: Country Analysis
#Spark Funds wants to invest in countries with the highest amount of funding for the chosen 
#investment type. This is a part of its broader strategy to invest where most investments are occurring.
#Spark Funds wants to see the top nine countries which have received the highest total
#funding (across ALL sectors for the chosen investment type)

#For the chosen investment type, make a data frame named top9 with the top nine countries 
#(based on the total investment amount each country has received)
#Identify the top three English-speaking countries in the data frame top9.

# Solution
# 1. Filter the master_frame based on funding_round_type = venture
# 2. Aggregate the countries based on the total amount invested
# 3. Obtain the top 9 countries.
# 4. Since there is a row in top 9 which has no country mentioned, evaluate if the total amount with 
#    no country is less than 10% of the total amount. If not then elemanate that content.
# 5. Obtain the new top_9 list
# 6. Append the information if the country has English as official language.
# 7. Identify the top3 countries which have english as offical language.

library(dplyr)


All_countries <- filter(master_frame, master_frame$funding_round_type == "venture")
All_countries
AllCountry_Venture_summary <- aggregate(All_countries$raised_amount_usd, by = list(All_countries$country_code), FUN = sum, na.rm = TRUE)
AllCountry_Venture_summary

top_10 <- top_n(AllCountry_Venture_summary, 10, x)
top_10 <- data.frame(top_10, stringsAsFactors = FALSE)
names(top_10)[1]<- paste("Country_code1")
names(top_10)[2]<- paste("SumAmount")
top_10
value_no_country <- top_10[which(top_9$Country_code1 == ""),]
value_no_country

#top_9$SumAmount <- as.numeric(top_9$SumAmount)
#Blank_value <- top_9[1,2]
#Total_Value <- sum(top_9$SumAmount)
#Total_Value
if (value_no_country[1,2] < (0.1 * sum(top_10$SumAmount)) ) {
  print(" Too small value to consider")
} else {
  print(" Value needs to be considered")
}

top_9 <- filter(top_10, top_10$Country_code1 != "")
top_9

EnglishOfficial <- c("Yes", "No", "No", "No", "Yes", "Yes", "No", "No", "Yes" )

top_9 <-cbind(top_9, EnglishOfficial)
top_9
names(top_9)[3]<- paste("IsEnglish")
top_9
top_EnglishSpeaking <- filter(top_9, top_9$IsEnglish == "Yes")
List_MaxAmount <- c(top_EnglishSpeaking$SumAmount)
List_MaxAmount <- sort(List_MaxAmount, decreasing = TRUE)
List_MaxAmount
n <- length(List_MaxAmount)

#first <- sort(top_9_EnglishSpeaking$SumAmount,decreasing = TRUE)[length(top_9_EnglishSpeaking$SumAmount)]
firstcountry <- subset(top_EnglishSpeaking,top_EnglishSpeaking$SumAmount == List_MaxAmount[1] )
secondcountry <- subset(top_EnglishSpeaking,top_EnglishSpeaking$SumAmount == List_MaxAmount[2] )
thirdcountry <- subset(top_EnglishSpeaking,top_EnglishSpeaking$SumAmount == List_MaxAmount[3] )

sprintf ("First english speaking country is %s with total investment of %s", firstcountry[1], firstcountry[2])
sprintf ("Second english speaking country is %s with total investment of %s", secondcountry[1], secondcountry[2])
sprintf ("Third english speaking country is %s with total investment of %s", thirdcountry[1], thirdcountry[2])

# Checkpoint 4: Sector Analysis 1
# You discuss with the CEO and come up with the business rule that the first string before the 
# vertical bar will be considered the primary sector. In the example above, ‘Application Platforms’ will 
# be considered the primary sector.
# Extract the primary sector of each category list from the category_list column
# Use the mapping file 'mapping.csv' to map each primary sector to one of the eight main sectors 
# (Note that ‘Others’ is also considered one of the main sectors)
# Expected Results: Code for a merged data frame with each primary sector mapped to 
# its main sector (the primary sector should be present in a separate column).

# Solution
# 1. Split the category_list column based on | using str_split_fixed to create 2 new columns.
# 2. Merge the dataframe master_data with the new primary and secondary categories.
# 3. Create a table with no blanks in category.
# 4. Read mapping.csv file into R
# 5. Change format from wide to long
# 6. Identify unique combinations by searching for val == 1.
# 7. Clean data in mapping sheet where test is incorrect
# 8. Merge the 2 dataframes to create master dataframe for primary category and main sector.

library(stringr)
library(tidyr)
library(dplyr)

new_str <- data.frame(str_split_fixed(master_frame$category_list, "\\|",2), stringsAsFactors = FALSE)
names(new_str)[1] <- paste("Primary_list")
names(new_str)[2] <- paste("Secondary_Category")
master_frame_new <- data.frame(cbind(master_frame,new_str), stringsAsFactors = FALSE)
master_frame_noblanks <- filter(master_frame_new,master_frame_new$Primary_list != "")

mapping <- read.csv("mapping.csv", stringsAsFactors = FALSE)
summary(mapping)

names_list <- c(names(mapping))

for (n in 1:nrow(mapping)) {
  mapping$category_list[n] <- str_replace_all(mapping$category_list[n], "0", "na")
  if (mapping$category_list[n] == "Enterprise 2.na") {
    mapping$category_list[n] <- "Enterprise 2.0"
  }
}
mapping_long <- gather(mapping, Category_Type, val, names_list[2]:names_list[10])
mapping_long <- subset(mapping_long, mapping_long$val == 1)
names(mapping_long)[1] <- paste("Primary_list")
mapping_long_noblanks <- filter(mapping_long, mapping_long$Primary_list != "")

final_table <- merge(mapping_long, master_frame_new, by = "Primary_list")

final_table_noblank <- merge(mapping_long_noblanks, master_frame_noblanks, by = "Primary_list")

names(final_table)[1] <- paste("Primary_Sector")
names(final_table)[2] <- paste("Main_Sector")

names(final_table_noblank)[1] <- paste("Primary_Sector")
names(final_table_noblank)[2] <- paste("Main_Sector")

write.csv(final_table, file = "final_table.csv", row.names = FALSE)
write.csv(final_table_noblank, file = "final_table_noblank.csv", row.names = FALSE)

## Checkpoint 5: Sector Analysis 2
# Now you have a data frame with each company’s main sector (main_sector) mapped to it. 
# When we say sector analysis, we refer to one of the eight main sectors.
# Also, you know the top three English speaking countries and the most suitable funding 
# type for Spark Funds. Let’s call the three countries 'Country 1', 'Country 2' and 'Country 3' 
# and the funding type 'FT'.
# Also, the range of funding preferred by Spark Funds is 5 to 15 million USD.
# Now, the aim is to find out the most heavily invested main sectors in each of the 
# three countries (for funding type FT and investments range of 5-15 M USD).

# Create three separate data frames D1, D2 and D3 for each of the three countries containing 
# the observations of funding type FT falling within the 5-15 million USD range. 
# The three data frames should contain:
# 1. All the columns of the master_frame along with the primary sector and the main sector
# 2. The total number (or count) of investments for each main sector in a separate column
# 3. The total amount invested in each main sector in a separate column

# Using the three data frames, you can calculate the total number and amount of investments in each main sector.

## Answer
# 1. Create each dataframe D1, D2, D3 by applying a filter based on country, venture, amount between 5M and 15M and remove the primary sectors 
#    which are blank. Blank sectors will not add value since it does not have any main sector assosciated with it.
# 2. Create 2 new dataframes to show the no. of investments and the sum of investments.
# 3. In order to merge the 2 columns of count. of investments and sum of investments, create a vector of length equal to the 
#    dataframe created in step1. Fill the values and let the remaining values be NA.
# 4. Merge the columns to create a dataframe D1, D2 and D3.
# 5. Use appropriate dataframes to answer each question of table 5.1

# Data Frame D1
D1_initial <- data.frame(filter(final_table, final_table$funding_round_type == "venture", final_table$country_code == "USA", final_table$raised_amount_usd >= 5000000, final_table$raised_amount_usd <= 15000000, final_table$Primary_Sector != ""), stringsAsFactors = FALSE, na.rm = TRUE)
D1_MainSector_sum <- aggregate(D1_initial$raised_amount_usd, by = list(D1_initial$Main_Sector), FUN = sum, na.rm = TRUE)
D1_MainSector_count <- aggregate(D1_initial$Main_Sector, by = list(D1_initial$Main_Sector), FUN = length)
D1_merged <- merge(D1_MainSector_sum, D1_MainSector_count, by = "Group.1")
D1_merged

SectorSum <- vector(mode = "numeric", length =nrow(D1_initial))
SectorCount <- vector(mode = "numeric", length =nrow(D1_initial))
length(SectorSum)

for (n in 1:length(SectorSum)) {
  SectorSum[n] <- D1_merged$x.x[n]
  SectorCount[n] <- D1_merged$x.y[n]
}

D1 <- data.frame(cbind(D1_initial,SectorSum, SectorCount), stringsAsFactors = FALSE)


write.csv(D1, file = "D1.csv", row.names = FALSE)

## Table 5.1 : Sector-wise Investment Analysis
# C1: 1. Total number of investments (count)

print (paste("Country", firstcountry[1], "has a total number of Investments (count) ", sum(D1_MainSector_count$x)))

# C1: 2. Total amount of investment (USD)
print (paste("Country", firstcountry[1], "has a total amount of investment (USD) ", sum(D1_MainSector_sum$x)))

# C1: 3. Top Sector name (no. of investment-wise)
print (paste("Country", firstcountry[1],"has top sector name as ", D1_merged$Group.1[which(D1_merged$x.y == max(D1_merged$x.y))]))
C1_Top_Sector <- D1_merged$Group.1[which(D1_merged$x.y == max(D1_merged$x.y))]

# C1: 4. Second Sector name (no. of investment-wise)
print (paste("Country", firstcountry[1],"has second sector name as ", D1_merged$Group.1[which(D1_merged$x.y == sort(D1_merged$x.y,decreasing = FALSE)[length(D1_merged$x.y)-1])]))
C1_Second_Sector <- D1_merged$Group.1[which(D1_merged$x.y == sort(D1_merged$x.y,decreasing = FALSE)[length(D1_merged$x.y)-1])]
  
# C1: 5. Third Sector name (no. of investment-wise)
print (paste("Country", firstcountry[1],"has third sector name as ", D1_merged$Group.1[which(D1_merged$x.y == sort(D1_merged$x.y,decreasing = FALSE)[length(D1_merged$x.y)-2])]))
C1_Third_Sector <- D1_merged$Group.1[which(D1_merged$x.y == sort(D1_merged$x.y,decreasing = FALSE)[length(D1_merged$x.y)-2])]

# C1: 6. Number of investments in top sector (3)
print (paste ("Country",firstcountry[1], "number of investments in top sector", D1_merged$x.y[which(D1_merged$Group.1 == C1_Top_Sector)]))

# C1: 7. Number of investments in second sector (3)
print (paste ("Country",firstcountry[1], "number of investments in second sector", D1_merged$x.y[which(D1_merged$Group.1 == C1_Second_Sector)]))

# C1: 8. Number of investments in third sector (3)
print (paste ("Country",firstcountry[1], "number of investments in third sector", D1_merged$x.y[which(D1_merged$Group.1 == C1_Third_Sector)]))

# C1: 9. For point 3 (top sector count-wise), which company received the highest investment?
D1_subset <- subset(D1, D1$Main_Sector == C1_Top_Sector)
D1_subset_name <- aggregate(D1_subset$raised_amount_usd,by = list(D1_subset$name), FUN = sum)
print (paste ("Company",D1_subset_name$Group.1[which(D1_subset_name$x == max(D1_subset_name$x))], "has received the highest investment for sector",C1_Top_Sector))

D1_primarySector_1 <- D1$Primary_Sector[which(D1$name == D1_subset_name$Group.1[which(D1_subset_name$x == max(D1_subset_name$x))])]
D1_primarySector_1[1]
print (paste ("Primary Sector for company",D1_subset_name$Group.1[which(D1_subset_name$x == max(D1_subset_name$x))], "is", D1_primarySector_1[1]))

# C1: 10. For point 4 (second best sector count-wise), which company received the highest investment?
D1_subset_2 <- subset(D1, D1$Main_Sector == C1_Second_Sector)
D1_subset_name_2 <- aggregate(D1_subset_2$raised_amount_usd,by = list(D1_subset_2$name), FUN = sum)
print (paste ("Company",D1_subset_name_2$Group.1[which(D1_subset_name_2$x == max(D1_subset_name_2$x))], "has received the highest investment for sector", C1_Second_Sector))

D1_primarySector_2 <- D1$Primary_Sector[which(D1$name == D1_subset_name_2$Group.1[which(D1_subset_name_2$x == max(D1_subset_name_2$x))])]
D1_primarySector_2[1]
print (paste ("Primary Sector for company",D1_subset_name_2$Group.1[which(D1_subset_name_2$x == max(D1_subset_name_2$x))], "is", D1_primarySector_2[1]))

####################
# Data Frame D2

D2_initial <- data.frame(filter(final_table, final_table$funding_round_type == "venture", final_table$country_code == "GBR", final_table$raised_amount_usd >= 5000000, final_table$raised_amount_usd <= 15000000, final_table$Primary_Sector != ""), stringsAsFactors = FALSE, na.rm = TRUE)
D2_MainSector_sum <- aggregate(D2_initial$raised_amount_usd, by = list(D2_initial$Main_Sector), FUN = sum, na.rm = TRUE)
D2_MainSector_count <- aggregate(D2_initial$Main_Sector, by = list(D2_initial$Main_Sector), FUN = length)
D2_merged <- merge(D2_MainSector_sum, D2_MainSector_count, by = "Group.1")
D2_merged

#D2$Sector_Sum <- D2_MainSector_sum$x

SectorSum <- vector(mode = "numeric", length =nrow(D2_initial))
SectorCount <- vector(mode = "numeric", length =nrow(D2_initial))
length(SectorSum)

for (n in 1:length(SectorSum)) {
  SectorSum[n] <- D2_merged$x.x[n]
  SectorCount[n] <- D2_merged$x.y[n]
}
#data
D2 <- data.frame(cbind(D2_initial,SectorSum, SectorCount), stringsAsFactors = FALSE)


write.csv(D2, file = "D2.csv", row.names = FALSE)

## Table 5.1 : Sector-wise Investment Analysis
# C2: 1. Total number of investments (count)

print (paste("Country", secondcountry[1], "has a total number of Investments (count) ", sum(D2_MainSector_count$x)))

# C2: 2. Total amount of investment (USD)
print (paste("Country", secondcountry[1], "has a total amount of investment (USD) ", sum(D2_MainSector_sum$x)))

# C2: 3. Top Sector name (no. of investment-wise)
print (paste("Country", secondcountry[1],"has top sector name as ", D2_merged$Group.1[which(D2_merged$x.y == max(D2_merged$x.y))]))
C2_Top_Sector <- D2_merged$Group.1[which(D2_merged$x.y == max(D2_merged$x.y))]

# C2: 4. Second Sector name (no. of investment-wise)
print (paste("Country", secondcountry[1],"has second sector name as ", D2_merged$Group.1[which(D2_merged$x.y == sort(D2_merged$x.y,decreasing = FALSE)[length(D2_merged$x.y)-1])]))
C2_Second_Sector <- D2_merged$Group.1[which(D2_merged$x.y == sort(D2_merged$x.y,decreasing = FALSE)[length(D2_merged$x.y)-1])]

# C2: 5. Third Sector name (no. of investment-wise)
print (paste("Country", secondcountry[1],"has third sector name as ", D2_merged$Group.1[which(D2_merged$x.y == sort(D2_merged$x.y,decreasing = FALSE)[length(D2_merged$x.y)-2])]))
C2_Third_Sector <- D2_merged$Group.1[which(D2_merged$x.y == sort(D2_merged$x.y,decreasing = FALSE)[length(D2_merged$x.y)-2])]

# C2: 6. Number of investments in top sector (3)
print (paste ("Country",secondcountry[1], "number of investments in top sector", D2_merged$x.y[which(D2_merged$Group.1 == C2_Top_Sector)]))

# C2: 7. Number of investments in second sector (3)
print (paste ("Country",secondcountry[1], "number of investments in second sector", D2_merged$x.y[which(D2_merged$Group.1 == C2_Second_Sector)]))

# C2: 8. Number of investments in third sector (3)
print (paste ("Country",secondcountry[1], "number of investments in third sector", D2_merged$x.y[which(D2_merged$Group.1 == C2_Third_Sector)]))

# C2: 9. For point 3 (top sector count-wise), which company received the highest investment?
D2_subset <- subset(D2, D2$Main_Sector == C2_Top_Sector)
D2_subset_name <- aggregate(D2_subset$raised_amount_usd,by = list(D2_subset$name), FUN = sum)
print (paste ("Company",D2_subset_name$Group.1[which(D2_subset_name$x == max(D2_subset_name$x))], "has received the highest investment for sector",C2_Top_Sector))

D2_primarySector_1 <- D2$Primary_Sector[which(D2$name == D2_subset_name$Group.1[which(D2_subset_name$x == max(D2_subset_name$x))])]
D2_primarySector_1[1]
print (paste ("Primary Sector for company",D2_subset_name$Group.1[which(D2_subset_name$x == max(D2_subset_name$x))], "is", D2_primarySector_1[1]))

# C2: 10. For point 4 (second best sector count-wise), which company received the highest investment?
D2_subset_2 <- subset(D2, D2$Main_Sector == C2_Second_Sector)
D2_subset_name_2 <- aggregate(D2_subset_2$raised_amount_usd,by = list(D2_subset_2$name), FUN = sum)
print (paste ("Company",D2_subset_name_2$Group.1[which(D2_subset_name_2$x == max(D2_subset_name_2$x))], "has received the highest investment for sector", C2_Second_Sector))

D2_primarySector_2 <- D2$Primary_Sector[which(D2$name == D2_subset_name_2$Group.1[which(D2_subset_name_2$x == max(D2_subset_name_2$x))])]
D2_primarySector_2[1]
print (paste ("Primary Sector for company",D2_subset_name_2$Group.1[which(D2_subset_name_2$x == max(D2_subset_name_2$x))], "is", D2_primarySector_2[1]))

####################
# Data Frame D3

D3_initial <- data.frame(filter(final_table, final_table$funding_round_type == "venture", final_table$country_code == "IND", final_table$raised_amount_usd >= 5000000, final_table$raised_amount_usd <= 15000000, final_table$Primary_Sector != ""), stringsAsFactors = FALSE, na.rm = TRUE)
D3_MainSector_sum <- aggregate(D3_initial$raised_amount_usd, by = list(D3_initial$Main_Sector), FUN = sum, na.rm = TRUE)
D3_MainSector_count <- aggregate(D3_initial$Main_Sector, by = list(D3_initial$Main_Sector), FUN = length)
D3_merged <- merge(D3_MainSector_sum, D3_MainSector_count, by = "Group.1")
D3_merged

#D3$Sector_Sum <- D3_MainSector_sum$x

SectorSum <- vector(mode = "numeric", length =nrow(D3_initial))
SectorCount <- vector(mode = "numeric", length =nrow(D3_initial))
length(SectorSum)

for (n in 1:length(SectorSum)) {
  SectorSum[n] <- D3_merged$x.x[n]
  SectorCount[n] <- D3_merged$x.y[n]
}
#data
D3 <- data.frame(cbind(D3_initial,SectorSum, SectorCount), stringsAsFactors = FALSE)


write.csv(D3, file = "D3.csv", row.names = FALSE)

## Table 5.1 : Sector-wise Investment Analysis
# C3: 1. Total number of investments (count)

print (paste("Country", thirdcountry[1], "has a total number of Investments (count) ", sum(D3_MainSector_count$x)))

# C3: 2. Total amount of investment (USD)
print (paste("Country", thirdcountry[1], "has a total amount of investment (USD) ", sum(D3_MainSector_sum$x)))

# C3: 3. Top Sector name (no. of investment-wise)
print (paste("Country", thirdcountry[1],"has top sector name as ", D3_merged$Group.1[which(D3_merged$x.y == max(D3_merged$x.y))]))
C3_Top_Sector <- D3_merged$Group.1[which(D3_merged$x.y == max(D3_merged$x.y))]

# C3: 4. Second Sector name (no. of investment-wise)
print (paste("Country", thirdcountry[1],"has second sector name as ", D3_merged$Group.1[which(D3_merged$x.y == sort(D3_merged$x.y,decreasing = FALSE)[length(D3_merged$x.y)-1])]))
C3_Second_Sector <- D3_merged$Group.1[which(D3_merged$x.y == sort(D3_merged$x.y,decreasing = FALSE)[length(D3_merged$x.y)-1])]

# C3: 5. Third Sector name (no. of investment-wise)
print (paste("Country", thirdcountry[1],"has third sector name as ", D3_merged$Group.1[which(D3_merged$x.y == sort(D3_merged$x.y,decreasing = FALSE)[length(D3_merged$x.y)-2])]))
C3_Third_Sector <- D3_merged$Group.1[which(D3_merged$x.y == sort(D3_merged$x.y,decreasing = FALSE)[length(D3_merged$x.y)-2])]

# C3: 6. Number of investments in top sector (3)
print (paste ("Country",thirdcountry[1], "number of investments in top sector", D3_merged$x.y[which(D3_merged$Group.1 == C3_Top_Sector)]))

# C3: 7. Number of investments in second sector (3)
print (paste ("Country",thirdcountry[1], "number of investments in second sector", D3_merged$x.y[which(D3_merged$Group.1 == C3_Second_Sector)]))

# C3: 8. Number of investments in third sector (3)
print (paste ("Country",thirdcountry[1], "number of investments in third sector", D3_merged$x.y[which(D3_merged$Group.1 == C3_Third_Sector)]))

# C3: 9. For point 3 (top sector count-wise), which company received the highest investment?
D3_subset <- subset(D3, D3$Main_Sector == C3_Top_Sector)
D3_subset_name <- aggregate(D3_subset$raised_amount_usd,by = list(D3_subset$name), FUN = sum)
print (paste ("Company",D3_subset_name$Group.1[which(D3_subset_name$x == max(D3_subset_name$x))], "has received the highest investment for sector",C3_Top_Sector))

D3_primarySector_1 <- D3$Primary_Sector[which(D3$name == D3_subset_name$Group.1[which(D3_subset_name$x == max(D3_subset_name$x))])]
D3_primarySector_1[1]
print (paste ("Primary Sector for company",D3_subset_name$Group.1[which(D3_subset_name$x == max(D3_subset_name$x))], "is", D3_primarySector_1[1]))


# C3: 10. For point 4 (second best sector count-wise), which company received the highest investment?
D3_subset_2 <- subset(D3, D3$Main_Sector == C3_Second_Sector)
D3_subset_name_2 <- aggregate(D3_subset_2$raised_amount_usd,by = list(D3_subset_2$name), FUN = sum)
print (paste ("Company",D3_subset_name_2$Group.1[which(D3_subset_name_2$x == max(D3_subset_name_2$x))], "has received the highest investment for sector", C3_Second_Sector))

D3_primarySector_2 <- D3$Primary_Sector[which(D3$name == D3_subset_name_2$Group.1[which(D3_subset_name_2$x == max(D3_subset_name_2$x))])]
D3_primarySector_2[1]
print (paste ("Primary Sector for company",D3_subset_name$Group.1[which(D3_subset_name$x == max(D3_subset_name$x))], "is", D3_primarySector_2[1]))

