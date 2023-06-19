# The-Cyclic-Bikeshare-data
# # # # # # # # # # # # # # # # # # # # # # # ###
# ##<<__Install & load required packages__>>####
# # # # # # # # # # # # # # # # # # # # # # # ##

install.packages("data.table")# For loading and subsequently, combining the spreadsheets conveniently( and efficiently).
install.packages("readr")    # For reading the spreadsheets
install.packages("tidyverse") # tidyverse for data import and wrangling!
install.packages("lubridate")# lubridate for date functions!


library(data.table)
library(readr)
library(tidyverse)  
library(lubridate)   

#=====================
# STEP 1: COLLECT DATA
#=====================
#Set windows destination & Upload Divvy datasets (csv files) here

setwd("C:/LAB/Capstone project/Extracted Data/R uploads")
trips <- list.files(pattern="*.csv")
print(trips_)
#Rename the above printed files to an appropriate dataframe names
apr_2023<- read_csv("202304-Trips.csv")
mar_2023<- read_csv("202303-Trips.csv")
feb_2023<- read_csv("202302-Trips.csv")
jan_2023<- read_csv("202301-Trips.csv")
dec_2022 <- read_csv("202212-Trips.csv")
nov_2022<- read_csv("202211-Trips.csv")
oct_2022<- read_csv("202210-Trips.csv")
sep_2022<- read_csv("202209-Trips.csv")
aug_2022<- read_csv("202208-Trips.csv")
jul_2022<- read_csv("202207-Trips.csv")
jun_2022<- read_csv("202206-Trips.csv")
may_2022 <- read_csv("202205-Trips.csv")

#============================================================================
# STEP 2: INSPECTED THE DATA FOR INCONSISTENCIES AND COMBINE INTO A SINGLE FILE
#============================================================================
# Compare column names each of the files
# While the names don't have to be in the same order, they DO need to match perfectly before we can use a command to join them into one file

str (apr_2023)
str (mar_2023)
str (feb_2023)
str (jan_2023)
str (dec_2022)
str (nov_2022)
str (oct_2022)
str (sep_2022)
str (jul_2022)
str (jun_2022)
str (jun_2022)
str (may_2022)

Because there were no inconsistencies or discrepancies in the data’s column names, I continued to bind the data in the coding script below:

trips_<- list.files("C:/LAB/Capstone project/Extracted Data/R uploads", pattern="*.csv")
print(filenames) #inspect the loaded files for any incongruencies.

trips_ <- rbindlist(lapply(trips_, fread))



# Stack individual quarter's data frames into one big data frame

trips <- bind_rows(apr_2023,mar_2023,feb_2023,jan_2023,dec_2022,nov_2022,oct_2022,sep_2022,jul_2022,jun_2022,may_2022)
names(trips)
#======================================================
# STEP 3: CLEAN  AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================

#_In doing this step I removed columns that I deemed as not useful for this particular analysis, through the data exploration (I did bearing in mind the main business task/challenge at hand):

# Remove ride_id, rideable_type,start_station_id, end_station_id
trips_ <- trips_%>%  
  select(-c(ride_id, rideable_type,start_station_id, end_station_id))


# Inspect the new table that remains

str(trips_)#See list of columns and data types (numeric, character, etc)
summary(trips_) #Statistical summary of data. Mainly numerical

#Upon inspection I saw it fit to rename the member_casual column to rider_type as its more precisely descriptive

colnames(trips_)[colnames(trips_) == "member_casual"] <- "rider_type"

names(trips_) #Inspect to see if indeed the member_casual column is changed to rider_type

#I also removed duplicate rows as I deemed them unnecessary and rather cumbersome for the processing of data
nrow(trips_)
[1] 5859061
trips_<- distinct(trips_) #Remove duplicate rows.
nrow(trips_)
[1] 5859027
#The above is showing that 21 rows (5859061-5859027) around were removed, cleaning effort highlight.

#_After the above, I did addition of insightful layers(columns)
# (1) One of the main layers I dimmed necessary to add was a column for length of trip. I will name it "trip_length”.


trips_$trip_length <- difftime(trips_$ended_at, trips_$started_at, units = "hours")

df <- arrange(df, column_name) 
trips_<- arrange(trips_, trip_length)

Inspect the new added column for any anomalies.
summary(trips_$trip_length)



#I found that I cannot even obtain statistical summaries from the new added column then that means we need to convert it to format which we can be able to obtain statistical summaries from

# Converted "trip_length" from Factor to numeric so we can run calculations on the data
is.factor(trips_$ride_length)
trips_$trip_length <- as.numeric(as.character(trips_$trip_length))
is.numeric(trips_$trip_length)

The above step is very paramount otherwise you won’t be able to find the statistical attributes from the column data like mean,min,max and Q3.

summary(trips_$trip_length) #now I was able to obtain statistical summaries and also found anomalies in the data such as a minimum value of -172 hours of ride length ,this obviously calls for further inspection of the data and in particular the trip length aspect of the data.

negative_values <- trips_ [trips_$ trip_length <= 0, ]
nrow(negative_values) #found out that there are 544 negative values or values with zero
print(negative_values$trip_length)
summary(negative_values$trip_length)

# Remove "bad" data
nrow(trips_)= 5859027
trips_ <- trips_[!(trips_$trip_length <= 0)]
nrow(trips_)= 5858483
#so around 544 trip duration values with zero or negative figures were removed, cleaning effort highlight. 



# (2) I continued to add other additional columns(layers) to the data -- such as day, day_of_week and month_year -- that provided me with additional insights about the data.

# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year ... before completing these operations we could only aggregate at the ride level
trips_$date <- as.Date(trips_$started_at) #The default format is yyyy-mm-dd
trips_$month_year <- format(as.Date(trips_$started_at), " %Y-%m")
trips_$day_of_week <- format(as.Date(trips_$date), "%A")
trips_$day <- format(as.Date(trips_$date), "%d")


# Inspect the structure of the columns
str(trips_)


Upon inspection we can see that column such as months, day and day of week are in character class of which is not ideal for statistical 
# Specify the file path and name for the CSV file 
 csv_file <- "trips_data.csv"
 # Write the data frame to a CSV file 
write.csv(trips_, file = csv_file, row.names = FALSE) 
# Print a message to confirm the CSV file creation
 cat("CSV file created:", csv_file, "\n")

 ![Two Rulers](https://github.com/KinsleyMaimela/The-Cyclic-Bikeshare-data/assets/134984278/aaa0403e-dbe4-4702-a75a-9717c7a6ee7f)


