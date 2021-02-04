#This script transfers the tables from database.sqlite into our global environtment.

#1. Package installation / load ----
#install.packages('RSQLite')
library(RSQLite)
library(dplyr)

#2. Reading .sqlite file ----

#Establish th connection object
con <- dbConnect(SQLite() , "raw data/database.sqlite")

#table names to read
table_names <- dbListTables(con)

#storing tables into list
pitchfork_table_list <- lapply(table_names, dbReadTable , conn = con)

#assigning list elements to R dataframe object
review_content <- as.data.frame(pitchfork_table_list[2])
genres <- as.data.frame(pitchfork_table_list[3])
labels <- as.data.frame(pitchfork_table_list[4])
reviews <- as.data.frame(pitchfork_table_list[5])
years <- as.data.frame(pitchfork_table_list[6])

#joining years to reviews
pitchfork_reviews <- reviews %>%
   inner_join(years, by = 'reviewid')

#3. Saving tables locally & closing db connection ----
write.csv(genres, 'clean data/genres.csv')
write.csv(labels, 'clean data/labels.csv')
write.csv(review_content, 'clean data/review_content.csv')
write.csv(pitchfork_reviews, 'clean data/pitchfork_reviews.csv')
write.csv(years , 'clean data/years.csv')

dbDisconnect(con)

