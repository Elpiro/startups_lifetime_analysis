library(dplyr)

path = "C:/Users/GALLICE/Documents/Perso/Etudes/DSTI/Cours/Survival Analysis/project/Data/crunchbase-october-2013-master/crunchbase-"
datafile = "rounds.csv"

startups <- read.csv(paste(path,datafile, sep= ""), 
                     sep = ',',
                     stringsAsFactors = FALSE)

table_df <- table(startups$company_name)
hist(table_df$Freq, freq = F)
sum(table_df$Freq > 1)/length(table_df$Freq) #41% of rounds are associated to more than 1 company
rm(table_df)


#sort rounds dataset by startup name
startups$company_name <- factor(startups$company_name)
startups <- startups[order(startups$company_name),]
startups$company_name <- sapply(startups$company_name, as.character)

path = "C:/Users/GALLICE/Documents/Perso/Etudes/DSTI/Cours/Survival Analysis/project/Data/crunchbase-october-2013-master/crunchbase-"
datafile = "companies.csv"

companies <- read.csv(paste(path,datafile, sep= ""), 
                   sep = ',',
                   stringsAsFactors = FALSE)


names(companies)[2] <- "company_name"

startups <- left_join(startups, companies, by = "company_name")
