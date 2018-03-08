#-------------import of companies.csv---------------#

path = "C:/Users/GALLICE/Documents/Perso/Etudes/DSTI/Cours/Survival Analysis/project/Data/crunchbase-october-2013-master/crunchbase-"

datafile = "companies.csv"
startups <- read.csv(paste(path,datafile, sep= ""), 
                     sep = ',',
                     stringsAsFactors = FALSE)


startups$funding_total_usd <- as.integer(startups$funding_total_usd)

startups$category_code <- factor(x = startups$category_code, ordered = FALSE)
startups$status <- factor(startups$status, ordered = FALSE)
startups$country_code <- factor(startups$country_code, ordered = FALSE)
startups$state_code <- factor(startups$state_code, ordered = FALSE)
startups$region <- factor(startups$region, ordered = FALSE)
startups$city <- factor(startups$city, ordered=FALSE)

startups$founded_at <- as.Date(startups$founded_at, "%Y-%m-%d")
startups$first_funding_at <- as.Date(as.character(startups$first_funding_at, format="%Y-%m-%d"))
startups$last_funding_at <- as.Date(as.character(startups$last_funding_at, format="%Y-%m-%d"))
startups$last_milestone_at <- as.Date(as.character(startups$last_milestone_at, format="%Y-%m-%d"))


names(startups)[2] <- "company_name"

table(startups$country_code)

#________________companies imported______________#



#------some plotting-----------#

plot(startups$status ~ log(startups$funding_total_usd), col = unique(startups$status))
legend("topleft", legend=levels(startups$status), pch=16, col=unique(startups$status))

#__________________done plotting________________#



#-----------computes lifetime of startup------------#

start_date <- startups$founded_at
startups$last_milestone_at[is.na(startups$last_milestone_at)] <- startups$last_funding_at[is.na(startups$last_milestone_at)]#imputes missing dates in last_milestone_at
end_date <- startups$last_milestone_at

if(FALSE){
end_date <- startups$last_funding_at #or end_date <- startups$last_milestone_at ?
                                     #for startups that are acquired, the last_milestone could be better
                                     # startups[which(startups$status=="acquired"), c("last_funding_at","last_milestone_at")]
}

current_lifetime <- data.frame(matrix(data = NA,nrow = dim(startups)[1], ncol = 1))

for(i in seq(1,dim(startups)[1])){
  current_lifetime[i,] <- as.integer(end_date[i]-start_date[i])
  if(!is.na(current_lifetime[i,])){
    if(current_lifetime[i,] < 0){
      current_lifetime[i,] <- NA
    }
  }
}

#create column for lifetime of the startup
names(current_lifetime) <- "days_of_existence"
summary(current_lifetime)
startups <- cbind(startups,current_lifetime)

startups <- startups[!is.na(current_lifetime),]
startups <- startups[!is.na(startups$funding_total_usd),]

rm(current_lifetime)

#____________done with lifetime_____________#






#------------Transform status column to alive/closed-----------#
table(startups$status)

target <- data.frame(matrix(data = NA,nrow = dim(startups)[1], ncol = 1))
names(target) <- "target"

for (i in seq(1, dim(startups)[1])){
  startup_status <- startups$status[i]
  if (startup_status == 'acquired'){
    target[i,1] <-  1
  }
  else if (startup_status == 'ipo'){
    target[i,1] <- 0
  }
  else if (startup_status == 'operating'){
    target[i,1] <- 0
  }
  else if (startup_status == "closed"){
    target[i,1] <- 1
  }
}

startups <- cbind(startups,target)
rm(target)

#___________target column created______________#




#----------handling rounds.csv-----------#


library(dplyr)

datafile = "rounds.csv"

rounds <- read.csv(paste(path,datafile, sep= ""), 
                     sep = ',',
                     stringsAsFactors = FALSE)

table_df <- data.frame(table(rounds$company_name))
hist(table_df$Freq, freq = F, main="repartition of number of rounds")
sum(table_df$Freq > 1)/length(table_df$Freq) #41% of rounds are associated to more than 1 company
rm(table_df)

#sort rounds dataset by startup name
rounds$company_name <- factor(rounds$company_name)
rounds <- rounds[order(rounds$company_name),]
rounds$company_name <- sapply(rounds$company_name, as.character)

rounds$funding_round_type <- factor(rounds$funding_round_type)


#_____________done with rounds____________#



#----------removes the values prior to the lower_date_limit--------------#

hist(na.omit(startups$founded_at), breaks = 30, main="repartition of foundations dates. Some outliers are visible")
lower_date_limit <- "1990-01-01"

rows_to_remove <- startups$founded_at < lower_date_limit
rows_to_remove[is.na(rows_to_remove)] <- TRUE

startups <- startups[!rows_to_remove,]
#cleaned dataset
summary(startups)

#TODO : impute missing values in funding_total_usd from raised_amount_usd


#______________removes the values prior to the lower_date_limit_____________#





#------------Survival analysis-------------#

library(survival)

#converting factor funding_rounds from rounds, to columns of count of this factor

funding_round_type_surv_shaped <- data.frame(matrix(data = 0, nrow=dim(startups)[1], ncol = 18))
col_names_fund_round_type <- levels(rounds$funding_round_type)
types_counts <- paste("count_",levels(rounds$funding_round_type), sep="")
names(funding_round_type_surv_shaped) <- c(col_names_fund_round_type, types_counts)

for (i in 1:dim(funding_round_type_surv_shaped)[1]){
  name <- startups$company_name[i]
  funding_rounds <- as.character(rounds$funding_round_type[which(rounds$company_name == name)])
  if (length(funding_rounds) > 0){
    for (j in 1:length(funding_rounds)){
      if (is.numeric(rounds$raised_amount_usd[which(rounds$company_name == name)][j])){
        #adds value
        funding_round_type_surv_shaped[i,funding_rounds[j]] <- rounds$raised_amount_usd[which(rounds$company_name == name)][j]
                                                             + funding_round_type_surv_shaped[i,funding_rounds[j]]
      }
      else{
        funding_round_type_surv_shaped[i,funding_rounds[j]] <- NA
      }
      
      #adds count to correponding type
      funding_round_type_surv_shaped[i,paste("count_", funding_rounds[j], sep ="")] <- 1 + funding_round_type_surv_shaped[i,paste("count_", funding_rounds[j], sep ="")]
      
    }
  } 
}



#------------Some more plotting, and selection of censoring_date-----------#

library(ggplot2)

censoring_date <- 15*365

graph_area_subset <- startups[c("days_of_existence", "target")]
closed_ones <- graph_area_subset[graph_area_subset$target == 1,]
closed_ones <- closed_ones[!is.na(closed_ones),]
censored_ones <- graph_area_subset[graph_area_subset$target == 0,]
censored_ones <- censored_ones[!is.na(censored_ones),]

ggplot(data = censored_ones[censored_ones$days_of_existence < censoring_date,], aes(x=days_of_existence)) + geom_histogram() 


rm(graph_area_subset)
rm(closed_ones)
rm(censored_ones)

#____________End plotting____________#


#shaping dataframe for survival analysis

censoring_filter <- function(x){
  if((x >censoring_date) || (x == 0)){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}

rows_to_remove <- sapply(startups$days_of_existence, censoring_filter )  #applying the censoring filter
rows_to_remove[is.na(rows_to_remove)] <- TRUE #also removing rows with NA lifetime

final_rounds_count <- funding_round_type_surv_shaped[!rows_to_remove,]
temp_startups <- startups[!rows_to_remove,]


surv_dataframe <- temp_startups[c("category_code", "funding_total_usd", "funding_rounds")]


surv_target <- Surv(time = temp_startups$days_of_existence, event = temp_startups$target)
surv_dataframe <- data.frame(surv_dataframe, final_rounds_count)
surv_dataframe <- data.frame(surv_dataframe,surv_target)
rm(temp_startups)
rm(final_rounds_count)

#TODO use function cut() to split numeric values into intervals

#fitting model
fit.KM <- survfit(surv_target ~ 1, data=surv_dataframe)
plot(fit.KM, xlab="days operating")

  
  if(FALSE){
    fit <- coxph(surv_target~., data=surv_dataframe)
    summary(fit)
  
  
  
  #______________end of Survival analysis_____________#
  
  
  
  
  #-------------linear model--------------#
  
  
  startups <- left_join(rounds, startups, by = "company_name") #merging startups and rounds to get a linear model shaped dataframe
  rm(rounds) 
  
  startups_subset <- startups[c("company_category_code", "funding_rounds","funding_round_type","raised_amount_usd","funded_at","funding_total_usd","days_of_existence","target")]
  startups_subset <- startups_subset[complete.cases(startups_subset),]
  
  
  DUMMY <- startups_subset$company_category_code
  DUMMY <- model.matrix(~DUMMY)
  startups_subset <- data.frame(startups_subset, DUMMY)
  rm(DUMMY)
  
  DUMMY <- startups_subset$funding_round_type
  DUMMY <- model.matrix(~DUMMY)
  startups_subset <- data.frame(startups_subset, DUMMY)
  rm(DUMMY)
  
  
  startups_subset <- startups_subset[which(startups_subset$days_of_existence<censoring_date),]
  
  #for regression
  X <- as.matrix(startups_subset[,-which(names(startups_subset) %in% c("company_category_code","target","X.Intercept.", "X.Intercept..1"))])
  Y <- as.matrix(startups_subset[,c("target")])
  
  
  #____________linear model_____________#
  
  }
