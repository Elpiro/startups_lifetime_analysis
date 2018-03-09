#-------------import of companies.csv---------------#

path = "C:/Users/GALLICE/Documents/Perso/Etudes/DSTI/Cours/Survival Analysis/project/Data/crunchbase-october-2013-master/crunchbase-"

datafile = "crunchbase-companies.csv"
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

summary(startups)

#________________companies imported______________#



#-----------computes lifetime of startup------------#

start_date <- startups$founded_at

startups$last_milestone_at[is.na(startups$last_milestone_at)] <- startups$last_funding_at[is.na(startups$last_milestone_at)]#imputes missing dates in last_milestone_at

end_date <- startups$last_milestone_at

if(FALSE){
  end_date <- startups$last_funding_at #or end_date <- startups$last_milestone_at ?
  #for startups that are acquired, the last_milestone could be better
  # startups[which(startups$status=="acquired"), c("last_funding_at","last_milestone_at")]
}

days_of_existence <- data.frame(matrix(data = NA,nrow = dim(startups)[1], ncol = 1))

for(i in seq(1,dim(startups)[1])){
  days_of_existence[i,] <- as.integer(end_date[i]-start_date[i])
  if(!is.na(days_of_existence[i,])){
    if(days_of_existence[i,] <= 30){
      days_of_existence[i,] <- NA
    }
  }
}

#create column for lifetime of the startup
names(days_of_existence) <- "days_of_existence"
summary(days_of_existence)
startups <- cbind(startups,days_of_existence)

startups <- startups[!is.na(days_of_existence),]
startups <- startups[!is.na(startups$funding_total_usd),]

rm(days_of_existence)

#____________done with lifetime_____________#


#------some plotting-----------#
library(ggplot2)

graph_area_subset <- startups[c("days_of_existence", "status", "funding_total_usd", "category_code")]
graph_area_subset <- graph_area_subset[graph_area_subset$days_of_existence<15000,]

ggplot(data=graph_area_subset,aes(x=days_of_existence, group=status, fill=status, alpha=0.3)) + geom_density() 
rm(graph_area_subset)

#__________________done plotting________________#



#------------Transform status column to alive/closed-----------#
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

#---------quick plotting------------#

graph_area_subset <- startups[c("days_of_existence", "target","category_code")]
categories <- graph_area_subset$category_code
t_categories <- data.frame(table(categories))
t_categories <- t_categories[order(t_categories$Freq, decreasing=T),]
graph_area_subset$category_code <- factor(graph_area_subset$category_code, levels=t_categories$categories, ordered=T)
graph_area_subset$target <- factor(graph_area_subset$target)

ggplot(data=graph_area_subset,aes(x=category_code, group=target, fill=target)) + geom_histogram(stat="count") 

rm(categories)
rm(t_categories)
rm(graph_area_subset)

hist(na.omit(startups$founded_at), breaks = 30, main="repartition of foundations dates")

#_____________done plotting______________#


lower_date_limit <- "1990-01-01"


#----------removes the values prior to the lower_date_limit--------------#
rows_to_remove <- startups$founded_at < lower_date_limit
rows_to_remove[is.na(rows_to_remove)] <- TRUE

startups <- startups[!rows_to_remove,]


#TODO : impute missing values in funding_total_usd from raised_amount_usd


#______________removes the values prior to the lower_date_limit_____________#


censoring_years = 15



#------------Some more plotting, and selection of censoring_date-----------#

library(gridExtra)


censoring_date <- censoring_years*365
censoring_date

graph_area_subset <- startups[c("days_of_existence", "target")]
graph_area_subset$target <- factor(graph_area_subset$target)

plot1 <- ggplot(data = graph_area_subset[graph_area_subset$days_of_existence,], 
                aes(x=days_of_existence, group=target, fill=target)) + geom_histogram() + xlab("Companies days of existence accross all data")

plot2 <- ggplot(data = graph_area_subset[graph_area_subset$days_of_existence < censoring_date,],
                aes(x=days_of_existence, group=target, fill=target)) + geom_histogram() + xlab(paste("startups days of existence for", censoring_years, "years", sep=" "))

grid.arrange(plot1, plot2, ncol=2)

rm(graph_area_subset)

#____________End plotting____________#

#----------handling rounds.csv-----------#


library(dplyr)

datafile = "crunchbase-rounds.csv"

rounds <- read.csv(paste(path,datafile, sep= ""), 
                   sep = ',',
                   stringsAsFactors = FALSE)

#sort rounds dataset by startup name
rounds$company_name <- factor(rounds$company_name)
rounds <- rounds[order(rounds$company_name),]
rounds$company_name <- sapply(rounds$company_name, as.character)

rounds$funding_round_type <- factor(rounds$funding_round_type)

str(rounds)

#_____________done with rounds____________#

table_df <- data.frame(table(rounds$company_name))

sum(table_df$Freq > 1)/length(table_df$Freq)

hist(table_df$Freq, freq = F, main="repartition of number of rounds")
rm(table_df)

#------------Survival analysis-------------#

library(survival)

#converting factor funding_rounds from rounds, to columns of count of this factor

library(survival)

#converting factor funding_rounds from rounds, to columns of count of this factor

funding_round_type_surv_shaped <- data.frame(matrix(data = 0, nrow=dim(startups)[1], ncol = 19))
col_names_fund_round_type <- levels(rounds$funding_round_type)
types_counts <- paste("count_",levels(rounds$funding_round_type), sep="")
names(funding_round_type_surv_shaped) <- c("company_name",col_names_fund_round_type, types_counts)

for (i in 1:dim(funding_round_type_surv_shaped)[1]){
  name <- startups$company_name[i]
  funding_round_type_surv_shaped[i,1] <- name
  funding_rounds <- as.character(rounds$funding_round_type[which(rounds$company_name == name)])#get all funding rounds type for startup i 
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

temp_startups <- startups[!rows_to_remove,]



surv_target <- Surv(time = temp_startups$days_of_existence, event = temp_startups$target)

surv_dataframe <- temp_startups[c("company_name","category_code", "funding_total_usd", "funding_rounds")]
surv_dataframe <- left_join(surv_dataframe, funding_round_type_surv_shaped, by="company_name")
surv_dataframe <- data.frame(surv_dataframe,surv_target)
surv_dataframe <- surv_dataframe[,-1]

rm(temp_startups)
rm(funding_round_type_surv_shaped)
str(surv_dataframe)

#TODO use function cut() to split numeric values into intervals

surv_dataframe$funding_total_usd <- log(surv_dataframe$funding_total_usd + 1, base=10)
surv_dataframe$angel <- log(surv_dataframe$angel + 1)
surv_dataframe$crowdfunding <- log(surv_dataframe$crowdfunding + 1)
surv_dataframe$other <- log(surv_dataframe$other + 1)
surv_dataframe$post.ipo <- log(surv_dataframe$post.ipo + 1)
surv_dataframe$private.equity <- log(surv_dataframe$private.equity + 1)
surv_dataframe$series.a <- log(surv_dataframe$series.a + 1)
surv_dataframe$series.b <- log(surv_dataframe$series.b + 1)
surv_dataframe$series.c. <- log(surv_dataframe$series.c. + 1)
surv_dataframe$venture <- log(surv_dataframe$venture + 1)


#fitting model
fit.KM <- survfit(surv_target ~ 1, data=surv_dataframe)
plot(fit.KM, xlab="days operating", ylab="percentage operating")

  
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

