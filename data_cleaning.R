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

str(startups)


table(startups$category_code)
table(startups$country_code)
table(startups$state_code)
table(startups$region)
table(startups$funding_rounds)


hist(startups$founded_at, breaks = 30)

plot(startups$status ~ log(startups$funding_total_usd), col = unique(startups$status))
legend("topleft", legend=levels(startups$status), pch=16, col=unique(startups$status))

plot(startups$status ~ startups$category_code, col = unique(startups$status))
legend("topleft", legend=levels(startups$status), pch=16, col=unique(startups$status))

plot(startups$status ~ startups$funding_rounds, col = unique(startups$status))
legend("topleft", legend=levels(startups$status), pch=16, col=unique(startups$status))



#adding a column duration

start_date <- startups$founded_at
end_date <- startups$last_funding_at #or end_date <- startups$last_milestone_at ?

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

rm(current_lifetime)

startups <- startups[!is.na(current_lifetime),]
startups <- startups[!is.na(startups$funding_total_usd),]
table(startups$status)


#Transform status column to alive/closed

target <- data.frame(matrix(data = NA,nrow = dim(startups)[1], ncol = 1))
names(target) <- "target"

for (i in seq(1, dim(startups)[1])){
  startup_status <- startups$status[i]
  if (startup_status == 'acquired'){
    target[i,1] <-  0
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


fit.km <- survfit(Surv(days_of_existence, target) ~ category_code, data = startups)
plot(fit.km)


