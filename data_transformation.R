
money <- startups$funding_total_usd
#money <- money[-boxplot(money, plot=F)$group]
#removing nas
money_1 <- sort(money[!is.na(money)])
quartile_L = 0.25
quartile_R = 0.75
barplot(sort(money[ceiling(length(money)*quartile_R):floor(length(money)*quartile_L)]),
        main = "barplot of values between Q1 and Q3, ordered by ascending value",
        ylab = "invested money",
        xlab="ID")


par(mfrow=c(2,2))

hist(money_1, main="histogram of raw investments")

hist((money_1-mean(money_1))/sqrt(var(money_1)),
     main = "Central limit theorem of money invested in all startups",
     xlab = "money invested in a startup")

log_transform <- log(money_1)

plot(log_transform, type='l', xlab = "ID", main="Sorted log investments")

hist_data = hist((log_transform - mean(log_transform)) / var(log_transform),
                 freq = F,
                 main="Histogram of central limit theorem of log(money)",
                 xlab = "investments repartition")

#We see outliers on the right that skew the distribution
#non_outliers <- hist_data$counts[1:(length(hist_data$counts)-10)]
#non_outliers = sum(non_outliers)
#clean_money <- log_transform[1:non_outliers]

