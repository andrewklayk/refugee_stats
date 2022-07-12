migr <- read.csv('migration.csv')

plot_results <- function(preds, x, y) {
  plot(x=x, y=y, type='l', lty=2)
  lines(x=x, y=y, type='p')
  lines(x=x, y=preds, type='l')
}

library(zoo)

# EXPLORATION

# plot data
plot(migr$all_diff,type='l')

# moving average
migr$all_diff_avg <- rollapply(migr$all_diff, 7,
                               mean, na.rm = TRUE, 
                               fill = NA, partial = TRUE)

lines(migr$all_diff_avg, type='l', lty=2)




# MODELLING & INTERPRETATION

# Take data from may 9th
data <- tail(migr, 43)
# recalculate the rolling average for only that data
data$all_diff_avg <- rollapply(data$all_diff, 7,
                               mean, na.rm = TRUE, 
                               fill = NA, partial = TRUE)

plot(x=data$X, data$all_diff,type='l')
lines(x=data$X, data$all_diff_avg, type='l', lty=2)

# linear regression on raw data

x <- data$X
y <- data$all_diff
lr <- lm(y~x)
preds <- predict(lr, data.frame(x))
plot_results(preds, x, y)

# calculate and plot residuals
residuals <- y - preds
plot(x=x, y=residuals)
abline(h=0, lty=2)
res.mavg <- rollapply(residuals, 7,
                      mean, na.rm = TRUE, 
                      fill = NA, partial = TRUE)
lines(x=x,y=res.mavg)
# calculate root mean squared error
rmse <- sqrt(mean(residuals^2))
rmse

# box-and-whiskers plot of the residuals
# the lowest ones in the first two weeks are, in fact, outliers
boxplot(residuals)

# calculate average residual for every week
n_weeks = length(residuals) %/% 7
week.avgs <- c(1:n_weeks)
for (week in c(1:n_weeks)) {
  endday <- week*7
  startday <- endday - 6
  week.avgs[week] <- mean(residuals[startday:endday])
}
barplot(week.avgs, xlab='Week', ylab='Average residual', names.arg = c(1:n_weeks))
abline(h=0)


# we see that first two weeks (esp. second) are outliers
# compared to the rest, so we remove them


data <- tail(data, 28)

# recalculate the rolling average
data$all_diff_avg <- rollapply(data$all_diff, 7,
                               mean, na.rm = TRUE, 
                               fill = NA, partial = TRUE)

plot(x=data$X, y=data$all_diff,type='l')
lines(x=data$X, y=data$all_diff_avg, type='l', lty=2)

# apply linear regression
x <- data$X
y <- data$all_diff
lr <- lm(y~x)
preds <- predict(lr, data.frame(x))
plot_results(preds, x, y)

# calculate residuals
residuals <- y - preds
plot(x=x, y=residuals)
abline(h=0, lty=2)
res.mavg <- rollapply(residuals, 7,
                      mean, na.rm = TRUE, 
                      fill = NA, partial = TRUE)
lines(x=x,y=res.mavg)
# significantly reduced RMSE!
rmse <- sqrt(mean(residuals^2))
rmse

# now try to eliminate the periodic component
# the trend is estimated with a moving average

library('lubridate')
data$day <- wday(data$data_date, week_start=1)

res <- data$all_diff - data$all_diff_avg
# calculate the average residual for each day of the week
avg_res <- c(1:7)
for (day in data$day[1:7]) {
  avg_res[day] <- mean(res[seq(day, length(res), 7)])
}
barplot(avg_res,xpd=FALSE,
        names.arg = c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun'))
abline(h=0)

data$all_diff_stat <- data$all_diff - 
  matrix(avg_res, nrow=1, 
         ncol=(7*(length(data$all_diff) %/% 7 + 1)),
         byrow=TRUE)[1:length(data$all_diff)]

plot(x=x, y=data$all_diff,type='l',lty=2)
lines(x=x, y=data$all_diff_stat,type='l')

# regression, again, but on all_diff_stat

x <- data$X
y <- data$all_diff_stat
lr <- lm(y~x)
preds <- predict(lr, data.frame(x))
plot_results(preds, x, y)

# calculate RMSE: reduced again!
# our end model: C_i = F_i + P_i + R_i, where
# F_i is the trend (linear regression),
# P_i is the periodic component
# R_i is random noise
rmse <- sqrt(mean((preds - data$all_diff_stat)^2))
rmse
