## 0. Get and clean data
#Import data
df <- read.csv("activity.csv")
df$date <- as.Date(df$date) 
str(df)





## 1. What is mean total number of steps taken per day?
# Create new data frame
df1 <- df
df1 <- aggregate(steps ~ date, data=df1, FUN=sum, na.rm = TRUE)
df1$sum.steps <- as.numeric(df1$steps)
str(df1)
# Create histogram
hist(df1$sum.steps)
summary(df1$sum.steps)





## 2. What is the average daily activity pattern?
# Create new data frame
df2 <- df
str(df2)
df2 <- aggregate(steps ~ interval, data=df2, FUN=mean, na.rm = TRUE)
df2$mean.steps <- as.numeric(df2$steps)

# Create plot
plot(df2$interval, df2$mean.steps, type = "l")
#Answer: 835





## 3. Imputing missing values
# Number of missing values
df3 <- df
colSums(is.na(df3))

# Replace NA with means
df3[is.na(df3)] <- mean(df3$steps, na.rm = TRUE)
colSums(is.na(df3))

# Create new data frame
df3x <- aggregate(cbind(steps, interval) ~ date, data=df3, FUN=sum, na.rm = TRUE)
str(df3x)
df3x$sum.steps <- as.numeric(df3x$steps)
df3x$sum.interval <- as.numeric(df3x$interval)
str(df3x)

# Create histogram
hist(df3x$sum.steps)
summary(df3x$sum.steps)
#Answer: Values do not differ considerably from the estimates from the first part of the assignment





## 4. Are there differences in activity patterns between weekdays and weekends?
# Create new data frame and variable
df4 <- df3
df4$dayOFweek1 <- weekdays(df4$date)
df4$dayOFweek2 <- ifelse(df4$dayOFweek1=="Saturday", "Weekend",
                         ifelse(df4$dayOFweek1=="Sunday", "Weekend", "Weekday"))
df4$dayOFweek2 <- as.factor(df4$dayOFweek2)
str(df4)

# Create graph
df4x <- aggregate(steps ~ interval + dayOFweek2, data=df4, FUN=mean, na.rm = TRUE)

library(ggplot2)
ggplot(data = df4x, aes(interval, steps)) +
  geom_line() +
  facet_grid( ~ dayOFweek2) 


