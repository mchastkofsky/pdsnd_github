
ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

head(ny)

head(wash)

head(chi)

by(chi$Trip.Duration, chi$Gender, sum) #breakdown of travel time by gender in Chicago

by(ny$Trip.Duration, ny$Gender, sum) #breakdown of travel time by gender in New York

require(dplyr)
chi <- chi %>% mutate_all(na_if,"") #convert blanks into null
ny <- ny %>% mutate_all(na_if,"") #convert blanks into null
chi$Trip.Minutes <- chi$Trip.Duration/60 #convert Chicago seconds to minutes
ny$Trip.Minutes <- ny$Trip.Duration/60 #convert NY seconds to minutes

library(ggplot2) #graphing package
qplot(x = Gender, y = Trip.Minutes,
      data = subset(chi, !is.na(Gender)),
      geom = 'boxplot',
      main = 'Do men or women take longer trips in Chicago?', ylab = 'Trip Duration') +
  coord_cartesian(ylim = c(0, 30))
qplot(x = Gender, y = Trip.Minutes,
      data = subset(ny, !is.na(Gender)),
      geom = 'boxplot',
      main = 'Do men or women take longer trips in New York?', ylab = 'Trip Duration') +
  coord_cartesian(ylim = c(0, 30))

birthage <- function(birthyear){ #function to adjust birth year into age (from 2017)
  age <- (2017-birthyear)
  return(age)
}
chi$Age <- birthage(chi$Birth.Year) #convert Chicago ages
ny$Age <- birthage(ny$Birth.Year) #convert NY ages

by(chi$Age, chi$Gender, summary) #a quick look at age distribution between male and female riders 

ggplot(aes(x=Age, y=Trip.Minutes), data = subset(chi, !is.na(Gender))) + #plot age versus trip duration
  xlim(13, 70) +
  ylim(0, 40) +
  geom_point(alpha=0.05, position = position_jitter(h=0), color='green') +
  geom_smooth(stat = 'summary', fun.y=mean, aes(color='blue')) +
  geom_smooth(stat = 'summary', fun.y=median, aes(color='red')) +
  geom_smooth(stat = 'summary', fun.y=quantile, fun.args=list(probs=0.05), linetype=2, aes(color='orange')) +
  geom_smooth(stat = 'summary', fun.y=quantile, fun.args=list(probs=0.95), linetype=2, aes(color='purple')) +
  ggtitle('Do older people take shorter trips in Chicago?') +
  xlab('Age') +
  ylab('Trip Duration') +
  scale_color_identity(name = "Statistics",
                          breaks = c("blue", "red", "orange", "purple"),
                          labels = c("Mean", "Median", "5%", "95%"),
                          guide = "legend")

by(ny$Birth.Year, ny$Gender, summary) #a quick look at age distribution between male and female riders 

ggplot(aes(x=Age, y=Trip.Minutes), data = subset(ny, !is.na(Gender))) + #plot age versus trip duration
  xlim(13, 80) +
  ylim(0, 50) +
  geom_point(alpha=0.05, position = position_jitter(h=0), color='green') +
  geom_smooth(stat = 'summary', fun.y=mean, aes(color='blue')) +
  geom_smooth(stat = 'summary', fun.y=median, aes(color='red')) +
  geom_smooth(stat = 'summary', fun.y=quantile, fun.args=list(probs=0.05), linetype=2, aes(color='orange')) +
  geom_smooth(stat = 'summary', fun.y=quantile, fun.args=list(probs=0.95), linetype=2, aes(color='purple')) +
  ggtitle('Do older people take shorter trips in New York?') +
  xlab('Age') +
  ylab('Trip Duration') +
  scale_color_identity(name = "Statistics",
                          breaks = c("blue", "red", "orange", "purple"),
                          labels = c("Mean", "Median", "5%", "95%"),
                          guide = "legend")

library(lubridate)
wash$Trip.Minutes <- wash$Trip.Duration/60 #convert Washington seconds to minutes
wash$Start.Time <- ymd_hms(wash$Start.Time) #resolve issue of washington time data

summary(wash$Start.Time) #summarize the hourly distribution of trips in Washington DC

wash$Start.hour <- hour(wash$Start.Time)
summary(wash$Start.hour)

by(wash$Trip.Duration, wash$Start.hour, summary) #Examine trip length for each hour of the day

ggplot(aes(x=Start.hour, y=Trip.Minutes), data = wash) + #plot of Bikeshare over time in Washington DC
  ylim(0, 200) +
  geom_point(alpha=0.05, position = position_jitter(h=0), color='green') +
  geom_smooth(stat = 'summary', fun.y=mean, aes(color='blue')) +
  geom_smooth(stat = 'summary', fun.y=median, aes(color='red')) +
  geom_smooth(stat = 'summary', fun.y=quantile, fun.args=list(probs=0.80), linetype=2, aes(color='orange')) +
  geom_smooth(stat = 'summary', fun.y=quantile, fun.args=list(probs=0.95), linetype=2, aes(color='purple')) +
  ggtitle('When do people take long trips in Washington DC?') +
  xlab('Hour of the Day') +
  ylab('Trip Duration') +
  scale_color_identity(name = "Statistics",
                          breaks = c("blue", "red", "orange", "purple"),
                          labels = c("Mean", "Median", "80%", "95%"),
                          guide = "legend")

wash$Start.month <- month(wash$Start.Time) #Examination of long rides in the middle of the night
wash.subset <- subset(wash, wash$Start.hour <= 5 & wash$Trip.Minutes >= 100)
summary(wash.subset)

chi$Start.hour <- hour(chi$Start.Time) #Comparison of data from Chicago and New York
ggplot(aes(x=Start.hour, y=Trip.Minutes), data = chi) +
  ylim(0, 60) +
  geom_point(alpha=0.05, position = position_jitter(h=0), color='green') +
  geom_smooth(stat = 'summary', fun.y=mean, aes(color='blue')) +
  geom_smooth(stat = 'summary', fun.y=median, aes(color='red')) +
  geom_smooth(stat = 'summary', fun.y=quantile, fun.args=list(probs=0.80), linetype=2, aes(color='orange')) +
  geom_smooth(stat = 'summary', fun.y=quantile, fun.args=list(probs=0.95), linetype=2, aes(color='purple')) +
  ggtitle('When do people take long trips in Chicago?') +
  xlab('Hour of the Day') +
  ylab('Trip Duration') +
  scale_color_identity(name = "Statistics",
                          breaks = c("blue", "red", "orange", "purple"),
                          labels = c("Mean", "Median", "80%", "95%"),
                          guide = "legend")

ny$Start.hour <- hour(ny$Start.Time)
ggplot(aes(x=Start.hour, y=Trip.Minutes), data = ny) +
  ylim(0, 60) +
  geom_point(alpha=0.05, position = position_jitter(h=0), color='green') +
  geom_smooth(stat = 'summary', fun.y=mean, aes(color='blue')) +
  geom_smooth(stat = 'summary', fun.y=median, aes(color='red')) +
  geom_smooth(stat = 'summary', fun.y=quantile, fun.args=list(probs=0.80), linetype=2, aes(color='orange')) +
  geom_smooth(stat = 'summary', fun.y=quantile, fun.args=list(probs=0.95), linetype=2, aes(color='purple')) +
  ggtitle('When do people take long trips in New York?') +
  xlab('Hour of the Day') +
  ylab('Trip Duration') +
  scale_color_identity(name = "Statistics",
                          breaks = c("blue", "red", "orange", "purple"),
                          labels = c("Mean", "Median", "80%", "95%"),
                          guide = "legend")

system('python -m nbconvert Explore_bikeshare_data.ipynb')
