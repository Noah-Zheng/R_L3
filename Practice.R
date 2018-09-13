getwd()
list.files()

df <- read.csv('pseudo_facebook.tsv', sep = '\t')

names(df)

remove(pd)

str(df)
summary(df)
?read.delim

library(ggplot2)
qplot(x = dob_day, data = df)
?qplot
?xlim
?aes


qplot(x = friend_count, data = subset(df, !is.na(gender)), xlim = c(0,1000))

qplot(x = friend_count, data=subset(df, !is.na(gender)), binwidth = 25) +
  scale_x_continuous(limits = c(0,1000), breaks = seq(0, 1000, 50)) +
  facet_wrap(~gender)

table(df$gender)
?table
by(df$friend_count, df$gender, summary)


qplot(x = tenure/365, data = df, binwidth = 0.25,
      xlab = 'Number of years using Facebook',
      ylab = 'Number of users in sample',
      color = I('black'), fill = I('#099DD9')) +
  scale_x_continuous(breaks = seq(1,7,1), limits = c(1,7))

?c()      

library(gridExtra)
p1 <- qplot(x = age, data = df,
      color = I('black'), fill = I('#099DD9')) +
  scale_x_continuous(breaks = seq(0,90,5), limits = c(0,90))

p2 <- qplot(x = age, data = df) +
  scale_x_continuous(breaks = seq(13,113,1))

grid.arrange(p1,p2, ncol =2)

summary(df$age)

#multi histgram in one plot
p1 <- qplot(x = friend_count, data = df) 

p2 <- qplot(x = friend_count, data = df) +
  scale_x_log10()

p3 <- qplot(x = friend_count, data = df) +
  scale_x_sqrt() 

scale1 <- grid.arrange(p1, p2, p3, ncol = 1)

#another syntax

p4 <- ggplot(aes(x = friend_count), data = df) + geom_histogram()
p5 <- p1 + scale_x_log10()
p6 <- p1 + scale_x_sqrt()

scale2 <- grid.arrange(p4,p5,p6, ncol=1)

grid.arrange(scale1, scale2, ncol = 2)


# frequency polygon
qplot(x = friend_count, data = subset(df, !is.na(gender)), 
      binwidth = 10, geom = "freqpoly", color = gender) +
  scale_x_continuous(limits = c(0,1000), breaks = seq(0,1000,50))


names(df)
levels(df$www_likes)
summary(df$www_likes)

qplot(x = www_likes, y = ..count../sum(..count..),
      data = subset(df, !is.na(gender)), 
      geom = "freqpoly", color = gender) +
  scale_x_continuous(limits = c(0,20))


qplot(x = www_likes, data = subset(df, !is.na(gender)),
      geom = ("freqpoly"), color = gender) +
  scale_x_log10()

by(df$www_likes, df$gender, sum)

#boxplot
qplot(x = gender, y = friend_count, 
      data = subset(df, !is.na(gender)),
      geom = "boxplot", ylim = c(0,1000)) #the limit will affect the calculation


qplot(x = gender, y = friend_count, 
      data = subset(df, !is.na(gender)),
      geom = "boxplot") +
  coord_cartesian(ylim = c(1,250))

by(df$friendships_initiated, df$gender, mean)
by(df$friendships_initiated, df$gender, summary)

qplot(x = gender, y = friendships_initiated, 
      data = subset(df, !is.na(gender)),
      geom = "boxplot") +
  coord_cartesian(ylim = c(1,150))


#transformation
mobile_checkin <- NA
df$mobile_checkin <- ifelse(df$mobile_likes > 0, 1,0)
sum(df$mobile_checkin)/99003

table(df$mobile_checkin)
