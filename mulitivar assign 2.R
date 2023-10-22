#1
attach(video_games_fall_2023)

summary(score)

boxplot(score, col='blue')

hist(score,col='blue',breaks=20)

summary(sales_global)

boxplot(sales_global, col='green')


hist(sales_global,col='green',breaks=100)


summary(release_year)

boxplot(release_year, col='orange')


hist(release_year,col='orange',breaks=25)


summary(count_critic)

boxplot(count_critic, col='purple')


hist(count_critic,col='purple',breaks=10)

unique_genres <- unique(genre)
colors <- rainbow(length(unique_genres))
col_map <- setNames(colors, unique_genres)

par(mfrow=c(1,3))  

plot(sales_global,col = col_map[genre], score, main="sales_globalvs score", xlab="sales_global", ylab="score")

plot(release_year,score,col = col_map[genre] , main="release_year vs score", xlab="release_year", ylab="score")

plot(count_critic, score,col = col_map[genre], main="count_critic vs score", xlab="count_critic", ylab="score")

lm.fit=lm(score~sales_global)

summary(lm.fit)

confint(lm.fit,sales_global, level=0.95)

require(visreg)

par(mfrow=c(1,1)) 
visreg(lm.fit,alpha=0.05)
#2
video_games=read.csv("C:/Users/95675/Desktop/R/Lab session/Lab1/video_games_fall_2023.csv")
attach(video_games)

names(video_games)   
View(video_games)    

lm.fit=lm(score~sales_global)
lm.fit

b0=coef(lm.fit)[1]
b1=coef(lm.fit)[2]

a = b0 + 80*b1
print (a)
summary(lm.fit)
confint(lm.fit, 'sales_global', level=0.95)   ###95% interval
install.packages("visreg")
require(visreg)

par(mfrow=c(1,2))
visreg(lm.fit, alpha=1-0.95)  ###95% confidence interval
plot(sales_global, score)
abline(lm.fit)

lm.fit=lm(score~release_year)
lm.fit

b0=coef(lm.fit)[1]
b1=coef(lm.fit)[2]

a = b0 + 80*b1
print (a)
summary(lm.fit)
confint(lm.fit, 'release_year', level=0.95)   ###95% interval
install.packages("visreg")
require(visreg)

par(mfrow=c(1,2))
visreg(lm.fit, alpha=1-0.95)  ###95% confidence interval
plot(release_year, score)
abline(lm.fit)

lm.fit=lm(score~count_critic)
lm.fit

b0=coef(lm.fit)[1]
b1=coef(lm.fit)[2]

a = b0 + 80*b1
print (a)
summary(lm.fit)
confint(lm.fit, 'count_critic', level=0.95)   ###95% interval
install.packages("visreg")
require(visreg)

par(mfrow=c(1,2))
visreg(lm.fit, alpha=1-0.95)  ###95% confidence interval
plot(count_critic, score)
abline(lm.fit)

#4
video_games=read.csv("C:/Users/95675/Desktop/R/Lab session/Lab1/video_games_fall_2023.csv")
attach(video_games)

names(video_games)

mreg=lm(score~sales_global+release_year+count_critic)
summary(mreg)

b0=coef(mreg)[1]
b1=coef(mreg)[2]
b2=coef(mreg)[3]
b3=coef(mreg)[4]


b0+b1*0.75+b2*2009+b3*80

#5

video_games=read.csv("C:/Users/95675/Desktop/R/Lab session/Lab1/video_games_fall_2023.csv")
attach(video_games)

names(video_games) 

video_games$Nintendo = ifelse(video_games$publisher == "Nintendo", 1, 0)
attach(video_games)
table(Nintendo)

mreg=lm(score~release_year+Nintendo)
summary(mreg)

b0=coef(mreg)[1]
b1=coef(mreg)[2]
b2=coef(mreg)[3]
print(b0)
print(b1)
print(b2) 
plot(release_year, score, col=ifelse(publisher=="Nintendo", "green", "blue"), pch = 15)
abline(b0 + b2, b1, col = "green", lwd = 2, lty = 2)
abline(b0, b1, col = "blue", lwd = 2, lty = 2)
legend("topright", pch=1, col=c( "green", "blue" ), c( "Nintendo", "Others"))


#6
table(genre)

video_games_fall_2023$genre = as.factor(video_games_fall_2023$genre)

attach(video_games_fall_2023)

genre = relevel(genre,ref = 'Racing')

mreg = lm(score~genre)

summary(mreg)


genre = relevel(genre,ref = 'Shooter')


mreg = lm(score~genre)

summary(mreg)

#7 
video_games_fall_2023$nintendo <- ifelse(video_games_fall_2023$publisher == "Nintendo", 1, 0)


video_games_fall_2023$strategy <- ifelse(video_games_fall_2023$genre == "Strategy", 1, 0)

attach(video_games_fall_2023)


mreg = lm(score~nintendo+strategy+nintendo*strategy)

summary(mreg)


mreg = lm(score~nintendo+release_year+nintendo*release_year)

summary(mreg)


b0 = coef(mreg)[1]
b1 = coef(mreg)[2]
b2 = coef(mreg)[3]
b3 = coef(mreg)[4]



plot(release_year,score, pch=2 , col=ifelse(nintendo==1,'green','blue') ,xlab="release_year", ylab="score")

abline(b0+b1,b2+b3,lty=2,lwd=2, col='green')

abline(b0,b2,lty=2,lwd=2, col='blue')

legend("bottomleft", pch=2, col=c( "green", "blue" ), c( "nintendo", "others"))


