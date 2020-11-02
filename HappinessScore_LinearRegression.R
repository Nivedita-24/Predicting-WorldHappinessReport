## PROJECT SUMMARY AND TASK DETAIL

library(ggplot2)
library(reshape)
library(forecast)

#----------------------------------------------------------------------------------------------

# What makes people in a country happy?
# This dataset shows the happiest countries on earth, which is great info when you're looking for your next move but what if you wanted to create a new country with the goal of having the happiest citizens? What if you're a president looking to improve their country? How would you do that?
# The goal of this task is to find out what factors contribute to happiness. You can join any other data and use any insights you might have that show a strong correlation between the factors you come up with.

#----------------------------------------------------------------------------------------------

# Reading the file
happinessScore <- read.csv("2019.csv")
summary(happinessScore)
str(happinessScore)
dim(happinessScore)
head(happinessScore)  

#-----------------------------------------------------------------------------------------------

# Distribution of Happiness Scores
hist(happinessScore$Score, xlab= 'Happiness Score')

# Relationship between GDP per Capita and Happiness Score
ggplot(happinessScore, aes(x=Score, y=GDP.per.capita)) + 
  geom_point()+
  geom_smooth(method=lm)

# Relationship between Social Support and Happiness Score
ggplot(happinessScore, aes(x=Score, y=Social.support)) + 
  geom_point()+
  geom_smooth(method=lm)

# Relationship between Healthy Life Expectancy and Happiness Score
ggplot(happinessScore, aes(x=Score, y=Healthy.life.expectancy)) + 
  geom_point()+
  geom_smooth(method=lm)

# Relationship between Freedom to make life choices and Happiness Score
ggplot(happinessScore, aes(x=Score, y=Freedom.to.make.life.choices)) + 
  geom_point()+
  geom_smooth(method=lm)

# Relationship between Generosity and Happiness Score
ggplot(happinessScore, aes(x=Score, y=Generosity)) + 
  geom_point()+
  geom_smooth(method=lm)

# Relationship between Perception of corruption and Happiness Score
ggplot(happinessScore, aes(x=Score, y=Perceptions.of.corruption)) + 
  geom_point()+
  geom_smooth(method=lm)

# HeatMap to see the correlation between the variables and the Happiness Score

# Removing country name and happiness rank from the dataset
happinessScoredf<-happinessScore[,-c(1,2)]
str(happinessScoredf)

cor.mat<-round(cor(happinessScoredf),2)
melted.cor.mat<-melt(cor.mat)
ggplot(melted.cor.mat, aes(x = X1, y= X2, fill = value))+ geom_tile()+geom_text(aes(x=X1, y= X2, label = value))

#-------------------------------------------------------------------------------------------------------
# Linear Regression Model
# Partitioning data
set.seed(1)
train.index<-sample(rownames(happinessScoredf),dim(happinessScoredf)[1]*0.6)
test.index<-setdiff(rownames(happinessScoredf),train.index)

train.df<-happinessScoredf[train.index,]
test.df<-happinessScoredf[test.index,]

linearmodel1<-lm(Score~.,data=train.df)
summary(linearmodel1)

linearmodel1.predict=predict(linearmodel1,test.df)
accuracy(linearmodel1.predict,test.df$Score)

#-----------------------------------------------------------------------------------
#Residual Analysis
test.res <- data.frame(test.df$Score, linearmodel1.predict, 
                       residuals = test.df$Score - linearmodel1.predict)
hist(test.res$residuals)
sd(test.res$residuals)
mean(test.res$residuals)
length(test.res$residuals[which(test.res$residuals>0.4985692 | test.res$residuals< - 0.4985692)])

#-------------------------------------------------------------------------------------