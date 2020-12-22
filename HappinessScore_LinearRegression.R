## PROJECT SUMMARY AND TASK DETAIL

library(ggplot2)
library(reshape)
library(forecast)
library(rpart)
library(rpart.plot)
library(randomForest)
#----------------------------------------------------------------------------------------------

# What makes people in a country happy?
# This dataset shows the happiest countries on earth, which is great info when you're looking for your next move but what if you wanted to create a new country with the goal of having the happiest citizens? What if you're a president looking to improve their country? How would you do that?
# The goal of this task is to find out what factors contribute to happiness. You can join any other data and use any insights you might have that show a strong correlation between the factors you come up with.

#----------------------------------------------------------------------------------------------

# Reading the file
happinessScore <- read.csv("2019.csv")
dim(happinessScore)
str(happinessScore)
head(happinessScore)  

# Summarising the dataset
summary(happinessScore)

# Removing country name and happiness rank from the dataset
happinessScoredf<-happinessScore[,-c(1,2)]
str(happinessScoredf)

#-----------------------------------------------------------------------------------------------

# Distribution of Happiness Scores
hist(happinessScore$Score,
     main="Histogram",
     xlab="Happiness Score",
     xlim=c(0,10),
     col="darkseagreen",
     freq=FALSE)

# Relationship between GDP per Capita and Happiness Score
p <- ggplot(happinessScore, aes(x=Score, y=GDP.per.capita)) +  geom_point(color = 'deepskyblue3')+
  geom_smooth(method=lm, color='gray46')

p + ggtitle("GDP vs Score") + theme(plot.title = element_text(hjust = 0.5))+ 
  xlab("GDP per capita") + ylab("Score")

# Relationship between Social Support and Happiness Score
p <- ggplot(happinessScore, aes(x=Score, y=Social.support)) +  geom_point(color = 'deepskyblue3')+
  geom_smooth(method=lm, color='gray46')

p + ggtitle("Social Support vs Score") + theme(plot.title = element_text(hjust = 0.5))+ 
  xlab("Social Support") + ylab("Score")


# Relationship between Healthy Life Expectancy and Happiness Score
p <- ggplot(happinessScore, aes(x=Score, y=Healthy.life.expectancy)) +  geom_point(color = 'deepskyblue3')+
  geom_smooth(method=lm, color='gray46')

p + ggtitle("Life Expectancy vs Score") + theme(plot.title = element_text(hjust = 0.5))+ 
  xlab("Healthy Life Expectancy") + ylab("Score")


# Relationship between Freedom to make life choices and Happiness Score
p <- ggplot(happinessScore, aes(x=Score, y=Freedom.to.make.life.choices)) +  geom_point(color = 'deepskyblue3')+
  geom_smooth(method=lm, color='gray46')

p + ggtitle("Freedom vs Score") + theme(plot.title = element_text(hjust = 0.5))+ 
  xlab("Freedom to make life choices") + ylab("Score")

# Relationship between Generosity and Happiness Score
p <- ggplot(happinessScore, aes(x=Score, y=Generosity)) +  geom_point(color = 'deepskyblue3')+
  geom_smooth(method=lm, color='gray46')

p + ggtitle("Generosity vs Score") + theme(plot.title = element_text(hjust = 0.5))+ 
  xlab("Generosity") + ylab("Score")

# Relationship between Perception of corruption and Happiness Score
p <- ggplot(happinessScore, aes(x=Score, y=Perceptions.of.corruption)) +  geom_point(color = 'deepskyblue3')+
  geom_smooth(method=lm, color='gray46')

p + ggtitle("Corruption vs Score") + theme(plot.title = element_text(hjust = 0.5))+ 
  xlab("Perceptions of corruption") + ylab("Score")

# HeatMap to see the correlation between the variables and the Happiness Score

cor.mat<-round(cor(happinessScoredf),2)
melted.cor.mat<-melt(cor.mat)

p<-ggplot(melted.cor.mat, aes(x = X1, y= X2, fill = value))+ geom_tile()+geom_text(aes(x=X1, y= X2, label = value))
p + ggtitle("Correlation heatmap for variables") + theme(plot.title = element_text(hjust = 0.5))+ 
  xlab("") + ylab("")
#-------------------------------------------------------------------------------------------------------
# LINEAR REGRESSION MODEL
# Partitioning data
set.seed(1)
train.index<-sample(rownames(happinessScoredf),dim(happinessScoredf)[1]*0.6)
test.index<-setdiff(rownames(happinessScoredf),train.index)

train.df<-happinessScoredf[train.index,]
test.df<-happinessScoredf[test.index,]

# Linear Regression Model training
linearmodel1<-lm(Score~.,data=train.df)
summary(linearmodel1)

# Linear Regression Model testing
linearmodel1.predict=predict(linearmodel1,test.df)
accuracy(linearmodel1.predict,test.df$Score)

# RMSE is 0.4951 and MAE 0.4020
#-----------------------------------------------------------------------------------
#Residual Analysis
test.res <- data.frame(test.df$Score, linearmodel1.predict, 
                       residuals = test.df$Score - linearmodel1.predict)

hist(test.res$residuals,
     main="Histogram",
     xlab="Residuals",
     col="darkseagreen",
     freq=FALSE)

standard_dev_res <- sd(test.res$residuals)
mean_res <- mean(test.res$residuals)

standard_dev_res
mean_res
length(test.res$residuals[which(test.res$residuals>standard_dev_res | test.res$residuals< - standard_dev_res)])

#-------------------------------------------------------------------------------------
# REGRESSION TREES MODEL

# Running a full grown regression tree
happiness_tree <- rpart(Score ~ ., data = train.df, method="anova",cp=0,xval=5)

printcp(happiness_tree)
plotcp(happiness_tree)

# using prp to draw this tree
prp(happiness_tree, type = 1, extra = 1, split.font = 1, varlen = -10, digits = -1)  

# prune the tree
happiness_tree_pruned<- prune(happiness_tree, cp=0.0134862) # from cptable 

happiness.ct.predict <- predict(happiness_tree_pruned, test.df)
accuracy(happiness.ct.predict,test.df$Score)

# RMSE is 0.6647 and MAE is 0.5283

#-------------------------------------------------------------------------------------
# RANDOM FOREST MODEL
set.seed(100)
happinessrf <- randomForest(Score ~ ., data = train.df, ntree = 500, 
                        mtry = 4, importance = TRUE)  

## variable importance plot
varImpPlot(happinessrf, type = 1)

Happinessrf.pred.test <- predict(happinessrf,test.df)
accuracy(Happinessrf.pred.test,test.df$Score)

# RMSE is 0.5244 and MAE 0.4026

#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------

## FINAL RECOMMENDED MODEL

# Final Recommended model is Linear Regression Model with an RMSE of 0.495 and MAE 0.4020.


