# Machine learning methods with Fangraphs leaderboard stats
# Based on a walk through by TheKidKudzu on Community.Fangraphs.com
# WAR! "huh", YEAH
# 
# Can we find what are the most important qualities that make up WAR
# for batters and pitchers? 
#
# Data sourced from Fangraphs.com
# Starting with Pitching. Complete leaderboard information from 2010 to 2019

# input csv files downloaded from Fangraphs
pitchers <- read_csv("~/Downloads/FanGraphs Leaderboard Pitching.csv")

# Let's keep only the conlumns with baseball statistics in them. 
pitchCols <- c('W','L','IP','ER','HR','BB','SO','WAR')

# Next we need to set a training group and testing group
inTrain <- createDataPartition(pitchers$WAR, p=0.7,list = FALSE)
training <- pitchers[inTrain,pitchCols]
testing <- pitchers[-inTrain,pitchCols]

# Lets run a linear regression to find what stats are the most important 
# for WAR in pitching
# To check our work let's use cross validation
# We will run a cv 10 times
crtl <- trainControl(method = 'repeatedcv',number = 10,repeats = 10)
modelfit <- train(WAR~.,method = "lm", data = training,trControl = crtl)
summary(modelfit)

# Let's check our model for fit and predictability
prediction <- predict(model2,newdata = testing)

# Check our R-squared values
cor(testing$WAR,predicted)^2

# Now we should plot our values and see how we did!
plot(testing$WAR,predicted)
# Looks like 4 variables matter most in this list
# IP, HR, BB, and SO
# Let's train a new model on these 4 items
model2 <- train(WAR~ IP + HR + BB + SO, method="lm",data = training)
summary(model2)

