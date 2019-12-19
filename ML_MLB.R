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
prediction <- predict(modelfit,newdata = testing)

# Check our R-squared values
cor(testing$WAR,prediction)^2

# Now we should plot our values and see how we did!
plot(testing$WAR,prediction)
# Looks like 4 variables matter most in this list
# IP, HR, BB, and SO
# Let's train a new model on these 4 items
model2 <- train(WAR~ IP + HR + BB + SO, method="lm",data = training)
summary(model2)

# Let's check our model for fit and predictability
prediction <- predict(model2,newdata = testing)
plot(testing$WAR,prediction)

# Not too bad for a start! Looks like we can calculate ~WAR using the
# following formula: WAR = 0.02*IP + (-0.11 * HR) + (-0.35 * BB) + 0.03 * SO

# So we have a basic WAR calculation for pitchers but what about batters?
# Can we use the Fangraphs leaderboard for batters to create something similar?

# First let's import what data we have
batters <- read_csv("~/Downloads/FanGraphs Leaderboard Batting.csv")

# Lot more columns to look at this time. Let's just keep the stats.
batCols <- c('G','PA','HR','R','RBI','SB','ISO','BABIP','AVG','OBP','SLG','wOBA','wRC+','BsR','Off','Def','WAR')

# Create our training and testing datasets
inTrain <- createDataPartition(batters$WAR,p=0.7,list = FALSE)
training <- batters[inTrain,batCols]
testing <- batters[-inTrain,batCols]

# Lets run a linear regression to find what stats are the most important 
# for WAR in batting
# To check our work let's use cross validation
# We will run a cv 10 times
crtl <- trainControl(method = 'repeatedcv',number = 10,repeats = 10)
modelfit <- train(WAR~.,method = "lm", data = training,trControl = crtl)
summary(modelfit)

# Let's check our model for fit and predictability
prediction <- predict(modelfit,newdata = testing)

# Check our R-squared values
cor(testing$WAR,prediction)^2

# Now we should plot our values and see how we did!
plot(testing$WAR,prediction)

# Intersting! Seems we have a nearly perfect model already. 
# Let's improve it by taking in our most important elements
model2 <- train(WAR~ OBP+wOBA+BsR+Off+Def, method="lm",data = training)
summary(model2)

# Let's check our model for fit and predictability
prediction <- predict(model2,newdata = testing)
plot(testing$WAR,prediction)

# Interesting, it seems that our limited model does not have as
# good a fit. Perhaps this warrants some future exploration
