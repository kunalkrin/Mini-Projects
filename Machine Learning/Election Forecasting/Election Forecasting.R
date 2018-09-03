polling = read_csv("PollingData.csv")
str(polling)
table(polling$Year)
summary(polling)

simple = polling [c("Rasmussen", "SurveyUSA", "PropR", "DiffCount")]
summary(simple)

set.seed(144)
imputed = complete(mice(simple))
summary(imputed)

polling$Rasmussen = imputed$Rasmussen
polling$SurveyUSA = imputed$SurveyUSA
summary(polling)


Train = subset(polling, Year == 2004 | Year == 2008)
Test = subset(polling, Year == 2012)
table(Train$Republican)


table(sign(Train$Rasmussen))

table(Train$Republican, sign(Train$Rasmussen))
cor(Train[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount", "Republican")])

mod1 = glm (Republican ~ PropR, data= Train, family="binomial")
summary(mod1)

pred1 = predict(mod1, type="response")
table(Train$Republican, pred1 >.5)

mod2 = glm (Republican ~ SurveyUSA + DiffCount, data= Train, family="binomial")
summary(mod2)

pred2 = predict(mod2, type="response")
table(Train$Republican, pred2 >.5)

table(Test$Republican, sign(Test$Rasmussen))
TestPrediction = predict (mod2, newdata = Test, type="response")
table(Test$Republican, TestPrediction>=.5)

subset(Test, TestPrediction>=.5 & Republican == 0)
