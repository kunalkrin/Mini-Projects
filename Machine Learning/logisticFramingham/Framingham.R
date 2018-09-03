framingham <- read_csv("framingham.csv")
str(framingham)

set.seed(1000)
split = sample.split(framingham$TenYearCHD, SplitRatio = .65)
train = subset(framingham, split == T)
test = subset(framingham, split == F)

framinghamLog = glm (TenYearCHD ~ ., data = train, family = binomial)
summary(framinghamLog)

predictTest = predict (framinghamLog, type = "response", newdata = test)
table(test$TenYearCHD, predictTest>.5)

ROCRpred = prediction (predictTest, test$TenYearCHD)
as.numeric(performance(ROCRpred, "auc")@y.values)
