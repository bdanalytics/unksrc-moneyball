# VIDEO 2

# Read in data
baseball = read.csv("data/baseball.csv")
str(baseball)

# Subset to only include moneyball years
moneyball = subset(baseball, Year < 2002)
str(moneyball)

# Compute Run Difference
moneyball$RD = moneyball$RS - moneyball$RA
str(moneyball)

# Scatterplot to check for linear relationship
plot(moneyball$RD, moneyball$W)

# Regression model to predict wins
WinsReg = lm(W ~ RD, data=moneyball)
summary(WinsReg)

newDf = data.frame(list(RD = 713 - 614))
print(predict(WinsReg, newDf))

# VIDEO 3

str(moneyball)

# Regression model to predict runs scored
RunsReg = lm(RS ~ OBP + SLG + BA, data=moneyball)
summary(RunsReg)

RunsReg = lm(RS ~ OBP + SLG, data=moneyball)
summary(RunsReg)

print(newDf <- data.frame(list(OBP = 0.311, SLG = 0.405)))
print(predict(RunsReg, newDf))

RAReg = lm(RA ~ OOBP + OSLG, data=moneyball)
summary(RAReg)

print(newDf <- data.frame(list(OOBP = 0.297, OSLG = 0.370)))
print(predict(RAReg, newDf))

print(newDf <- data.frame(list(OBP = 0.311, SLG = 0.405)))
print(predict(RunsReg, newDf))