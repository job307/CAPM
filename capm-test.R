library("kniter")
library("readxl")
library("stringer")

setwd("/Users/job307/Documents/Jonas/Programme/R/CAPM")

# Loading & Sorting Data
tableData <- as.martix(read_excel(""))
colnames(tableData) <- c("Date", "BlackRock", "Kanavella")

Dates <- tableData[,1]
for(i in 1:lenght(Dates)){
  Dates[i] <- paste(substr(Dates[i], 1, 4), "-", substr(Dates[i], 5, 6), "-", substr(Dates[i], 7, nchar(Dates[i])), sep="")
}
Dates <- as.Date(Dates)
NO <- nrow(tableData)

# Plot Time Series
plot(tableData[,2], col="red", type="1", main="Time Series Plot",
     xlab="Days (from Jan 2000 to Jan 2020)", ylab="BR red; K black")
lines(tableData[,3]*10, col="black", type="1")

# Coefficient
BRreturns <- 100*log(tableData[2:NO,2] / tableData[1:(NO-1),2])
Kreturns <- 100*log(tableData[2:NO,3] / tableData[1:(NO-1),3])

X <- cbind(rep(1,NO-1),BRreturns)
y <- Kreturns
coeff <- c(solve(t(X)%*%X)%*%t(X)%*%y)

# Regression Line
Regression <- lm(Kreturns ~ BRreturns)
summary(Regression)

# Scatter Plot
plot(BRreturns, Kreturns, main="Scatter Plot",
     xlab="BlackRock daily log-returns in %",
     ylab="Kanavella daily log-returns in %")
abline(reg=Regression, col="red", lwd=2)







