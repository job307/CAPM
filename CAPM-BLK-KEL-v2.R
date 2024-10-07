rm(list = ls())

library(lubridate)
library(moments)
library(readxl)
library(sandwich)
library(lmtest)
library(dplyr)
library(ggplot2)

# Set Workspace
setwd("/Users/job307/Documents/Jonas/Programme/R/CAPM")

# Read Data-Files
SP500 <- read_xlsx("SP500.xlsx")
FFFactors <- read.csv2("FFFactors.csv", skip = 4, header = T, sep = ",")

# Format FFFactors 
colnames(FFFactors) = c("Date", "Mkt_RF", "SMB", "HML", "RF")
FFFactors <- FFFactors %>% 
  mutate(Date = ymd(Date)) %>%
  mutate(Mkt_RF = as.numeric(Mkt_RF)) %>% 
  mutate(SMB = as.numeric(SMB)) %>% 
  mutate(HML = as.numeric(HML)) %>% 
  mutate(RF = as.numeric(RF))

# Format SP500 
SP500 <- SP500[c("Name", "BLACKROCK", "KELLANOVA")]
colnames(SP500) = c("Date", "BLK", "KEL")
SP500 <- SP500 %>% mutate(Date = ymd(Date))

# Calc percentage change of net returns
# Dividends out of scope by definition. Total returns would include dividends
returns <- SP500 %>%
  arrange(Date, .by_group = TRUE) %>%
  mutate(BLK_pct_change = (BLK/lag(BLK) - 1) * 100) %>%
  mutate(KEL_pct_change = (KEL/lag(KEL) - 1) * 100) %>%
  mutate(PF_pct_change = (BLK_pct_change + KEL_pct_change)/2)
  
# Replace NA for percentage change in first line
returns[c("BLK_pct_change","KEL_pct_change","PF_pct_change")][1,] <- 0

# Merge FFFactors in returns (left join)
CAPMdat <- merge(x = returns, y = FFFactors, by = "Date", all.x = TRUE)

# Fill Sundays where values are NA
CAPMdat <- CAPMdat %>% 
  mutate(Mkt_RF = ifelse(is.na(Mkt_RF), 0, Mkt_RF)) %>%
  mutate(SMB = ifelse(is.na(SMB), 0, SMB)) %>% 
  mutate(HML = ifelse(is.na(HML), 0, HML)) %>% 
  mutate(RF = ifelse(is.na(RF), 0, RF))

summary(CAPMdat)

# Calc excess return
exret <- CAPMdat %>%
  mutate(BLK_excess_return = BLK_pct_change - RF) %>%
  mutate(KEL_excess_return = KEL_pct_change - RF) %>%
  mutate(PF_excess_return = PF_pct_change - RF) %>%
  select(Date, Mkt_RF, SMB, HML, BLK_excess_return, KEL_excess_return, PF_excess_return)

# Calc linear models
attach(exret)
BLK_model <- lm(BLK_excess_return ~ Mkt_RF)
KEL_model <- lm(KEL_excess_return ~ Mkt_RF)
PF_model <- lm(PF_excess_return ~ Mkt_RF)

summary(BLK_model)
summary(KEL_model)
summary(PF_model)

# # heteroskedasticity and autocorrelation consistent
# BLK_HAC_report <- coeftest(BLK_model, vcov = vcovHAC(BLK_model, lag = 1))
# KEL_HAC_report <- coeftest(KEL_model, vcov = vcovHAC(KEL_model, lag = 1))
# PF_HAC_report <- coeftest(PF_model, vcov = vcovHAC(PF_model, lag = 1))
# summary(BLK_HAC_report)
# summary(KEL_HAC_report)
# summary(PF_HAC_report)

result_CAPM <- data.frame(Factors=c('Beta_Mkt'),
  Blackrock=c(summary(BLK_model)$coefficients[2, 1]),
  Kellanova=c(summary(KEL_model)$coefficients[2, 1]),
  Portfolio=c(summary(PF_model)$coefficients[2, 1])
)

# Validation Beta
#     yahoo 5Y-M    R 9Y-D
# BLK      1.38       1.21
# KEL      0.40       0.33
# Yahoo calculated with adjusted returns incl. dividends, not included in DALAHO files
# different valuation period 5Y vs. 9Y
# monthly basis vs. daily basis
# the Yahoo Beta is from May, DALAHO SP500 ends in March
# risk free rate of Yahoo and Fama-French website might differ (usually taken from selected bonds with AAA rating)
# p-value is < 2.2e-16, below the significance level of 5%, so the results are reliable

BLK_model <- lm(BLK_excess_return ~ Mkt_RF + SMB + HML)
KEL_model <- lm(KEL_excess_return ~ Mkt_RF + SMB + HML)
PF_model <- lm(PF_excess_return ~ Mkt_RF + SMB + HML)

summary(BLK_model)
summary(KEL_model)
summary(PF_model)

result_Fama_French <- data.frame(Factors=c('Beta_Mkt','Beta_SMB','Beta_HML'),
  Blackrock=c(summary(BLK_model)$coefficients[2, 1],summary(BLK_model)$coefficients[3, 1],summary(BLK_model)$coefficients[4, 1]),
  Kellanova=c(summary(KEL_model)$coefficients[2, 1],summary(KEL_model)$coefficients[3, 1],summary(KEL_model)$coefficients[4, 1]),
  Portfolio=c(summary(PF_model)$coefficients[2, 1],summary(PF_model)$coefficients[3, 1],summary(PF_model)$coefficients[4, 1])
)

result <- NULL
result[[1]] <-result_CAPM
result[[2]] <-result_Fama_French
names(result) <- c("result_CAPM","result_Fama_French")

str(result$result_CAPM)
str(result$result_Fama_French$Blackrock[1])

# Scatter Plots
plot(Mkt_RF, BLK_excess_return, main="Scatter Plot BlackRock",
     xlab="BlackRock daily log-returns in %",
     ylab="Mkt_RF daily log-returns in %")
abline(reg=BLK_model, col="red", lwd=2)

plot(Mkt_RF, KEL_excess_return, main="Scatter Plot Kellanova",
     xlab="Kellanova daily log-returns in %",
     ylab="Mkt_RF daily log-returns in %")
abline(reg=KEL_model, col="red", lwd=2)

plot(Mkt_RF, PF_excess_return, main="Scatter Plot Portfolio",
     xlab="Portfolio daily log-returns in %",
     ylab="Mkt_RF daily log-returns in %")
abline(reg=PF_model, col="red", lwd=2)

# Plot Time Series
plot(CAPMdat[,2], col="white", main="Time Series Plot",
     xlab="Days (from Jan 2000 to Jan 2020)", ylab="BLK red; KEL*6 black; PF/120 blue")
lines(CAPMdat[,2], col="red")
lines(CAPMdat[,3]*6, col="black")
# summary(KEL_model)

detach(exret)


