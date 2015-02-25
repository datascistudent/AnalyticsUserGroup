# Simple Linear Regression using R

# Applying Linear Regression on Auto Insurance in Sweden data

# Start of code

# Read the data into R from your "working directory"
SwedenAutoIns <- read.csv("SwedenAutoInsurance.csv", header = T)

# Update the column names
colnames(SwedenAutoIns) <- c("NoofClaims", "TotalPayments")

# To see first few lines of the data
head(SwedenAutoIns,5)

# To visualize the data
plot(SwedenAutoIns$NoofClaims, SwedenAutoIns$TotalPayments, col = 4, xlab = "Number of Claims", ylab = "Total Payments (in SEK)", main = "Sweden Auto Insurance")

# To build a linear model
lmfit <- lm(TotalPayments ~ NoofClaims, data = SwedenAutoIns)

# To see the details from the Linear Model
summary(lmfit)

# To draw the regression line on the data plot
p <- qplot(NoofClaims, TotalPayments, data = SwedenAutoIns, xlab = "Number of Claims", ylab = "Total Payments (in thousands SEK)", main = "Sweden Auto Insurance")
p + geom_point(color="red") + geom_smooth(method=lm)


# To predict y value for new values of x
NoofClaims <- c(75, 150)
PredTotPay <- data.frame(NoofClaims)


# End of code
