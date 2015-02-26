# Multiple Linear Regression using R

# Applying Linear Regression on Cigarette data

# Start of code

# Installing the packages required for plots
install.packages("ggplot2")
install.packages("car")

# Adding the installed packages
require("ggplot2")
require("car")

# Read the data into R from your "working directory"
cigarettes <- read.table("cigarettes.txt", header = F, sep = " ")

# Update the column names
colnames(cigarettes) <- c("Brand", "Tar", "Nicotine", "Weight", "CO")

# To see first few lines of the data
head(cigarettes)

# To visualize the data

# Since there are three independent variables, we create three 
# seperate plots and stich them in the same output

p1 <- qplot(Tar, CO, data = cigarettes) + labs(title = "CO vs Tar")

p2 <- qplot(Nicotine, CO, data = cigarettes) + labs(title = "CO vs Nicotine")

p3 <- qplot(Weight, CO, data = cigarettes) + labs(title = "CO vs Weight")

# The following command takes care of showing three plots 
# in one output screen
grid.arrange(p1, p2, p3, ncol = 3, main = "Cigarettes")

# To build a linear model
mlfit <- lm(CO ~ Tar+Nicotine+Weight, data = cigarettes)

# To see the details from the Linear Model
summary(mlfit)

# Building the prediction dataframe
PredCO <- data.frame(Tar=12.22, Nicotine=0.87, Weight=0.97)

# To predict y value for new values of x1, x2, x3
predict(mlfit, PredCO)

# Visualise the model
leveragePlots(mlfit, main = "Cigarette - After building Linear Regression Model")


# End of code
