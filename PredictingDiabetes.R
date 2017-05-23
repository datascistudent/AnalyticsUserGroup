#
#Pima Indians Diabetes Data
#http://analyticsusergroup.blogspot.com/2017/05/predicting-onset-of-diabetes.html
#

#Sourcing the data
pima <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.data"), header = FALSE, col.names = c('Pregnancies', 'Glucose', 'BP', 'SkinThickness', 'Insulin', 'BMI', 'DPF', 'Age', 'Outcome'))

#Glancing the data
head(pima)

#Summarizing the data
summary(pima)

#Understanding dsitribution of features
table(pima$Pregnancies)

#Making Outcome as a factor for ggplot to apply color
pima$Outcome <- as.factor(pima$Outcome)

#Histogram Plot for Pregnencies
ggplot(pima, aes(x=pima$Pregnancies, fill = pima$Outcome)) + geom_histogram(binwidth = 1) + ggtitle("Histogram - Pregnencies") + theme(plot.title = element_text(hjust = 0.5))

#Histogram Plot for Glucose
ggplot(pima, aes(x=pima$Glucose, fill = pima$Outcome)) + geom_histogram(binwidth = 10) + ggtitle("Histogram - Glucose") + theme(plot.title = element_text(hjust = 0.5))

#Histogram Plot for Blood Pressure
ggplot(pima, aes(x=pima$BP, fill = pima$Outcome)) + geom_histogram(binwidth = 10) + ggtitle("Histogram - Blood Pressure") + theme(plot.title = element_text(hjust = 0.5))

#Histogram Plot for SkinThickness
ggplot(pima, aes(x=pima$SkinThickness, fill = pima$Outcome)) + geom_histogram(binwidth = 10) + ggtitle("Histogram - Skin Thickness") + theme(plot.title = element_text(hjust = 0.5))

#Histogram Plot for Insulin
ggplot(pima, aes(x=pima$Insulin, fill = pima$Outcome)) + geom_histogram(binwidth = 100) + ggtitle("Histogram - Insulin") + theme(plot.title = element_text(hjust = 0.5))

#Histogram Plot for BMI
ggplot(pima, aes(x=pima$BMI, fill = pima$Outcome)) + geom_histogram(binwidth = 10) + ggtitle("Histogram - Body Mass Index") + theme(plot.title = element_text(hjust = 0.5))

#Histogram Plot for DPF
ggplot(pima, aes(x=pima$DPF, fill = pima$Outcome)) + geom_histogram(binwidth = 0.1) + ggtitle("Histogram - Diabetes Pedigree Function") + theme(plot.title = element_text(hjust = 0.5))

#Histogram Plot for Age
ggplot(pima, aes(x=pima$Age, fill = pima$Outcome)) + geom_histogram(binwidth = 10) + ggtitle("Histogram - Age") + theme(plot.title = element_text(hjust = 0.5))

#
#Data Preparation
#

#Tabulate 'Pregnancies'
table(pima$Pregnancies)

#Check the record with maximum 'Pregnancies' value
pima[pima$Pregnancies == max(pima$Pregnancies),]

#Create another data set for imputing missing values
pimaimpute <- pima

#Replace Zeroes with NA in pimaimpute data set for the *required columns*
is.na(pima[c("Glucose","BP", "SkinThickness", "Insulin", "BMI")]) <- !pima[c("Glucose","BP", "SkinThickness", "Insulin", "BMI")]

#Visualize Missing Values #VIM package required
summary(aggr(pimaimpute, plot=TRUE))

#Imputing missing values with mean values
for(i in 2:ncol(pimaimpute)-1){pimaimpute[is.na(pimaimpute[,i]), i] <- mean(pimaimpute[,i], na.rm = TRUE)}

#Attaching pimaimpute data frame
attach(pimaimpute)

#Fitting a Logistic Model
full_fit <- glm(Outcome~Pregnancies+Glucose+BP+SkinThickness+Insulin+BMI+DPF+Age, family = binomial)
summary(full_fit)

#Odds Ratio Calculation
exp(full_fit$coefficients)

#Predict the Outcome on the *same* data
predicted <- predict(full_fit, new_data=pimaimpute, type = "response") #predicts the probability
predicted_Outcome <- ifelse(predicted > 0.5,1,0) #convert it to an Outcome

#Tabluate the Results from Prediction
table(predicted_Outcome, pimaimpute$Outcome)

#Visualize the Confusion Matrix
ActualOutcome <- factor(c(0, 0, 1, 1))
PredictedOutcome <- factor(c(0, 1, 0, 1))
Y      <- c(442, 58, 113, 155)
df <- data.frame(ActualOutcome, PredictedOutcome, Y)

ggplot(data =  df, mapping = aes(x = ActualOutcome, y = PredictedOutcome)) +
    geom_tile(aes(fill = Y), colour = "white") +
    geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1, fontface = 'bold', color = 'white', size=10) +
    scale_fill_gradient(low = "#6D9EC1", high = "#ff8080") +
    theme_bw() + theme(legend.position = "none") + theme(axis.text = element_text(size=20)) + theme(axis.title = element_text(size=20,face="bold"))



#Trying out Classification Trees on PIMA dataset
install.packages("rpart")
install.packages("rpart.plot")
require(rpart)
require("rpart.plot")

#Building the rpart model
rpart.fit <- rpart(Outcome~.,data=pimaimpute)

#Plot the Decision Tree
prp(rpart.fit, faclen = 0, box.palette = "auto", branch.type = 5, cex = 1)

#Predicting with the model built
rpart.pred <- predict(rpart.fit, data=pimaimpute, type = "class")

#Building Confusion Matrix
confmat <- table(rpart.pred, pimaimpute$Outcome)
sum(diag(confmat)/sum(confmat))

#Visualize the Confusion Matrix - Decision Tree
ActualOutcomeDT <- factor(c(0, 0, 1, 1))
PredictedOutcomeDT <- factor(c(0, 1, 0, 1))
Y      <- c(449, 51, 72, 196)
df <- data.frame(ActualOutcomeDT, PredictedOutcomeDT, Y)

ggplot(data =  df, mapping = aes(x = ActualOutcomeDT, y = PredictedOutcomeDT)) +
  geom_tile(aes(fill = Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1, fontface = 'bold', color = 'white', size=10) +
  scale_fill_gradient(low = "#6D9EC1", high = "#ff8080") +
  theme_bw() + theme(legend.position = "none") + theme(axis.text = element_text(size=20)) + theme(axis.title = element_text(size=20,face="bold"))
