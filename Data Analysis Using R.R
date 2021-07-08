#--- R as calculator --- 
########################
(10+8)/4 #simple calculations using R
4*5^2    #multiplication and power
7 / 2 #division
7 %/% 2 #integer division
7 %% 2 #remainder of integer division
sqrt(4) # function of square root

x = sqrt(4 * max(-3, 9,0.8)) #saving a result into a variable
x
flag <- TRUE #boolean variable
flag

# Identifying the Data types of a variable
class(flag)
class(x)

# Vectors
#########
weight <- c(4.4,5.3,7.2,5.2,8.5,7.3,6.0,10.4,10.2,6.1) # c() - combines the arguments into a vector or list
weight
a <- c(1, 2, 5, 3, 6, -2, 4)
b <- c("one", "two", "three")
a[3]
a[c(1, 3, 5)]
a[2:6]

# Data frames
#############
'A data frame is more general than a matrix in that different columns can contain different
modes of data (numeric, character, and so on).'
# mydata <- data.frame(col1, col2, col3,...)  # ceated with the data.frame() function
'where col1, col2, col3, and so on are column vectors of any type (such as character,
numeric, or logical). Names for each column can be provided with the names function.'

# Example: Creating a data frame
patientID <- c(1, 2, 3, 4)
age <- c(25, 34, 28, 52)
diabetes <- c("Type1", "Type2", "Type1", "Type1")
status <- c("Poor", "Improved", "Excellent", "Poor")
patientdata <- data.frame(patientID, age, diabetes, status)
patientdata
str(patientdata)

class(patientdata)

# Access the subset of the data
patientdata[1:2]
patientdata[1,]
patientdata[c("diabetes", "status")]
patientdata$age


# Statistical Inference
'----------------------'

'One-Sample t test'
'-----------------'
# library(MASS)
# Example # 1 (One Tailed Test) - Carbonated Beverage data set
day <- c(108,124,124,106,115,138,163,159,134,139)

summary(day)
mean(day)
sd(day)

par(mfrow=c(2,2))
plot(day)
hist(day, main= "Histogram of Days")
qqnorm(day, main= "QQ-plot for Days")
qqline(day)
boxplot(day)

# Check the Normality of the data (Is data follow the normal distribution?)

# Anderson Darling normailty Test
library(nortest)
ad.test(day)
# Shapiro-Wilk normality test
shapiro.test(day) 

# One Sample t test
t.test(day, mu = 120, alternative = "greater")


'Two-sample Independent T-test'
'-----------------------------'
HospitalComparison <- read.csv(file.choose(),header=TRUE)
View(HospitalComparison)

Hospital.A <- HospitalComparison[HospitalComparison$Hospital=="A",]
Hospital.B <- HospitalComparison[HospitalComparison$Hospital=="B",]
par(mfrow=c(1,1))
boxplot(Hospital.A$Rating,Hospital.B$Rating, main='Hospital Ratings', 
        ylab='Ratings', names = c('Hospital-A','Hospital-B'))

# Check the Normailty of data 
par(mfrow=c(2,2))
plot(HospitalComparison$Rating)
hist(HospitalComparison$Rating, main= "Histogram of Rating")
qqnorm(HospitalComparison$Rating, main= "QQ-plot for Rating")
qqline(HospitalComparison$Rating)
boxplot(HospitalComparison$Rating)

# Shapiro-Wilk normality test
shapiro.test(HospitalComparison$Rating)

# Test the homogeneity of variances
var.test(HospitalComparison$Rating ~ HospitalComparison$Hospital)

# Two-sample Independent T-test with equal variance
t.test(HospitalComparison$Rating ~ HospitalComparison$Hospital, var.equal=T)


# Two-sample Independent T-test with unequal variance (Welch approx.)
t.test(HospitalComparison$Rating ~ HospitalComparison$Hospital)


'Paired t Test'
'-------------'
# attach the data set
RestingHeartRate <- read.csv(file.choose(),header=TRUE)
View(RestingHeartRate)
t.test(RestingHeartRate$Difference)

'One Way ANOVA'
'-------------'
SupplierQuality <- read.csv(file.choose(),header=TRUE)
View(SupplierQuality)

'Assumption of One-Way ANOVA'
# Test for Normality
shapiro.test(SupplierQuality$QualityRating)

# Test for Homogeneity 
bartlett.test(SupplierQuality$QualityRating ~ SupplierQuality$Supplier)

# Box Plot
par(mfrow=c(1,1))
boxplot(SupplierQuality$QualityRating ~ SupplierQuality$Supplier, col = rainbow(3))

# Estimation of One-Way ANOVA Model
model1 <-aov(SupplierQuality$QualityRating ~ SupplierQuality$Supplier)
summary(model1)

# Model Adequacy Checking
par(mfrow=c(2,2))
plot(model1)

# Multiple Comparisons: Post-Hoc Test
'Tukey HSD Test'
TukeyHSD(model1, conf.level = 0.95)

par(mfrow=c(1,1))
plot(TukeyHSD(model1, conf.level = 0.95),col="red")


'Correlation'
Salary <- read.csv(file.choose(),header=TRUE)
View(Salary)
cor(Salary$Salary,Salary$YearsExperience)

cor.test(Salary$Salary,Salary$YearsExperience)

'Simple Linear Regression Analysis'
Salary.lm <- lm(Salary$Salary ~ Salary$YearsExperience)
summary(Salary.lm)

'Multiple Linear Regression'
Year <- c(2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2016,2016,2016,2016,2016,2016,2016,2016,2016,2016,2016,2016)
Month <- c(12, 11,10,9,8,7,6,5,4,3,2,1,12,11,10,9,8,7,6,5,4,3,2,1)
Interest_Rate <- c(2.75,2.5,2.5,2.5,2.5,2.5,2.5,2.25,2.25,2.25,2,2,2,1.75,1.75,1.75,1.75,1.75,1.75,1.75,1.75,1.75,1.75,1.75)
Unemployment_Rate <- c(5.3,5.3,5.3,5.3,5.4,5.6,5.5,5.5,5.5,5.6,5.7,5.9,6,5.9,5.8,6.1,6.2,6.1,6.1,6.1,5.9,6.2,6.2,6.1)
Stock_Index_Price <- c(1464,1394,1357,1293,1256,1254,1234,1195,1159,1167,1130,1075,1047,965,943,958,971,949,884,866,876,822,704,719)        

model <- lm(Stock_Index_Price ~ Interest_Rate + Unemployment_Rate)
summary(model)

predict(model, interval='confidence')




