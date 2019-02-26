######SIMPLE LINEAR REGRESSION#####

#1.Problem-Statement: Calories_consumed-> predict weight gained using calories consumed

# Choose the calories_consumed data set
cc<- read.csv(file.choose()) 
View(cc)
attach(cc)

##summarize the data
summary(cc)
#Weight.gained.grams: Mean= 357.7, Median= 200.0; As Mean>Median,it is skewed to the left
#Calories.Consumed: Mean= 2341, Median= 2250; As Mean>Median,it is skewed to the left

library(DataExplorer)
plot_str(cc)
str(cc)

plot_missing(cc)

##Data Visualization- Histogram
plot_histogram(Weight.gained..grams.)
plot_histogram(Calories.Consumed)

##Data Visualization- Plot Density
plot_density(Weight.gained..grams.)
plot_density(Calories.Consumed)

plot(Weight.gained..grams.,Calories.Consumed)

# Correlation coefficient value 
cor(Weight.gained..grams.,Calories.Consumed) #Strong correlation

#EDA
m0<-lm(Weight.gained..grams.~Calories.Consumed)
summary(m0) 
#High R^2 value, low p-value-> model significant
#Multiple R-squared= 0.8968,Adjusted R-squared= 0.8882; Hence, strong correlation 

#Confidence Interval
confint(m0,level = 0.95)
predict(m0,interval="predict")
m0$residuals

#RMSE
sqrt(mean(m0$residuals^2))



#2. Problem-Statement: Delivery_time -> Predict delivery time using sorting time 

#Choose delivery time dataset
dt<- read.csv(file.choose()) 
View(dt)
attach(dt)

#Summarize the data
summary(dt)
#Delivery.Time: Mean=16.79, Median=17.83; As Mean<Median,it is skewed to the right
#Sorting.Time: Mean=43.30; Median=43.00; As Mean>Median,it is skewed to the left

library(DataExplorer)
plot_str(dt)
str(dt)

plot_missing(dt)

##Data Visualization- Histogram
plot_histogram(Delivery.Time)
plot_histogram(Sorting.Time)

##Data Visualization- Plot Density
plot_density(Delivery.Time)
plot_density(Sorting.Time)


plot(Sorting.Time,Delivery.Time)

# Correlation coefficient value
cor(Sorting.Time, Delivery.Time) #Poor Correlation between sorting time and delivery time

#EDA
m1<- lm(Delivery.Time~Sorting.Time)
summary(m1)
#Moderate Correlation; but p-value<0.05-> model significant
#Multiple R-squared=0.6823,	Adjusted R-squared=0.6655; Hence, moderate correlation

#Confidence Interval
confint(m1,level = 0.95)
predict(m1,interval="predict")
m1$residuals

#RMSE
sqrt(mean(m1$residuals^2))



#3. Problem-Statement: Emp_data -> Build a prediction model for Churn_out_rate 

#Choose employee dataset
emp<- read.csv(file.choose()) 
View(emp)
attach(emp)

##summarize the data
summary(emp)
#Salary_hike: Mean= 1689, Median= 1675; As Mean>Median,it is skewed to the left
#Churn_out_rate: Mean= 72.90, Median= 71.00; As Mean>Median, it is skewed to the left

library(DataExplorer)
plot_str(emp)
str(emp)

plot_missing(emp)

##Data Visualization- Histogram
plot_histogram(Salary_hike)
plot_histogram(Churn_out_rate)

##Data Visualization- Plot Density
plot_density(Salary_hike)
plot_density(Churn_out_rate)

plot(Salary_hike~Churn_out_rate)

#Correlation coefficient time
cor(Salary_hike, Churn_out_rate) #good relationship between salary hike and churn out rate

#EDA
m2<- lm(Salary_hike~Churn_out_rate)
summary(m2) 
#Good Correlation; p-value<0.05-> model significant
#Multiple R-squared= 0.8312,	Adjusted R-squared= 0.8101; Hence, Strong correlation.

#Confidence Interval
confint(m2,level = 0.95)
predict(m2,interval="predict")
m2$residuals

#RMSE
sqrt(mean(m2$residuals^2))



#4. Problem-Statement:Build a prediction model for Salary_hike

#Choose Salary hike dataset
slr<- read.csv(file.choose()) 
View(slr)
attach(slr)

##summarize the data
summary(slr)
#YearsExperience: Mean= 5.313, Median= 4.700; As Mean>Median,it is skewed to the left
#Salary: Mean= 76003, Median= 65237; As Median<Mean, it is skewed to the left

library(DataExplorer)
plot_str(slr)
str(slr)

plot_missing(slr)

##Data Visualization- Histogram
plot_histogram(YearsExperience)
plot_histogram(Salary)

##Data Visualization- Plot Density
plot_density(YearsExperience)
plot_density(Salary)

plot(YearsExperience~Salary)

#Correlation coefficient time
cor(YearsExperience, Salary) #good relationship between Years of experience and salary

#EDA
m3<- lm(YearsExperience~Salary)
summary(m3)
#Strong Correlation; p-value<0.05-> model significant
#Multiple R-squared= 0.957,	Adjusted R-squared= 0.9554; Hence, Strong correlation

#Confidence Interval
confint(m3,level = 0.95)
predict(m3,interval="predict")
m3$residuals

#RMSE
sqrt(mean(m3$residuals^2))

