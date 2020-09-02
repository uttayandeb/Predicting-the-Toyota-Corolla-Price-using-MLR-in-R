################ Required Packages ##########

library(e1071)
install.packages("car")
library(car)
install.packages("corpcor")
library(corpcor)


#####**** Reading and understanding The Data ###########

Toyota_Corolla<-read.csv(file.choose())
View(Toyota_Corolla)
names(Toyota_Corolla)

### Now we have to build the prediction model only for the following input Features ####

Corolla<-Toyota_Corolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
#now we have 9 i/p features


View(Corolla)
nrow(Corolla)
ncol(Corolla)

str(Corolla)
attach(Corolla)

summary(Corolla)

######## Standard deviation #########

sd(Price)### gives the standard deviation
#[1] 3626.965

sd(Age_08_04)
#[1] 18.59999

sd(KM)
#[1] 37506.45

sd(HP)#[1] 14.98108

sd(cc)#[1] 424.3868

sd(Doors)#[1] 0.9526766

sd(Gears)#[1] 0.1885104

sd(Quarterly_Tax)#[1] 41.12861

sd(Weight)#[1] 52.64112

###### varience  ###########

var(Price)#[1] 13154872

var(Age_08_04)#[1] 345.9596

var(KM)#[1] 1406733707

var(HP)#[1] 224.4327

var(cc)#[1] 180104.1

var(Doors)#[1] 0.9075927

var(Gears)#[1] 0.03553619

var(Quarterly_Tax)#[1] 1691.563

var(Weight)#[1] 2771.088

####### skewness #########

skewness(Price)
## [1] 1.700327

skewness(Age_08_04)
## [1] -0.8249756

skewness(KM)
# [1] 1.013791

skewness(HP)
# [1] 0.9538397

skewness(cc)
# [1] 27.37451

skewness(Doors)
# [1] -0.07623547

skewness(Gears)
#[1] 2.27919

skewness(Quarterly_Tax)
# [1] 1.98967

skewness(Weight)
#[1] 3.102148


#########  Kurtosis ##########

kurtosis(Price)
# [1] 3.711247

kurtosis(Age_08_04)
#[1] -0.08460596

kurtosis(KM)
#[1] 1.668511

kurtosis(HP)
# [1] 8.78509

kurtosis(cc)
#[1] 926.1741

kurtosis(Doors)
# [1] -1.873989

kurtosis(Gears)
# [1] 37.51167

kurtosis(Quarterly_Tax)
#[1] 4.269083

kurtosis(Weight)
# [1] 19.26034


###### Exploratory Data analysis #####
plot(Age_08_04, Price)
#### from teh graph it can b seen that the lesser the cars age higher is its price

plot(KM, Price)
## teh more kilometers the car have accumulated the cheaper the price of the car

plot(HP, Price)
# more is the Horse power of the car more expensive it is

plot(cc,Price)
## the size of cc doesnt have that much effect on the price of the car

plot(Doors,Price)


plot(Gears, Price)

plot(Quarterly_Tax,Price)

plot(Weight,Price)## the graph consists of some outliers though heigher teh weight more is the price

### no. of cars of fule type
summary(Toyota_Corolla$Fuel_Type)

summary(Toyota_Corolla$Color)

summary(Toyota_Corolla$Model)

###to find correlation between input and output ####
pairs(Corolla)


# Correlation Coefficient matrix  Strength & Direction of Correlation
cor(Corolla)

##Pure Correlation  b/n the varibles

cor2pcor(cor(Corolla))


## Building linear model or linear regression model
model <- lm(Price ~ ., data = Corolla)
summary(model)



# cc and Doors are influence to each other so  predicting the model based on individual records
model.carcc <- lm(Price ~ cc)
summary(model.carcc) 


model.cardoor <- lm(Price ~ Doors)
summary(model.cardoor) 


# Build model with cc and Doors
model.car <- lm(Price ~ cc + Doors)
summary(model.car) 



###### Finding out the influencial record  #####
influence.measures(model.car)





###################### plotting influential measures############

influenceIndexPlot(model.car)


influencePlot(model.car)


# Delete influentails records and build the model
model1 <- lm(Price ~ ., data = Corolla[-c(81),])
summary(model1)

# Applying VIF function on model built on all inputs
## Variance Inflation factor to check collinearity b/n variables 
vif(model1)#Variance Inflation Factors
?vif

avPlots(model1)

finalmodel <- lm(Price ~ Age_08_04 + KM + HP + cc + Gears + Quarterly_Tax + Weight, data = Corolla[-c(81),])
summary(finalmodel)


## Evaluate model LINE assumptions 
plot(finalmodel)


#Residual plots,QQplot,std-Residuals Vs Fitted,Cook's Distance 
qqPlot(model)
