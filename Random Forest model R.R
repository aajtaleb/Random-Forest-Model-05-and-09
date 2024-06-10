## Random Forest Model 2005 and 2009
## RF model binary2005 
data= read.csv("C:/Users/aajta/OneDrive/Desktop/Data0509/Book2005.csv")
str(data)
rows <-nrow(data)
colums <-ncol(data)
str(rows)
str(colums)
head(data[,1:10])
pacman::p_load(raster, shapefiles, sp, rgdal, randomForest, caret, e1071, 
               MASS, ROCR, corrplot, rfUtilities, VSURF, rmarkdown)
library(plyr)
count(data ["PresenceAb"])
set.seed(123)
#create new data frame with only the raster
rastercolumns<- data[, 11:24]
print(rastercolumns)

##('NBR' , 'B54' ,' AnomB54 ' ,' band1' , ' TCG' ,' DEM', 'TCW')
rastercolumns1 <- data[,c(14,18,19,20,22,23,24)]
print(rastercolumns1)
# create a list of names
Predictors1<- c(' band4' , 'NBR',  'TCG' , 'DEM' ,'AnomB54 ' , 'DiffB54 ')
# define the response variable
response_var1<- as.factor(data$PresenceAb)
predictor_var1<- rastercolumns1
## combine the two to a single data frame
dataset1<-cbind(response_var1,predictor_var1)
print(dataset1)

rf_model05= randomForest(x= predictor_var1,y= response_var1,data =dataset1, importance = TRUE)
rf_model05
## the accuracy 
predicted1 <- predict(rf_model05)
observed1 <- data$PresenceAb
accuracy(x=predicted1, y = observed1)
###
set.seed(123)
ind <- sample(2, nrow(dataset1), replace = TRUE, prob = c(0.7, 0.3)) 
train <- dataset1[ind==1,]
print(train)
test <- dataset1[ind==2,]
rf_model2 <- randomForest(response_var1~., data= train,
                          ntree = 500,
                          mtry = 4,
                          importance = TRUE,
                          proximity = TRUE)
print(rf_model2)
attributes(rf_model2)
#prediction p1 library caret
library(caret)
p1 <- predict(rf_model2, train)
confusionMatrix(p1, train$response_var1)

# prediction & confusion matrix with test data
p2 <- predict(rf_model2, test)
confusionMatrix(p2,test$response_var1)
accuracy(x=p2,y=test$response_var1)
# Error rate of Random forest 
plot(rf_model2)
# tune mtry
t <- tuneRF(train[,-7], train[,7],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 500,
            trace = TRUE,
            improve = 0.05)
#####
library(raster)
library(sp)
library(rgdal)

NBR== raster("C:/Users/aajta/OneDrive/Desktop/Data0509/rasterData05/NBR05.tif")
print (NBR)
B54 <- raster ("C:/Users/aajta/OneDrive/Desktop/Data0509/rasterData05/B54clip.tif")
print (B54)
NDVI <- raster("C:/Users/aajta/OneDrive/Desktop/Data0509/rasterData05/NDVIclip.tif")
print(NDVI)
band1 <- raster ("C:/Users/aajta/OneDrive/Desktop/Data0509/rasterData05/band1.tif")
print (band1)
AnomNBR <- raster("C:/Users/aajta/OneDrive/Desktop/Data0509/rasterData05/AnomNBR.tif")
print(AnomNBR)
DEM = raster("C:/Users/aajta/OneDrive/Desktop/Data0509/rasterdata/DEMclip.tif")
print (DEM)
TCG <- raster("C:/Users/aajta/OneDrive/Desktop/Data0509/rasterData05/TCG.tif")
print (TCG)
band4 <- raster("C:/Users/aajta/OneDrive/Desktop/Data0509/rasterData05/b4_2005_2.tif")
print (band4)
DiffB54 <- raster("C:/Users/aajta/OneDrive/Desktop/Data0509/rasterData05/diffB54CILP.tif")
print(DiffB54)
MeanB54<- raster("C:/Users/aajta/OneDrive/Desktop/Data0509/rasterdata/MeanB54.tif")
print(MeanB54)
DEM <- raster ("C:/Users/aajta/OneDrive/Desktop/Data0509/rasterdata/DEMclip.tif")
print(DEM)
AnomB54 <- raster("C:/Users/aajta/OneDrive/Desktop/Data0509/rasterData05/AnomB54_05.tif")
print (AnomB54)
## The binary model
stack_5band7<- stack(NBR,B54,AnomB54,band4,TCG,DiffB54,DEM)
names(stack_5band7) =c('NBR' , 'B54' ,' AnomB54 ' ,' band4' , ' TCG' ,' DiffB54', 'DEM' )

predict(stack_5band7 ,rf_model2,filename="NEWFinalBINARY2005.tif",
        fun=predict,
        # format="Gtiff",
        # datatype="FLT4S",
        overwrite=TRUE)
### convert to raster
library(rgdal)
library(raster)
binary <- raster("NEWFinalBINARY2005.tif")
## Plot the results
par(mfrow=c(1,2), mar=c(3,3,3,5))
plot(binary, main= "RFPresence and Absence tree mortality in 2005", xaxt= 'n', yaxt='n')

#### RF for 2009 regression and Binary model 
data= read.csv("C:/Users/aajta/OneDrive/Desktop/Data0509/Book2009.csv")
str(data)
rows <-nrow(data)
colums <-ncol(data)
str(rows)
str(colums)
head(data[,1:10])
pacman::p_load(raster, shapefiles, sp, rgdal, randomForest, caret, e1071, 
               MASS, ROCR, corrplot, rfUtilities, VSURF, rmarkdown)
library(plyr)
count(data ["PresenceAb"])
count(data["Percent_Cov"])
################# Regression model percent % tree mortality
#create new data frame with only the raster
rastercolumns<- data[, 11:24]
# define the response variable
response_var <- data$Percent_Cov
print(response_var)
# define the predicted var
rastercolumns_cov2 <-  data[,c(15,18,19,20,21,22)]
Predictors1<- c(' NBR  ' , 'B54',  'TCG' , 'MeanB54' ,'band5 ' , 'DEM ')
predictor_var<- rastercolumns_cov2
print(predictor_var)
dataset09 <-cbind(response_var, predictor_var)
print (dataset09)
##Random forest model (REGRESSION)
rf_model09= randomForest(response_var~ . , data =dataset09, importance = TRUE)
# print rfmodel1
rf_model09
par(mfrow=c(1,1))
# create vector of predicted values of MPB cover 
# actual obsevation 
predicted1<- rf_model09$predicted
# create a vector values of observed 
observed1<- data$Percent_Cov
R2(predicted1, observed1) # r_squared
# plot observed value on X-axis and predicted on y-axis
plot(observed, predicted, ylab = "Predicted", xlab = "observed", main = " RF2009 predicted vs. observed", pch = 20)
abline(fit <- lm(predicted ~ observed, col ='red'))
# we can add a legend that calculates an R2 value (ranges from 0 to 1, the
# closer to 1 the better)
legend("topright", bty="n", legend=paste("R2 is", format(summary(fit)$adj.r.squared, digits=4)))
# data partition
set.seed(123)
ind <- sample(2, nrow(dataset09), replace = TRUE, prob = c(0.7, 0.3)) 
train <- dataset09[ind==1,]
test <- dataset09[ind==2,]
rf_model09_2 <- randomForest(response_var~., data= train,
                             ntree = 500,
                             mtry = 4,
                             importance = TRUE,
                             proximity = TRUE)
print(rf_model09_2)
attributes(rf_model09_2)
#prediction p1 library caret
library(caret)
p1 <- predict(rf_model09_2, train)

confusionMatrix(p1, train$response_var)
# prediction & confusion matrix with test data
p2 <- predict(rf_model09_2, test)
confusionMatrix(p2,test$response_var)
R2(p2, test$response_var)
# Error rate of Random forest 
plot(rf_model09_2)
# tune mtry
t <- tuneRF(train[,-5], train[,5],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 500,
            trace = TRUE,
            improve = 0.05)
#### Binary model presence and absence dead trees
#####Binary model 
library(plyr)
count(data ["PresenceAb"])
count(data["Percent_Cov"])
#Predictors1<- c('' band4' , 'NBR',  'TCG' , 'DEM' ,'AnomB54 ' , 'DiffB54 ')
rastercolumns_cov21<-  data[,c(14,18,19,20,22,23,24)]
# define the response variable
response_var1<- as.factor(data$PresenceAb)
predictor_var1<- rastercolumns_cov21
print(predictor_var1)
## combine the two to a single data frame
dataset09b<-cbind(response_var1,predictor_var1)
print(dataset09b)
##### model Binary
rf_model09b= randomForest(x= predictor_var1,y= response_var1,data =dataset09b, importance = TRUE)
rf_model09b
## the accuracy 
predicted1 <- predict(rf_model09b)
observed1 <- data$PresenceAb
accuracy(x=predicted1, y = observed1)
# data partition
set.seed(123)
ind2 <- sample(2, nrow(dataset09b), replace = TRUE, prob = c(0.7, 0.3)) 
train1 <- dataset09b[ind==1,]
test1 <- dataset09b[ind==2,]
rf_model09_b2 <- randomForest(response_var1~., data= train1,
                              ntree = 500,
                              mtry = 4,
                              importance = TRUE,
                              proximity = TRUE)
print(rf_model09_b2)
attributes(rf_model09_2)
#prediction p1 library caret
library(caret)
p09_1 <- predict(rf_model09_b2, train1)
confusionMatrix(p09_1, train1$response_var1)
# prediction & confusion matrix with test data
p09_2 <- predict(rf_model09_b2, test1)
confusionMatrix(p09_2,test1$response_var1)
accuracy(x=p09_2, y=test$response_var1)
accuracy(x=p09_2, y = test1$response_var1)
# Error rate of Random forest 
plot(rf_model09_b2)
# tune mtry
t <- tuneRF(train1[,-7], train[,7],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 500,
            trace = TRUE,
            improve = 0.05)
#####Making Results spatial 
library(raster)
library(sp)
library(rgdal)
NBR == raster("C:/Users/aajta/OneDrive/Desktop/Data0509/rasterdata/nbr09_Clip2.tif")
print(NBR)
B54 <- raster ("C:/Users/aajta/OneDrive/Desktop/Data0509/rasterdata/B54clip.tif")
print(B54)
TCG <- raster ("C:/Users/aajta/OneDrive/Desktop/Data0509/rasterdata/TCGclip2.tif")
print(TCG)
AnomB54 <- raster("C:/Users/aajta/OneDrive/Desktop/Data0509/rasterdata/AnomB54clip.tif")
print(AnomB54)
DiffB54 <- raster ("C:/Users/aajta/OneDrive/Desktop/Data0509/rasterdata/DiffB54clip2.tif")
print(DiffB54)
band4 <- raster ("C:/Users/aajta/OneDrive/Desktop/Data0509/rasterdata/BAND409.tif")
print(band4)
DEM <- raster ("C:/Users/aajta/OneDrive/Desktop/Data0509/rasterdata/DEMclip.tif")
print(DEM)
band5 <- raster ("C:/Users/aajta/OneDrive/Desktop/Data0509/rasterdata/BAND509.tif")
print(band5)
MeanB54 <- raster("C:/Users/aajta/OneDrive/Desktop/Data0509/rasterdata/MeanB54.tif")
print(MeanB54)

##Binary 2009
stack_5band7<- stack(NBR,B54,AnomB54,band4,TCG,DiffB54,DEM)
names(stack_5band7) =c('NBR' , 'B54' ,' AnomB54 ' ,' band4' , ' TCG' ,' DiffB54', 'DEM' )
predict(stack_5band7 ,rf_model09_b2,filename="NEWBINARY2009Final.tif",
        fun=predict,
        # format="Gtiff",
        # datatype="FLT4S",
        overwrite=TRUE)
## map regression based the model
##' NBR  ' , 'B54',  'TCG' , 'MeanB54' ,'band5 ' , 'DEM '
# stack the objects 
stack_band6 <- stack(NBR,B54,TCG,MeanB54,band5,DEM)
# add the head
names(stack_band6)= c('NBR','B54', 'TCG', 'MeanB54','band5', 'DEM')
predict(stack_band6,rf_model09_2,filename="NEWPercentRF2009Final.tif",
        fun=predict,
        #format="Gtiff",
        #datatype="FLT4S",
        overwrite=TRUE)
### convert to raster
library(rgdal)
library(raster)
regression <-raster("NEWPercentRF2009Final.tif")
binary <- raster("NEWBINARY2009Final.tif")
## Plot the results
par(mfrow=c(1,2), mar=c(3,3,3,5))
plot(regression, main="NEW Final Percent of tree mortality RF2009", xaxt='n', yaxt='n')
plot(binary, main= "NEW Final Presence and Absence tree mortality RF2009", xaxt= 'n', yaxt='n')
