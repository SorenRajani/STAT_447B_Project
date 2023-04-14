#' Interval score function for prediction intervals, smaller value is better #' @description
#' Interval score for prediction intervals
#'
#' @param predobj has 3 (or more) columns: pointprediction, predLB, predUB
#' @param actual corresponding vector of actual values
# (in holdout set, for example)
#' @param level level for prediction interval, e.g., 0.5 or 0.8
#' @return list with
#' summary consisting of level, average length, interval score, coverage rate #' and
#' imiss with cases where prediction intervals don't contain actual values
#'
intervalScore = function(predObj,actual,level){ 
n = nrow(predObj)
alpha = 1- level
ilow = (actual<predObj[,2]) # overestimation
ihigh = (actual>predObj[,3]) # underestimation
sumlength = sum(predObj[,3]-predObj[,2]) # sum of lengths of prediction intervals 
sumlow = sum(predObj[ilow,2]-actual[ilow])*2/alpha
sumhigh = sum(actual[ihigh]-predObj[ihigh,3])*2/alpha
avglength = sumlength/n
IS = (sumlength+sumlow+sumhigh)/n # average length + average under/over penalties 
cover = mean(actual>= predObj[,2] & actual<=predObj[,3])
summ = c(level,avglength,IS,cover) # summary with level, average length, interval score, coverage rate
imiss = which(ilow | ihigh)
list(summary=summ)
}
                      
## Function to perform K-fold valdiation and produces AUC measure for each fold
#' @param kfold integer - number of folds desired
#' @param seed integer - setting seed for reproducible folds
#' @param datafr dataframe - data to be split - needs to have `idefault` column 
#' @param threshhold [0,1] - thresh hold for comparing predictions, 0.5 default value
#' @param switchVar {1,2,3,4} - 1: logit mode (default), 
#'                              2: randomForest mode, 
#'                              3: weighted randomForest, 
#'                              4: general methods from carter() packages
#' @param methodVar string: enters method for train() function - default = "gbm"
#' @return AUC measure for each fold
kFold = function(Kfold, seed, datafr)
{ set.seed(seed)
  n = nrow(datafr)
  iperm<<-sample(n) # set as global for debugging check
  nhold = round(n/Kfold)
  reg = list()
  pred = list() 
  scoreVar = list()
  rocVar = list()
  pred_y = sample(n-nhold)
  for(k in 1:Kfold)
  { ilow = (k-1)*nhold+1
  ihigh = k*nhold
  if(k==Kfold) { ihigh = n }
  ifold = iperm[ilow:ihigh]
  holdo = datafr[ifold,]
  train = datafr[-ifold,]
  reg[[k]] = myTrain(data = as.data.frame(train))
#    switch( switchVar, 
#                      glm(idefault~.-id, family=binomial(link="logit"), data=datafr[-ifold,]),
#                      randomForest(idefault~.-id, data=datafr[-ifold,], ntree = 1000,
#                                   importance = TRUE, proximity=TRUE),
#                      randomForest(idefault~.-id, data=datafr[-ifold,], ntree = 1000,
#                                   importance = TRUE, proximity=TRUE, classwt = c(10,1)),
#                      train(idefault~.-id, data = datafr[-ifold,], method = methodVar))
  pred[[k]] = myPredict(reg[[k]],newdata = holdo)
#    switch(switchVar, 
#                      predict(reg[[k]], newdata=datafr[ifold,],type = "response"),
#                      predict(reg[[k]], newdata=datafr[ifold,]),
#                      predict(reg[[k]], newdata=datafr[ifold,]), 
#                      predict(reg[[k]], newdata=datafr[ifold,]))
  # Get performance measures from pred[[k]] and datafr[ifold,]
#   rocVar[[k]] = switch(switchVar, 
#                        roc(datafr[ifold,]$idefault, as.numeric(pred[[k]]>=threshhold)),
#                        roc(datafr[ifold,]$idefault, as.numeric(pred[[k]]>=threshhold)),
#                        roc(datafr[ifold,]$idefault, as.numeric(pred[[k]])),
#                        roc(datafr[ifold,]$idefault, as.numeric(pred[[k]])))
  
#   aucVar[[k]] = auc(rocVar[[k]])
   scoreVar[[k]] = myScore(pred[[k]],holdo)
  }
  # list(reg=reg, pred=pred)
  ##aucVar
 scoreVar
}

## function to return vector of car ages from a dataframe
# @param df dataframe -the dataframe to create ages for, must have a column named price
# @return age vector -a column vector corresponding to car ages
ageVehicle <- function(df){
    year = df$year
    return(2022 - year)}

## Function to return a manufacturer's country of origin
# @param manufacturer string: a car's manufacturing company
# @return originCountry string: a manufacturor's country of origin
country_of_orign<- function(manufacturer){
    originCountry = switch(manufacturer,
        "missing" = "missing",
        "acura" = "Japan",
        "alfa-romeo" = "Italy",
        "aston-martin" = "UK",
        "audi" = "Germany",
        "bmw" = "Germany", 
        "buick" = "USA",
        "cadillac" = "USA",
        "chevrolet" = "USA",
        "chrysler" = "USA",
        "dodge" = "USA",
        "ferrari" = "Italy",
        "fiat" = "Italy",
        "ford" = "USA",
        "gmc" = "USA",
        "harley-davidson" = "USA", 
        "honda" = "Japan",
        "hyundai" = "South Korea",
        "infiniti" = "Japan",
        "jaguar" = "UK",
        "jeep" = "USA",
        "kia" = "South Korea",
        "land rover" = "UK",
        "lexus" = "Japan",
        "lincoln" = "USA",
        "mazda" = "Japan",
        "mercedes-benz" = "Germany",
        "mercury" = "USA",
        "mini" = "UK",
        "mitsubishi" = "Japan",
        "mogran" = "UK",
        "nissan" = "Japan",
        "pontiac" = "USA",
        "porsche" = "Germany",
        "ram" = "USA",
        "rover" = "UK",
        "saturn" = "USA",
        "subaru" = "Japan",
        "tesla" = "USA",
        "toyota"  = "Japan",
        "volkswagen" = "Germany",
        "volvo" = "Sweden")
    return(unlist(originCountry, use.names = FALSE))
    
    }

## Function to return whether a car is domestic (USA) or foreign
# @param originCountry string: a car's country of origin
# @return is_domestic_manufacturer string: a string variable indicating if the manufacturer is a domestic brand
is_domestic<- function(originCountry){
    if (originCountry == "USA") is_domestic_manufacturer = "domestic"
    else if (originCountry == "missing") is_domestic_manufacturer = "missing"
    else is_domestic_manufacturer = "foreign"
    return(is_domestic_manufacturer)
    }

## Function to indicate if a car brand is luxury
# @param manufacturer string: a vehicle manufacturer
# @return: is_luxury_brand binary: a binary variable indicating if the manufacturer is a luxury brand
is_luxury<- function(manufacturer){
    luxury_brands = list("acura", "alfa-romeo", "aston-martin", "audi", "bmw", "ferrari", "harley-davidson", "infiniti",
                         "jaguar", "land rover", "lexus", "lincoln", "mercedes-benz", "mini", "mogran", "porsche", "rover",
                         "tesla", "volvo")
    is_luxury_brand = ifelse(manufacturer %in% luxury_brands, 1, 0)

    return(is_luxury_brand)
    }

## Function to apply country_of_origin to a dataframe
# @param data dataframe: the data to retrieve the manufacturer's country of origin from, must have a columm called 'manufacturer'
# @retrun new_data datframe: a copy of the original dataframe containing the new variable countryOrigin
country_origin_transform<-function(data){
    var <- as.character(data$manufacturer)
    var[var==""] = "missing"
    data$manufacturer =  var
    country = sapply(data$manufacturer, function(i) country_of_orign(i))
    new_data = data%>%
        mutate(countryOrigin = country)
    return(new_data)             
    }
                      
## Function to apply is_luxury to a dataframe
# @param data dataframe: the data to retrieve the manufacturer's country of origin from, must have a columm called 'manufacturer'
# @retrun new_data datframe: a copy of the original dataframe containing the new variable is_luxury
is_luxury_transform<-function(data){
    var <- as.character(data$manufacturer)
    var[var==""] = "missing"
    data$manufacturer =  var
    isLuxury = sapply(data$manufacturer, function(i) is_luxury(i))
    new_data = data%>%
        mutate(isLuxury = isLuxury)
    return(new_data)             
    }
                      
## Function to bin off-road and and bus into other
# @param: df dataframe: dataframe to alter
# @return: df_binned: the original dataframe altered to have the new bins
type_bin <- function(df){
    vals <- df[,'type']
    vals[vals == 'off-road'] == 'other'
    vals[vals == 'bus'] =='other'
    df = subset(df,select = -c(type)) #this step prevents the dataframe from recalling 'bus' type should exist when using tree based methods
    df$type = vals
    return(df)}
                      
## Function to collectively perform all wrangling necessary 
# @param: data dataframe: dataframe to be wrangled, must include columns - "manufacturer", "type" & "year"
wrangling_function<- function(data){
    new_data = type_bin(data)

    new_data = country_origin_transform(new_data)

    new_data = is_luxury_transform(new_data)

    new_data = mutate(new_data, age =  ageVehicle(new_data))
    
    new_data = mutate(new_data, countryOrigin = as.character(countryOrigin))%>%
    mutate_if(sapply(., is.character), as.factor) #factorizes <chr> variables
    
    new_data = new_data %>% mutate(sqrt_odometer = sqrt(odometer))
    
    new_data = subset(new_data, select = -c(year,odometer,manufacturer))
    return(new_data)
    }
                      
## Function to perform feature selection and return dataset
feature_selection = function(data){
    new_data = subset(data, select = c(price, age, fuel, drive, type, countryOrigin, isLuxury))
    return(new_data)
    }

## 