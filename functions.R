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
splitData = function(Kfold, seed, datafr)
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
  reg[[k]] = myTrain()
#    switch( switchVar, 
#                      glm(idefault~.-id, family=binomial(link="logit"), data=datafr[-ifold,]),
#                      randomForest(idefault~.-id, data=datafr[-ifold,], ntree = 1000,
#                                   importance = TRUE, proximity=TRUE),
#                      randomForest(idefault~.-id, data=datafr[-ifold,], ntree = 1000,
#                                   importance = TRUE, proximity=TRUE, classwt = c(10,1)),
#                      train(idefault~.-id, data = datafr[-ifold,], method = methodVar))
  pred[[k]] = myPredict()
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
   scoreVar[[k]] = myScore()
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
    return(originCountry)
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

## Function to apply country_of_origin and is_luxury to a dataframe
# @param data dataframe: the data to retrieve the manufacturer's country of origin from, must have a columm called 'manufacturer'
# @retrun new_data datframe: a copy of the original dataframe containing the new variable countryOrigin and is_luxury
country_origin_transform<-function(data){
    var <- as.character(data$manufacturer)
    var[var==""] = "missing"
    data$manufacturer =  var
    isLuxury = sapply(data$manufacturer, function(i) is_luxury(i))
    new_data = data%>%
        mutate(isLuxury = isLuxury)
    return(new_data)             
    }
