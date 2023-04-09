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



