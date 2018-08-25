predict.robust <- function(model, data, robust_vcov = NULL, level = 0.95, 
                           interval = "prediction"){
  # adapted from
  # https://stackoverflow.com/questions/38109501/how-does-predict-lm-compute-confidence-interval-and-prediction-interval
  # model is an lm object from r
  # data is the dataset to predict from
  # robust_vcov must be a robust vcov matrix created by V <- sandwich::vcovHC(model, ...)
  # level = the % of the confidence interval, default is 95%
  # interval = either "prediction" or "confidence" - prediction includes uncertainty about the model itself
  if(is.null(robust_vcov)){
    robust_vcov <- vcov(model)
  }
  if(is.null(data)){
    data <- model$model
  }
  fit <- as.numeric(model.matrix(model, data=data) %*% model$coefficients)
  se2 <- unname(rowSums((model.matrix(model) %*% robust_vcov) * model.matrix(model)))
  alpha <- qt((1-level)/2, df = model$df.residual)
  if(interval == "confidence"){
    upr <- fit + alpha*sqrt(se2)
    lwr <- fit - alpha*sqrt(se2)
  } else if(interval == "prediction"){
    sigma2 <- sum(model$residuals ^ 2) / model$df.residual
    upr <- fit + alpha*sqrt(se2+sigma2)
    lwr <- fit - alpha*sqrt(se2+sigma2)
  }
  preds <- cbind(fit, lwr, upr)
  return(list(fit = preds, se.fit = sqrt(se2)))
}


get_CL_vcov <- function(model, cluster){
  # cluster is an actual vector of clusters from data passed to model
  # from: http://rforpublichealth.blogspot.com/2014/10/easy-clustered-standard-errors-in-r.html
  require(sandwich, quietly = TRUE)
  require(lmtest, quietly = TRUE)
  
  # NA
  cluster <- as.character(cluster)
  
  #calculate degree of freedom adjustment
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- model$rank
  dfc <- (M/(M-1))*((N-1)/(N-K))
  
  #calculate the uj's
  uj  <- apply(estfun(model), 2, function(x) tapply(x, cluster, sum))
  
  #use sandwich to get the var-covar matrix
  vcovCL <- dfc*sandwich(model, meat=crossprod(uj)/N)
  return(vcovCL)
}

coefplot_coeftest <- function(coeftest, intercept = FALSE){
  plotdf <- data.frame(est = coeftest[, 1], 
                          stderr = coeftest[, 2], 
                          vars = names(coeftest[, 1]), 
                       stringsAsFactors = FALSE)
  
  if(intercept == FALSE){
    plotdf <- plotdf[plotdf$vars != "(Intercept)", ]
  }
  
  plotdf$upr_95 <- plotdf$est + (1.96 * plotdf$stderr)
  plotdf$lwr_95 <- plotdf$est - (1.96 * plotdf$stderr)
  plotdf$upr_80 <- plotdf$est + (1.6 * plotdf$stderr)
  plotdf$lwr_80 <- plotdf$est - (1.6 * plotdf$stderr)
  
  ggplot(plotdf, aes(x = vars)) + 
    geom_point(aes(y = est)) + 
    geom_hline(yintercept = 0, linetype = 2, color = I("red")) +
    geom_segment(aes(y = lwr_95, yend = upr_95, xend = vars)) + 
    geom_segment(aes(y = lwr_80, yend = upr_80, xend = vars), size = I(1.1)) + 
    labs(x = "Varialbe", y = "Estimate from Model") + 
    coord_flip() + theme_bw() 
    
  
  
}
