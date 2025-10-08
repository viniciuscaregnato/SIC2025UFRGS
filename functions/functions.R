# dataprep ####

dataprep = function(ind, df, variable, horizon = horizon, add_dummy = TRUE, univar = FALSE, nlags=nlags)
  
{
  
  df=df[ind,]
  y=df[,variable]                                    
  
  
  if(univar==FALSE){
    factors=princomp(scale(df))$scores[,1:4]
    x=cbind(df,factors)
    x=as.data.frame(x)
    X=f_add_lags(x, nlags)
  }else{
    x = as.matrix(df[,variable])                   
    X=embed(as.matrix(x),nlags)
  }
  
  X=as.matrix(X)
  
  Xin=X[-c((nrow(X)-horizon+1):nrow(X)),]            
  Xout=X[nrow(X),]                                   
  Xout=t(as.vector(Xout))                            
  yin=tail(y,nrow(Xin))                              
  
  
  
  if("11/1/2008" %in% names(yin)){                  
    
    dummy=rep(0,length(yin))                         
    intervention=which(names(yin)=="11/1/2008")     
    dummy[intervention]=1                            
    if(add_dummy == TRUE){                            
      Xin=cbind(Xin,dummy)                           
      Xout=cbind(Xout,0)                             
    }
    
  }else{                                             
    dummy = rep(0,length(yin))                       
    if(add_dummy == TRUE){                           
      Xin=cbind(Xin,dummy)                           
      Xout=cbind(Xout,0)                             
    }
  }
  
  return(list(dummy = dummy, Xin = Xin, Xout = Xout, yin = yin))
  
}



# autoregressive runar ####

runar=function(ind,df,variable = variable,horizon = horizon, type = "bic", nlags=nlags){
  prep_data = dataprep(ind,df,variable,horizon, add_dummy = FALSE, univar = TRUE, nlags = nlags)
  Xin = prep_data$Xin
  yin = prep_data$yin
  Xout = prep_data$Xout
  dummy = prep_data$dummy
  
  if(type=="bic"){
    bb=Inf
    best = 1
    for(i in seq(1,ncol(Xin),1)){
      m=lm(yin~Xin[,1:i]+dummy)
      crit=BIC(m)
      if(crit<bb){
        bb=crit
        modelest=m
        best = i
      }
    }
  }
  coef=coef(modelest)                            
  coef[is.na(coef)] = 0                         
  forecast=c(1,Xout[,1:best],0)%*%coef               
  
  return(list(forecast=forecast))
}

# lasso runlasso ####

runlasso=function(ind,df,variable,horizon, alpha = 1, nlags=nlags){
  
  prep_data = dataprep(ind,df,variable,horizon, nlags=nlags)
  Xin = prep_data$Xin
  yin = prep_data$yin
  Xout = prep_data$Xout
  
  modelest = ic.glmnet(Xin,yin,alpha = alpha)
  
  forecast=predict(modelest,Xout)
  
  ## outputs
  coeflvl=coef(modelest)[-1]
  coefpar=coeflvl*apply(Xin,2,sd)
  lambda=modelest$lambda
  outputs=list(coeflvl=coeflvl,coefpar=coefpar,lambda=lambda)
  
  return(list(forecast=forecast, outputs=outputs))
}

# ridge runridge ####

runridge=function(ind,df,variable,horizon, alpha = 0, nlags=nlags){
  
  prep_data = dataprep(ind,df,variable,horizon, nlags=nlags)
  Xin = prep_data$Xin
  yin = prep_data$yin
  Xout = prep_data$Xout
  
  modelest = ic.glmnet(Xin,yin,alpha = alpha)
  
  forecast=predict(modelest,Xout)
  
  ## outputs
  coeflvl=coef(modelest)[-1]
  coefpar=coeflvl*apply(Xin,2,sd)
  lambda=modelest$lambda
  outputs=list(coeflvl=coeflvl,coefpar=coefpar,lambda=lambda)
  
  return(list(forecast=forecast, outputs=outputs))
}


# lasso elastic_net ####

runelasnet=function(ind,df,variable,horizon, alpha = 0.5, nlags=nlags){
  
  prep_data = dataprep(ind,df,variable,horizon, nlags=nlags)
  Xin = prep_data$Xin
  yin = prep_data$yin
  Xout = prep_data$Xout
  
  modelest = ic.glmnet(Xin,yin,alpha = alpha)
  
  forecast=predict(modelest,Xout)
  
  ## outputs
  coeflvl=coef(modelest)[-1]
  coefpar=coeflvl*apply(Xin,2,sd)
  lambda=modelest$lambda
  outputs=list(coeflvl=coeflvl,coefpar=coefpar,lambda=lambda)
  
  return(list(forecast=forecast, outputs=outputs))
}

# random forest runrf ####

runrf=function(ind,df,variable,horizon, nlags = nlags)
  
  
{
  prep_data = dataprep(ind,df,variable,horizon, nlags = nlags)     
  Xin = prep_data$Xin                               
  yin = prep_data$yin                               
  Xout = prep_data$Xout                             
  
  modelest=randomForest::randomForest(Xin,yin, importance = TRUE)
  
  
  forecast=predict(modelest,Xout)                           
  
  ## outputs
  importance = randomForest::importance(modelest)    
  outputs = list(importance = importance)           
  
  return(list(forecast=forecast, outputs = outputs))
  
}


# component wise boosting ####

runcwb=function(ind,df,variable = variable,horizon = horizon, nlags=nlags, lrate = 0.1, nite = 100){
  
  prep_data=dataprep(ind, df, variable, horizon = horizon, add_dummy = TRUE, univar = FALSE, nlags=nlags)
  Xin = prep_data$Xin
  yin = prep_data$yin
  Xout = prep_data$Xout
  
 modelest= glmboost(
    y = yin,
    x = Xin,
    offset = 0,
    center = TRUE,
    control = boost_control(
      mstop = nite,
      nu = lrate
    )
  )
  
  aic = AIC(modelest, method = "corrected")
  aic_seq = attributes(aic)$AIC
  m_opt = min(
    c(
      which(diff(aic_seq) > 0)[1],
      which.min(aic_seq)
    ),
    na.rm = TRUE
  )
  
  modelest = modelest[m_opt]
  
  
  coef_opt_aux = mboost::extract(modelest, what = "coefficients")
  coef_opt = rep(0, ncol(Xin))
  names(coef_opt) = colnames(Xin)
  coef_opt[names(coef_opt_aux)] = coef_opt_aux
  
  #forecast
  
  Xin_mean = as.vector(apply(Xin, 2, mean))
  yin_mean=mean(yin)
  
  forecast = sum(((Xout-Xin_mean)*coef_opt))+yin_mean
  
  outputs=list(coef_opt)
  
  
  
  
  return(list(
    forecast=forecast,
    outputs=outputs
  )
  )
}


# component wise boosting over auto regressive ####

runcwbar=function(ind,df,variable = variable,horizon = horizon, nlags=nlags, lrate = 0.1, nite = 100){
  prep_data = dataprep(ind,df,variable,horizon, add_dummy = TRUE, univar = TRUE, nlags = nlags)
  Xin = prep_data$Xin
  yin = prep_data$yin
  Xout = prep_data$Xout
  
  
  Xin = as.matrix(Xin)
  Xout = as.numeric(unlist(Xout))
  
  modelest= glmboost(
    y = yin,
    x = Xin,
    offset = 0,
    center = TRUE,
    control = boost_control(
      mstop = nite,
      nu = lrate
    )
  )
  
  aic = AIC(modelest, method = "corrected")
  aic_seq = attributes(aic)$AIC
  m_opt = min(
    c(
      which(diff(aic_seq) > 0)[1],
      which.min(aic_seq)
    ),
    na.rm = TRUE
  )
  
  modelest = modelest[m_opt]
  
  
  coef_opt_aux = mboost::extract(modelest, what = "coefficients")
  coef_opt = rep(0, ncol(Xin))
  names(coef_opt) = colnames(Xin)
  coef_opt[names(coef_opt_aux)] = coef_opt_aux
  
  #forecast
  
  Xin_mean = as.vector(apply(Xin, 2, mean))
  yin_mean=mean(yin)
  
  forecast = sum(((Xout-Xin_mean)*coef_opt))+yin_mean
  
  outputs=list(coef_opt)
  
  
  
  
  return(list(
    forecast=forecast,
    outputs=outputs
  )
  )
}




# accumulate_model ####

accumulate_model = function(forecasts){
  
  acc3 = c(rep(NA,2),sapply(1:(nrow(forecasts)-2), function(x){
    prod(1+diag(forecasts[x:(x+2),1:3]))-1
  })) 
  acc6 = c(rep(NA,5),sapply(1:(nrow(forecasts)-5), function(x){
    prod(1+diag(forecasts[x:(x+5),1:6]))-1
  }))
  acc12 = c(rep(NA,11),sapply(1:(nrow(forecasts)-11), function(x){
    prod(1+diag(forecasts[x:(x+11),1:12]))-1
  }))
  
  forecasts = cbind(forecasts,acc3,acc6,acc12)
  colnames(forecasts) = c(paste("t+",1:12,sep = ""),"acc3","acc6","acc12")
  
  return(forecasts)
  
}

# name lags ####

f_add_lags_aux <- function(
    x, # vetor com os valores numéricos
    nlags, # lag máx. desejado
    name # nome a ser colocado no vetor resultante
) {
  result <- embed(x, nlags) %>% data.frame()
  colnames(result) <- c(
    paste(name, "_t_", 1:nlags, sep = "")
  )
  
  return(result)
}

f_add_lags <- function(
    x, # data.frame com colunas nomeadas
    nlags # lag máx. desejado
) {
  
  x=data.frame(x)
  
  if (!is.data.frame(x)) {
    stop(
      "x must be a data.frame"
    )
  }
  
  dados <- data.frame(
    temp = rep(1, nrow(x) - nlags + 1)
  )
  name_aux <- colnames(x)
  
  for (i in seq_len(ncol(x))) {
    dados <- data.frame(
      dados,
      f_add_lags_aux(
        x = x[, i],
        nlags = nlags,
        name = name_aux[i]
      )
    )
  }
  dados_aux <- data.frame(dados[, -1])
  colnames(dados_aux) <- colnames(dados)[-1]
  return(dados_aux)
}

# ic.glmnet ####


ic.glmnet = function (x, y, crit = c("bic", "aic", "aicc", 
                                     "hqc"), alpha = 1, ...) 
{
  if (is.matrix(x) == FALSE) {
    x = as.matrix(x)
  }
  if (is.vector(y) == FALSE) {
    y = as.vector(y)
  }
  crit = match.arg(crit)
  n = length(y)
  model = glmnet(x = x, y = y, alpha = alpha,...)
  coef = coef(model)
  lambda = model$lambda
  df = model$df
  yhat = cbind(1, x) %*% coef
  residuals = (y - yhat)
  mse = colMeans(residuals^2)
  sse = colSums(residuals^2)
  nvar = df + 1
  bic = n * log(mse) + nvar * log(n)
  aic = n * log(mse) + 2 * nvar
  aicc = aic + (2 * nvar * (nvar + 1))/(n - nvar - 1)
  hqc = n * log(mse) + 2 * nvar * log(log(n))
  sst = (n - 1) * var(y)
  r2 = 1 - (sse/sst)
  adjr2 = (1 - (1 - r2) * (n - 1)/(nrow(x) - nvar - 1))
  crit = switch(crit, bic = bic, aic = aic, aicc = aicc, hqc = hqc)
  selected = best.model = which(crit == min(crit))
  ic = c(bic = bic[selected], aic = aic[selected], aicc = aicc[selected], 
         hqc = hqc[selected])
  result = list(coefficients = coef[, selected], ic = ic, lambda = lambda[selected], 
                nvar = nvar[selected], glmnet = model, residuals = residuals[, 
                                                                             selected], fitted.values = yhat[, selected], ic.range = crit, 
                df = df, call = match.call())
  class(result) = "ic.glmnet"
  return(result)
}


# GW TEST ####


gw.test <- function(
    x,
    y,
    p,
    T,
    tau,
    method = c("HAC", "NeweyWest", "Andrews", "LumleyHeagerty"),
    alternative = c("two.sided", "less", "greater")) {
  if (is.matrix(x) && ncol(x) > 2) {
    stop("multivariate time series not allowed")
  }
  if (is.matrix(y) && ncol(y) > 2) {
    stop("multivariate time series not allowed")
  }
  if (is.matrix(p) && ncol(p) > 2) {
    stop("multivariate time series not allowed")
  }
  
  # x: predicciones modelo 1
  # y: predicciones modelo 2
  # p: observaciones
  # T: sample total size
  # tau: horizonte de prediccion
  # method: if tau=1, method=NA. if tau>1, methods
  # alternative: "two.sided","less","greater"
  
  if (NCOL(x) > 1) stop("x is not a vector or univariate time series")
  if (tau < 1) stop("Predictive Horizon must to be a positive integer")
  if (length(x) != length(y)) stop("size of x and y difier")
  
  alternative <- match.arg(alternative)
  DNAME <- deparse(substitute(x))
  
  l1 <- abs(x - p)
  l2 <- abs(y - p)
  dif <- l1 - l2
  q <- length(dif)
  m <- T - q
  n <- T - tau - m + 1
  delta <- mean(dif)
  mod <- lm(dif ~ 0 + rep(1, q))
  
  if (tau == 1) {
    re <- summary(mod)
    STATISTIC <- re$coefficients[1, 3]
    if (alternative == "two.sided") {
      PVAL <- 2 * pnorm(-abs(STATISTIC))
    } else if (alternative == "less") {
      PVAL <- round(pnorm(STATISTIC), 4)
    } else if (alternative == "greater") {
      PVAL <- round(pnorm(STATISTIC, lower.tail = FALSE), 4)
    }
    names(STATISTIC) <- "Normal Standard"
    METHOD <- "Standard Statistic Simple Regression Estimator"
  }
  
  if (tau > 1) {
    if (method == "HAC") {
      METHOD <- "HAC Covariance matrix Estimation"
      ds <- sqrt(vcovHAC(mod)[1, 1])
    }
    if (method == "NeweyWest") {
      METHOD <- "Newey-West HAC Covariance matrix Estimation"
      ds <- sqrt(NeweyWest(mod, tau)[1, 1])
    }
    if (method == "LumleyHeagerty") {
      METHOD <- "Lumley HAC Covariance matrix Estimation"
      ds <- sqrt(weave(mod)[1, 1])
    }
    if (method == "Andrews") {
      METHOD <- "kernel-based HAC Covariance matrix Estimator"
      ds <- sqrt(kernHAC(mod)[1, 1])
    }
    # STATISTIC = sqrt(n)*delta/ds
    STATISTIC <- delta / ds
    if (alternative == "two.sided") {
      PVAL <- 2 * pnorm(-abs(STATISTIC))
    } else if (alternative == "less") {
      PVAL <- pnorm(STATISTIC)
    } else if (alternative == "greater") {
      PVAL <- pnorm(STATISTIC, lower.tail = FALSE)
      names(STATISTIC) <- "Normal Standar"
    }
  }
  structure(
    list(
      statistic = STATISTIC,
      alternative = alternative,
      p.value = PVAL,
      method = METHOD,
      data.name = DNAME
    )
  )
}
