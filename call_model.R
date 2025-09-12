library(devtools)


#install_github("gabrielrvsc/HDeconometrics", force = TRUE)
library(HDeconometrics)
library(glmnet)
library(randomForest)
library(dplyr)
library(mboost)


source("functions/rolling_window.R")
source("functions/functions.R")


model_name <- "RIDGE"
model_function <- runridge



load("data/rawdata.RData")
data<-dados
colnames(data)[1]="CPIAUCSL"

#View(data)
#dim(data)

#dates = data$date
#data = data%>%select(-date)%>%as.matrix()
#rownames(data) = as.character(dates)

nwindows = 312
model_list = list() 

for(i in 1:12){
  model = rolling_window(model_function,data,nwindows+i-1,i,"CPIAUCSL")
  model_list[[i]] = model
  cat(i,"\n")
}


#cria matriz forecasts 

forecasts = Reduce(cbind,lapply(model_list, function(x)head(x$forecast,nwindows)))



# o accumulate_model calcula as diagonais, sendo assim, os valores de previsao de 3 e 6 meses e adiciona as colunas referetnes
forecasts = accumulate_model(forecasts)

View(forecasts)

save(forecasts,file = paste("forecasts/",model_name,".rda",sep = ""))


plot(tail(data[,"CPIAUCSL"],312),type = "l")
lines(forecasts[,1],col = 2)
