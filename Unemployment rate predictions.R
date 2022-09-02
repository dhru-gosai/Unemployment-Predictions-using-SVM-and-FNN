# Install packages if not available already
if (!require("car")) install.packages("car")
if (!require("datasets")) install.packages("datasets")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("qqplotr")) install.packages("qqplotr")
if (!require("ggfortify")) install.packages("ggfortify")
if (!require("ggthemes")) install.packages("ggthemes")
if (!require("hrbrthemes")) install.packages("hrbrthemes")
if (!require("ISLR")) install.packages("ISLR")
if (!require("caret")) install.packages("caret")
if (!require("GGally")) install.packages("GGally")
if (!require("knitr")) install.packages("knitr")
if (!require("MASS")) install.packages("MASS")
if (!require("ROCR")) install.packages("ROCR")
if (!require("corrplot")) install.packages("corrplot")
if (!require("ggridges")) install.packages("ggridges")
if (!require("klaR")) install.packages("klaR")
if (!require("psych")) install.packages("psych")
if (!require("yaml")) install.packages("yaml")
if (!require("cluster")) install.packages("cluster")
if (!require("factoextra")) install.packages("factoextra")
if (!require("reshape2")) install.packages("reshape2")
if (!require("broom")) install.packages("broom")
if (!require("aod")) install.packages("aod")
if (!require("ggpubr")) install.packages("ggpubr")
if (!require("gridExtra")) install.packages("gridExtra")
if (!require("fpc")) install.packages("fpc")
if (!require("datarium")) install.packages("datarium")
if (!require("e1071")) install.packages("e1071")
if (!require("glmpath")) install.packages("glmpath")
if (!require("quadprog")) install.packages("quadprog")
if (!require("data.table")) install.packages("data.table")
if (!require("readxl")) install.packages("readxl")
if (!require("randomForest")) install.packages("randomForest")
if (!require("e1071")) install.packages("e1071")
if (!require("rpart")) install.packages("rpart")
if (!require("rpart.plot")) install.packages("rpart.plot")
if (!require("rattle")) install.packages("rattle")
if (!require("gbm")) install.packages("gbm")
if (!require("ranger")) install.packages("ranger")
if (!require("doParallel")) install.packages("doParallel")
if (!require("rminer")) install.packages("rminer")
if (!require("reticulate")) install.packages("reticulate") #ANN
if (!require("tensorflow")) install.packages("tensorflow") #ANN
if (!require("tfdatasets")) install.packages("tfdatasets") #ANN
if (!require("keras")) install.packages("keras") #ANN
if (!require("zoo")) install.packages("zoo") #imputation
if (!require("Metrics")) install.packages("Metrics") #imputation
if (!require("neuralnet")) install.packages("neuralnet") #ANN

# Loading relevant R packages
library(car, warn.conflicts = F, quietly = T)
library(datasets, warn.conflicts = F, quietly = T)
library(ggplot2, warn.conflicts = F, quietly = T)
library(MASS, warn.conflicts = F, quietly = T)
library(dplyr, warn.conflicts = F, quietly = T) #for piping
library(tidyverse, warn.conflicts = F, quietly = T)
library(qqplotr, warn.conflicts = F, quietly = T) # for qq plots
library(ggfortify, warn.conflicts = F, quietly = T) # for visualisations
library(ggthemes, warn.conflicts = F, quietly = T) # for ggplot themes
library(hrbrthemes, warn.conflicts = F, quietly = T) # for ggplot background themes
library(ISLR, warn.conflicts = F, quietly = T) #for data
library(caret, warn.conflicts = F, quietly = T) #for splitting the data
library(GGally, warn.conflicts = F, quietly = T)
library(knitr, warn.conflicts = F, quietly = T) # to add appendix in the end
library(ROCR, warn.conflicts = F, quietly = T)
library(corrplot, warn.conflicts = F, quietly = T) # Correlation matrix
library(ggridges, warn.conflicts = F, quietly = T)
library(klaR, warn.conflicts = F, quietly = T)
library(psych, warn.conflicts = F, quietly = T) # Visualise
library(yaml, warn.conflicts = F, quietly = T)
library(cluster, warn.conflicts = F, quietly = T)
library(factoextra, warn.conflicts = F, quietly = T)
library(reshape2, warn.conflicts = F, quietly = T)#reshaping data
library(broom, warn.conflicts = F, quietly = T)
library(aod, warn.conflicts = F, quietly = T) # for wald test
library(ggpubr, warn.conflicts = F, quietly = T)
library(gridExtra, warn.conflicts = F, quietly = T)
library(fpc, warn.conflicts = F, quietly = T)
library(datarium, warn.conflicts = F, quietly = T) #to get marketing dataset from datarium
library(e1071, warn.conflicts = F, quietly = T)    #for svm
library(glmpath, warn.conflicts = F, quietly = T)  #for svm
library(quadprog, warn.conflicts = F, quietly = T)  #for QP
library(data.table, warn.conflicts = F, quietly = T)
library(readxl, warn.conflicts = F, quietly = T) # importing xls file type
library(randomForest, warn.conflicts = F, quietly = T)   #For applying Random Forest
library(e1071, warn.conflicts = F, quietly = T)          #For SVM
library(rpart, warn.conflicts = F, quietly = T)          #For tree models
library(rpart.plot, warn.conflicts = F, quietly = T)     #for plotting tree
library(rattle, warn.conflicts = F, quietly = T)
library(gbm, warn.conflicts = F, quietly = T)
library(ranger, warn.conflicts = F, quietly = T)
library(doParallel, warn.conflicts = F, quietly = T)
library(rminer, warn.conflicts = F, quietly = T)
library(reticulate, warn.conflicts = F, quietly = T) #ANN
library(tensorflow, warn.conflicts = F, quietly = T) #ANN
library(tfdatasets, warn.conflicts = F, quietly = T)
library(keras, warn.conflicts = F, quietly = T) #ANN
library(zoo, warn.conflicts = F, quietly = T) #imputation
library(Metrics, warn.conflicts = F, quietly = T) #ML metrics
library(neuralnet, warn.conflicts = F, quietly = T) #ML metrics

set_random_seed(7789) # For tensorflow and keras


# Import data from excel
aus_data <- read_excel("D:/Uni/MA5832_AdvMachineLearning/Assignment 3/AUS_Data.xlsx",
                       col_types = c("date","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))

# aus_data <- read_excel("D:/Dhru Folder/JCU - Master of Data science/MA5832 - Data Mining and Machine Learning/Assignment 3/AUS_Data.xlsx",
#                        col_types = c("date","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))
aus_data = aus_data[-1,]  # drop 1st row

#Adding logical column names
colnames(aus_data) <- c("quarter","unemployment","GDP","GFCE","FCE","trade_index","CPI","vacancy","population")
aus_data$quarter <- as.Date(aus_data$quarter) #Converting default POSIXct quarter to as.date

summary(aus_data)

# Data spread
boxplot(scale(aus_data[,2:9]),
        col = "darkolivegreen3",
        ylab = "Standardised Value",
        las = 1,
        main = "Boxplot for data spread")
out <- round(boxplot.stats(scale(aus_data[,2:9]))$out,2)
out <- out[out < -5]
mtext(paste("Outliers: ", paste(out, collapse = ", ")))

# GGPlot to make sure linear growth of population is met + check prediction accuracy
ggplot(aus_data, aes(x = quarter, y=unemployment)) +
    geom_line(aes(y=unemployment), colour="red") +
    ggtitle("Unemployment rate (in %) in Australia")+
    # scale_y_continuous(limits=c(3, 12))+
    theme_bw()+
    theme(plot.title = element_text(size = 16),
          axis.title = element_text(size = 12, face = "bold"))

# ======================
# Data cleaning
# ======================
sum(is.na(aus_data))

# --------------------------------------------
# Update GDP, FCE outlier values - Imputation/interpolation
# --------------------------------------------

# Delete existing outlier values
# aus_data <- aus_data %>%
#             mutate(GDP = ifelse(quarter=="2020-06-01",NA,GDP),
#                    FCE = ifelse(quarter=="2020-06-01",NA,FCE)) ##Removal of outliers
#
# aus_gdp_fce$GDP <- zoo::na.approx(aus_data$GDP)
# aus_gdp_fce$FCE <- zoo::na.approx(aus_data$FCE)
#
# aus_gdp_fce <- aus_gdp_fce %>%
#                 select(quarter,GDP,FCE)
#
# aus_data <- aus_data %>% left_join(aus_gdp_fce, by="quarter") %>%
#   mutate(GDP = coalesce(GDP.x, GDP.y),
#          FCE = coalesce(FCE.x, FCE.y))%>%
#   dplyr::select(-c("GDP.x","GDP.y","FCE.x","FCE.y" ))

# boxplot(scale(aus_data[,2:9]),
#         col = "darkolivegreen3",
#         ylab = "Standardised Value",
#         las = 1,
#         main = "Boxplot for data spread - post taking outliers out")
# out <- round(boxplot.stats(scale(aus_data[,2:9]))$out,2)
# out <- out[out < -5]
# mtext(paste("Outliers: ", paste(out, collapse = ", ")))

# ----------------------------------------
# Missing population records - extrapolate
# ----------------------------------------
aus_pop <- aus_data[c("quarter","population")] %>%
    na.omit(aus_data) %>%
    filter(year(quarter) > 2010) # Selecting records from 2010 only to have accurate extrapolation

aus_pop$pred1 <- predict(lm(population ~ quarter, data=aus_pop)) # Using lm as population growth is assumed to be linear

# GGPlot to make sure linear growth of population is met + check prediction accuracy
ggplot(aus_pop, aes(x = quarter, y=population)) +
    geom_line() +
    geom_point() +
    geom_hline(aes(yintercept=0))+
    geom_line(aes(y = pred1), color="red")+
    ggtitle("Population growth against predicted (using lm)")+
    theme_bw()+
    theme(plot.title = element_text(size = 16),
          axis.title = element_text(size = 12, face = "bold"))

# Create new dataset with extrapolation based on model
pred <- data.frame(quarter=seq(as.Date("2011-03-01"), as.Date("2020-09-01"), by = "quarter"))

lm_population <- (lm(population ~ quarter, data=aus_pop))
summary(lm_population)

pred$population <- predict(lm_population,newdata=pred) #Add predictions

#Actual vs. predicted
ggplot(aus_pop, aes(x = quarter, y=population)) +
    geom_line() +
    geom_point() +
    geom_line(color="red", data=pred)+
    ggtitle("Extrapolated data using lm()")+
    scale_y_continuous(limits=c(2e+05, 2.7e+05))+
    theme_bw()+
    theme(plot.title = element_text(size = 16),
          axis.title = element_text(size = 12, face = "bold"))

pred <- pred %>% filter(quarter > as.Date("2019-06-01"))

aus_data <- aus_data %>% left_join(pred, by="quarter") %>%
    mutate(population = coalesce(population.x, population.y)) %>%
    dplyr::select(-c("population.x","population.y"))
# ------------------------------------
# Missing vacancy records - Imputation/interpolation
# ------------------------------------
aus_vacancy <- aus_data[c("quarter","vacancy")] %>%
    filter(year(quarter) > 2004 & year(quarter) < 2016) # Selecting records between 2005 and 2015 to have accurate imputation

aus_vacancy$vacancy <- zoo::na.approx(aus_vacancy$vacancy)

aus_data <- aus_data %>% left_join(aus_vacancy, by="quarter") %>%
    mutate(vacancy = coalesce(vacancy.x, vacancy.y))

# GGPlot to make sure linear growth of population is met + check prediction accuracy
ggplot(aus_data, aes(x = quarter, y=vacancy.x)) +
    geom_line() +
    geom_point() +
    geom_line(aes(y = vacancy.y), color="red")+
    ggtitle("Vacancy value imputations")+
    theme_bw()+
    theme(plot.title = element_text(size = 16),
          axis.title = element_text(size = 12, face = "bold"))

aus_data <- aus_data %>% dplyr::select(-c("vacancy.x","vacancy.y"))
# ------------------------------------

# =================================
# Data Exploration & Visualisation
# =================================

describe(aus_data)

# Correlation to get relationship between variables
# M <- round(cor(aus_data[,2:9]), 2) # Create the correlation matrix
# corrplot(M,order="hclust",
#          tl.cex = 0.90,
#          addCoef.col ='black',
#          method = "square",
#          type = 'lower',
#          diag = FALSE)# Create corr plot
# title("Correlation plot for numeric variables")

# plot variables to understand the spread
gg <- GGally::ggpairs(aus_data[,2:9])
gg


# ======================
# Data Partition
# ======================

sum(is.na(aus_data)) # Check for missing data
summary(aus_data) # Check summary before running models


# Split the data into 70% train and 30% test
set.seed(7789)
train <- aus_data %>%
    filter(quarter < "2018-03-01")
test  <- aus_data %>%
    filter(quarter >= "2018-03-01")

dim(train)

# Convert unemployment as.factor
# train$unemployment <- as.factor(train$unemployment)
# test$unemployment  <- as.factor(test$unemployment)

# ======================
# ALGORITHM 1: SVM
# ======================
#Support Vector Machine (using radial kernel)
set.seed(7789)

# Specify training control cross validation parameters
train_control <- trainControl(method="repeatedcv",
                              number=10,
                              repeats=3,
                              savePredictions=TRUE)

# SVM model - without tuning
# --------------------------
train_svm <- as.matrix(train[,2:9]) # Caret for svm requires input of matrix dataset form

svm_start <- Sys.time() # Start time of the model
set.seed(7789)
SVM_caret <- caret::train(unemployment ~ .,
                          data = train_svm,
                          method = "svmRadial",
                          trControl = train_control,
                          preProcess = c("center","scale"),
                          tuneLength = 10)

print(SVM_caret)

svm_end <- Sys.time()  # End time of the model
print(svm_end - svm_start) #Time taken to run model

SVM_results <- data.frame(SVM_caret$results) # Add results into a dataframe to plot
# GGPlot for best C value against Rsquared
ggplot(aes(x = C, y=Rsquared), data=SVM_results) +
    geom_line() +
    geom_text(aes(label=C), color="red")+
    ggtitle(" SVM Model performance for each cost where sigma = 0.2241661")+
    scale_y_continuous(limits=c(0.68, 0.83))+
    theme_bw()

SVM_caret$bestTune # maximizes model accuracy - Use these C and sigma

SVM_Grid <- expand.grid(.C = SVM_caret$bestTune$C,.sigma=SVM_caret$bestTune$sigma) # Results from grid search tuning

set.seed(7789)
SVM_tuned.2 <- caret::train(unemployment ~ .,
                            data = train_svm,
                            method = "svmRadial",
                            tuneGrid = SVM_Grid,
                            trControl = train_control,
                            preProcess = c("center","scale"),
                            tuneLength = 10)

print(SVM_tuned.2) #R_squared = 0.8003504
pred_SVM <- predict(SVM_tuned.2,train_svm) # Predict to assess model performance

# GGPlot for tuned svm model
ggplot(train, aes(x = quarter, y=unemployment)) +
    geom_line() +
    geom_point() +
    geom_line(aes(y = pred_SVM), color="red")+ #untuned model
    ggtitle(" SVM Model performance (Tuned) - Training data")+
    scale_y_continuous(limits=c(3, 12))+
    theme_bw()

test_svm <- as.matrix(test[,2:9])# Caret for svm requires input of matrix dataset form

pred_SVM_test <- predict(SVM_tuned.2,test_svm) # Predict to assess model performance
# GGPlot for tuned svm model
ggplot(test, aes(x = quarter, y=unemployment)) +
    geom_line() +
    geom_point() +
    geom_line(aes(y = pred_SVM_test), color="red")+ #untuned model
    ggtitle(" SVM Model performance (Tuned) - Testing data")+
    scale_y_continuous(limits=c(3, 12))+
    theme_bw()

res <- caret::postResample(pred_SVM_test,test$unemployment) #RMSE - root of mean squared error
rsquare <- res[2] # R-square helps in assessing model performance on test
print(rsquare) #0.8435274

# ====================================
# ALGORITHM 2: Neural Networks
# ====================================
# get current time to check duration of the neural network run time at the end
ann_time <- Sys.time()

# Define hyper parameters
# -----------------------
verbose = 1
validation = 0.15  # validation split (% of dataset to be withheld for validation)
epoch = 50       # iterations of dataset

train_ann <- train[,2:9]
test_ann  <- test[,2:9]

# Scaling using python
# --------------------
spec <- feature_spec(train_ann, unemployment ~ . ) %>%
  step_numeric_column(all_numeric(), normalizer_fn = scaler_standard()) %>%
  fit()
spec

# set the seeds
set.seed(7789)
set_random_seed(7789)
build_model <- function() {
  input <- layer_input_from_dataset(train_ann)
  output <- input %>%
    layer_dense_features(dense_features(spec))   %>%
    layer_dense(units = 113, activation = "relu",
                input_shape = dim(train_ann)[[2]]) %>%
    layer_dense(units = 65, activation = "relu") %>%
    layer_dense(units = 1)

  model <- keras_model(input, output)

  model %>%
    compile(
      loss = "mse",
      optimizer = "rmsprop",
      metrics = list("mae","mape","mse")
      )
}

model <- build_model()

print_dot_callback <- callback_lambda(  on_epoch_end = function(epoch, logs) {
  if (epoch %% 80 == 0) cat("\n")
  cat(".") } )

set.seed(7789)
set_random_seed(7789)
# This is to find optimum epochs
history <- model %>% fit(x = train_ann,
                         y = train_ann$unemployment,
                         epochs = epoch,
                         validation_split = validation,
                         verbose = verbose,
                         callbacks = list(print_dot_callback))

# plot(history)

c(loss,mae,mape,mse) %<-% (model %>% evaluate(test_ann , test_ann$unemployment, verbose = 0))
xxx <- (model %>% evaluate(test_ann , test$unemployment, verbose = 0))

train_predictions <- model %>% predict(train_ann)
train_predictions[,1]
paste0("MAE: ", round(mae,2))
paste0("MAPE: ", round(mape,2),"%")
paste0("MSE: ", round(mse,2))

ggplot(train, aes(x = quarter, y=unemployment)) +
  geom_line() +
  geom_point() +
  geom_line(aes(y = train_predictions), color="red")+ #model output
  ggtitle(" ANN Model performance - Train data")+
  # scale_y_continuous(limits=c(3, 12))+
  theme_bw()

test_predictions <- model %>% predict(test_ann)
test_predictions[,1]
paste0("MAE: ", round(mae,2))
paste0("MAPE: ", round(mape,2),"%")
paste0("MSE: ", round(mse,2))

ggplot(test, aes(x = quarter, y=unemployment)) +
    geom_line() +
    geom_point() +
    geom_line(aes(y = test_predictions), color="red")+ #model output
    ggtitle(" ANN Model performance - Test data")+
    # scale_y_continuous(limits=c(3, 12))+
    theme_bw()


ann_end <- Sys.time()

ann_end - ann_time # Total run-time of 9.52 seconds
