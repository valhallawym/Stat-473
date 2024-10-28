library(tidyverse)
library(tidymodels)
library(skimr)          
library(vip)            
library(GGally)
library(caret)
library(dplyr)
library(tidyr)
library(ggplot2)

#' ## 1. Load the data
heart_data <- read.csv("C:/Users/valha/Stat473/heart.csv") 

#' ## 2. Split features into input and output
#' ### Categories --> factor
heart_data <- heart_data %>%
  mutate(HeartDiseaseorAttack = factor(HeartDiseaseorAttack, 
                                levels = c(1, 0),                       
                                labels = c("Yes", "No")),
         Smoker = factor(Smoker, 
                         levels = c(1, 0),                       
                         labels = c("Yes", "No")),
         Education  = factor(Education),
         )

#' ### Selecting features needed
heart_df <- heart_data[, c("HeartDiseaseorAttack","BMI", "Smoker", "MentHlth", "Age", "Education", "Income")]

#' ### Selecting input features
input <- heart_df[, c("BMI", "Smoker", "MentHlth", "Age", "Education", "Income")]

#' ### Selecting the outcome variable
output <- heart_df$HeartDiseaseorAttack

#' ### Check the structure
str(input)
str(output)

#' ## 3. Split data into training and test data using 20% for the test data
set.seed(123) 
heart_split <- heart_df %>%
  initial_split(prop = 0.8) 

train_data <- heart_split %>% training()
test_data  <- heart_split %>% testing()

train_data %>% 
  count(HeartDiseaseorAttack) %>% 
  mutate(prop = n/sum(n))

test_data %>% 
  count(HeartDiseaseorAttack) %>% 
  mutate(prop = n/sum(n))

#' ## 4.Use DecisionTreeClassifier in Training
ctrl <- trainControl(classProbs = TRUE,savePredictions = TRUE,summaryFunction = twoClassSummary)
tree = train(HeartDiseaseorAttack ~ ., 
             data = train_data, 
             method="rpart", trControl = ctrl, metric="ROC")

#' ## 5.Confusion matrix and ROC curve
predicted <- predict(tree, newdata = test_data)
confusionMatrix(predicted, test_data$HeartDiseaseorAttack, mode = "prec_recall")

lift_result <- data.frame(truth = test_data$HeartDiseaseorAttack)
lift_result$yes <- predict(tree, test_data, type = "prob")[,"Yes"]
lift_result$no <- predict(tree, test_data, type = "prob")[,"No"]
lift_result$predicted <- predict(tree, test_data)


gain_curve(lift_result, truth, yes) %>% autoplot()
roc_curve(lift_result, truth, yes) %>% autoplot()

#' ## 6. Draw a lift curve and determine the percentage of 2 times patients comparing with no model
lift_curve(lift_result, truth, yes) %>% autoplot()

#' #### From the Life graph, if we want to reach around 2 times as many patients with a heart problem as if we use no model, we can check approximately 30% of the data.
