# install.packages("tidyverse") 
# install.packages("tidymodels")
# install.packages("skimr")
# install.packages("vip")
# install.packages("GGally")
library(tidyverse)
library(tidymodels)
library(skimr)           
library(vip)             # Finding important features
library(GGally) 
library(caret)
library(dplyr)
library(tidyr)
library(ggplot2)

# 1. Load Data
bank_tb <- read_csv("C:/Users/valha/Stat473/UniversalBank.csv")
bank_tb <- bank_tb %>%
  rename(c('Personal_Loan'= 'Personal Loan',
           'CD_Account' = 'CD Account',
           'Securities_Account' = 'Securities Account'))

# 2. Preprocessing
bank_tb <- bank_tb %>%
  mutate(Personal_Loan = factor(Personal_Loan, 
                                levels = c(1, 0),                       
                                labels = c("Yes", "No")),
         Securities_Account = factor(Securities_Account, 
                                     levels = c(0,1),
                                     labels = c("No", "Yes")),
         CD_Account  = factor(CD_Account, 
                              levels = c(0,1),
                              labels = c("No", "Yes")),
         Online = factor(Online,
                         levels = c(0,1),
                         labels = c("No", "Yes")),
         CreditCard = factor(CreditCard,
                             levels = c(0,1),
                             labels = c("No", "Yes")),
         Education  = factor(Education ,
                             levels = c(1:3),
                             labels = c("Undergrad", 
                                        "Graduate", 
                                        "Professional")))

bank_tb <- bank_tb %>%
  select(-c(ID, `ZIP Code`))



# 3. Data Partition
set.seed(123) 
bank_split <- bank_tb %>%
  initial_split(prop = 0.7, strata = Personal_Loan) # Default = 75%/25%

train_data <- bank_split %>% training()
test_data  <- bank_split %>% testing()

train_data %>% 
  count(Personal_Loan) %>% 
  mutate(prop = n/sum(n))

test_data %>% 
  count(Personal_Loan) %>% 
  mutate(prop = n/sum(n))


# 4. Model (Later)
ctrl <- trainControl(classProbs = TRUE,savePredictions = TRUE,summaryFunction = twoClassSummary)
tree = train(Personal_Loan ~ ., 
                  data=train_data, 
                  method="rpart",trControl = ctrl,metric="ROC")

# 6. Model Test# 6. Model Testroc_auc()
tree %>%
  predict(test_data)

# 7. Prediction
predicted <- predict(tree, newdata = test_data)
confusionMatrix(predicted, test_data$Personal_Loan, mode = "prec_recall")

lift_result <- data.frame(truth = test_data$Personal_Loan)
lift_result$yes <- predict(tree, test_data, type = "prob")[,"Yes"]
lift_result$no <- predict(tree, test_data, type = "prob")[,"No"]
lift_result$predicted <- predict(tree, test_data)


gain_curve(lift_result, truth, yes) %>% autoplot()
roc_curve(lift_result, truth, yes) %>% autoplot()
lift_curve(lift_result, truth, yes) %>% autoplot()

#' #### From the Life graph, if we want to reach around 2 times as many patients with a heart problem as if we use no model, we can check approximately 30% of the data.

