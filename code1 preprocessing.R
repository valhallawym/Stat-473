library(tidyverse)
library(tidymodels)
library(skimr)          
library(vip)            
library(GGally)

# 2.Load Data
bank_tb <- read_csv("C:/Users/valha/Stat473/UniversalBank.csv", col_names = TRUE, na=".") %>% mutate_if(is.character, as.factor)
str(bank_tb)

# 3.Preprocessing

## 3.1 Change feature (variable) name
bank_tb <- bank_tb %>% 
      rename(c('Personal_Loan'= 'Personal Loan',
      'CD_Account' = 'CD Account',
      'Securities_Account' = 'Securities Account'))
str(bank_tb)

## 3.2 Categories --> factor
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
str(bank_tb)


## 3.3 Delete insignificant features
bank_tb <- bank_tb %>% select(-c(ID, `ZIP Code`))  
str(bank_tb)


bank_tb %>%
  skimr::skim() 

# base accuracy
bank_tb %>% 
  count(Personal_Loan) %>% 
  mutate(prop = n/sum(n))


bank_tb %>%
  ggpairs(columns = 1:5, 
          mapping = aes(color = Personal_Loan, 
                        alpha = 0.5))

bank_tb %>%
  select(Personal_Loan, Education:CreditCard) %>%
  pivot_longer(Education:CreditCard) %>%
  ggplot(mapping = aes(y = value, 
                       fill = Personal_Loan)) +
  geom_bar(position = "fill") +
  facet_wrap(vars(name), 
             scales = "free", 
             ncol = 2) +
  labs(x = NULL, y = NULL, fill = NULL)  


set.seed(123)

## Default = 75%
bank_split <- bank_tb %>%
  initial_split(prop = 0.7, strata = Personal_Loan)
bank_split

train_data <- bank_split %>%
  training()
test_data  <- bank_split %>%
  testing()
train_data %>% 
  count(Personal_Loan) %>% 
  mutate(prop = n/sum(n))
test_data %>% 
  count(Personal_Loan) %>% 
  mutate(prop = n/sum(n))

