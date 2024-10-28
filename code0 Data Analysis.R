library(tidyverse)
library(tidymodels)
library(skimr)           
library(GGally)  

bank_tb <- read_csv("C:/Users/valha/Stat473/UniversalBank.csv")
str(bank_tb)
bank_tb

bank_tb %>%
  skimr::skim() 

bank_tb %>%
  rename('Personal_Loan'= 'Personal Loan') %>%
  group_by(Personal_Loan) %>%
  skimr::skim() 

bank_tb %>% 
  rename('Personal_Loan'= 'Personal Loan') %>%
  count(Personal_Loan) %>% 
  mutate(prop = n/sum(n))

bank_tb %>%
  rename('Personal_Loan'= 'Personal Loan') %>%
  select(Personal_Loan, Income:CCAvg) %>%
  ggpairs(columns = 2:3, 
          mapping = aes(color = Personal_Loan, alpha = 0.5))









