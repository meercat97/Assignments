library(tidyverse)
library(stargazer)
class(IHDS1$RO3)
IHDS1$RO3 <- factor(IHDS1$RO3, levels = c(1,2), labels = c("Male", "Female"))
IHDS1$ID13 <- factor(IHDS1$ID13, levels = c(1,2,3,4,5,6), labels = c("Brahmin", "General", "OBC", "SC",
                                                                   "ST", "Others"))
IHDS1$ID11 <- factor(IHDS1$ID11, levels = c(1,2,3,4,5,6,7,8,9),
                     labels = c("Hindu", "Muslim", "Christian", "Sikh", "Buddhist", "Jain", "Tribal",
                                "Others", "None"))

IHDS1$RO3New <- IHDS1$RO3
levels(IHDS1$RO3New) <- c(0,1)
#Question 1.1-1.2
avg_earnings_sex <- IHDS1 %>%
  filter(!is.na(RO3)) %>% 
  group_by(RO3) %>% summarise(avg_earnings = weighted.mean(WSEARN, WT, na.rm = T))
stargazer(avg_earnings_sex,
          summary = F,
          type = 'html',
          title = 'Mean Per Capita Annual Income by Gender (In Rs.)',
          covariate.labels = c("S. No", "Gender", "Annual Income"),
          out = 'table1.html',
          digits = 1)

avg_earnings_religion <- IHDS1 %>% 
  filter(!is.na(ID11)) %>% 
  group_by(ID11) %>% summarise(avg_earnings = weighted.mean(WSEARN, WT, na.rm = T))
stargazer(as.data.frame(avg_earnings_religion),
          summary = F,
          type = 'html',
          title = 'Mean Per Capita Annual Income by Religion (In Rs.)',
          out = "table2.html",
          covariate.labels = c("S. No", "Religion", "Annual Income"),
          digits = 1)

avg_earnings_caste <- IHDS1 %>% 
  filter(!is.na(ID13)) %>% 
  group_by(ID13) %>% summarise(avg_earnings = weighted.mean(WSEARN, WT, na.rm = T))

stargazer(as.data.frame(avg_earnings_caste),
          summary = F,
          title = 'Mean Per Capita Annual Income by Caste (In Rs.)',
          covariate.labels = c("S. No", "Caste", "Annual Income"),
          type = 'html',
          out = 'table3.html',
          digits = 1)

avg_earnings_activity <- IHDS1 %>% 
  filter(!is.na(RO7)) %>% 
  group_by(RO7) %>% summarise(avg_earnings = weighted.mean(WSEARN, WT, na.rm = T))

stargazer(as.data.frame(avg_earnings_activity),
          summary = F,
          title = 'Mean Per Capita Annual Income by Primary Activity (In Rs.)',
          covariate.labels = c("S. No", "Primary Activity", "Annual Income"),
          out = 'table4.html',
          type = 'html',
          digits = 1)

#Question 1.3, 1.4

reg1 <- lm(log(WSEARN) ~ RO3New, IHDS1, weights = WT)

reg2 <- lm(log(WSEARN) ~ RO3New + RO5 + I(RO5^2) + ED6, IHDS1, weights = WT)
stargazer(reg1,
          type = 'html',
          out = c("Regression 1.html"),
          title = "Regression Results: Log Earnings with Gender Dummy")
stargazer(reg2,
          type = 'html',
          out = c("Regression 2.html"),
          title = "Regression Results: Log Earnings with Gender, Education and Age")

reg3 <- lm(log(WSEARN) ~ RO3 + factor(ID11) + RO5 + I(RO5^2) + ED6, IHDS1, weights = WT)
stargazer(reg3)

reg4 <- lm(log(WSEARN) ~ RO3 + factor(ID11) + factor(ID13) + RO5 + I(RO5^2) + ED6, IHDS1, weights = WT)
stargazer(reg4)

reg5 <- lm(log(WSEARN) ~ RO3 + factor(ID11) + factor(ID13) + factor(RO7) + RO5 + I(RO5^2) + ED6, IHDS1,
           weights = WT)
stargazer(reg3, reg4, reg5, type = 'html',
          title = "Adding Religion, Caste and Primary Activity Status",
          out = "Regression 3.html")
stargazer(reg1, reg2, type = 'text')

#Question 2
IHDS1_casual <- IHDS1 %>% filter(INCNONAG > 0)
reg6 <- lm(log(INCNONAG) ~ ED6 + RO5 + I(RO5^2), IHDS1_casual, weights = WT)
stargazer(reg6, type = 'text')

IHDS1_salary <- IHDS1 %>% filter(INCSALARY > 0)
reg7 <- lm(log(INCSALARY) ~ ED6 + RO5 + I(RO5^2), IHDS1_salary, weights = WT)
stargazer(reg6, reg7, type = 'html',
          out = 'regression 4.html',
          title = "Income Regressed with Education for Casual and Salaried Workers")

IHDS1_casual_male <- IHDS1 %>% filter(INCNONAG > 0, RO3 == 'Male')
reg8 <- lm(log(INCNONAG) ~ ED6 + RO5 + I(RO5^2), IHDS1_casual_male, weights = WT)
stargazer(reg8, type = 'text')

IHDS1_casual_female <- IHDS1 %>% filter(INCNONAG > 0, RO3 == 'Female')
reg9 <- lm(log(INCNONAG) ~ ED6 + RO5 + I(RO5^2), IHDS1_casual_female, weights = WT)
stargazer(reg8, reg9, type = 'html',
          title = "Income Regressed with Education for Casual Workers by Gender -- Male and Female Respectively",
          out = "regression5.html")

IHDS1_salary_male <- IHDS1 %>% filter(INCSALARY > 0, RO3 == 'Male')
reg10 <-  lm(log(INCSALARY) ~ ED6 + RO5 + I(RO5^2), IHDS1_salary_male, weights = WT)
stargazer(reg10, type = 'text')

IHDS1_salary_female <- IHDS1 %>% filter(INCSALARY > 0, RO3 == 'Female')
reg11 <-  lm(log(INCSALARY) ~ ED6 + RO5 + I(RO5^2), IHDS1_salary_female, weights = WT)
stargazer(reg10, reg11, type = 'html',
          out = 'regression6.html',
          title = 'Income Regressed with Education for Salaried Workers by Gender -- Male and Female Respectively')

IHDS1_casual_education <- IHDS1 %>% filter(INCNONAG > 0, ED6 > 10)
reg12 <- lm(log(INCNONAG) ~ ED6 + RO5 + I(RO5^2), IHDS1_casual_education, weights = WT)
stargazer(reg6, reg12, type = 'text')
stargazer(reg12, type = 'text')

IHDS1_salary_education <- IHDS1 %>% filter(INCSALARY > 0, ED6 > 10)
reg13 <- lm(log(INCSALARY) ~ ED6 + RO5 + I(RO5^2), IHDS1_salary_education, weights = WT)
stargazer(reg7, reg13, type = 'text')
stargazer(reg12, reg13, type = 'html',
          out = 'regression7.html',
          title = 'Income Regressed with Education for Casual and Salaried Workers, Years of Education > 10')
