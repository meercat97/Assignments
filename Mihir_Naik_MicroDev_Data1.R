library(tidyverse)
library(srvyr)
library(dineq)
NSSO1_des <- NSSO1 %>% 
  as_survey_design(weights = wt_int)

#Question 1
#Part 1
mpce_cal_year <- NSSO1_des %>% 
  group_by(year) %>% 
  summarise(mpce = survey_mean(mpce, na.rm = T),
            cal_pcpd = survey_mean(cal_pcpd, na.rm = T)) %>% 
  select(-mpce_se, -cal_pcpd_se)


plot1 <- mpce_cal_year %>% 
  ggplot() +
  geom_line(aes(x = year, y = mpce, color = 'red'))+
  geom_line(aes(x = year, y = cal_pcpd, color = 'blue')) +
  ylab("MPCE (INR), Daily Per Capita Calorie Consmp. (kCal)")+
  xlab("Year")+
  scale_color_manual(labels = c("MPCE (INR)", "Daily Calorie Consumption (kCal)"), 
                     values = c("red", "blue"))+
  ggtitle("Monthly Per Capita Expenditure and Daily Calorie Consumption by Year")
plot1


#Part 2
NSSO1_des <- NSSO1_des %>% 
  mutate(foodshare = fe/te)


#Urban Exp Share
urban_2011_des <- NSSO1_des %>% 
  filter(year == 2011, urban  == 1) %>% 
  mutate(exp.decile = ntiles.wtd(te, 10, wt_int))

exp_share_2011_urban <- urban_2011_des %>% 
  group_by(exp.decile) %>% 
  summarise(foodshare = survey_mean(foodshare)) %>% 
  select(-foodshare_se)

#Rural Exp Share
rural_2011_des <- NSSO1_des %>% 
  filter(year == 2011, urban  == 0) %>% 
  mutate(exp.decile = ntiles.wtd(te, 10, wt_int))

exp_share_2011_rural <- rural_2011_des %>% 
  group_by(exp.decile) %>% 
  summarise(foodshare = survey_mean(foodshare)) %>% 
  select(-foodshare_se)

#bar plot for rural
rural_fe_plot <- exp_share_2011_rural %>% 
  ggplot(aes(x = exp.decile, y = foodshare))+
  geom_col(fill = 'plum')+
  xlab("Decile by Expenditure")+
  ylab("Share of Food Expenditure")+
  ggtitle("Share of Food Expenditure for Rural Regions")
rural_fe_plot

#bar plot for urban
urban_fe_plot <- exp_share_2011_urban %>% 
  ggplot(aes(x = exp.decile, y = foodshare))+
  geom_col(fill = 'plum')+
  xlab("Decile by Expenditure")+
  ylab("Share of Food Expenditure")+
  ggtitle("Share of Food Expenditure for Urban Regions")
urban_fe_plot

NSSO_2011 <- NSSO1 %>% filter(year == 2011)

#Part 3
#log-log regression
reg_data <- NSSO1 %>% 
  filter(year == 2011) %>% 
  select(te, cal) %>% 
  na.omit()
reg_data$te <- log(reg_data$te)
reg_data$cal <- log(reg_data$cal)
lm1 <- lm(log(cal) ~ log(te), data = reg_data)
summary(lm1)



#Question 2
#create bpl variable
urban_line_2011 = 1407
rural_line_2011 = 972
NSSO_2011 <- NSSO_2011 %>% mutate(poverty_line = ifelse(urban == 1, urban_line_2011, rural_line_2011))
NSSO_2011 <- NSSO_2011 %>% mutate(bpl = ifelse(mpce < poverty_line, "Yes", "No"))
NSSO_2011_des <- NSSO_2011 %>% as_survey_design(weights = wt_int)

#calculate poverty rate for rural and urban
poverty_rate <- NSSO_2011_des %>%
  group_by(urban) %>% 
  summarise(poverty_rate = survey_ratio(bpl == "Yes", bpl == "Yes" | bpl == "No"))

#by religion
poverty_rate_religion <- NSSO_2011_des %>%
  filter(!is.na(religion)) %>% 
  group_by(urban, religion) %>% 
  summarise(poverty_rate = survey_ratio(bpl == "Yes", bpl == "Yes" | bpl == "No"))

#by caste
poverty_rate_caste <- NSSO_2011_des %>%
  filter(!is.na(caste)) %>% 
  group_by(urban, caste) %>% 
  summarise(poverty_rate = survey_ratio(bpl == "Yes", bpl == "Yes" | bpl == "No"))

#1993
urban_line_1993 = 401
rural_line_1993 = 310
NSSO_1993 <- NSSO1 %>% filter(year == 1993)
NSSO_1993 <- NSSO_1993 %>% mutate(poverty_line = ifelse(urban == 1, urban_line_1993, rural_line_1993))
NSSO_1993 <- NSSO_1993 %>% mutate(bpl = ifelse(mpce < poverty_line, "Yes", "No"))
NSSO_1993_des <- NSSO_1993 %>% as_survey_design(weights = wt_int)

poverty_rate_1993 <- NSSO_1993_des %>%
  group_by(urban) %>% 
  summarise(poverty_rate = survey_ratio(bpl == "Yes", bpl == "Yes" | bpl == "No"))

#2004
urban_line_2004 = 810
rural_line_2004 = 540
NSSO_2004 <- NSSO1 %>% filter(year == 2004)
NSSO_2004 <- NSSO_2004 %>% mutate(poverty_line = ifelse(urban == 1, urban_line_2004, rural_line_2004))
NSSO_2004 <- NSSO_2004 %>% mutate(bpl = ifelse(mpce < poverty_line, "Yes", "No"))
NSSO_2004_des <- NSSO_2004 %>% as_survey_design(weights = wt_int)

poverty_rate_2004 <- NSSO_2004_des %>%
  filter(!is.na(urban)) %>% 
  group_by(urban) %>% 
  summarise(poverty_rate = survey_ratio(bpl == "Yes", bpl == "Yes" | bpl == "No", na.rm = T))


