library(tidyverse)
library(sandwich)
library(lmtest)
library(ggtext)
library(stargazer)
library(ggshowtext)
library(ggrepel)
library(fixest)
wdi_1<- wdi_struc_change_assignment %>% filter(not_high_inc == 1, not_tiny == 1, year == 2015) %>% 
  mutate(log_gdp_pc_ppp = log(gdp_pc_ppp))
# part1
plot7 <- wdi_1 %>% ggplot(aes(x = log_gdp_pc_ppp, y = agri_gdp_sh)) + geom_point() +
  xlab("Log of GDP Per Capita (PPP) for 2015") +
  ylab("Agriculture's share of GDP ")+
  ggtitle("Agriculture's Share of GDP vs Log GDP Per Capita (PPP) (2015)")
plot7
ggsave("graph1.png", plot = plot7)
plot1 <- wdi_1 %>% ggplot(aes(x = log_gdp_pc_ppp, y = agri_gdp_sh, label = CountryCode)) +
  geom_text_repel(point.size = NA, max.overlaps = 50) + 
  geom_point(data = ~filter(.x, CountryCode == "IND"), colour = "red") +
  geom_smooth(method = 'lm')+
  xlab("Log of GDP Per Capita (PPP) for 2015") +
  ylab("Agriculture's share of GDP ")+
  ggtitle("Agriculture's Share of GDP vs Log GDP Per Capita (PPP) (2015)")
plot1
ggsave("graph1.png", plot = plot1)
lm1 <- lm(agri_gdp_sh ~ log_gdp_pc_ppp,wdi_1)
stargazer(lm1, type = 'html', out = c('1_agri gdp share vs log gdp pc.html'))
plot2 <- wdi_1 %>% ggplot(aes(x = log_gdp_pc_ppp, y = agri_emp_sh, label = CountryCode)) +
  geom_text_repel(point.size = NA, max.overlaps = 50) + 
  geom_point(data = ~filter(.x, CountryCode == "IND"), colour = "red") +
  geom_smooth(method = 'lm')+
  xlab("Log of GDP Per Capita (PPP) for 2015")+
  ylab("Agriculture's share of employment")+
  ggtitle("Agriculture's Share of Employment vs Log GDP Per Capita (PPP) (2015)")
ggsave("graph2.png", plot = plot2)
plot2
lm2 <- lm(agri_emp_sh ~ log_gdp_pc_ppp, wdi_1)
stargazer(lm2, type = 'html', out = c('2_agri emp share vs log gdp pc.html'))

plot8 <- wdi_1 %>% ggplot(aes(x = log_gdp_pc_ppp, y = self_emp_sh)) + geom_point()+
  xlab("Log of GDP Per Capita (PPP) for 2015")+
  ylab("Self Employment Share")+
  ggtitle("Self Employment Share vs. Log of GDP Per Capita (PPP) (2015)")
ggsave("graph8.png", plot = plot8)
#part2
plot3 <- wdi_1 %>% ggplot(aes(x = log_gdp_pc_ppp, y = self_emp_sh, label = CountryCode)) +
  geom_text_repel(point.size = NA, max.overlaps = 50) + 
  geom_point(data = ~filter(.x, CountryCode == "IND"), colour = "red") +
  geom_smooth(method = 'lm')+
  xlab("Log of GDP for 2015")+
  ylab("Self Employment Share")+
  ggtitle("")
plot3
ggsave("graph5.png", plot = plot3)

plot9 <- 
lm3 <- lm(self_emp_sh ~ log_gdp_pc_ppp, wdi_1)
stargazer(lm3, type = 'html', out = c('3_self emp share vs log gdp pc.html'))

wdi_2 <- wdi_struc_change_assignment %>% mutate(log_gdp_pc_ppp = log(gdp_pc_ppp)) %>%
  filter(not_high_inc == 1, not_tiny == 1, CountryName %in%  c("India","Vietnam"))

plot4 <- wdi_2 %>% ggplot(aes(x = year, y = agri_emp_sh, colour = CountryCode)) +
  geom_line()
plot4
ggsave("ind_vnm_agri_emp_share.png", plot = plot4)

plot5 <- wdi_2 %>% ggplot(aes(x = year, y = self_emp_sh, colour = CountryCode)) +
  geom_line()
plot5
ggsave("ind_vnm_self_emp_share.png", plot = plot5)
#fixed effects
wdi_3 <- wdi_struc_change_assignment %>% filter(not_tiny == 1, not_high_inc == 1) %>% 
  mutate(log_gdp_pc_ppp = log(gdp_pc_ppp))
lm4 <- feols(self_emp_sh ~ log_gdp_pc_ppp|CountryCode, wdi_3)
summary(lm4)

lm5 <- feols(agri_emp_sh ~ log_gdp_pc_ppp|CountryCode, wdi_3)
summary(lm5)
