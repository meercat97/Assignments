library(tidyverse)
library(stargazer)
wdi <- wdi %>% mutate(g1 =  1/10*log(wdi1970/wdi1960),
                      g2 = 1/10*log(wdi1975/wdi1965),
                      g3 = 1/10*log(wdi1980/wdi1970),
                      g4 = 1/10*log(wdi1985/wdi1975),
                      g5 = 1/10*log(wdi1990/wdi1980),
                      g6 = 1/10*log(wdi1995/wdi1985),
                      g7 = 1/10*log(wdi2000/wdi1990),
                      g8 = 1/10*log(wdi2005/wdi1995),
                      g9 = 1/10*log(wdi2010/wdi2000),
                      g10 = 1/10*log(wdi2015/wdi2005),
                      g11 = 1/8*log(wdi2018/wdi2010))
reg1960 <- lm(g1 ~ log(wdi1960), wdi) #g 1960-70, 1960 gdp
reg1965 <- lm(g2 ~ log(wdi1965), wdi) # g 1965-75, 1965 gdp
reg1970 <- lm(g3 ~ log(wdi1970), wdi) # g 1970-80, 1970 gdp
reg1975 <- lm(g4 ~ log(wdi1975), wdi) # g 1975-85, 1975 gdp
reg1980 <- lm(g5 ~ log(wdi1980), wdi) # g 1980-90, 1980 gdp
reg1985 <- lm(g6 ~ log(wdi1985), wdi) # g 1985-95, 1985 gdp
reg1990 <- lm(g7 ~ log(wdi1990), wdi) # g 1990-2000, 1990 gdp
reg1995 <- lm(g8 ~ log(wdi1995), wdi) # g 1995-2005, 1995 gdp
reg2000 <- lm(g9 ~ log(wdi2000), wdi) # g 2000-2010, 2000 gdp
reg2005 <- lm(g10 ~ log(wdi2005), wdi) #g 2005-15, 2005 gdp
reg2010 <- lm(g11 ~ log(wdi2010), wdi) #g 2010-18, 2010 gdp

stargazer(reg1960,
          reg1965,
          reg1970,
          reg1975, 
          reg1980, 
          reg1985, 
          reg1990, 
          reg1995, 
          reg2000, 
          reg2005,
          reg2010, 
          type = 'html', out = c('result1.html'))

year <- c(1960, 1965, 1970, 1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010)
beta <- c(0.009, 0.007, 0.003, -0.001, 0.001, 0.003, 0.002, -0.001, -0.006, -0.007, -0.003)
std_errors <- c(0.002, 0.007, 0.002, 0.002, 0.002, 0.003, 0.002, 0.002, 0.002, 0.001, 0.001)
betas_by_year <- data.frame(year, beta, std_errors)

#plot betas
beta_plot <- betas_by_year %>% ggplot(aes(x= year, y = beta)) + 
  geom_line()+
  geom_point() +
  ggtitle("Regression Betas by Year")
beta_plot
ggsave("betas_plot.png", plot = beta_plot)

wdi2 <- wdi2 %>% mutate(g1 = 1/20*log(wdi1980/wdi1960),
                        g2 = 1/20*log(wdi2000/wdi1980),
                        g3 = 1/18*log(wdi2018/wdi2000))

reg1960_1980 <-  lm(g1 ~ log(wdi1960), wdi2)
reg1980_2000 <-  lm(g2 ~ log(wdi1980), wdi2)
reg2000_2018 <- lm(g3 ~ log(wdi2000), wdi2)
                                           
stargazer(reg1960_1980, reg1980_2000, reg2000_2018, type = 'html',
          out = c('result2.html'))                                       
plot1960_80 <- wdi2 %>% ggplot(aes(x = log(wdi1960), y = g1)) +
  geom_point() +
  geom_smooth(method = lm)+
  ggtitle("Growth Rate (1960-1980) vs Per Capita GDP (1960)")+
  xlab("Log of GDP Per Capita")+
  ylab("Growth Rate")

plot1960_80
ggsave("Growth 1960-80.png", plot = plot1960_80)

plot1980_2000 <- wdi2 %>% ggplot(aes(x = log(wdi1980), y = g2)) +
  geom_point() +
  geom_smooth(method = lm) +
  ggtitle("Growth Rate (1980-2000) vs Per Capita GDP (1980)")+
  xlab("Log of GDP Per Capita")+
  ylab("Growth Rate")
plot1980_2000
ggsave("Growth 1980-2000.png", plot = plot1980_2000)

plot2000_2018 <- wdi2 %>% ggplot(aes(x = log(wdi2000), y = g3)) +
  geom_point() +
  ggtitle("Growth Rate (2000-2018) vs Per Capita GDP (2000)")+
  xlab("Log of GDP Per Capita")+
  ylab("Growth Rate")+
  geom_smooth(method = lm) 
plot2000_2018
ggsave("Growth 2000-2018.png", plot = plot2000_2018)

wdi3 <- wdi2 %>% filter(code != 'CHN')
plot1960_80_CHN <- wdi3 %>% ggplot(aes(x = log(wdi1960), y = g1)) +
  geom_point() +
  geom_smooth(method = lm)+
  ggtitle("Growth Rate (1960-1980) vs Per Capita GDP (1960) without China")+
  xlab("Log of GDP Per Capita")+
  ylab("Growth Rate")
plot1960_80_CHN
ggsave("Growth 1960-1980 China.png", plot = plot1960_80_CHN)

plot1980_2000_CHN <- wdi3 %>% ggplot(aes(x = log(wdi1980), y = g2)) +
  geom_point() +
  geom_smooth(method = lm)+
  ggtitle("Growth Rate (1980-2000) vs Per Capita GDP (1980) without China")+
  xlab("Log of GDP Per Capita")+
  ylab("Growth Rate")
plot1980_2000_CHN
ggsave("Growth 1980-2000 China.png", plot = plot1980_2000_CHN)

plot2000_2018_CHN <- wdi3 %>% ggplot(aes(x = log(wdi2000), y = g3)) +
  geom_point() +
  ggtitle("Growth Rate (2000-2018) vs Per Capita GDP (2000) without China")+
  xlab("Log of GDP Per Capita")+
  ylab("Growth Rate")+
  geom_smooth(method = lm)
plot2000_2018_CHN
ggsave("Growth 2000-2018 China.png", plot = plot2000_2018_CHN)

wdi4 <- wdi2 %>% filter(code != 'IND')
plot1960_80_IND <- wdi4 %>% ggplot(aes(x = log(wdi1960), y = g1)) +
  geom_point() +
  geom_smooth(method = lm)+
  xlab("Log of GDP Per Capita")+
  ylab("Growth Rate")+
  ggtitle("Growth Rate (1960-1980) vs Per Capita GDP (1960) without India")
plot1960_80_IND
ggsave("Growth 1960-1980 India.png", plot = plot1960_80_IND)

plot1980_2000_IND <- wdi4 %>% ggplot(aes(x = log(wdi1980), y = g2)) +
  geom_point() +
  geom_smooth(method = lm)+
  xlab("Log of GDP Per Capita")+
  ylab("Growth Rate")+
  ggtitle("Growth Rate (1980-2000) vs Per Capita GDP (1980) without India")
  
plot1980_2000_IND
ggsave("Growth 1980-2000 India.png", plot = plot1980_2000_IND)

plot2000_2018_IND <- wdi4 %>% ggplot(aes(x = log(wdi2000), y = g3)) +
  geom_point() +
  geom_smooth(method = lm)+
  xlab("Log of GDP Per Capita")+
  ylab("Growth Rate")+
  ggtitle("Growth Rate (2000-2018) vs Per Capita GDP (2000) without India")
plot2000_2018_IND
ggsave("Growth Rate 2000-2018 India.png", plot = plot2000_2018_IND)

wdi5 <- wdi2 %>% filter(code != 'CHN' & code != 'IND')
plot1960_80_IND_CHN <- wdi5 %>% ggplot(aes(x = log(wdi1960), y = g1)) +
  geom_point() +
  geom_smooth(method = lm)
plot1960_80_IND_CHN

plot1980_2000_IND_CHN <- wdi5 %>% ggplot(aes(x = log(wdi1980), y = g2)) +
  geom_point() +
  geom_smooth(method = lm)
plot1980_2000_IND_CHN

plot2000_2018_IND_CHN <- wdi5 %>% ggplot(aes(x = log(wdi2000), y = g3)) +
  geom_point() +
  geom_smooth(method = lm) 
plot2000_2018_IND_CHN


#regressions
reg1960_1980_CHN <-  lm(g1 ~ log(wdi1960), wdi3)
reg1980_2000_CHN <-  lm(g2 ~ log(wdi1980), wdi3)
reg2000_2018_CHN <- lm(g3 ~ log(wdi2000), wdi3)

stargazer(reg1960_1980_CHN, reg1980_2000_CHN, reg2000_2018_CHN, type = 'html',
          out = c('result_china.html'))

reg1960_1980_IND <-  lm(g1 ~ log(wdi1960), wdi4)
reg1980_2000_IND <-  lm(g2 ~ log(wdi1980), wdi4)
reg2000_2018_IND <- lm(g3 ~ log(wdi2000), wdi4)

stargazer(reg1960_1980_IND, reg1980_2000_IND, reg2000_2018_IND, type = 'html',
          out = c('result_indio.html'))

reg1960_1980_CHN_IND <-  lm(g1 ~ log(wdi1960), wdi5)
reg1980_2000_CHN_IND <-  lm(g2 ~ log(wdi1980), wdi5)
reg2000_2018_CHN_IND <- lm(g3 ~ log(wdi2000), wdi5)

stargazer(reg1960_1980_CHN_IND, reg1980_2000_CHN_IND, reg2000_2018_CHN_IND, type = 'html',
          out = c('result_india_china.html'))

