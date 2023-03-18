library(readxl)
library(tidyverse)
library(lubridate)

setwd("~/Documents/Data Science/STINAPA/Temp & Light Project")
tl2020 <- read_excel("2020 raw.xlsx", sheet = 1)
tl2021 <- read_excel("2021 raw.xlsx", sheet = 1)
tl2022 <- read_excel("2022 raw.xlsx", sheet = 1)


### Overview of data collected
tl2020 %>% group_by(depth_m) %>%
  ggplot(aes(site, fill = factor(depth_m))) + geom_bar(position = "dodge") +
  xlab("Location") +
  ylab("Data Points") +
  labs(fill = "Depth (m)") +
  ggtitle("2020 Data Collected")

tl2021 %>% group_by(depth_m) %>%
  ggplot(aes(site, fill = factor(depth_m))) + geom_bar(position = "dodge") +
  ylab("Data Points") +
  xlab("Location") +
  labs(fill = "Depth (m)") +
  ggtitle("2021 Data Collected")

tl2022 %>% group_by(depth_m) %>%
  ggplot(aes(site, fill = factor(depth_m))) + geom_bar(position = "dodge") +
  ylab("Data Points") +
  xlab("Location") +
  labs(fill = "Depth (m)") +
  ggtitle("2022 Data Collected")

########### Reviewing 2020 Temp Data for Outliers
summary(tl2020$temp_c)

ggplot(tl2020, aes(temp_c)) + 
  stat_boxplot(geom ='errorbar', width = 0.2) + 
  geom_boxplot() +
  xlab("Water Temp") +
  ggtitle("2020 Data")

ggplot(tl2020, aes(temp_c)) +
  geom_bar(binwidth = 0.5) + 
  scale_x_continuous(limits = c(25, 35)) +
  facet_wrap(~depth_m) +
  ggtitle("2020 Distribution")

quant2020 <- quantile(tl2020$temp_c, probs = c(0.25, 0.75))

tl2020_capped <- tl2020 %>% filter(temp_c <= (quant2020[2] + (1.5 * IQR(temp_c))))
tl2020_outliers <- tl2020 %>% filter(temp_c > (quant2020[2] + (1.5 * IQR(temp_c)))) 
tl2020_outliers %>% group_by(date) %>%
  summarize(avg_temp = mean(temp_c)) %>%
  arrange(date)

ggplot(tl2020, aes(x = temp_c)) + 
  geom_bar() + 
  geom_vline(xintercept = 31.883) +
      annotate("text", x = 40, y = 40000, label = "Statistical Outlier") +
  xlab("Water Temp") +
  ggtitle("2020 Distribution")

tl2020 %>% filter(month(date) == 9) %>%
  summarize(max_temp = max(temp_c))

ggplot(tl2020, aes(x=factor(month(date)), y = temp_c)) + 
    stat_boxplot(geom ='errorbar', width = 0.2) + 
    geom_boxplot() +
    xlab("Month") + 
    ylab("Water Temp") +
    ggtitle("2020 Data") +
    geom_hline(yintercept = 33, col = "red") + 
      annotate("text", x = 9, y = 36, label = "33 degrees", col = "red") +
    geom_hline(yintercept = 31.623, col = "blue") +
      annotate("text", x = 5, y = 34, label = "statistical outlier", col = "blue")

tl2020_33 <- tl2020 %>% filter(temp_c <= 33)
  
#################### Reviewing 2021 Temp Data for Outliers
summary(tl2021$temp_c)

ggplot(tl2021, aes(temp_c)) + 
  stat_boxplot(geom ='errorbar', width = 0.2) + 
  geom_boxplot() +
  xlab("Water Temp") +
  ggtitle("2021 Data")

ggplot(tl2021, aes(temp_c)) +
  geom_bar(binwidth = 0.5) + 
  scale_x_continuous(limits = c(24, 35)) +
  facet_wrap(~depth_m) +
  ggtitle("2021 Distribution")

quant2021 <- quantile(tl2021$temp_c, probs = c(0.25, 0.75))

tl2021_capped <- tl2021 %>% filter(temp_c < (quant2021[2] + (1.5 * IQR(temp_c))))
tl2021_outliers <- tl2021 %>% filter(temp_c >= (quant2021[2] + (1.5 * IQR(temp_c)))) 
tl2021_outliers %>% group_by(date) %>%
  summarize(avg_temp = mean(temp_c)) %>%
  arrange(date) %>% print(n=100)

ggplot(tl2021, aes(x = temp_c)) + 
  geom_bar() + 
  geom_vline(xintercept = 31.623) + 
    annotate("text", x = 38, y = 30000, label = "Statistical Outlier") +
  xlab("Water Temp") +
  ggtitle("2021 Distribution")

ggplot(tl2021, aes(x=factor(month(date)), y = temp_c)) + 
  stat_boxplot(geom ='errorbar', width = 0.2) + 
  geom_boxplot() +
  xlab("Month") + 
  ylab("Water Temp") +
  ggtitle("2021 Data") +
  geom_hline(yintercept = 33, col = "red") + 
  annotate("text", x = 2, y = 36, label = "33 degrees", col = "red") +
  geom_hline(yintercept = 31.623, col = "blue") +
  annotate("text", x = 3, y = 34, label = "statistical outlier", col = "blue")

tl2021 %>% filter(month(date) == 9 & temp_c < 40) %>%
  summarize(max_temp_below_40 = max(temp_c))

tl2021_33 <- tl2021 %>% filter(temp_c <= 33)

############## Reviewing 2022 Temp Data for Outliers
summary(tl2022$temp_c)

ggplot(tl2022, aes(temp_c)) + 
  stat_boxplot(geom ='errorbar', width = 0.2) + 
  geom_boxplot() +
  xlab("Water Temp") +
  ggtitle("2022 Data")

ggplot(tl2022, aes(temp_c)) +
  geom_bar(binwidth = 0.5) + 
  scale_x_continuous(limits = c(24, 35)) +
  facet_wrap(~depth_m) +
  ggtitle("2022 Distribution")

quant2022 <- quantile(tl2021$temp_c, probs = c(0.25, 0.75))

tl2022_capped <- tl2022 %>% filter(temp_c < (quant2022[2] + (1.5 * IQR(temp_c))))
tl2022_outliers <- tl2022 %>% filter(temp_c >= (quant2022[2] + (1.5 * IQR(temp_c)))) 
tl2022_outliers %>% group_by(date) %>%
  summarize(avg_temp = mean(temp_c)) %>%
  arrange(date) %>%
  print(n=100)

ggplot(tl2022, aes(x = temp_c)) + 
  geom_bar() + 
  geom_vline(xintercept = 31.929) + 
  annotate("text", x = 28, y = 18000, label = "Statistical Outlier") +
  xlab("Water Temp") +
  ggtitle("2022 Distribution")

ggplot(tl2022_outliers, aes(temp_c)) + 
  geom_bar() +
  ggtitle("2022 Distribution of Statistical Outliers") 

ggplot(tl2022, aes(x=factor(month(date)), y = temp_c)) + 
  stat_boxplot(geom ='errorbar', width = 0.2) + 
  geom_boxplot() +
  xlab("Month") + 
  ylab("Water Temp") +
  ggtitle("2022 Data") +
  geom_hline(yintercept = 33, col = "red") + 
    annotate("text", x = 9, y = 34, label = "33 degrees", col = "red") +
  geom_hline(yintercept = 31.929, col = "blue") +
    annotate("text", x = 10, y = 32.5, label = "statistical outlier", col = "blue")


tl2022 %>% filter(month(date) == 8) %>%
  summarize(max_temp_below_40 = max(temp_c))

tl2022 %>% filter(temp_c < 15) %>% group_by(date, depth_m, site) %>% 
  summarize(mean_d = mean(temp_c))

tl2022_33 <- tl2022 %>% filter(temp_c <= 33)

#### Simplifying Dates and Times
tl2020$date <- as.Date(tl2020$date)
tl2020$time <- format(as.POSIXct(
  tl2020$time),format = "%H:%M:%S")




### Reviewing Temp Outliers
tl2020 %>% filter(temp_c < 20)
tl2020 %>% filter(temp_c > 35)

#### monthly temp avgs.  Need to deal with outliers.
tl2020 %>%
  mutate(month = month(date)) %>% 
  group_by(month) %>%
  summarize(mean = mean(temp_c))
  

### Reviewing Light Data
summary(tl2020$light)
ggplot(tl2020, aes(light)) + geom_boxplot()


#### Time Series for Temp
ggplot(tl2020, aes(date, temp_c, color = site)) +
  geom_line()

tl2020 %>% 
  filter(date >= "2020-07-15" & date <= "2020-07-31") %>%
  ggplot(aes(date, temp_c, color = site)) +
      geom_line()

tl2020 %>% 
  filter(date >= "2020-07-15" & date <= "2020-07-31", site == "HarbourVillage") %>%
  group_by(depth_m) %>%
  ggplot(aes(date, temp_c, color = factor(depth_m))) +
  geom_line()



### Looking at Time Series for Light
tl2020 %>% 
  filter(date >= "2020-07-15" & date <= "2020-07-31", site == "HarbourVillage") %>%
  group_by(depth_m) %>%
  ggplot(aes(date, light, color = factor(depth_m))) +
  geom_line()

kdata <- tl2020 %>% 
  filter(date >= "2020-08-15" & date <= "2020-11-30",
         time >= "10:00:00" & time <= "15:00:00",
         site == "HarbourVillage", 
         depth_m == 6) %>%
  select(temp_c, light)

kdata_scaled <- as_tibble(scale(kdata))
kmodel <- kmeans(kdata_scaled, centers = 2, nstart = 20)
kdata_scaled$cluster <- kmodel$cluster

                 
ggplot(kdata_scaled, aes(temp_c, light, color = cluster)) +
  geom_point() 