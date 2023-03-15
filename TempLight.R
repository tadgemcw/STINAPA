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

### Reviewing 2020 Temp Data for Outliers
summary(tl2020$temp_c)

ggplot(tl2020, aes(temp_c)) + 
  stat_boxplot(geom ='errorbar', width = 0.2) + 
  geom_boxplot() +
  xlab("Water Temp") +
  ggtitle("2020 Data")
  
ggplot(tl2020, aes(x=factor(month(date)), y = temp_c)) + 
    stat_boxplot(geom ='errorbar', width = 0.2) + 
    geom_boxplot() +
    xlab("Month") + 
    ylab("Water Temp") +
    ggtitle("2020 Data")
  
### Reviewing 2021 Temp Data for Outliers
summary(tl2021$temp_c)

ggplot(tl2021, aes(temp_c)) + 
  stat_boxplot(geom ='errorbar', width = 0.2) + 
  geom_boxplot() +
  xlab("Water Temp") +
  ggtitle("2021 Data")

ggplot(tl2021, aes(x=factor(month(date)), y = temp_c)) + 
  stat_boxplot(geom ='errorbar', width = 0.2) + 
  geom_boxplot() +
  xlab("Month") + 
  ylab("Water Temp") +
  ggtitle("2021 Data")

### Reviewing 2022 Temp Data for Outliers
summary(tl2022$temp_c)

ggplot(tl2022, aes(temp_c)) + 
  stat_boxplot(geom ='errorbar', width = 0.2) + 
  geom_boxplot() +
  xlab("Water Temp") +
  ggtitle("2022 Data")

ggplot(tl2022, aes(x=factor(month(date)), y = temp_c)) + 
  stat_boxplot(geom ='errorbar', width = 0.2) + 
  geom_boxplot() +
  xlab("Month") + 
  ylab("Water Temp") +
  ggtitle("2022 Data")



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