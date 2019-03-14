# 15. Write a function to caculate the annual daily mean 
# (what is the annual mean of the daily mean?). 
# Apply that function to Merced County. 
# What is the annual daily mean of o3 for Merced County? 
# Report your results in quantititive format (i.e., prose, or a table), 
# and in visual format (i.e., a graph). 

library(tidyverse)
library(readxl)
library(rlist)
library(data.table)

# Loading data
setwd("C:/Users/Jorge Valero/Desktop/d/spring_2019/eda/hw4")
loc <- read_excel("data/ca_ozone/location.xls")


#1. Selec of the Merced county
merced_code1 <- subset(loc$Site,loc$`County Name`=="Merced")
merced_code2<-merced_code1[complete.cases(merced_code1)]
merced_code3 <- tibble(site=merced_code2)

#2. Load ozone files
# Load files
setwd("C:/Users/Jorge Valero/Desktop/d/spring_2019/eda/hw4/data/ca_ozone")
library(readr)
p1 <- read_delim("ozone19801984.txt", delim = "|")
p2 <- read_delim("ozone19851989.txt", delim = "|")
p3 <- read_delim("ozone19901994.txt", delim = "|")
p4 <- read_delim("ozone19951999.txt", delim = "|")
p5 <- read_delim("ozone20002004.txt", delim = "|")
p6 <- read_delim("ozone20052009.txt", delim = "|")
p7 <- read_delim("ozone20102011.txt", delim = "|")
consolidado1 <- rbind(p1,p2,p3,p4,p5,p6,p7)


# Use of semi_join function to select sites in Merced county
consolidado2 <- consolidado1 %>% semi_join(merced_code3, by = "site")

# Average per day
consolidado_day <- consolidado2 %>%
  group_by(site = as.factor(site), date) %>%
  summarize(daily_mean = mean(obs, na.rm = TRUE))

# Auxiliary variables
year =substr(consolidado_day$date,1,4)
new_list <- cbind(consolidado_day,year=year) # Adiciona la columna year

# Average per year
consolidado_year <- new_list %>%
  group_by(site,year) %>%
  summarize(year_mean = mean(daily_mean, na.rm = TRUE))

# Ozone annual daily mean
consolidado_year

sites_merced <- unique(consolidado_year$site)
sites_merced

# Only three sites have "ozone annual daily mean in Merced"

site2046 <- subset(consolidado_year,consolidado_year$site==2046)
site2084 <- subset(consolidado_year,consolidado_year$site==2084)
site3022 <- subset(consolidado_year,consolidado_year$site==3022)

# Graphs for each site
par(mfrow=c(2,2)) # Definicion del numero de graficas por hoja
plot(site2046$year,site2046$year_mean,
     main="Ozone annual daily mean. Site 2046",
     xlab="Year",
     ylab="Mean")

plot(site2084$year,site2084$year_mean,
     main="Ozone annual daily mean. Site 2084",
     xlab="Year",
     ylab="Mean",
     xlim = c(1981,1982))

plot(site3022$year,site3022$year_mean,
     main="Ozone annual daily mean. Site 3022",
     xlab="Year",
     ylab="Mean")