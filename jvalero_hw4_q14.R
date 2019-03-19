# 14. Write a function to calculate the ANNUAL (calendar year) mean, median, 
# max and min of all sites that have "San" or "Santa" in their name.

library(tidyverse)
library(readxl)
library(rlist)

# Loading data

loc <- read_excel("data/ca_ozone/location.xls")

# 1. Selection of the points that have "san" or "santa"
# Search the word "san".
# Since the word "san" is embedded in the word "santa", 
# only is necessary to search the word "san"

# Create a new column with lowercase of "Site Name"
loc$low_site_name <- tolower(loc$`Site Name`)
loc$filtro <- str_detect(loc$low_site_name, "san")
loc_san1 <- subset(loc$Site,loc$filtro==TRUE)
loc_san2<-loc_san1[complete.cases(loc_san1)]
loc_san3 <- tibble(site=loc_san2)


#2. Load ozone files
# Load files
library(readr)
p1 <- read_delim("ozone19801984.txt", delim = "|")
p2 <- read_delim("ozone19851989.txt", delim = "|")
p3 <- read_delim("ozone19901994.txt", delim = "|")
p4 <- read_delim("ozone19951999.txt", delim = "|")
p5 <- read_delim("ozone20002004.txt", delim = "|")
p6 <- read_delim("ozone20052009.txt", delim = "|")
p7 <- read_delim("ozone20102011.txt", delim = "|")
consolidado1 <- rbind(p1,p2,p3,p4,p5,p6,p7)

# Use of semi_join function to select sites that contains "San" or "Santa"
consolidado2 <- consolidado1 %>% semi_join(loc_san3, by = "site")


# Yearly summary function
year =substr(consolidado2 $date,1,4)
new_list <- cbind(consolidado2 ,year) # Adiciona la columna year


rta <- new_list %>%
  group_by(site = as.factor(site), year) %>%
  summarize(Min = min(obs, na.rm = TRUE),
            Max = max(obs, na.rm = TRUE),
            Mean = mean(obs, na.rm = TRUE),
            Median = median(obs, na.rm = TRUE))


rta # Print the answer
