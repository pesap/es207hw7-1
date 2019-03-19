# 9. How many site names in the CA air quality location dataset "Site Name" 
# contain "San" or "Santa?".

library(tidyverse)
library(readxl)

loc <- read_excel("data/ca_ozone/location.xls")

# The first step is to convert all elements of the column "Site Name" to lowercase letters
lower_col <- tolower(loc$`Site Name`)

# Then, calculate the times than the words "san" and "santa" appear.
san_count_total <- sum(str_detect(lower_col, "san"))
santa_count <- sum(str_detect(lower_col, "santa"))

# Since the word "san" is embedded in the word "santa", a subtraction must be made
san_count_net <-san_count_total-santa_count

# Finally, print the answers
san_count_net
santa_count
