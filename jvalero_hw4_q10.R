#10. Identify the number of sites that do not have a complete address 
# (full street address and zip code).

setwd("C:/Users/Jorge Valero/Desktop/d/spring_2019/eda/hw4")
loc <- read_excel("data/ca_ozone/location.xls")

# 1. Texts in "Address" column are converted into lowercase letters
loc$addr_low <- tolower(loc$Address)

# 2. Wrong address search. The selection criteria are:
loc$addr_check <- loc$addr_low=="address not known" |
  loc$addr_low=="location approximate" |
  loc$addr_low==""

# 3. ZIP code check
loc$zipc_check <- loc$`Zip Code`< 90000

# 4. Final result. Wrong address
loc$full <- loc$addr_check | loc$zipc_check
wrong_address1 <- sum(loc$full, na.rm = TRUE)
wrong_address2 <- sum(is.na(loc$full))
wrong_address <- wrong_address1+wrong_address2
wrong_address

#  From the 921 elements of the file, 404 do not have a correct address under the defined criteria