
# 1st version data cleaning

library(lubridate)
library(dplyr)
setwd("~/UM/Biostatistics/Courses/STATS531/midproject")

cheese = read.csv("weekly-dairy-product-prices/Block_Cheddar_Cheese_40pounds.csv")
butter = read.csv("weekly-dairy-product-prices/Butter.csv")
whey = read.csv("weekly-dairy-product-prices/Dry_Whey.csv")
milk = read.csv("weekly-dairy-product-prices/Nonfat_Dry_Milk.csv")

data_process = function(rawdata){
  # fix the date time formats of three columns
  rawdata$Week.Ending.Date = as.Date(rawdata$Week.Ending.Date, format='%m/%d/%Y')
  rawdata$Report.Date = as.Date(rawdata$Report.Date, format='%m/%d/%Y')
  rawdata$Date = as.Date(rawdata$Date, format='%m/%d')
  year(rawdata$Date) = year(rawdata$Week.Ending.Date)
  # only need the date and weighted prices
  subsetdata = rawdata[,c("Date","Weighted.Prices")]
  # remove duplicate values
  removeduplicates = unique(subsetdata)
  rankeddata = removeduplicates[order(removeduplicates$Date),]
  # there are some values collected at the same date, so I aggregate them by averaging them
  summarized_rankeddata = aggregate(rankeddata$Weighted.Prices,list(rankeddata$Date), mean)
  colnames(summarized_rankeddata) = c("Date", "Price")
  return(summarized_rankeddata)
}

processed_butter = data_process(butter)
processed_cheese = data_process(cheese)
processed_milk = data_process(milk)
processed_whey = data_process(whey)

# combining all the columns
combined_data = processed_butter %>% inner_join(processed_cheese, by="Date") %>%
  inner_join(processed_whey, by="Date") %>%
  inner_join(processed_milk, by="Date")

colnames(combined_data) = c("date", "butter", "cheese", "whey", "milk")

# make sure that I have all the data points one week apart without gaps
dateranges = seq(combined_data$date[1], by="week", length = nrow(combined_data))
final_data = combined_data %>% filter(date %in% dateranges)

write.csv(final_data, "cleaned_data.csv", row.names=FALSE)

