install.packages("stringr")
library(stringr)
install.packages("ggplot2")
library(ggplot2)
install.packages("tidyverse")
library(tidyverse)
install.packages("dplyr")
library(dplyr)
library(readr)

## Exploring dataset
rental_df = read.csv("COURSERA DA/Rental Pricing.csv")
rentaldrop_df = na.omit(rental_df) ## works for entire dataframe. not just specific column

colnames(rental_df)
head(rental_df)
str(rental)
View(rental_df)

## Checking empty cells
emptycell <- colSums(is.na(rental_df)) ##for numerical data, only 2 empty cells are observed from monthly_rent




####### plotting graph ##########

ggplot (data = rental_df) +
  geom_point(mapping = aes(x = size, y = monthly_rent, color = property_type)) 
### outliers has skewed the plot, to remove outliers


# Filtered dataframe with selected columns using pipelines
filteredrental_df = rental_df %>%
  filter(monthly_rent <= 5000 & size <= 5000)

ggplot (data = filteredrental_df) +
  geom_point(mapping = aes(x = size, y = monthly_rent, color = property_type)) +
  facet_wrap(~region)

aptrental_df = filteredrental_df %>%
  filter(property_type %in%  c("Apartment", "Condominium", "Service Residence", "Flat", "Houses"))

ggplot (data = aptrental_df) +
  geom_point(mapping = aes(x = size, y = monthly_rent, color = property_type)) +
  facet_wrap(~region)

######### data processing - near KTM ########


##### data processing using boxplot ########
##### identifying outliers in RENT (using IQR) #####

rentsummary = summary(rental_df$monthly_rent)
q1 = rentsummary["1st Qu."]
median = rentsummary['Median']
q3 = rentsummary["3rd Qu."]

lower_limit = q1 - 1.5 * IQR(rental_df$monthly_rent, na.rm = TRUE)
upper_limit = q3 + 1.5 * IQR(rental_df$monthly_rent, na.rm = TRUE)

par(mfrow = c(1, 2))
boxplot(rental_df$monthly_rent, main = "Boxplot of Monthly Rent", ylab = "Monthly Rent")
boxplot(rental_df$monthly_rent, main = "Boxplot of Monthly Rent", ylab = "Monthly Rent", ylim = c(lower_limit, upper_limit)) +
  labs(title = "Defining outliers")

ggplot(data = rental_df) +
  geom_boxplot(mapping = aes(y = monthly_rent)) +
  labs(title = "Outliers for Monthly Rent")

ggplot(data = rental_df) +
  geom_boxplot(mapping = aes(y = monthly_rent)) +
  labs(title = "Outliers for Monthly Rent") +
  coord_cartesian(ylim = c(lower_limit, upper_limit)
                  
                  
                  ####### boxplot of rent vs furnished ########33
                  
                  #to omit empty cells in column 'furnished'
                  rentaltry_df = rental_df %>%
                    filter(!(furnished %in% c("Fully Furnished", "Not Furnished", "Partially Furnished")))
                  table(rental_df$furnished) #sanity check to see any missing values in furnished column
                  
                  median_data = filteredrental %>%
                    group_by(furnished) %>%
                    summarize(median_rent = median(monthly_rent))
                  
                  median_text = paste("Median Rent for", paste(median_data$furnished, median_data$median_rent, sep = " = "))
                  
                  filteredrental %>%
                    select(furnished, monthly_rent) %>%
                    ggplot(aes(x = furnished, y = monthly_rent, fill = furnished)) +
                    geom_boxplot() +
                    labs(title = "Boxplot of Monthly Rent with Furnished Condition",
                         subtitle = paste(median_text)) +
                    theme(plot.margin = margin(b=10))
                  
                  
                  #### boxplot of rent vs region ###
                  table(rental_df$region)
                  rental_df %>%
                    select(region, monthly_rent) %>%
                    ggplot(aes(x = region, y = monthly_rent, fill = region)) +
                    geom_boxplot() 
                  
                  
                  ### distribution of location ####
                  location_count = (table(rental_df$district)) %>%
                    as.data.frame() %>%
                    arrange(desc(Freq))
                  
                  location_count %>%
                    filter(Freq >= 100) %>%
                    ggplot(aes(x = Freq, y = Var1, size = Freq, color = Freq)) +
                    geom_point() +
                    labs(title = "Frequency of Locations", x = "Location", y = "Frequency") +
                    theme_bw() +
                    theme(axis.text.y = element_text(size = 8))
                  
                  
                  locationsort_df = location_df %>%
                    arrange(desc(frequency))
                  
                  location_df %>%
                    filter(frequency >= 100) %>%
                    arrange(desc(frequency)) %>%
                    ggplot(aes(x = frequency, y = location, size = frequency, color = frequency)) +
                    geom_point() +
                    labs(title = "Frequency of Locations", x = "Location", y = "Frequency") +
                    theme_bw() +
                    theme(axis.text.y = element_text(size = 8))
                  
                  rental_df %>%
                    filter(monthly_rent <= 5000) %>%
                    ggplot(aes(x = monthly_rent, y = size, size = district)) +
                    geom_point()
                  
                  
                  
                  
                  
                  
                  ###### feature imporance - random forest #####
                  
                  
                  
                  # extract value, appended additional column
                  rental_df$NearKTM = str_extract(rental_df$additional_facilities, "Near KTM/LRT")
                  View(rental_df)
                  
                  length(unique(rental_df$NearKTM))
                  table(rental_df$district)
                  table(rental_df$NearKTM)
                  table(rental_df$region)
                  table(rental_df$furnished)
                  table(rental_df$property_type)
                  
                  selangorapt_df = filteredrental_df %>%
                    filter(region == "Selangor")
                  
                  
                  ###### imputing non-numerical
                  encoded_data1 <- model.matrix(~ ., data = filteredrental)
                  
                  
                  
                  
                  
                  