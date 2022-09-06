library(tidyverse)
library(readxl)
library(magrittr)
library(stringr)
library(broom)
library(tidyr)
library(purrr)
options(scipen = 100)

#skip first 4 rows as they are not data
manhattan_data <- read_xlsx("rollingsales_manhattan.xlsx", skip = 4)
bronx_data <- read_xlsx("rollingsales_bronx.xlsx", skip = 4)
brooklyn_data <- read_xlsx("rollingsales_brooklyn.xlsx", skip = 4)
queens_data <- read_xlsx("rollingsales_queens.xlsx", skip = 4)
si_data <- read_xlsx("rollingsales_statenisland.xlsx", skip = 4)

# Combining rows from all 5 tables 
nyc_property_data <- bind_rows(bronx_data, brooklyn_data, manhattan_data, queens_data, si_data)

# drop other tables 
remove(bronx_data, brooklyn_data, manhattan_data, queens_data, si_data)

# Change borough names from numbers to words
nyc_property_data <- nyc_property_data %>%
  mutate(BOROUGH=case_when(BOROUGH == 1 ~ 'MANHATTAN', 
                           BOROUGH == 2 ~ 'BRONX',
                           BOROUGH == 3 ~ 'BROOKLYN',
                           BOROUGH == 4 ~ 'QUEENS',
                           BOROUGH == 5 ~ 'STATEN_ISLAND',
                           TRUE ~ 'na'))

# Convert columns to lower case, remove spaces
names(nyc_property_data) <- tolower(names(nyc_property_data))
names(nyc_property_data) <-  str_replace_all(names(nyc_property_data)," ", "_")

# Convert capitalized fields to title case
nyc_property_data <- nyc_property_data %>%
  mutate(borough = str_to_title(nyc_property_data$borough, locale = 'en')) %>%
  mutate(neighborhood = str_to_title(nyc_property_data$neighborhood, locale = 'en')) %>%
  mutate(building_class_category = str_to_title(nyc_property_data$building_class_category, locale = 'en')) %>%
  mutate(address = str_to_title(nyc_property_data$address, locale = 'en'))

# Remove possible duplicates
nyc_property_data <- nyc_property_data %>%
  distinct()

# Drop irrelevant columns
nyc_property_data <- subset(nyc_property_data, select = -easement )

# Drop sales within families (assume price under 10k), 
# <150 sqft entries (The NYC Building Code requires all dwellings to be >150),
# remove NA entries for sales and gross square feet
nyc_property_data <- nyc_property_data %>%
  filter(sale_price>10000) %>%
  filter(gross_square_feet>=150) %>%
  drop_na(sale_price, gross_square_feet) %>%
  arrange(borough, neighborhood)

# Export a cleaned CSV copy
write_csv(nyc_property_data, file='nycsales_cleaned.csv')

# Filter for one family attached or semi-detached housing
one_family_attached <- nyc_property_data %>%
  filter(building_class_at_time_of_sale == 'A5')

# Scatter plot
ggplot(data= one_family_attached, mapping = aes(x= sale_price, y= gross_square_feet, color=borough))+
  geom_point()+
  xlim(0,7500000)+
  geom_smooth(method = 'lm', se= FALSE, aes(group = 1)) + theme(axis.title = element_text(size = 12),
    plot.title = element_text(face = "bold")) +labs(title = "The effect of gross square footage on sale price for attached single family homes in NYC",
    x = "Sale price ($)", y = "Gross Square Footage",
    colour = "Borough") +
  ggthemr("grape")+
  ylim(0,5000) + theme(plot.title = element_text(face = "bold")) +labs(title = "Sale Price vs Gross Square Footage in NYC")+labs(subtitle = "Single family homes, attached or semi-detached")

# Scatterplot by borough

borough.labs <- c('Bronx' = "Bronx",
                  'Brooklyn' = "Brooklyn",
                  'Manhattan' = "Manhattan",
                  'Queens' = "Queens", 
                  'Staten_island' = "Staten Island")

ggplot(data= one_family_attached, mapping = aes(x= sale_price, y= gross_square_feet))+
  geom_point()+
  facet_wrap(~borough, ncol = 1, scales = 'free', labeller = as_labeller(borough.labs))+
  scale_y_continuous()+
  geom_smooth(method = 'lm', se= FALSE, linetype="dashed") +
  theme(plot.title = element_text(face = "bold")) +labs(title = "Sale Price vs Gross Square Footage by Borough",
    x = "Sale Price", y = "Gross Square Footage")+labs(subtitle = "Single family homes, attached or semi-detached") + theme(legend.position = "none")+labs(x = "Sale Price ($)")

# Top sale in Manhattan is the Herbert M Strouse House, likely the largest private residence in Manhattan, it has a celebrity factor.
# I did not consider this an outlier as it is still a legitimate sale.

# Checking distribution of data. As Manhattan has at least 10 points we will still create a model.
one_family_attached %>% count(borough)

# Generate linear regression models
one_family_lm <- lm(sale_price ~ gross_square_feet,data = one_family_attached)
summary(one_family_lm)

# The model has a sufficiently low p value (below 0.05) and large enough t value(above an absolute value of 2). One can conclude that square footage is a predictor for the sale price of a property. This is  as the t value exceeds

# However, the r squared score is a weak to moderate 44%. This implies that a significant proportion of sale price is explained by other variables. 

# Generate separate linear regression models for each borough.
borough_lms <- one_family_attached %>%
  group_by(borough) %>%
  nest()

# Verify nesting
print(borough_lms)
head(borough_lms$data[[3]])

#use map function to apply the linear regression function to each nested element
borough_lms <- borough_lms %>%
  mutate(linear_model = map(.x = data,
                            .f = ~lm(sale_price ~ gross_square_feet,
                            data = .)))
# Verify lms
summary(borough_lms$linear_model[[3]])

borough_lms <- borough_lms %>%
  mutate(tidy_coefficients = map(.x = linear_model,
                                 .f = tidy,
                                 conf.int = TRUE))
# Verify tidy format
print(borough_lms$tidy_coefficients[[3]])

#Unnest
tidy_unnested <- borough_lms %>%
  select(borough, tidy_coefficients) %>%
  unnest(cols = tidy_coefficients)

print(tidy_unnested)

#Filter for slopes only
borough_slopes <- tidy_unnested %>%
  filter(term == 'gross_square_feet') %>%
  arrange(desc(estimate))

#Generate regression summaries
borough_regressions <- one_family_attached %>%
  group_by(borough) %>%
  nest() %>%
  mutate(linear_regression = map(.x = data,
                                 .f = ~lm(sale_price ~ gross_square_feet, data =.
                                       )))
#Tidy the regressions
borough_regressions <- borough_regressions %>%
  mutate(tidy_regressions = map(.x = linear_regression,
                                .f = glance,
                                conf.int = TRUE)) 
#Unnest
 borough_regressions <- borough_regressions %>%
  select(borough, tidy_regressions)%>%
  unnest(cols = tidy_regressions)
  
 borough_regressions <- borough_regressions %>%
   rename(r_squared = r.squared)
   
 borough_regressions <- borough_regressions %>%
   arrange(desc(r_squared))

print(borough_regressions[,1:2])

#Looking at the r-squared values for all 5 boroughs it appears that Manhattan has a strong value and Brooklyn has a low-moderate one.

# Remove neighborhoods with less than 10 sales
neighborhoods_count <- one_family_attached %>%
  count(neighborhood) %>%
  filter(n>9)

# New dataset for neighborhoods
neighborhoods_cleaned <- one_family_attached %>%
  filter(neighborhood %in% neighborhoods_count$neighborhood)

# Linear regressions by neighborhood
neighborhood_regressions <- neighborhoods_cleaned %>%
  group_by(borough, neighborhood) %>%
  nest() %>%
  mutate(linear_model = map(.x = data,
             .f = ~lm(sale_price ~ gross_square_feet, data = .))) %>%
  mutate(tidy_regressions = map(.x = linear_model,
                                 .f = glance,
                                 conf.int = TRUE)) %>%
  select(borough, neighborhood, tidy_regressions) %>%
  unnest(cols=tidy_regressions)

print(neighborhood_regressions)

# Remove high p values
significant_neighborhood_regressions <- neighborhood_regressions %>%
  filter(p.value <0.05) %>%
  filter(adj.r.squared > 0.5) %>%
  arrange(desc(adj.r.squared))

print(significant_neighborhood_regressions)

# Graph these 6 neighborhoods

neighborhood_graphs <- one_family_attached %>%
  filter(neighborhood %in% significant_neighborhood_regressions$neighborhood)

ggplot(data=neighborhood_graphs, mapping= aes(x=sale_price, y=gross_square_feet))+
  geom_point() +
  geom_smooth(method = 'lm', se=FALSE, linetype="dashed") +
  facet_wrap(~neighborhood, scales = 'free') + theme(axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold")) +labs(title = "Sale Price vs Gross Square Footage for Select Neighborhoods",
    x = "Sale Price ($)", y = "Gross Square Footage",
    subtitle = "Attached or semi-detached single family homes")
