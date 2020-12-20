# Code to load in CSV file and prepare working datasets(s).

library(here) # Load in library 'here' to make loading CSV file easier.
library(tidyverse)
library(ggplot2)


# Load in CSV file and assign a working double so that the original does not get accidentally adjusted.
suicide_load <- read.csv(here("data", "suicide-rates-overview-1985-to-2016", "master.csv"))
suicide_working <- suicide_load # Apply loaded dataset to new, working dataset so original remains untouched.

# Build main dataset using all age groups.
suicide_us <- suicide_working %>%
  select(1:7, 10, 11) %>% # Select only columns that will be used.
  rename(country = ï..country) %>% # Odd symbol and column name could cause issues later. Rename ï..country to country to remove odd symbol.
  rename(gdp_per_capita = gdp_per_capita....) %>% # Rename odd column name.
  rename(gdp_per_year = gdp_for_year....) %>% # Rename odd column name.
  rename(gender = sex) %>% # Rename sex column to gender.
  filter(country == "United States") %>% # Filter so only United States remains and apply to new dataset.
  filter(year >= 2004 & year <= 2010) %>% # Filter the years so only those occurring at the end of the dotcom bubble to a few years after the Great Recession are included.
  arrange(year, age) %>% # Arrange the dataset so that it is ordered first by year then by age group.
  mutate(working_age = as.integer(age == '25-34 years' | age == '35-54 years' | age == '55-74 years')) %>% # Create dummy binary variable for working age vs. non-working age.
  mutate(recession_year = as.integer(year >= 2007)) %>% # Create dummy binary variable for whether a year is pre recession or in/post Great Recession.
  mutate(after_recession = as.integer(recession_year * working_age)) %>%  # Create dummy binary variable for working age and affected by the Great Recession.
  mutate(gender_factor = case_when(gender == 'male' ~ 1,
                                   gender == 'female' ~ 0)) %>% # Crate numeric binary variable for gender.
  mutate(after_male = as.integer(recession_year * working_age * gender_factor)) %>%  # Create dummy binary variable for males of working age affected by the Great Recession.
  mutate(age_relevel = relevel(age, ref = 6)) # Relevel age reference to 75+ years.
  
# Build main dataset using all age groups, females.
suicide_us_female <- suicide_working %>%
  select(1:7, 10, 11) %>% # Select only columns that will be used.
  rename(country = ï..country) %>% # Odd symbol and column name could cause issues later. Rename ï..country to country to remove odd symbol.
  rename(gdp_per_capita = gdp_per_capita....) %>% # Rename odd column name.
  rename(gdp_per_year = gdp_for_year....) %>% # Rename odd column name.
  rename(gender = sex) %>% # Rename sex column to gender.
  filter(country == "United States") %>% # Filter so only United States remains and apply to new dataset.
  filter(year >= 2004 & year <= 2010) %>% # Filter the years so only those occurring at the end of the dotcom bubble to a few years after the Great Recession are included.
  filter(gender == 'female') %>% # Filter only 'female' gender.
  arrange(year, age) %>% # Arrange the dataset so that it is ordered first by year then by age group.
  mutate(working_age = as.integer(age == '25-34 years' | age == '35-54 years' | age == '55-74 years')) %>% # Create dummy binary variable for working age vs. non-working age.
  mutate(recession_year = as.integer(year >= 2007)) %>% # Create dummy binary variable for whether a year is pre recession or in/post Great Recession.
  mutate(after_recession = as.integer(recession_year * working_age)) # Create dummy binary variable for working age and affected by the Great Recession.

# Build main dataset using all age groups, males.
suicide_us_male <- suicide_working %>%
  select(1:7, 10, 11) %>% # Select only columns that will be used.
  rename(country = ï..country) %>% # Odd symbol and column name could cause issues later. Rename ï..country to country to remove odd symbol.
  rename(gdp_per_capita = gdp_per_capita....) %>% # Rename odd column name.
  rename(gdp_per_year = gdp_for_year....) %>% # Rename odd column name.
  rename(gender = sex) %>% # Rename sex column to gender.
  filter(country == "United States") %>% # Filter so only United States remains and apply to new dataset.
  filter(year >= 2004 & year <= 2010) %>% # Filter the years so only those occurring at the end of the dotcom bubble to a few years after the Great Recession are included.
  filter(gender == 'male') %>% # Filter only 'male' gender.
  arrange(year, age) %>% # Arrange the dataset so that it is ordered first by year then by age group.
  mutate(working_age = as.integer(age == '25-34 years' | age == '35-54 years' | age == '55-74 years')) %>% # Create dummy binary variable for working age vs. non-working age.
  mutate(recession_year = as.integer(year >= 2007)) %>% # Create dummy binary variable for whether a year is pre recession or in/post Great Recession.
  mutate(after_recession = as.integer(recession_year * working_age)) # Create dummy binary variable for working age and affected by the Great Recession.






# Build dataset excluding 5-14 years age group due to possible skew in results.
suicide_us_clipped <- suicide_working %>%
  select(1:7, 10, 11) %>% # Select only columns that will be used.
  rename(country = ï..country) %>% # Odd symbol and column name could cause issues later. Rename ï..country to country to remove odd symbol.
  rename(gdp_per_capita = gdp_per_capita....) %>% # Rename odd column name.
  rename(gdp_per_year = gdp_for_year....) %>% # Rename odd column name.
  rename(gender = sex) %>% # Rename sex column to gender.
  filter(country == "United States") %>% # Filter so only United States remains and apply to new dataset.
  filter(year >= 2004 & year <= 2010) %>% # Filter the years so only those occurring at the end of the dotcom bubble to a few years after the Great Recession are included.
  filter(age == '15-24 years'| age == '25-34 years' | age == '35-54 years' | age == '55-74 years' | age == '75+ years') %>% # Remove 5-14 years agre group.
  arrange(year, age) %>% # Arrange the dataset so that it is ordered first by year then by age group.
  mutate(working_age = as.integer(age == '25-34 years' | age == '35-54 years' | age == '55-74 years')) %>% # Create dummy binary variable for working age vs. non-working age.
  mutate(recession_year = as.integer(year >= 2007)) %>% # Create dummy binary variable for whether a year is pre recession or in/post Great Recession.
  mutate(after_recession = as.integer(recession_year * working_age)) %>%  # Create dummy binary variable for working age and affected by the Great Recession.
  mutate(gender_factor = case_when(gender == 'male' ~ 1,
                                   gender == 'female' ~ 0)) %>% # Crate numeric binary variable for gender.
  mutate(after_male = as.integer(recession_year * working_age * gender_factor)) %>%  # Create dummy binary variable for males of working age affected by the Great Recession.
  mutate(age_relevel = relevel(age, ref = 6))# Relevel age reference to 75+ years.
  
# Build dataset excluding 5-14 years age group due to possible skew in results, females.
suicide_us_clipped_female <- suicide_working %>%
  select(1:7, 10, 11) %>% # Select only columns that will be used.
  rename(country = ï..country) %>% # Odd symbol and column name could cause issues later. Rename ï..country to country to remove odd symbol.
  rename(gdp_per_capita = gdp_per_capita....) %>% # Rename odd column name.
  rename(gdp_per_year = gdp_for_year....) %>% # Rename odd column name.
  rename(gender = sex) %>% # Rename sex column to gender.
  filter(country == "United States") %>% # Filter so only United States remains and apply to new dataset.
  filter(year >= 2004 & year <= 2010) %>% # Filter the years so only those occurring at the end of the dotcom bubble to a few years after the Great Recession are included.
  filter(age == '15-24 years'| age == '25-34 years' | age == '35-54 years' | age == '55-74 years' | age == '75+ years') %>% # Remove 5-14 years agre group.
  filter(gender == 'female') %>% # Filter only 'female' gender.
  arrange(year, age) %>% # Arrange the dataset so that it is ordered first by year then by age group.
  mutate(working_age = as.integer(age == '25-34 years' | age == '35-54 years' | age == '55-74 years')) %>% # Create dummy binary variable for working age vs. non-working age.
  mutate(recession_year = as.integer(year >= 2007)) %>% # Create dummy binary variable for whether a year is pre recession or in/post Great Recession.
  mutate(after_recession = as.integer(recession_year * working_age)) # Create dummy binary variable for working age and affected by the Great Recession.

# Build dataset excluding 5-14 years age group due to possible skew in results, males.
suicide_us_clipped_male <- suicide_working %>%
  select(1:7, 10, 11) %>% # Select only columns that will be used.
  rename(country = ï..country) %>% # Odd symbol and column name could cause issues later. Rename ï..country to country to remove odd symbol.
  rename(gdp_per_capita = gdp_per_capita....) %>% # Rename odd column name.
  rename(gdp_per_year = gdp_for_year....) %>% # Rename odd column name.
  rename(gender = sex) %>% # Rename sex column to gender.
  filter(country == "United States") %>% # Filter so only United States remains and apply to new dataset.
  filter(year >= 2004 & year <= 2010) %>% # Filter the years so only those occurring at the end of the dotcom bubble to a few years after the Great Recession are included.
  filter(age == '15-24 years'| age == '25-34 years' | age == '35-54 years' | age == '55-74 years' | age == '75+ years') %>% # Remove 5-14 years agre group.
  filter(gender == 'male') %>% # Filter only 'male' gender.
  arrange(year, age) %>% # Arrange the dataset so that it is ordered first by year then by age group.
  mutate(working_age = as.integer(age == '25-34 years' | age == '35-54 years' | age == '55-74 years')) %>% # Create dummy binary variable for working age vs. non-working age.
  mutate(recession_year = as.integer(year >= 2007)) %>% # Create dummy binary variable for whether a year is pre recession or in/post Great Recession.
  mutate(after_recession = as.integer(recession_year * working_age)) # Create dummy binary variable for working age and affected by the Great Recession.





# Build dataset excluding 5-14 years and 25-34 years age group due to possible skew and to focus on age groups in senior positions.
suicide_us_seniority <- suicide_working %>%
  select(1:7, 10, 11) %>% # Select only columns that will be used.
  rename(country = ï..country) %>% # Odd symbol and column name could cause issues later. Rename ï..country to country to remove odd symbol.
  rename(gdp_per_capita = gdp_per_capita....) %>% # Rename odd column name.
  rename(gdp_per_year = gdp_for_year....) %>% # Rename odd column name.
  rename(gender = sex) %>% # Rename sex column to gender.
  filter(country == "United States") %>% # Filter so only United States remains and apply to new dataset.
  filter(year >= 2004 & year <= 2010) %>% # Filter the years so only those occurring at the end of the dotcom bubble to a few years after the Great Recession are included.
  filter(age == '15-24 years'| age == '35-54 years' | age == '55-74 years' | age == '75+ years') %>% # Remove 5-14 years agre group.
  arrange(year, age) %>% # Arrange the dataset so that it is ordered first by year then by age group.
  mutate(working_age = as.integer(age == '25-34 years' | age == '35-54 years' | age == '55-74 years')) %>% # Create dummy binary variable for working age vs. non-working age.
  mutate(recession_year = as.integer(year >= 2007)) %>% # Create dummy binary variable for whether a year is pre recession or in/post Great Recession.
  mutate(after_recession = as.integer(recession_year * working_age)) %>%  # Create dummy binary variable for working age and affected by the Great Recession.
  mutate(gender_factor = case_when(gender == 'male' ~ 1,
                                   gender == 'female' ~ 0)) %>% # Crate numeric binary variable for gender.
  mutate(after_male = as.integer(recession_year * working_age * gender_factor)) %>%  # Create dummy binary variable for males of working age affected by the Great Recession.
  mutate(age_relevel = relevel(age, ref = 6))# Relevel age reference to 75+ years.
  
# Build dataset excluding 5-14 years and 25-34 years age group due to possible skew and to focus on age groups in senior positions, females.
suicide_us_seniority_female <- suicide_working %>%
  select(1:7, 10, 11) %>% # Select only columns that will be used.
  rename(country = ï..country) %>% # Odd symbol and column name could cause issues later. Rename ï..country to country to remove odd symbol.
  rename(gdp_per_capita = gdp_per_capita....) %>% # Rename odd column name.
  rename(gdp_per_year = gdp_for_year....) %>% # Rename odd column name.
  rename(gender = sex) %>% # Rename sex column to gender.
  filter(country == "United States") %>% # Filter so only United States remains and apply to new dataset.
  filter(year >= 2004 & year <= 2010) %>% # Filter the years so only those occurring at the end of the dotcom bubble to a few years after the Great Recession are included.
  filter(age == '15-24 years'| age == '35-54 years' | age == '55-74 years' | age == '75+ years') %>% # Remove 5-14 years agre group.
  filter(gender == 'female') %>% # Filter only 'female' gender.
  arrange(year, age) %>% # Arrange the dataset so that it is ordered first by year then by age group.
  mutate(working_age = as.integer(age == '25-34 years' | age == '35-54 years' | age == '55-74 years')) %>% # Create dummy binary variable for working age vs. non-working age.
  mutate(recession_year = as.integer(year >= 2007)) %>% # Create dummy binary variable for whether a year is pre recession or in/post Great Recession.
  mutate(after_recession = as.integer(recession_year * working_age)) # Create dummy binary variable for working age and affected by the Great Recession.

# Build dataset excluding 5-14 years and 25-34 years age group due to possible skew and to focus on age groups in senior positions, males.
suicide_us_seniority_male <- suicide_working %>%
  select(1:7, 10, 11) %>% # Select only columns that will be used.
  rename(country = ï..country) %>% # Odd symbol and column name could cause issues later. Rename ï..country to country to remove odd symbol.
  rename(gdp_per_capita = gdp_per_capita....) %>% # Rename odd column name.
  rename(gdp_per_year = gdp_for_year....) %>% # Rename odd column name.
  rename(gender = sex) %>% # Rename sex column to gender.
  filter(country == "United States") %>% # Filter so only United States remains and apply to new dataset.
  filter(year >= 2004 & year <= 2010) %>% # Filter the years so only those occurring at the end of the dotcom bubble to a few years after the Great Recession are included.
  filter(age == '15-24 years'| age == '35-54 years' | age == '55-74 years' | age == '75+ years') %>% # Remove 5-14 years agre group.
  filter(gender == 'male') %>% # Filter only 'male' gender.
  arrange(year, age) %>% # Arrange the dataset so that it is ordered first by year then by age group.
  mutate(working_age = as.integer(age == '25-34 years' | age == '35-54 years' | age == '55-74 years')) %>% # Create dummy binary variable for working age vs. non-working age.
  mutate(recession_year = as.integer(year >= 2007)) %>% # Create dummy binary variable for whether a year is pre recession or in/post Great Recession.
  mutate(after_recession = as.integer(recession_year * working_age)) # Create dummy binary variable for working age and affected by the Great Recession.
