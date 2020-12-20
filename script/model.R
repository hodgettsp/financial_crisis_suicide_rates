library(huxtable)


suicides_after_recession <- lm(suicides.100k.pop ~ after_recession, data = suicide_us)# Full dataset model using all age groups.
suicides_after_recession_age_year <- lm(suicides.100k.pop ~ after_recession + age_relevel + year, data = suicide_us)# Full dataset model using all age groups controlling for age and year.
suicides_full <- lm(suicides.100k.pop ~ after_recession + gdp_per_capita + gender + age_relevel + year, data = suicide_us)# Full dataset model using all age groups controlling for gdp/capita, gender, age, and year.
suicides_ddd <- lm(suicides.100k.pop ~ after_male + gdp_per_capita + gender + age_relevel + year, data = suicide_us)# Full dataset DDD model using all age groups and controlling for gdp/capita, gender, age, and year.
suicides_clipped <- lm(suicides.100k.pop ~ after_recession + age_relevel + gender + gdp_per_capita + year, data = suicide_us_clipped)# Model using clipped dataset. Excludes 5-14 years age group.
suicides_seniority <- lm(suicides.100k.pop ~ after_recession + age_relevel + gender + gdp_per_capita + year, data = suicide_us_seniority)# Model using seniority dataset. Excludes 5-14 years and 25-34 years age groups.


huxreg("Suicide Rate" = suicide_rate)

# Models summaries table.
huxreg("Suicides After Recession" = suicides_after_recession,
       "Suicides + Age & Year" = suicides_after_recession_age_year,
       "Suicides + GDP & Gender" = suicides_full,
       "Suicides DDD" = suicides_ddd,
       "Suicides w/o 5-14 years" = suicides_clipped,
       "Suicides Job Seniority" = suicides_seniority,
       coefs = c("After Recession" = "after_recession",
                 "15-24 Years" = "age_relevel15-24 years",
                 "25-34 Years" = "age_relevel25-34 years",
                 "35-54 Years" = "age_relevel35-54 years",
                 "5-14 Years" = "age_relevel5-14 years",
                 "55-74 Years" = "age_relevel55-74 years",
                 "Year" = "year",
                 "GDP/Capita" = "gdp_per_capita",
                 "Males vs. Females" = "gendermale",
                 "After Male" = "after_male"),
              number_format = 2)
