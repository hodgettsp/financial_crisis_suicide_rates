suicide_us_test <- suicide_us %>%
  select(1:4,7) %>% 
  pivot_wider(names_from = year,
              values_from = suicides.100k.pop,
              names_prefix = "Year_") %>% 
  mutate(average_pre = mean(c(Year_2003, Year_2004, Year_2005, Year_2006))) %>% 
  mutate(average_post = mean(c(Year_2007, Year_2008, Year_2009, Year_2010, Year_2011)))

suicide_us_male_treatment <- rbind(suicide_us_male_3554_pre, suicide_us_male_3554_post)

suicide_us_male_treatment_diffs <- suicide_us_male_treatment %>%
  select(1:4,7) %>% 
  pivot_wider(names_from = year,
              values_from = suicides.100k.pop,
              names_prefix = "Year_") %>%
  mutate(average_pre = mean(c(Year_2002, Year_2003, Year_2004, Year_2005, Year_2006))) %>% 
  mutate(average_post = mean(c(Year_2007, Year_2008, Year_2009, Year_2010, Year_2011)))

suicide_us_male_diffs <- rbind(suicide_us_male_control_diffs, suicide_us_male_treatment_diffs) %>% 
  mutate(difference_post_pre = average_post - average_pre)

suicide_us_male_diffs$difference_post_pre[2] - suicide_us_male_diffs$difference_post_pre[1]

suicide_rate <- lm(log())

suicide_us_female_1524_pre <- suicide_us %>% 
  filter(year <= 2006) %>% 
  filter(sex == 'female') %>% 
  filter(age == '15-24 years')

suicide_us_female_1524_post <- suicide_us %>% 
  filter(year >= 2007) %>% 
  filter(sex == 'female') %>% 
  filter(age == '15-24 years')

suicide_us_female_3554_pre <- suicide_us %>% 
  filter(year <= 2006) %>% 
  filter(sex == 'female') %>% 
  filter(age == '35-54 years')

suicide_us_female_3554_post <- suicide_us %>% 
  filter(year >= 2007) %>% 
  filter(sex == 'female') %>% 
  filter(age == '35-54 years')

suicide_us_female_control <- rbind(suicide_us_female_1524_pre, suicide_us_female_1524_post)

suicide_us_female_control_diffs <- suicide_us_female_control %>%
  select(1:4,7) %>% 
  pivot_wider(names_from = year,
              values_from = suicides.100k.pop,
              names_prefix = "Year_") %>% 
  mutate(average_pre = mean(c(Year_2002, Year_2003, Year_2004, Year_2005, Year_2006))) %>% 
  mutate(average_post = mean(c(Year_2007, Year_2008, Year_2009, Year_2010, Year_2011)))

suicide_us_female_treatment <- rbind(suicide_us_female_3554_pre, suicide_us_female_3554_post)

suicide_us_female_treatment_diffs <- suicide_us_female_treatment %>%
  select(1:4,7) %>% 
  pivot_wider(names_from = year,
              values_from = suicides.100k.pop,
              names_prefix = "Year_") %>% 
  mutate(average_pre = mean(c(Year_2002, Year_2003, Year_2004, Year_2005, Year_2006))) %>% 
  mutate(average_post = mean(c(Year_2007, Year_2008, Year_2009, Year_2010, Year_2011)))

suicide_us_female_diffs <- rbind(suicide_us_female_control_diffs, suicide_us_female_treatment_diffs) %>% 
  mutate(difference = average_post - average_pre)

suicide_us_female_diffs$difference[2] - suicide_us_female_diffs$difference[1]

