suicide_us_male_1524_pre <- suicide_us %>% 
  filter(year <= 2006) %>% 
  filter(sex == 'male') %>% 
  filter(age == '15-24 years')

suicide_us_male_1524_post <- suicide_us %>% 
  filter(year >= 2007) %>% 
  filter(sex == 'male') %>% 
  filter(age == '15-24 years')

suicide_us_male_3554_pre <- suicide_us %>% 
  filter(year <= 2006) %>% 
  filter(sex == 'male') %>% 
  filter(age == '35-54 years')

suicide_us_male_3554_post <- suicide_us %>% 
  filter(year >= 2007) %>% 
  filter(sex == 'male') %>% 
  filter(age == '35-54 years')

suicide_us_male_control <- rbind(suicide_us_male_1524_pre, suicide_us_male_1524_post)

suicide_us_male_control_diffs <- suicide_us_male_control %>%
  select(1:4,7) %>% 
  pivot_wider(names_from = year,
              values_from = suicides.100k.pop,
              names_prefix = "Year_") %>% 
  mutate(average_pre = mean(c(Year_2002, Year_2003, Year_2004, Year_2005, Year_2006))) %>% 
  mutate(average_post = mean(c(Year_2007, Year_2008, Year_2009, Year_2010, Year_2011)))

control_diff_03_02 = suicide_us_male_control_diffs$Year_2003 - suicide_us_male_control_diffs$Year_2002
control_diff_04_03 = suicide_us_male_control_diffs$Year_2004 - suicide_us_male_control_diffs$Year_2003
control_diff_05_04 = suicide_us_male_control_diffs$Year_2005 - suicide_us_male_control_diffs$Year_2004
control_diff_06_05 = suicide_us_male_control_diffs$Year_2006 - suicide_us_male_control_diffs$Year_2005

control_diff_08_07 = suicide_us_male_control_diffs$Year_2008 - suicide_us_male_control_diffs$Year_2007
control_diff_09_08 = suicide_us_male_control_diffs$Year_2009 - suicide_us_male_control_diffs$Year_2008
control_diff_10_09 = suicide_us_male_control_diffs$Year_2010 - suicide_us_male_control_diffs$Year_2009
control_diff_11_10 = suicide_us_male_control_diffs$Year_2011 - suicide_us_male_control_diffs$Year_2010

suicide_us_male_control_diffs <- suicide_us_male_control_diffs %>% 
  mutate(average_yr_to_yr_diff_pre = mean(c(control_diff_03_02, control_diff_04_03, control_diff_05_04, control_diff_06_05))) %>% 
  mutate(average_yr_to_yr_diff_post = mean(c(control_diff_08_07, control_diff_09_08, control_diff_10_09, control_diff_11_10)))

suicide_us_male_treatment <- rbind(suicide_us_male_3554_pre, suicide_us_male_3554_post)

suicide_us_male_treatment_diffs <- suicide_us_male_treatment %>%
  select(1:4,7) %>% 
  pivot_wider(names_from = year,
              values_from = suicides.100k.pop,
              names_prefix = "Year_") %>%
  mutate(average_pre = mean(c(Year_2002, Year_2003, Year_2004, Year_2005, Year_2006))) %>% 
  mutate(average_post = mean(c(Year_2007, Year_2008, Year_2009, Year_2010, Year_2011)))

treat_diff_03_02 = suicide_us_male_treatment_diffs$Year_2003 - suicide_us_male_treatment_diffs$Year_2002
treat_diff_04_03 = suicide_us_male_treatment_diffs$Year_2004 - suicide_us_male_treatment_diffs$Year_2003
treat_diff_05_04 = suicide_us_male_treatment_diffs$Year_2005 - suicide_us_male_treatment_diffs$Year_2004
treat_diff_06_05 = suicide_us_male_treatment_diffs$Year_2006 - suicide_us_male_treatment_diffs$Year_2005

treat_diff_08_07 = suicide_us_male_treatment_diffs$Year_2008 - suicide_us_male_treatment_diffs$Year_2007
treat_diff_09_08 = suicide_us_male_treatment_diffs$Year_2009 - suicide_us_male_treatment_diffs$Year_2008
treat_diff_10_09 = suicide_us_male_treatment_diffs$Year_2010 - suicide_us_male_treatment_diffs$Year_2009
treat_diff_11_10 = suicide_us_male_treatment_diffs$Year_2011 - suicide_us_male_treatment_diffs$Year_2010


suicide_us_male_treatment_diffs <- suicide_us_male_treatment_diffs %>% 
  mutate(average_yr_to_yr_diff_pre = mean(c(treat_diff_03_02, treat_diff_04_03, treat_diff_05_04, treat_diff_06_05))) %>% 
  mutate(average_yr_to_yr_diff_post = mean(c(treat_diff_08_07, treat_diff_09_08, treat_diff_10_09, treat_diff_11_10)))

suicide_us_males_diffs <- rbind(suicide_us_male_control_diffs, suicide_us_male_treatment_diffs) %>% 
  mutate(difference_post_pre = average_post - average_pre) %>% 
  mutate(difference_yr_to_yr = average_yr_to_yr_diff_post - average_yr_to_yr_diff_pre)

suicide_us_males_diffs$difference_post_pre[2] - suicide_us_males_diffs$difference_post_pre[1]
suicide_us_males_diffs$difference_yr_to_yr[2] - suicide_us_males_diffs$difference_yr_to_yr[1]

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

