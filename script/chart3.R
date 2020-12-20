# Code to create figure 3. United States Female Identified Suicides, 2004-2010, per 100k People by Working-age.

library(ggplot2)
library(ggthemes)

suicide_us_female %>% 
  mutate(type = if_else(working_age == 1, "Working Age", "Non-Working Age")) %>% 
  ggplot(aes(x = year, y = suicides.100k.pop, colour = gender, shape = age)) +
  geom_point(alpha = 0.9) +
  geom_line(alpha = 0.5) +
  labs(x = "Year",
       y = "Suicides per 100k people",
       shape = "Age Group") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing.y = unit(2, "lines")) +
  guides(colour = FALSE) +
  scale_colour_manual(values = c("darkturquoise")) +
  scale_shape_manual(values = c(6, 17, 18, 15, 19, 8)) +
  geom_vline(xintercept = 2006.5, linetype = "dashed") +
  geom_vline(xintercept = 2009.5, linetype = "dashed") +
  scale_x_discrete(limits = c(2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011)) +
  facet_wrap(vars(type),
             nrow = 2)
