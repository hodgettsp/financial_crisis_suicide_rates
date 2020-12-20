# Code to create figure 1. United States Suicides, 2004-2010, per 100k People by Age Groups

library(ggplot2)
library(ggthemes)
library(scales)

suicide_us %>% 
  ggplot(aes(x = year, y = suicides.100k.pop, colour = gender)) +
  geom_point(alpha = 0.8) +
  geom_line(alpha = 0.5) +
  labs(x = "Year",
       y = "Suicides per 100k people",
       colour = "Gender Identifier (binary)") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing.y = unit(2, "lines"),
        axis.line = element_line(colour = "black"),
        legend.position = "bottom") +
  scale_colour_manual(values = c("darkturquoise", "deeppink")) +
  scale_x_discrete(breaks = c(2004, 2007, 2010),
                   limits = c(2004, 2007, 2010)) +
  geom_vline(xintercept = 2006.5, linetype = "dashed") +
  scale_y_discrete(limits = c(0, 10, 20, 30, 40)) +
  facet_wrap(vars(age),
             nrow = 2)
