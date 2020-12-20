# Code to create figure 5. U.S. GDP Per Capita, 2004-2010.

library(ggplot2)
library(ggthemes)

suicide_us %>% 
  ggplot(aes(x = year, y = gdp_per_capita)) +
  geom_point(alpha = 0.8) +
  geom_line(alpha = 0.5) +
  labs(x = "Year",
       y = "GDP/Capita") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.spacing.y = unit(2, "lines"),
        legend.position = "bottom") +
  scale_x_discrete(breaks = c(2004, 2005, 2006, 2007, 2008, 2009, 2010),
                   limits = c(2004, 2005, 2006, 2007, 2008, 2009, 2010)) +
  scale_y_continuous(labels = dollar_format(prefix = "$"))
