library(dplyr)
library(ggplot2)
library(gapminder)

data <- gapminder



sp <- ggplot(data, aes(x=gapminder$gdpPercap, y=gapminder$lifeExp, color=gapminder$continent,size=gapminder$pop )) +
  geom_point(alpha=0.7) +
  labs(x='Per-capita GDP', 
       y='Life expectancy at birth',
       color='Continent',
       size='Total population') +
  scale_x_continuous(trans='log10') +  
  facet_wrap(~ year) +
  theme_light()  + 
  scale_color_brewer(palette="Dark2")
  
sp

my_plot <- data %>% 
  filter(country %in% c('Italy', 'Spain', 'Ireland', 'Sweden', 'Netherlands')) %>% 
  ggplot(aes(x=year, y=pop, color=country))  +
  geom_line() +
  labs(x='Year', 
       y='Total population',
       color='Country'
  ) +
  theme_light() 
my_plot