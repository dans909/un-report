library(tidyverse)
#can type alt + '-' to automatically type out <-
gapminder_1997 <- read_csv("data/gapminder_1997.csv")

ggplot(data=gapminder_1997) + 
  aes(x=gdpPercap, 
      y=lifeExp,
      color=continent,
      shape=continent,
      size=pop/1000000) +
  labs(x="GDP Per Capita", 
       y="Life Expectancy", 
       title="Do people in wealthy countries live longer?",
       color="Continent",
       size="Population (in Millions)") + 
  geom_point() + 
  scale_color_brewer(palette="Set1")

#other color palettes include Viridis, National Parks, Lacroix, Wes Anderson
#Use 'RColorBrewer::display.brewer.all()' to display all color schemes

gapminder_data <- read_csv("data/gapminder_data.csv")

#dimensions of data
dim(gapminder_data)

#snapshot of data
head(gapminder_data)

ggplot(data=gapminder_data) + 
  aes(x=year,
      y=lifeExp,
      color=continent,
      group=country) + 
  labs(x="Year",
       y="Life Expectancy",
       color="Continent") + 
  geom_line()

#Look at structure of data 
str(gapminder_data)

ggplot(data=gapminder_1997)+
  geom_violin()+
  geom_jitter()+ #like geom_point but you move points to the side so they don't stack around
  aes(x=continent,
      y=lifeExp,
      color=continent)+
  labs(x="Continent",
       y="Life Expectancy")

ggplot(data=gapminder_1997)+
  geom_jitter()+
  geom_violin()+ #changing order of functions changes which gets stacked first and second etc.
  aes(x=continent,
      y=lifeExp,
      color=continent)+
  labs(x="Continent",
       y="Life Expectancy")

ggplot(data=gapminder_1997, mapping = aes(x=continent,y=lifeExp))+
  geom_violin(color="black", aes(fill=continent), alpha=0.5)+
  geom_jitter(aes(size=pop))
#Using mapping helps set a default setting for all geom layers
#can also use aes within the geom layer to give specific trait
#this layering works because R always passes inner function first, then outer function
#can use color=xxx directly without aes() because we aren't varying color by variable
#run colors() to see full list of colors, can choose random via sample(colors(), size=x)
#using alpha = x manipulates opacity of the colors

ggplot(gapminder_1997)+
  aes(x=lifeExp)+
  geom_histogram(bins=20)+
  theme_classic()+
  theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))
#for histograms, bins=30 tells us the no. of columns drawn on the graph


ggplot(gapminder_1997)+
  aes(x=gdpPercap, y=lifeExp)+
  geom_point()+
  facet_wrap(vars(continent))
#faceting splits graph into smaller onces based on variable you want

stacked_plot <- ggplot(gapminder_1997)+
  aes(x=gdpPercap, y=lifeExp)+
  geom_point()+
  facet_grid(rows=vars(continent))
#faceting grid makes sure they share same axis, make sure you explain how you want to stack variables

stacked_plot + theme_classic()

ggsave("figures/plot.png", width=6, height=6)
#your file name determines file type
#can supply your own width & height

ggsave("figures/stacked_plot.png", plot=stacked_plot+theme_minimal(), width=10, height=10)

install.packages(c("gganimate","gifski"))
library(gganimate)
library(gifski)

bonus_plot <- ggplot(data=gapminder_data)+
  geom_point(alpha=0.5)+
  aes(x=log(gdpPercap), y=lifeExp, 
      size=pop/1000000, color=continent)+
  labs(y="Life Expectancy", x="GDP per capita (log)", 
       color="Continent", size="Population (in Millions)")+
  theme_classic()

animated_plot <- bonus_plot + 
  transition_states(year, transition_length = 1, state_length = 1)+
  ggtitle("{closest_state}")
#transition states tells graph to transition by year, each frame has to be different year
#title set so tht it always show which year is currently being plotted

animated_plot

anim_save("figures/animated_plot.gif",
          plot=animated_plot,
          width=10,
          height=10,
          renderer=gifski_renderer)

install.packages("mapproj")
library(mapproj)
install.packages("ggthemes")
library(ggthemes)


# make sure names of countries match between the map info and the data
# NOTE: we haven't learned how to modify the data in this way yet, but we'll learn about that in the next lesson. Just take for granted that it works for now :)
mapdata <- map_data("world") %>%
  mutate(region = recode(region,
                         USA="United States",
                         UK="United Kingdom"))

#install.packages("mapproj")
gapminder_1997 %>%
  ggplot() +
  geom_map(aes(map_id=country, fill=lifeExp), map=mapdata) +
  expand_limits(x = mapdata$long, y = mapdata$lat) +
  coord_map(projection = "mollweide", xlim = c(-180, 180)) +
  ggthemes::theme_map()
