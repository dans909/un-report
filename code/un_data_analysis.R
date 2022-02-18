install.packages("tidyverse")
library("tidyverse")
gapminder_data <-  read_csv("C:/Users/Dan/Desktop/un-report/data/gapminder_data.csv")
#summarize() passes all values in a column through other functions, 
#returning a new data object and column
#summarize(gapminder_data, averageLifeExp=mean(lifeExp))

#can also rewrite code as gapminder_data %>% summarize(averageLifeExp=mean(lifeExp))
#%>% tells us to pass the first bit as the first argument in your function

gapminder_data %>% 
  summarize(averageLifeExp=mean(lifeExp))

gapminder_data %>%  
  summarize(recent_year=max(year))

gapminder_data %>%
  summarize(meanpop=mean(pop))

#filter and choose only data for year 2007, and then summarize averagelifespan
#based on that year

gapminder_data %>%
  filter(year == 2007) %>%
  summarize(average=mean(lifeExp))

gapminder_data %>% 
  summarize(oldest_year=min(year))

#note how %>% allows us to combine 2 functions for data extraction    
gapminder_data %>%
  filter(year == 1952) %>%
  summarize(meanGDP=mean(gdpPercap))

#we can use group() to treat each row as its own group and get a summary for each group
gapminder_data %>%
  group_by(year) %>%
  summarize(averageLifeExp=mean(lifeExp))

gapminder_data %>%
  group_by(continent) %>%
  summarize(averageLifeExp=mean(lifeExp))

#can also build on summarize by using comma to add a new function
gapminder_data %>%
  group_by(continent) %>%
  summarize(averageLifeExp=mean(lifeExp), minLifeExp=min(lifeExp))

#we can use mutate() to add a new column to our data
gapminder_data %>%
  mutate(gdp=pop*gdpPercap)

gapminder_data %>%
  mutate(popinmil=pop/1000000)

#filter() for selecting subset of rows, while select() used for selecting subset of columns
gapminder_data %>%
  select(pop, year)

#can add - in front of column to remove it!
gapminder_data %>%
  select(-continent)

gapminder_data %>%
  select(country, continent, year, lifeExp)

#can use starts_with("xx") to be even more specific, check ?select for more info
gapminder_data %>%
  select(starts_with("c"))

gapminder_data %>%
  select(ends_with("p"))


#first select columns, then can use pivot wider to split the column year
#into multiple columns, and give values based on lifeExp
#can alternatively use pivot longer to do the opposite, and group columns into a single variable column
#and then list the variables underneath the column as something separate

gapminder_data %>%
  select(country, continent, year, lifeExp) %>%
  pivot_wider(names_from = year, values_from = lifeExp )

gapminder_data_2007 <- read_csv("data/gapminder_data.csv") %>%
  filter(year == 2007, continent == "Americas") %>%
  select(-year, -continent)

gapminder_data_2007









