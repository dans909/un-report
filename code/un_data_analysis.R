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

gapminder_data_2007 <- 
  read_csv("C:/Users/Dan/Desktop/un-report/data/gapminder_data.csv") %>%
  filter(year == 2007, continent == "Americas") %>%
  select(-year, -continent)

gapminder_data_2007

read_csv("data/co2-un-data.csv")

#use skip argument to skip rows, then use col_names to set column headings
co2_emissions_dirty <- 
  read_csv("data/co2-un-data.csv", skip=2, 
         col_names=c("region", "country", "year", 
        "series", "value", "footnotes", "source"))

#alternatively, could rename columns by using function rename_all and use tolower to make
#all columns lowercase

read_csv("data/co2-un-data.csv", skip=1) %>%
  rename_all(tolower)

co2_emissions_dirty %>%
  select(country, year, series, value)

#use mutate function to change column, and recode to replace old values with new ones
#then use pivot_wider to split emissions column and gauge by their values
co2_emissions_dirty %>%
  select(country, year, series, value) %>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions", 
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>%
  pivot_wider(names_from=series, values_from=value)
  
#We cant to merge the above with the 2007 GDP dataset, so need to find year with 
#decent amount of data but is also decently close to 2005, so use count()

co2_emissions_dirty %>%
  select(country, year, series, value) %>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions", 
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>%
  pivot_wider(names_from=series, values_from=value) %>%
  count(year)

#select for 2005, and drop year column
co2_emissions <- co2_emissions_dirty %>%
  select(country, year, series, value) %>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions", 
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>%
  pivot_wider(names_from=series, values_from=value) %>%
  filter(year == 2005) %>%
  select(-year)
  
#to merge gapminder_2007 and co2_emissions, have to choose column column, i.e. country
#this makes country our key
#but may have issues since might have missing data, or some countries have different names
#solution: use inner_join() to only merge rows when keys are found on both datasets
inner_join(gapminder_data_2007, co2_emissions, by='country')

#can also use other types of inner_join functions, like left_join(), right_join() and full_join()
#left_join() merges data, if key present in leftside data frame, then it will present in merged data, even if right hand data frame doiesn't have any data
#right_join() same as left, but looks at righrside data frame for ref
#full_join() looks at both data frames and puts it in

#anti_join() shows data that is missing, specifically the key found in left, but not right 
anti_join(gapminder_data_2007, co2_emissions, by='country')

view(co2_emissions)

#so mutate and recode country data to match names on gapminder_data_2007

co2_emissions <- co2_emissions_dirty %>%
  select(country, year, series, value) %>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions", 
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>%
  pivot_wider(names_from=series, values_from=value) %>%
  filter(year == 2005) %>%
  select(-year) %>%
  mutate(country=recode(country,"Bolivia (Plurin. State of)" = "Bolivia", 
                        "United States of America" = "United States",
                        "Venezuela (Boliv. Rep. of)" = "Venezuela"))

anti_join(gapminder_data_2007, co2_emissions, by="country")

#replace puerto rico as USA, group by country and then get a weighted average of lifeExp and gdpPercap
gapminder_data_2007 <-  read_csv("C:/Users/Dan/Desktop/un-report/data/gapminder_data.csv") %>%
  filter(year == 2007, continent == "Americas") %>%
  select(-year, -continent) %>%
  mutate(country=recode(country, "Puerto Rico" = "United States")) %>%
  group_by(country) %>%
  summarize(lifeExp = sum(lifeExp*pop)/sum(pop),
            gdpPercap=sum(gdpPercap*pop)/sum(pop),
            pop=sum(pop))

gapminder_data_2007

#empty data frame suggests all keys have been matched with the data frame on the right
anti_join(gapminder_data_2007, co2_emissions, by="country")

#can finally join data
gapminder_co2 <- inner_join(gapminder_data_2007, co2_emissions, by="country")

#let's say we want to split countries into NA and SA, i.e. Canada/US/Mexico in one
#and the rest in another, can use if_else function
#mutate adds a column region, where if country is Canada, US or Mexico, then north
#everything else is south
gapminder_co2 %>%
  mutate(region=if_else(country == "Canada" | country == "United States" | 
                          country == "Mexico", "north", "south"))

#save data with write_csv()
write_csv(gapminder_co2, "data/gapminder_co2.csv")

#now can use data for analysis, analyze relationship between country GDP and CO2 emissions per capita
#use \and in the title to induce line break
#use method='lm' to force line of best fit function, geo_smooth, to be linear
ggplot(gapminder_co2, aes(x=gdpPercap, y=lifeExp))+
  geom_point()+
  labs(x='GDP (per Capita)', y="Life Expectancy", title="There is a strong association
       between a nation's GDP \nand the amount of CO2 it produces")+
  geom_smooth(method='lm')
  theme_bw()

#next, answer question what percent of total CO2 does NA account for in the Americas
  gapminder_co2 %>%
    mutate(region=if_else(country == "Canada" | country == "United States" | 
                            country == "Mexico", "north", "south")) %>%
    group_by(region) %>%
    summarize(sumtotal=sum(total_emissions), sumpop=sum(pop))
  
  gapminder_co2 %>%
    mutate(region=if_else(country == "Canada" | country == "United States" | 
                            country == "Mexico", "north", "south")) %>%
    mutate(perCO2=total_emissions/sum(total_emissions)) %>%
    mutate(perpop=pop/sum(pop)) %>%
    group_by(region)%>%
    summarize(sumperco2=sum(perCO2),sumperpop=sum(perpop))
