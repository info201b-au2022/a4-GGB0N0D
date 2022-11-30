library(tidyverse)
library(ggplot2)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
# store the dataframe into a variable
incarceration_df <- get_data()

# calculate the proportion of black people in jail in 2013 in each county
black_jail_county_proportion_2013 <- incarceration_df %>% 
  filter(year == 2013) %>% 
  group_by(county_name) %>% 
  summarise(Avg_black = mean(black_jail_pop, na.rm = TRUE),
            Avg_jail = mean(total_jail_pop, na.rm = TRUE),
            proportion = round(Avg_black / Avg_jail, digit = 4))

  
# find the counties that has the highest proportion of black people in jail in 2013
highest_proportion_black_2013 <- black_jail_county_proportion_2013 %>% 
  filter(proportion == max(proportion, na.rm = TRUE))

# find the counties that has the lowest proportion of black people in jail in 2013
lowest_proportion_black_2013 <- black_jail_county_proportion_2013 %>% 
  filter(proportion ==min(proportion, na.rm = TRUE))

# calculate the proportion of black people in jail in 2018 in each county
black_jail_county_proportion_2018 <- incarceration_df %>% 
  filter(year == 2018) %>% 
  group_by(county_name) %>% 
  summarise(Avg_black = mean(black_jail_pop, na.rm = TRUE),
            Avg_jail = mean(total_jail_pop, na.rm = TRUE),
            proportion = round(Avg_black / Avg_jail, digits = 4))

# find the counties that has the highest proportion of black people in jail in 2013
highest_proportion_black_2018 <- black_jail_county_proportion_2018 %>% 
  filter(proportion == max(proportion, na.rm = TRUE))

# find the counties that has the lowest proportion of black people in jail in 2013
lowest_proportion_black_2013 <- black_jail_county_proportion_2018 %>% 
  filter(proportion ==min(proportion, na.rm = TRUE))

# find the change in proportion of black people in jail from 2013 to 2018
change_in_proportion <- left_join(black_jail_county_proportion_2013, black_jail_county_proportion_2018, "county_name") %>%
  group_by(county_name) %>% 
  summarise(change_in_proportion = round(proportion.y - proportion.x, digit = 4))

# find the highest change of black people proportion in jail from 2013 to 2018
highest_change_proportion_black <- change_in_proportion %>% 
  filter(abs(change_in_proportion) == max(abs(change_in_proportion), na.rm = TRUE))

# calculate the proportion of black people in jail to total black people population 
# in each county in 2013
proportion_black_jail_pop <- incarceration_df %>% 
  filter(year == 2013) %>% 
  group_by(county_name) %>% 
  summarise(Avg_black = mean(black_jail_pop, na.rm = TRUE),
            Avg_total = mean(black_pop_15to64, na.rm = TRUE),
            proportion = round(Avg_black / Avg_total, digit = 4)) %>% 
  arrange(-proportion)


  
#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function takes no parameter and return a dataframe that shows the total jail
# population each year in the U.S.
get_year_jail_pop <- function() {
  jail_pop_years <- incarceration_df %>% 
    group_by(year) %>% 
    select(total_jail_pop)
    
  return(jail_pop_years)   
}

# This function takes no parameter and return a bar chart that reveals the change 
# in total jail population each year in the U.S.
plot_jail_pop_for_us <- function()  {
  plot <- ggplot(data = get_year_jail_pop()) +
    geom_col(mapping = aes(x = year, y = total_jail_pop)) +
    labs(title = "Increase of Jail Population in U.S. (1970-2018)",
         x = "Year",
         y = "Total Jail Population",
         caption = "This chart reveals the change of jail population in the whole
         US from 1970 to 2018. We can see that the trend of the chart is increasing")
  
  return(plot)
} 

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 

# This function takes parameter of a string or vector of states and returns a 
# dataframe that contains the total jail population each year in certain states.
get_jail_pop_by_states <- function(states) {
  state_jail_pop_years <- incarceration_df %>% 
    group_by(state, year) %>% 
    filter(state %in% states) %>% 
    summarise(total_jail_pop = sum(total_jail_pop, na.rm = TRUE))
  
  return(state_jail_pop_years)
}

# This function takes parameter of string or vector of states and returns a line 
# chart that shows the change in total jail population each year in certain states
plot_jail_pop_by_states <- function(states) {
  plot <- ggplot(get_jail_pop_by_states(states), aes(x = year, y = total_jail_pop, group = state)) +
    geom_line(aes(color = state)) +
    geom_point(aes(color = state)) +
    labs(title = "Growth of Jail Population in States (1970-2018)",
         x = "Year",
         y = "Total Jail Population",
         caption = "This chart shows the change in jail population in five different states 
         from 1970 to 2018. We can see that California has the highest jail population all time
         and has the greatest rate of increase trend")
  
  return(plot)
}

# See Canvas
#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <Change in Black People Jail Proportion in Mobile County (1970-2018)>
# This function takes no parameter and returns the proportion of black people jail 
# population in Mobile County each year
get_jail_prop_year_Mobile <- function() {
  Mobile_jail_pop_year <- incarceration_df %>% 
    filter(county_name == "Mobile County") %>% 
    group_by(year) %>% 
    summarise(Avg_black = mean(black_jail_pop, na.rm = TRUE),
              Avg_jail = mean(total_jail_pop, na.rm = TRUE),
              proportion = round(Avg_black / Avg_jail, digit = 4))
}

# This function takes no parameter and returns the chart that shows the change in 
# the proportion of black people jail population in Mobile County each year
plot_jail_prop_year_Mobile <- function() {
  plot <- ggplot(get_jail_prop_year_Mobile(), aes(x = year, y = proportion)) +
    geom_col() +
    labs(title = "Change in Black People Jail Proportion in Mobile County (1970-2018)",
         x = "Year",
         y = "Black People Jail Proportion",
         caption = "This chart shows that in 2013, the proportion of black people jail
         population in Mobile County increased a huge amount and remains high in the next few years")
  
  return(plot)
}

#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <Proportion of Black People in Jail Populatoin Each State 2013>
# This function takes no parameter and returns the proportion of black people in
# jail population in each state in 2013
get_state_black_jail_prop <- function() {
  black_jail_state_proportion_2013 <- incarceration_df %>% 
    filter(year == 2013) %>% 
    group_by(state) %>% 
    summarise(Avg_black = mean(black_jail_pop, na.rm = TRUE),
              Avg_jail = mean(total_jail_pop, na.rm = TRUE),
              proportion = round(Avg_black / Avg_jail, digit = 4))
  
  black_jail_state_proportion_2013$state <- tolower(state.name[match(black_jail_state_proportion_2013$state, state.abb)])
  
  return(black_jail_state_proportion_2013)
}

# This function graphs the proportion of black people in jail population in each 
# state in 2013 in the United States Map
map_black_jail_prop_states <- function() {
  state_shape <- map_data("state") %>% 
    rename(state = region) %>% 
    left_join(get_state_black_jail_prop(), by = "state")
  
  blank_theme <- theme_bw() +
    theme(
      axis.line = element_blank(),        # remove axis lines
      axis.text = element_blank(),        # remove axis labels
      axis.ticks = element_blank(),       # remove axis ticks
      axis.title = element_blank(),       # remove axis titles
      plot.background = element_blank(),  # remove gray background
      panel.grid.major = element_blank(), # remove major grid lines
      panel.grid.minor = element_blank(), # remove minor grid lines
      panel.border = element_blank()      # remove border around plot
    )
  
  plot <- ggplot(state_shape) +
    geom_polygon(
      aes(x = long, y = lat, group = group, fill = proportion),
      color = "white",
      size = .1
    ) +
    coord_map() +
    scale_fill_continuous(low = "White", high = "Red") +
    labs(title = "Proportion of Black People in Jail Populatoin Each State 2013", 
         fill = "Proportion",
         caption = "This map shows the states in midsouth such as Louisiana has the highest proportion
         of black people in jail population in 2013") +
    blank_theme
  
  return(plot)
}

#----------------------------------------------------------------------------#

## Load data frame ---- 


