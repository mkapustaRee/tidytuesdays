#' french_trains_gganimate.R
#'
#' @author Ben Moretti
#'
#' @description 
#' 
#' Reads data for the 26 Feb 2019 #tidytuesday - French Trains - and visualises using ggalluvial
#' Also attempts to add a gganimate animation for each month of the year 2017
#' This does NOT work!


# Libraries -----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(ggalluvial)
library(gganimate)

# Gather ------------------------------------------------------------------

trains_raw <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-26/full_trains.csv")


# Condition ---------------------------------------------------------------

# select data for paris departures for 2017
# we require the month so as to be able to loop over it in ggaminate
paris_2017_trips <- trains_raw %>% 
  filter(
    year == 2017,
    str_detect(departure_station, "PARIS")
  ) %>% 
  select(departure_station, arrival_station, total_num_trips, month) 

# check that it's in alluvial format - should return true
is_alluvia_form(paris_2017_trips, axes = 1:2, silent = TRUE)  

# Plot --------------------------------------------------------------------

# create alluvial plot as per the original
paris_2017_trips %>%
  
  # axis1 is on the left and axis2 on the right of the alluvial plot
  ggplot(aes(y = total_num_trips, axis1 = departure_station, axis2 = arrival_station)) +
  
  # this sets the fill colour to be the departure station
  geom_alluvium(aes(fill = departure_station)) +
  guides(fill = FALSE) +
  
  # this labels the "strata" of the alluvial plot with teh names of the stations
  geom_label(stat = "stratum",
             label.strata = TRUE,
             size = 2) +
  
  # for the x axis 
  scale_x_discrete(limits = c("Departure", "Arrival"),
                   expand = c(0.05, 0.05)) +
  
  # nice colours and minimal theme with no legend
  scale_fill_viridis_d() + theme_minimal() + theme(legend.position = "none") +
  
  # some labels
  labs(
    title = "Train trips departing from Paris stations in July 2017",
    y = "Total Number of Trips",
    subtitle = "Source: SNCF",
    caption = "#tidytuesday by @benmoretti"
  ) +
  
  # finally call gganimate::transition_time to iterate over the month -- doesn't work though
  transition_time(month)

# the final call to transition_time(month) throws the following error
#
# Error in mapply(FUN = f, ..., SIMPLIFY = FALSE) : 
# zero-length inputs cannot be mixed with those of non-zero length

