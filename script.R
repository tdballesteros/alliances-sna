
# load libraries
library(igraph)
library(lubridate)
library(stringr)
library(dplyr)
library(tidyverse)

# import data

data <- read.csv("alliance_v4.1_by_dyad.csv")

# format data

data <- data %>%
  dplyr::mutate(
    ongoing_flag = ifelse(is.na(dyad_end_year), 1, 0),
    dyad_start = lubridate::as_date(
      lubridate::ymd(paste0(stringr::str_pad(dyad_st_year, 4, "left", "0"),
                            stringr::str_pad(dyad_st_month, 2, "left", "0"),
                            stringr::str_pad(dyad_st_day, 2, "left", "0")))
    ),
    dyad_end = lubridate::as_date(
      lubridate::ymd(paste0(stringr::str_pad(dyad_end_year, 4, "left", "0"),
                            stringr::str_pad(dyad_end_month, 2, "left", "0"),
                            stringr::str_pad(dyad_end_day, 2, "left", "0")))
      ),
    length = ifelse(ongoing_flag == 0, dyad_end - dyad_start, Sys.Date() - dyad_start) / 365
  ) %>%
  # filter out negative lengths (treaty ended before it started)
  dplyr::filter(length >= 0) %>%
  # drop unneeded variables
  dplyr::select(uniqueid = version4id, state_name1, state_name2, left_censor, right_censor, defense,
                neutrality, nonaggression, entente, asymmetric, ongoing_flag, dyad_start, dyad_end,
                length)

# defense treaties ---------------------------------------------------------------------------------
defense <- data %>%
  dplyr::filter(defense == 1,
                ongoing_flag == 1)

defense_edgelist <- defense %>%
  dplyr::select(state_name1, state_name2) %>%
  as.matrix(nc = 2) %>%
  igraph::graph_from_edgelist(directed = FALSE)

plot(defense_edgelist)

defense_df <- defense %>%
  dplyr::select(state_name1, state_name2, length) %>%
  igraph::graph_from_data_frame(directed = FALSE)

plot(defense_df)
