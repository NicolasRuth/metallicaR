# First analyses of the data

library(tidyverse)
library(knitr) # for report generation

metallicaR <- read.csv("./data/metallicaR.csv")

# Metallica's favourite key
metallicaR %>%
  count(key_mode, sort = TRUE) %>%
  head(5) %>%
  kable()

#Metallica's most cheerful songs
metallicaR %>%
  arrange(-valence) %>%
  select(name, valence) %>%
  head(5) %>%
  kable()

#Metallica's least cheerful songs
metallicaR %>%
  arrange(valence) %>%
  select(name, valence) %>%
  head(5) %>%
  kable()



