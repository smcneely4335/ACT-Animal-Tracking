# Session 3

# load libraries
library(tidyverse)
library(hexbin)
library(patchwork)
library(RSQLite)

# set wd
setwd('C:/Users/mcneelys/OneDrive - Smithsonian Institution/Desktop/R/R for Ecologists')
getwd()

surveys <- read_csv("data_raw/portal_data_joined.csv")

# Mutate ----

surveys %>% 
  mutate(weight_kg = weight/1000)

surveys %>%
  mutate(weight_kg = weight/1000,
         weight_lb = weight_kg * 2.2) %>% 
  head()

surveys %>% 
  filter(!is.na(weight)) %>% 
  mutate(weight_kg = weight/1000) %>% 
  head()

# CHALLENGE #
# create a new data frame from the surveys data that:
# contains only the species_id column and a new column called hindfoot_cm
# containing the hindfoot_length values (currently in mm) converted to
# cm. In this hindfoot_cm column, there are no NAs and all values are 
# less than 3.

new_df <- surveys %>% 
  filter(!is.na(hindfoot_length)) %>% 
  mutate(hindfoot_cm = hindfoot_length/10) %>% 
  filter(hindfoot_cm < 3) %>% 
  select(species_id, hindfoot_cm)

view(new_df)

# split apply combine ----

# get mean weight of each sex
surveys %>% 
  group_by(sex) %>% 
  summarize(mean_weight = mean(weight, na.rm = TRUE))

# get mean weight of each sex for each species
surveys %>% 
  filter(!is.na(weight)) %>% 
  group_by(sex, species_id) %>% 
  summarize(mean_weight = mean(weight)) 

surveys %>% 
  filter(!is.na(weight)) %>% 
  group_by(sex, species_id) %>% 
  summarize(mean_weight = mean(weight),
            min_weight = min(weight)) %>% 
  arrange(desc(min_weight))

# counting ----

surveys %>% 
  count(sex)

surveys %>% 
  group_by(sex) %>% 
  summarize(count = n()) # same as the above

surveys %>% 
  count(sex, sort = TRUE)

surveys %>% 
  count(sex, species) %>% 
  arrange(species, desc(n))

# CHALLENGE #
# 1) how many animals were caught in each plot_type surveyed?
colnames(surveys)
surveys %>% 
  count(plot_type)

# 2) Use group_by() and summarize() to find the mean, min, and max 
# hindfoot length for each species (using species_id). Also add the 
# number of observations.
surveys %>% 
  filter(!is.na(hindfoot_length)) %>% 
  group_by(species_id) %>% 
  summarize(mean_hfl = mean(hindfoot_length), 
            min_hfl = min(hindfoot_length),
            max_hfl = max(hindfoot_length),
            count_hfl = n()) %>% 
  print(n = 48)

# 3) What was the heaviest animal measured in each year? Returns the 
# columns year, genus, species_id, and weight.
surveys %>% 
  filter(!is.na(weight)) %>% 
  group_by(year) %>% 
  filter(weight == max(weight)) %>% 
  select(year, genus, species, weight) %>% 
  arrange(year)

# wider vs longer table ----

surveys_gw <- surveys %>% 
  filter(!is.na(weight)) %>% 
  group_by(plot_id, genus) %>% 
  summarize(mean_weight = mean(weight))

surveys_gw
str(surveys_gw)

# take the data and form it into a wider table
surveys_wide <- surveys_gw %>% 
  pivot_wider(names_from = genus, values_from = mean_weight)

surveys_wide

# take the data back from the wider form to longer
surveys_long <- surveys_wide %>% 
  pivot_longer(names_to = 'genus', values_to = 'mean_weight', cols = -plot_id)

surveys_long

# exporting data ----

surveys_complete <- surveys %>% 
  filter(!is.na(weight),
         !is.na(hindfoot_length),
         !is.na(sex))

species_counts <- surveys_complete %>% 
  count(species_id) %>% 
  filter(n >= 50)

species_counts

surveys_complete <- surveys_complete %>% 
  filter(species_id %in% species_counts$species_id)

dim(surveys_complete)

write_csv(surveys_complete, file = "data/surveys_complete.csv")

# Intro to plotting ----
# ggplot(data = <DATA>, mapping = aes(<MAPPING>)) + <GEOM_FUNCTION>()

ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length)) +
  geom_point()

surveys_plot <- ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length))
surveys_plot + geom_point()
surveys_plot + geom_hex()

ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length)) +
  geom_point(alpha = 0.1, aes(color = species_id))

ggplot(data = surveys_complete, mapping = aes(x = species_id, y = weight)) +
  geom_boxplot(alpha = 0.1) +
  geom_jitter()
