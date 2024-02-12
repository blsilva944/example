##  Report on Activities 2023

library(krsp)
library (tidyverse)
library(lubridate)

#connections
con <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                     dbname ="krsp",
                     username = Sys.getenv("krsp_user"),
                     password = Sys.getenv("krsp_password")
)

con2 <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                     dbname ="krsp2023",
                     username = Sys.getenv("krsp_user"),
                     password = Sys.getenv("krsp_password")
)

# Start and End Date
date_s<-"2022-06-01"
date_e<-"2023-05-31"


grids<-c("KL", "SU", "LL", "AG", "JO", "BT", "SUX")

###########################
# Import and Arrange Data #
###########################

trapping_current<- tbl(con, "trapping") %>% 
  rename(grid = gr) %>% 
  mutate(year = year(date)) %>% 
  filter(date>=date_s,
       date<=date_e) %>% 
  collect(n = Inf) %>% 
  mutate(squirrel_id = as.integer(squirrel_id),
         date = ymd(date))


trapping_current2<- tbl(con2, "trapping") %>% 
  rename(grid = gr) %>% 
  mutate(year = year(date)) %>% 
  filter(date>=date_s,
         date<=date_e) %>% 
  collect(n = Inf) %>% 
  mutate(squirrel_id = as.integer(squirrel_id),
         date = ymd(date))


trapping_current<-trapping_current %>% 
  rbind(trapping_current2)


# Behaviour
behavior_current<- tbl(con, "behaviour") %>% 
  mutate(year = year(date)) %>% 
  filter(date>=date_s,
         date<=date_e) %>% 
  collect(n = Inf) %>% 
  mutate(squirrel_id = as.integer(squirrel_id),
         date = ymd(date))


behavior_current2<- tbl(con2, "behaviour") %>% 
  mutate(year = year(date)) %>% 
  filter(date>=date_s,
         date<=date_e) %>% 
  collect(n = Inf) %>% 
  mutate(squirrel_id = as.integer(squirrel_id),
         date = ymd(date))


behavior_current<-behavior_current %>% 
  rbind(behavior_current2)


# Juvenile Tables
juvenile_current<-tbl(con, "juvenile") %>% 
  select(squirrel_id, litter_id, weight, tagwt = tagWT, dna1, tagLft) %>% 
  collect()

juvenile_current2<-tbl(con2, "juvenile") %>% 
  select(squirrel_id, litter_id, weight, tagwt = tagWT, dna1, tagLft) %>% 
  collect()

juvenile_current<-juvenile_current %>%
  rbind(juvenile_current2)

# Litter Tables
litter_current<-tbl(con, "litter") %>% 
  filter(grid %in% grids) %>% 
  # remove invalid breeding codes
  filter(!is.na(br), !is.na(yr),
         br >= 0, br <= 7) %>% 
  mutate(date = coalesce(fieldBDate, date1, tagDt)) %>% 
  rename(year = yr) %>% 
  collect(n = Inf) %>% 
  mutate(squirrel_id = as.integer(squirrel_id),
         date = ymd(date))

litter_current2<-tbl(con2, "litter") %>% 
  filter(grid %in% grids) %>% 
  # remove invalid breeding codes
  filter(!is.na(br), !is.na(yr),
         br >= 0, br <= 7) %>% 
  mutate(date = coalesce(fieldBDate, date1, tagDt)) %>% 
  rename(year = yr) %>% 
  collect(n = Inf) %>% 
  mutate(squirrel_id = as.integer(squirrel_id),
         date = ymd(date))

litter_current<-litter_current %>%
  rbind(litter_current2) %>% 
  rename (litter_id = id,
          mother_id = squirrel_id)

juvenile_current<-juvenile_current %>% 
  left_join(litter_current, by="litter_id") %>% 
  mutate (date1=ymd(date1),
          tagDt=ymd(tagDt)) %>% 
  filter(date1>=date_s,
         date1<=date_e)



##################
# Data Summaries #
##################

###############################
# Number of Animals Monitored #
###############################

trap_temp<-trapping_current %>% 
  select(squirrel_id)

behav_temp<-behavior_current %>% 
  select(squirrel_id)

juv_temp<-juvenile_current %>% 
  select(squirrel_id)

animals_monitored<-trap_temp %>% 
  rbind(behav_temp) %>% 
  rbind(juv_temp) %>% 
  filter(!is.na(squirrel_id)) %>% 
  group_by(squirrel_id) %>% 
  summarize(n=n())

# Number of animals monitored between start date and end date
nrow (animals_monitored)


######################################################
# Other Information that Might be Helpful for Others #
######################################################

## Bycatch
trapping_current %>%
  filter(ft %in% c(91, 92, 93, 94)) %>% 
  group_by (ft) %>% 
  summarise(sum(!is.na(date))) %>% 
  print()
#91 is chickadee
#92 is gray jay
#93 is ground squirrel
#94 is chipmunk
  
# Remove bycatch records and move on
trapping_current <-trapping_current %>% 
  filter(!is.na(squirrel_id),
       !(ft %in% c(91, 92, 93, 94)))

# Number of captures
nrow(trapping_current)

# Number of squirrels captured
trapping_current %>% 
  group_by(squirrel_id) %>% 
  summarise(sum(!is.na(squirrel_id))) %>% 
  nrow()
 
# Un-natural deaths
trapping_current %>%
  filter(ft %in% c(4, 5, 12, 22)) %>% 
  group_by (ft) %>% 
  summarise(sum(!is.na(date))) %>% 
  print()

# ft 4 = new animal dead in trap
# ft 5 = recap dead in trap
# ft 12 = handling death
# ft 22 = planned euthanasia

# Released unhandled excluded
trapping_current %>%
  filter(ft !=21) %>% 
  nrow()

# Released unhandled excluded
trapping_current %>%
  filter(ft !=21) %>% 
  group_by(squirrel_id) %>% 
  summarise(sum(!is.na(squirrel_id))) %>% 
  nrow()



# Number of Adult Ear Taggings
tag_subset<-c(2,3,7,13)
tags_trapped<-group_by(trapping_current, squirrel_id) %>% 
  filter(ft %in% tag_subset) %>% 
  summarise(sum(!is.na(squirrel_id))) %>% 
  nrow()
tags_trapped

#Number of Juvenile Taggings
tags_in_nest<-juvenile_current %>% 
  filter(!is.na(tagLft)) %>% 
  nrow()
tags_in_nest

# Total Number of Taggings
total_tags<-tags_trapped+tags_in_nest
total_tags


##################
# DNA Collection #
##################

dna_subset<-c(2,7,13)
dna_trapped<-group_by(trapping_current, squirrel_id) %>% 
  filter(!is.na(dna1)) %>% 
  nrow()
dna_trapped

dna_in_nest<-juvenile_current %>% 
  filter(!is.na(dna1)) %>% 
  nrow()
dna_in_nest

total_dna<-dna_trapped+dna_in_nest
total_dna


# Radio-Collaring #
###################

trapping_current%>% 
  filter(radio==1) %>% 
  nrow()

