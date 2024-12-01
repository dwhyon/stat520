library(tidyverse)



# API Master --------------------------------------------------------------

apiMaster <- read_csv("./master.csv")



apiMaster %>% 
  select(country) %>% 
  distinct() %>% 
  arrange(country)




# Crosswalk ---------------------------------------------------------------

crosswalk <- read_csv("data/raw/data-verbose.csv", col_names = FALSE)



# Econ Data


econMaster <- read_csv("data/clean/masterEcon.csv")






# Join --------------------------------------------------------------------

joinMaster <- apiMaster %>% 
  left_join(crosswalk, by = join_by(country == X2)) %>% 
  left_join(econMaster, by = join_by(X3 == Country, year == Year))



write_csv(joinMaster, "./master2.csv")

