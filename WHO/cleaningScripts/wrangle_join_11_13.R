library(tidyverse)
library(readxl)




# Load in Cleaned Data ----------------------------------------------------
###############################################
#WHO LIFE EXPECtancy
lifeExpecRaw <- read_xlsx("./data/WHO_Life_Expectancy.xlsx") %>% 
  filter(Indicator != "Location")



# Create new column names
name1 <- lifeExpecRaw %>% 
  slice(1) %>% 
  as.character()

name1[2] <- "NA"

name2 <- lifeExpecRaw %>% 
  colnames()


final <- str_c(name2, name1)

final[1] <- "Country"
final[2] <- "Year"
lifeExpecRaw %>% view()

final <- str_replace(final, "\\.\\.\\.[:digit:]", " ")

# Rename columns
lifeExpec <- lifeExpecRaw %>% 
  slice(-1)

colnames(lifeExpec) <- final

#Export
write_csv(lifeExpec, "./cleanedData/WHOlifeExpecClean.csv")



#################################################
# BCG Immunity
bcg_immun <- read_xlsx("./data/BCG_immunization_coverage.xlsx")


bcg_immun %>% view()


write_csv(bcg_immun, "./cleanedData/bcgClean.csv")

####################################################
# Alcohol ADI
adiRaw <- read_xlsx("./data/Alcohol_ADI_among_drinkers (needs cleaning).xlsx", skip = 2)


adi <- adiRaw %>% 
  mutate(
    Indicator = "adi_grams"
  ) %>% 
  unite(Indicator, Indicator, Dim1) %>%
  mutate(
    Indicator = str_replace_all(Indicator, "[:space:]", "_")
  ) %>% 
  select(1:4) %>%
  pivot_wider(names_from = Indicator, values_from = Tooltip) 

write_csv(adi, "./cleanedData/adiClean.csv")

####################################################
# Alcohol Consumption Per capita

alcRaw <- read_xlsx("./data/Alcohol_consumption_per_capita (needs cleaning).xlsx", skip = 2)

alcClean <- alcRaw %>% 
  #view()
  mutate(
    Indicator = "apc_liters"
  ) %>% 
  #unite(Indicator, Indicator, Dim1) %>%
  # mutate(
  #   Indicator = str_replace_all(Indicator, "[:space:]", "_")
  # ) %>% 
  select(1:4) %>%
  pivot_wider(names_from = Indicator, values_from = Tooltip) #%>% 
  #view()

write_csv(alcClean, "./cleanedData/alcClean.csv")


#########################################################
# DTP3 Immun


dtp3Raw <- read_xlsx("./data/DTP3_immunization_coverage (needs cleaning).xlsx",
                     skip = 2
                     )

noSexFunc <- function(tibble, varName){
  
  tibble %>% 
    mutate(
      Indicator = varName
    ) %>% 
    select(1:4) %>%
    pivot_wider(names_from = Indicator, values_from = Tooltip)

}


dtp3 <- noSexFunc(dtp3Raw, "dtp3_1yo_perc")

view(dtp3, "clean")

write_csv(dtp3, "./cleanedData/dtp3Clean.csv")




#########################################################
# Hepb3 Immun


hepb3Raw <- read_xlsx("./data/HepB3_immunization_coverage (needs cleaning).xlsx",
                     skip = 2
)

hepb3 <- noSexFunc(hepb3Raw, "hepb3_1yo_perc")

view(hepb3, "clean")

write_csv(hepb3, "./cleanedData/hepb3Clean.csv")


#########################################################
# hib3 Immun


hib3Raw <- read_xlsx("./data/Hib3_immunization_coverage (needs cleaning).xlsx",
                      skip = 2
)

hib3 <- noSexFunc(hib3Raw, "hib3_1yo_perc")

view(hib3, "clean")

write_csv(hepb3, "./cleanedData/hepb3Clean.csv")



#########################################################
# HPV Immun


hpvRaw <- read_xlsx("./data/HPV_immunization_coverage (needs cleaning).xlsx",
                     skip = 2
)

hpv <- noSexFunc(hpvRaw, "hpv_9_14_yo_girls_perc")

view(hpv, "clean")

write_csv(hpv, "./cleanedData/hepb3Clean.csv")

#########################################################
# MCV1


mcv1Raw <- read_xlsx("./data/MCV1_immunization_coverage (needs cleaning).xlsx",
                    skip = 2
)

mcv1 <- noSexFunc(mcv1Raw, "mcv1_1_yo_perc")

view(mcv1, "clean")

write_csv(mcv1, "./cleanedData/mcv1Clean.csv")

#########################################################
# MCV2


mcv2Raw <- read_xlsx("./data/MCV2_immunization_coverage (needs cleaning).xlsx",
                     skip = 2
)

mcv2 <- noSexFunc(mcv2Raw, "mcv2_perc")

view(mcv2, "clean")

write_csv(mcv2, "./cleanedData/mcv2Clean.csv")




#########################################################
# PAB


pabRaw <- read_xlsx("./data/PAB_immunization_coverage (needs cleaning).xlsx",
                     skip = 2
)

pab <- noSexFunc(pabRaw, "pab_perc")

#view(mcv2, "clean")

write_csv(pab, "./cleanedData/pabClean.csv")


#########################################################
# PCV3


pcv3Raw <- read_xlsx("./data/PCV3_immunization_coverage (needs cleaning).xlsx",
                    skip = 2
)

pcv3 <- noSexFunc(pcv3Raw, "pcv3_1_yo_perc")

#view(mcv2, "clean")

write_csv(pcv3, "./cleanedData/pcv3Clean.csv")


#########################################################
# POL3


pol3Raw <- read_xlsx("./data/Pol3_immunization_coverage (needs cleaning).xlsx",
                     skip = 2
)

pol3 <- noSexFunc(pol3Raw, "pol3_1_yo_perc")

#view(mcv2, "clean")

write_csv(pol3, "./cleanedData/pol3Clean.csv")

#########################################################
# ROTAC


rotaCRaw <- read_xlsx("./data/RotaC_immunization_coverage (needs cleaning).xlsx",
                     skip = 2
)

rotaC <- noSexFunc(rotaCRaw, "rotaC_1_yo_perc")

#view(mcv2, "clean")

write_csv(rotaC, "./cleanedData/rotaCClean.csv")


#######################################################

#GDP


gdpRaw <- read_xls("./data/IMF_GDP_per_capita (Need to unpivot).xls")

gdp <- gdpRaw %>% 
  pivot_longer(cols = 2:23, names_to = "Year", values_to = "GDP_USD") %>% 
  rename(Country = "GDP per capita, current prices\n (U.S. dollars per capita)")


write_csv(gdp, "./cleanedData/gdpClean.csv")





# Join Datasets -----------------------------------------------------------

master <- adi %>% 
  full_join(alcClean, by = join_by(Location, Period)) %>% 
  full_join(dtp3) %>% 
  full_join(hepb3) %>% 
  full_join(hib3) %>% 
  full_join(hpv) %>%
  full_join(mcv1) %>%
  full_join(mcv2) %>%
  full_join(pab) %>%
  full_join(pcv3) %>%
  full_join(pol3) %>%
  full_join(rotaC) %>%
  mutate(
    Period = as.numeric(Period)
  ) %>%
  full_join(bcg_immun) %>% 
  mutate(
    Country = Location,
    Year = as.character(Period),
    .keep = "unused"
  ) %>% 
  select(Country, Year, everything()) %>% 
  full_join(gdp) %>% 
  full_join(lifeExpec) 

write_csv(master, "./cleanedData/master.csv")


master %>% 
  arrange(Country, Year) %>% 
  view()


# WHO KAGGLE COMBINE Data
lifeExpec2 <- read_xlsx("./data/Life_Expectancy_Data.xlsx")

lifeExpec2 %>% view()
