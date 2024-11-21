library(tidyverse)
library(readxl)




# Load in Cleaned Data ----------------------------------------------------
###############################################
#WHO LIFE EXPECtancy
lifeExpecRaw <- read_xlsx("./data/raw/WHO_Life_Expectancy.xlsx") %>% 
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
  slice(-1) %>% 
  slice(-4071)

colnames(lifeExpec) <- final



# lifeExpec <- lifeExpec %>% 
#   mutate(
#     across(starts_with("Life"), ~ str_replace_all(.x, "((?<=\\s).+)", "") ),
#     across(starts_with("Life"), str_trim),
#     across(starts_with("Life"), as.numeric)
#   )

#Export
write_csv(lifeExpec, "./data/interim/WHOlifeExpecClean.csv")



#################################################
# BCG Immunity
bcg_immun <- read_xlsx("./data/raw/BCG_immunization_coverage.xlsx")


bcg_immun %>% view()


write_csv(bcg_immun, "./data/interim/bcgClean.csv")

####################################################
# Alcohol ADI
adiRaw <- read_xlsx("./data/raw/Alcohol_ADI_among_drinkers (needs cleaning).xlsx", skip = 2)


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

write_csv(adi, "./data/interim/adiClean.csv")

####################################################
# Alcohol Consumption Per capita

alcRaw <- read_xlsx("./data/raw/Alcohol_consumption_per_capita (needs cleaning).xlsx", skip = 2)

alc <- alcRaw %>% 
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

write_csv(alc, "./data/interim/alcClean.csv")


#########################################################
# DTP3 Immun


dtp3Raw <- read_xlsx("./data/raw/DTP3_immunization_coverage (needs cleaning).xlsx",
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

write_csv(dtp3, "./data/interim/dtp3Clean.csv")




#########################################################
# Hepb3 Immun


hepb3Raw <- read_xlsx("./data/raw/HepB3_immunization_coverage (needs cleaning).xlsx",
                     skip = 2
)

hepb3 <- noSexFunc(hepb3Raw, "hepb3_1yo_perc")

view(hepb3, "clean")

write_csv(hepb3, "./data/interim/hepb3Clean.csv")


#########################################################
# hib3 Immun


hib3Raw <- read_xlsx("./data/raw/Hib3_immunization_coverage (needs cleaning).xlsx",
                      skip = 2
)

hib3 <- noSexFunc(hib3Raw, "hib3_1yo_perc")

view(hib3, "clean")

write_csv(hepb3, "./data/interim/hepb3Clean.csv")



#########################################################
# HPV Immun


hpvRaw <- read_xlsx("./data/raw/HPV_immunization_coverage (needs cleaning).xlsx",
                     skip = 2
)

hpv <- noSexFunc(hpvRaw, "hpv_9_14_yo_girls_perc")

view(hpv, "clean")

write_csv(hpv, "./data/interim/hepb3Clean.csv")

#########################################################
# MCV1


mcv1Raw <- read_xlsx("./data/raw/MCV1_immunization_coverage (needs cleaning).xlsx",
                    skip = 2
)

mcv1 <- noSexFunc(mcv1Raw, "mcv1_1_yo_perc")

view(mcv1, "clean")

write_csv(mcv1, "./data/interim/mcv1Clean.csv")

#########################################################
# MCV2


mcv2Raw <- read_xlsx("./data/raw/MCV2_immunization_coverage (needs cleaning).xlsx",
                     skip = 2
)

mcv2 <- noSexFunc(mcv2Raw, "mcv2_perc")

view(mcv2, "clean")

write_csv(mcv2, "./data/interim/mcv2Clean.csv")




#########################################################
# PAB


pabRaw <- read_xlsx("./data/raw/PAB_immunization_coverage (needs cleaning).xlsx",
                     skip = 2
)

pab <- noSexFunc(pabRaw, "pab_perc")

#view(mcv2, "clean")

write_csv(pab, "./data/interim/pabClean.csv")


#########################################################
# PCV3


pcv3Raw <- read_xlsx("./data/raw/PCV3_immunization_coverage (needs cleaning).xlsx",
                    skip = 2
)

pcv3 <- noSexFunc(pcv3Raw, "pcv3_1_yo_perc")

#view(mcv2, "clean")

write_csv(pcv3, "./data/interim/pcv3Clean.csv")


#########################################################
# POL3


pol3Raw <- read_xlsx("./data/raw/Pol3_immunization_coverage (needs cleaning).xlsx",
                     skip = 2
)

pol3 <- noSexFunc(pol3Raw, "pol3_1_yo_perc")

#view(mcv2, "clean")

write_csv(pol3, "./data/interim/pol3Clean.csv")

#########################################################
# ROTAC


rotaCRaw <- read_xlsx("./data/raw/RotaC_immunization_coverage (needs cleaning).xlsx",
                     skip = 2
)

rotaC <- noSexFunc(rotaCRaw, "rotaC_1_yo_perc")

#view(mcv2, "clean")

write_csv(rotaC, "./data/interim/rotaCClean.csv")



#################################################################

# School

avgSchoolRaw <- read_xlsx("data/raw/average hours of school.xlsx")

avgSchoolRaw %>% 
  pivot_longer(`2000`:`2023`, names_to = "Year", values_to = "avg_school")



##################################################################


#Researcher per Million

researcherRaw <- read_csv("data/raw/researcher per million inhabitants.csv")


researcher <- researcherRaw %>% 
  pivot_longer(`2000`:`2022`, names_to = "Year", values_to = "researcher_per_million")

# 
# #School 25 and up
# 
# school25Raw <- read_csv("data/raw/mean average year of school 25 years and older .csv")
#   


#######################################################

# Status 

statusRaw <- read_xlsx("./data/raw/country status imf.xlsx")

# Employment

unemploymentRaw <- read_xls("./data/raw/imf-dm-export-20241116.xls")

unemployment <- unemploymentRaw %>% 
  pivot_longer(cols = 2:31, names_to = "Year", values_to = "unemployment") %>% 
  rename(Country = `Unemployment rate (Percent)`) %>% 
  mutate(
    unemployment = as.numeric(unemployment)
  ) %>% 
  full_join(statusRaw)




#GDP


gdpRaw <- read_xls("./data/raw/IMF_GDP_per_capita (Need to unpivot).xls")

gdp <- gdpRaw %>% 
  pivot_longer(cols = 2:23, names_to = "Year", values_to = "GDP_percapita_USD") %>% 
  rename(Country = "GDP per capita, current prices\n (U.S. dollars per capita)") %>% 
  left_join(unemployment) %>% 
  mutate(
    Country = case_when(
      Country == "Bahamas, The" ~ "Bahamas",
      Country == "Bolivia" ~ "Bolivia (Plurinational State of)",
      Country == "China, People's Republic of" ~ "China",
      Country == "Congo, Dem. Rep. of the" ~ "Democratic Republic of the Congo",
      Country == "Congo, Republic of" ~ "Congo",
      Country == "Côte d'Ivoire" ~ "Cote d'Ivoire",
      Country == "Côte d'Ivoire" ~ "Cote d'Ivoire",
      Country == "Czech Republic" ~ "Czechia",
      Country == "Gambia, The" ~ "Gambia",
      Country == "Iran" ~ "Iran (Islamic Republic of)",
      Country == "Korea, Republic of" ~ "Republic of Korea",
      Country == "Kyrgyz Republic" ~ "Kyrgyzstan",
      Country == "Lao P.D.R." ~ "Lao People's Democratic Republic",
      Country == "Micronesia, Fed. States of" ~ "Micronesia (Federated States of)",
      Country == "Moldova" ~ "Republic of Moldova",
      Country == "Netherlands" ~ "Netherlands (Kingdom of the)",
      Country == "São Tomé and Príncipe" ~ "Sao Tome and Principe",
      Country == "Slovak Republic" ~ "Slovakia",
      Country == "South Sudan, Republic of" ~ "South Sudan",
      Country == "Syria" ~ "Syrian Arab Republic",
      Country == "Türkiye, Republic of" ~ "Türkiye",
      Country == "United Kingdom" ~ "United Kingdom of Great Britain and Northern Ireland",
      Country == "Tanzania" ~ "United Republic of Tanzania",
      Country == "United States" ~ "United States of America",
      Country == "Venezuela" ~ "Venezuela (Bolivarian Republic of)",
      Country == "Vietnam" ~ "Viet Nam",
      Country == "West Bank and Gaza" ~ 
        "occupied Palestinian territory, including east Jerusalem",
      .default = Country
      
    )
  )


# 
# 
# gdpCountry <- gdp %>% 
#   select(Country) %>% 
#   unique() %>% 
#   mutate(
#     Data = "gdp"
#   )
# 
# 
# masterCountry <- master %>% 
#   select(Country) %>% 
#   unique()%>% 
#   mutate(
#     Data = "master"
#   )
# 
# test <- full_join(gdpCountry, masterCountry, by = join_by(Country)) %>% 
#   filter(if_any(everything(), is.na))


write_csv(gdp, "./data/interim/gdpClean.csv")







# Join Datasets -----------------------------------------------------------

immunGDP <- adi %>% 
  full_join(alc, by = join_by(Location, Period)) %>% 
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
  left_join(gdp) %>% 
  full_join(lifeExpec) %>% 
  arrange(Country, Year) %>% 
  mutate(
    across(starts_with(c("Life", "adi", "apc")), ~ str_replace_all(.x, "((?<=\\s).+)", "") ),
    across(starts_with(c("Life", "adi", "apc")), str_trim),
    across(starts_with(c("Life", "adi", "apc")), as.numeric)
  ) 







# HIV ---------------------------------------------------------------------

hivRaw <- read_csv("./data/raw/HIV_prevelance_newData.csv")

hiv <- hivRaw %>% 
  pivot_longer(cols = 2:24, names_to = "Year", values_to = "hiv") %>% 
  separate(hiv,  c("hiv", NA), sep = "\\s") %>% 
  mutate(
    hiv2 = case_when(hiv == "No" ~ NA,
                     hiv == "<0.1" ~ "0.05",
                     .default = hiv),
    hiv_perc = as.numeric(hiv2),
    .keep = "unused",
  ) %>%
  select(-hiv2) 





# hivCountry <- hiv %>%
#   select(Country) %>%
#   unique() %>%
#   mutate(
#     Data = "hiv"
#   )
# 
# 
# immunGDPCountry <- immunGDP %>%
#   select(Country) %>%
#   unique()%>%
#   mutate(
#     Data = "master"
#   )
# 
# test <- full_join(hivCountry, immunGDPCountry, by = join_by(Country)) %>%
#   filter(if_any(everything(), is.na))


master <- immunGDP %>% 
  mutate(
    Country = if_else(Country == "Türkiye", "Turkiye", Country)
  ) %>% 
  full_join(hiv) 



write_csv(master, "./data/clean/master.csv")




# 
# # Check Artifacts ---------------------------------------------------------
# 
# datasets <- list(
#   adi, alcClean, dtp3, gdp, hepb3, hib3, hpv, mcv1, mcv2, pab, pcv3, pol3,
#   rotaC
# )
# 
# 
# findFunc <- function(tibble) {
#   
#   
#    
#     vec <- str_detect(tibble$Location, "Appl")
#     
#     match(TRUE, vec)
#   
#   
# }
# 
# findFunc(hepb3)
# 
# 
# map(datasets, findFunc)

# 
# # WHO KAGGLE COMBINE Data
# lifeExpec2 <- read_xlsx("./data/Life_Expectancy_Data.xlsx")
# 
# lifeExpec2 %>% view()
