
library(rfishbase)
library(dplyr)
library(data.table)



All_data <- species()

Fish_only <- load_taxa() %>%
  filter(Class == "Teleostei") %>%
  select(SpecCode, Species, Family, Class) %>%
  left_join(All_data, by="SpecCode", relationship = "many-to-many") 



pelagic <- Fish_only %>%
    filter(DemersPelag == "bathypelagic" 
           | DemersPelag == "pelagic-neritic" 
           | DemersPelag == "benthopelagic" 
           | DemersPelag == "pelagic" 
           | DemersPelag=="pelagic-oceanic", 
           Saltwater == 1) %>%
    select(SpecCode, FBname, Species, Genus, Family, Class, Weight, LongevityWild, Length, LTypeMaxM, DemersPelag) %>%
  filter_at(vars(Weight), all_vars(!is.na(.)))


food <-fooditems()
maturity <- maturity()
fecundity <- fecundity()
larvae <- larvae()

#piscivorous <- food %>%
#  filter(FoodII=="finfish") %>%
#  select(SpecCode, FoodII)

predatory <- food %>%
  filter(FoodII=="finfish" | FoodII=="cephalopods") %>%
  select(SpecCode, FoodII) %>%
  group_by(SpecCode) %>%
  summarise(across(everything(), first, na.rm = TRUE))


maturity_data <- maturity %>%
  select(SpecCode, AgeMatMin, LengthMatMin) %>%
  group_by(SpecCode)%>%
  summarise(across(everything(), mean, na.rm = TRUE))
  

fecundity_data <- fecundity %>%
  select(SpecCode, FecundityMin, FecundityMax, FecundityMean) %>%
  group_by(SpecCode)%>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  mutate(Fecundityavg = case_when(
    !is.na(FecundityMean) ~ FecundityMean,
    !is.na(FecundityMax) & !is.na(FecundityMin) ~ (FecundityMax+FecundityMin)/2,
    TRUE ~ NA)) %>%
  select(SpecCode, Fecundityavg)


larvae_data <- larvae %>%
  select(SpecCode, LhMax, LhMin, LhMid) %>%
  group_by(SpecCode)%>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  mutate(Loffspring = case_when(
    !is.na(LhMid) ~ LhMid/10,
    !is.na(LhMin) & !is.na(LhMax) ~ (LhMin+LhMax)/2/10,
    TRUE ~ NA)) %>%
  select(SpecCode, Loffspring)

##/10 voor cm unit ipv mm!! 

#eggs <- fb_tbl("eggdev")
#egg_data <- eggs %>%
#  select(SpecCode, EggDevTime, EggDiameter) %>%
#  group_by(SpecCode)%>%
#  summarise(across(everything(), mean, na.rm = TRUE))

#spawning <- spawning()
#spawning_data <- spawning %>%
#  select(SpecCode, SpawningCycles)



#Pelagic_Piscivorous <- merge(pelagic, piscivorous,
#                             by.x = "SpecCode",
#                             by.y = "SpecCode")

#Pelagic_Piscivorous_Unique <- distinct(Pelagic_Piscivorous)

Fish_List <- pelagic %>%
  left_join(predatory, by="SpecCode", relationship = "many-to-many") %>%
  left_join(maturity_data, by="SpecCode", relationship = "many-to-many") %>%
  left_join(fecundity_data, by="SpecCode", relationship = "many-to-many") %>%
  left_join(larvae_data, by="SpecCode", relationship = "many-to-many") %>%
  #left_join(egg_data, by="SpecCode", relationship = "many-to-many") %>%
  filter_at(vars(Weight), all_vars(!is.na(.))) %>%
  filter(FoodII=="finfish" | FoodII=="cephalopods")
 # group_by(SpecCode) %>%
 # summarise(across(everything(), mean, na.rm = TRUE))

Fish_list_unique <- distinct(Fish_List)
  
cetacea <- filter(data, Category=="Cetacea")

write.csv(Fish_list_unique, file = "FishList_predatory_new.csv")


#food_test <- food %>%
 # group_by(FoodII) %>%
#  summarise(across(everything(), mean, na.rm = TRUE))

#food_test2 <- All_data %>%
 # left_join(All_data, by="SpecCode", relationship = "many-to-many") %>%
 # filter( == "Gadus") #%>%
 # select(Genus, SpecCode, FoodI, FoodII, FoodIII) #%>%
 # group_by(DemersPelag) %>%
# summarise(across(everything(), mean, na.rm = TRUE))

#larval_dynamics <- fb_tbl("larvdyn")
#eggs <- fb_tbl("eggdev")
#reproduction <- fb_tbl("reproduc")
