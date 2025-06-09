### Plot showing change over time for key groups ####
### By Nyssa Silbiger #####
### Created on June 9th, 2025 ########
#################################

# load libraries ####
library(here)
library(tidyverse)
library(lubridate)


### read in datasets ##### 
#LTER coral forereef and fringe
coral_lter_ff<- read_csv(here("Data","LTER","coral_quads_fore_fring.csv"))

### Bob's data
coral_algae_survey<- read_csv(here("Data","LTER","MCR_LTER_Annual_Survey_Benthic_Cover_20241219.csv"))


## remove everything that isn't coral
coral_lter_summary<- coral_lter_ff %>%
  mutate(Percent_Cover = as.numeric(Percent_Cover))%>% # this  is a character for some reason
  filter(!Taxonomy_Substrate_or_Functional_Group %in% c("CTB", "Sand", 
            "Non-coralline Crustose Algae",
            "Lithophyllon", 
            "Unknown or Other")) %>%
  group_by(Date, Location, Site, Habitat, Depth, Quad40) %>%
  summarise(quad_cover = sum(Percent_Cover))  %>% # sum by quadrat
  separate(Date, sep = "-", into = c("Year","Month")) %>%
  mutate(Year = as.numeric(Year)) %>%
  group_by(Year, Site, Habitat, Depth) %>%
  summarise(mean_cover = mean(quad_cover, na.rm = TRUE),
            se_cover = sd(quad_cover, na.rm = TRUE)/sqrt(n()))
  
### plot coral data with pete
coral_lter_summary %>%
  ggplot(aes(x = Year, y = mean_cover, color = as.factor(Depth)))+
  geom_point()+
  geom_line()+
  facet_wrap(Site~Habitat)


### Cleaning coral data with Bob
# Algae: sum all algae/turf, remove CCA
algae.names<-c("Acanthophora spicifera","Actinotrichia fragilis","Algal Turf",
               "Amansia rhodantha","Amphiroa fragilissima",
               "Asparagopsis taxiformis","Boodlea kaeneana","Caulerpa peltata",
               "Caulerpa pickeringii","Caulerpa racemosa","Caulerpa serrulata",
               "Chaetomorpha antennina","Chlorodesmis fastigiata",
               "Chnoospora implexa","Cladophoropsis luxurians",
               "Cladophoropsis membranacea","Codium geppiorum",
               "Coelothrix irregularis","Colpomenia sinuosa","Damselfish Turf",
               "Dichotomaria marginata","Dichotomaria obtusata",
               "Dictyosphaeria cavernosa","Dictyosphaeria versluysii","Dictyota bartayresiana",
               "Dictyota divaricata","Dictyota friabilis","Dictyota hamifera",
               "Dictyota implexa","Dictyota sp.","Galaxaura filamentosa","Galaxaura rugosa",
               "Galaxaura sp.","Gelidiella acerosa","Gelidiella sp.",
               "Gibsmithia hawaiiensis","Halimeda discoidea","Halimeda distorta",
               "Halimeda incrassata","Halimeda macroloba","Halimeda minima",
               "Halimeda opuntia","Halimeda sp.","Halimeda taenicola",
               "Hydroclathrus clathratus","Hypnea spinella","Jania sp.",
               "Liagora ceranoides","Lobophora variegata","Martensia elegans",
               "Microdictyon okamurae","Microdictyon umbilicatum",
               "Neomeris vanbosseae","Padina boryana","Phyllodictyon anastomosans",
               "Ralfsia sp.","Rhipidosiphon javensis","Sargassum pacificum","Turbinaria ornata","Valonia aegagropila","Valonia ventricosa")         

coral_algae_summary <- coral_algae_survey %>%
  mutate(coral_algae = case_when(Taxonomy_Substrate_Functional_Group == "Coral" ~ "Coral",
                                 Taxonomy_Substrate_Functional_Group %in% c("Lithophyllum kotschyanum","Crustose Corallines") ~ "CCA",
                                 Taxonomy_Substrate_Functional_Group %in% algae.names ~ "Macroalgae")) %>%
  group_by(Year, Habitat, Site, Depth, Transect, Quadrat coral_algae, ) %>%
  
