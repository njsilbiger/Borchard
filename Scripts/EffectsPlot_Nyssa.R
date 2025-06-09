### Plot showing change over time for key groups ####
### By Nyssa Silbiger #####
### Created on June 9th, 2025 ########
#################################

# load libraries ####
library(here)
library(tidyverse)
library(lubridate)
library(broom)
library(patchwork)


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
turf.names<-c("Algal Turf","Damselfish Turf")
algae.names<-c("Acanthophora spicifera","Actinotrichia fragilis",
               "Amansia rhodantha","Amphiroa fragilissima",
               "Asparagopsis taxiformis","Boodlea kaeneana","Caulerpa peltata",
               "Caulerpa pickeringii","Caulerpa racemosa","Caulerpa serrulata",
               "Chaetomorpha antennina","Chlorodesmis fastigiata",
               "Chnoospora implexa","Cladophoropsis luxurians",
               "Cladophoropsis membranacea","Codium geppiorum",
               "Coelothrix irregularis","Colpomenia sinuosa",
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
               "Ralfsia sp.","Rhipidosiphon javensis",
               "Sargassum pacificum","Turbinaria ornata",
               "Valonia aegagropila","Valonia ventricosa")         

coral_algae_summary <- coral_algae_survey %>%
  mutate(coral_algae = case_when(Taxonomy_Substrate_Functional_Group == "Coral" ~ "Coral",
                                 Taxonomy_Substrate_Functional_Group %in% c("Lithophyllum kotschyanum","Crustose Corallines") ~ "CCA",
                                 Taxonomy_Substrate_Functional_Group %in% algae.names ~ "Macroalgae")) %>%
  group_by(Year, Habitat, Site, Depth, Transect, Quadrat, coral_algae) %>%
  summarise(sum_cover = sum(Percent_Cover, na.rm = TRUE)) %>% # sum across the quads
  group_by(Year, Habitat,Site, Depth, coral_algae) %>%
  summarise(mean_cover = mean(sum_cover, na.rm = TRUE),
            se_cover = sd(sum_cover, na.rm = TRUE)) %>%
  drop_na(coral_algae) %>%# drop the "other" category
  filter(Depth != 17)  %>%# drop 17m
  mutate(Habitat = factor(Habitat, 
                             levels = c("Fringing","Backreef","Forereef")))


  ## make a timeseries plot (call "Macroalgae/Turf)
time_series_plot<-coral_algae_summary %>%
  filter(coral_algae != "CCA")%>%
  group_by(Year, Habitat, Depth, coral_algae) %>%
  summarise(mean_cover2 = mean(mean_cover, na.rm = TRUE),
            se_cover = sd(mean_cover, na.rm = TRUE)) %>%
  ggplot(aes(x = Year, y = mean_cover2, color = coral_algae))+
  geom_vline(xintercept  = c(2005,2010, 2019, 2024), lty = 2)+
  geom_line(size = 1.5)+
  geom_ribbon(aes(x = Year, ymax = mean_cover2+se_cover, ymin = mean_cover2-se_cover, 
                  fill =coral_algae), alpha = 0.1, color = NA)+
  scale_color_manual(values = c("coral","darkgreen"))+
  #xlim(0,75)+
  coord_cartesian(ylim = c(0,75))+
  labs(x = "",
       y = "% Cover",
       color = "")+
  guides(fill = "none")+
  facet_wrap(~Habitat)+
  theme_bw()
  


## Pull the coral and algae data so that it is just the 5 years that we want

five_year <-coral_algae_summary %>%
  mutate(year_groups = case_when(between(Year, 2005, 2010)~"2005-2010",
                                 between(Year, 2019, 2024)~"2019-2024")) %>%
  drop_na(year_groups) 

# calculate the rate of change
ungroup()%>% # the penguin data are grouped so we need to ungroup them
  nest(.by = species) %>% # nest all the data by species 
  mutate(fit = map(data, ~lm(bill_length_mm~body_mass_g, data = .)))


LTER_benthic<-five_year %>%
  ungroup()%>%
  nest(.by = c(year_groups, Habitat,Site, coral_algae))%>%
  mutate(fit = map(data, ~lm(mean_cover~Year, data = .))) %>%
  mutate(coeffs = map(fit, tidy)) %>%
  select(-c(data,fit)) %>%
  unnest(coeffs) %>% 
  filter(term == "Year") # only pull out the slopes


## get the average value per time point (but scale these so when we bring in fish and recruits everything is on the same scale)
LTER_benthic_avg <- five_year %>%
  mutate(coral_algae = factor(coral_algae, levels = c("CCA","Macroalgae","Coral")))%>%
  group_by(Habitat,coral_algae, year_groups) %>%
  summarise(cover = mean(mean_cover, na.rm = TRUE))

# make a plot
effect_plot<-LTER_benthic %>%
  mutate(coral_algae = factor(coral_algae, levels = c("CCA","Macroalgae","Coral")))%>%
  left_join(LTER_benthic_avg) %>%
ggplot(aes(x = estimate, y = coral_algae, color = year_groups))+
  geom_vline(xintercept = 0)+
  geom_point(alpha = 0.1)+
  stat_summary(aes(size = cover))+
  stat_summary(fun.data = mean_se, geom = "errorbarh", height = 0)+
  scale_size(range = c(0.1,1), trans = "log", breaks = c(0.3,1,3,8,20))+
  labs(x = "Average Effect size",
       y = "",
       color= "Time Period",
       size = "% Cover")+
  facet_wrap(~Habitat, scales = "free_x")+
  theme_bw()+
  theme(strip.text = element_blank())

time_series_plot/effect_plot
ggsave(here("Output","CompositePlot.png"), height = 6, width = 10)
