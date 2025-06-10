### Plot showing bar chart for metadata ####
### By Nyssa Silbiger #####
### Created on June 10th, 2025 ########
#################################

# load libraries ####
library(here)
library(tidyverse)
library(lubridate)
library(broom)
library(patchwork)
library(ggstream)
library(wesanderson)


## read in data -----
data<- read_csv(here("Data","bar_graph_data_entry.csv"))

pal <- wes_palette("Zissou1", 15, type = "continuous")

ggplot(data, aes(year, log(number_sites+1)*bar_value, linetype = gump_criobe)) +
  geom_area(aes(fill = dataset))+
  #geom_hline(yintercept = 0, yend = 2025,linewidth = 1)+
  geom_segment(aes(y = 0, yend = 0, x = 1980, xend = 2025), size = 1)+
  annotate(geom = "text", x = 1990, y = 20,label = "Gump", size = 8)+
  annotate(geom = "text", x = 1990, y = -20, label = "CRIOBE", size = 8)+
  labs(y = "Number of Sites",
       x = "",
       fill = "")+
  scale_fill_manual(values = pal)+
  guides(linetype = "none")+
  theme_classic()+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

ggsave(here("Output","datasets.png"), height = 5, width = 7)
#ggplot(data, aes(year, number_sites*bar_value, fill = dataset)) +
 # geom_stream(type = "ridge")
