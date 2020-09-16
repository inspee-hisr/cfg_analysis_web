#' ---
#' title: "Cave Fauna of Crete Analysis"
#' author: "Hellenic Institute of Speleological Research"
#' date: '`r Sys.Date()`'
#' output:
#'   html_document:
#'     toc: yes
#'     toc_depth: 2
#'     toc_float: yes
#'   pdf_document:
#'     toc: yes
#'     toc_depth: '2'
#'   word_document:
#'     toc: yes
#'     toc_depth: '2'
#' link-citations: yes
#' bibliography: packages_used.bib
#' site: bookdown::bookdown_site
#' biblio-style: apalike
#' ---
#' 
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------
### Working Directory

#setwd("C:/Users/inikoloudakis/Dropbox/INSPEE Team Folder/Conservation of the cave fauna of Greece - MAVA/Cave_Fauna_database/Cave_Fauna_Database_Analysis")

# Data manipulation packages
library(readxl)
library(readr)
library(ggplot2)
library(scales)
library(gridExtra)
library(grid)
library(reshape2)
library(dplyr)
library(knitr)
library(tidyr)
library(httr)
library(broom)
library(stringr)

## Data for species

library(rredlist)
library(taxize)
library(rgbif)
library(ISOcodes)
library(spocc)

# Spatial analysis packages

# x <- c("spocc","isocodes","rgbif","taxize","rredlist","raster","RColorBrewer","ggmap", "rgdal", "rgeos", "maptools", "tmap","Rcpp","sp")
# #install.packages(x) # warning: uncommenting this may take a number of minutes
# lapply(x, library, character.only = TRUE)
library(RColorBrewer)
library(ggmap)
library(rgdal)
library(geosphere)
library(GISTools)
library(leaflet)
library(rgeos)
library(maptools)
library(tmap)
library(Rcpp)
library(sp)
library(raster) ##Load the Raster Library

# Species statistics

library(red)
library(vegan)

packages <- c("readxl","readr","ggplot2","scales","gridExtra","dplyr", "knitr", "tidyr","RColorBrewer","ggmap","rgdal","rgeos","maptools","tmap","Rcpp","sp","raster","broom","red","vegan")

write_bib(x = packages,file = "packages_used.bib")


#' 
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------
# Store the file names of the Data folder
 
 data_files <- list.files(path = "Data")
 
 # Data import from Database Export, the files are choosen automatically based on their name. The folder Data must contain only the latest data files.
 Cave_References <- read_delim(file = paste0("Data/",grep("Cave_References",data_files,value = TRUE)),delim = "\t")
 
 caves <- read_delim(file = paste0("Data/",grep("Caves",data_files,value = TRUE)),delim = "\t")
 
 census <- read_delim(file = paste0("Data/",grep("Census_\\d",data_files,value = TRUE)),delim = "\t")
 
 Census_references <- read_delim(file = paste0("Data/",grep("Census_references",data_files,value = TRUE)),delim = "\t")
 
species <- read_delim(file = paste0("Data/",grep("Species_",data_files,value = TRUE)),delim = "\t") %>% mutate(Classification=gsub(pattern="\\?",replacement = "",x = Classification))# Data import from Database Export


#' 
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------
census$species_epithet <- as.character(lapply(strsplit(as.character(census$Species), split=" "), "[", n=2))

census_all_species <- census %>% left_join(species,by=c("Species"="Species_Full_Name"))

census_all_species_all_caves <- census_all_species %>% dplyr::select(-Cave_Name) %>% left_join(caves, by=c("Cave_ID"="Cave_ID"))

census_long_str_man <- strsplit(x = census_all_species$Reference_Short,split = "|",fixed=TRUE)
census_long_str_man_id <- strsplit(x = census_all_species$Reference_ID,split = "|",fixed=TRUE)

census_long_man <- data_frame(ReferenceShort=unlist(census_long_str_man),reference_id=unlist(census_long_str_man_id),CaveName=rep.int(census_all_species$Cave_Name,times = sapply(census_long_str_man,length)),Cave_ID=rep.int(census_all_species$Cave_ID,times = sapply(census_long_str_man,length)),Census_id=rep.int(census_all_species$Census_ID,times = sapply(census_long_str_man,length)),Species=rep.int(census_all_species$Species,times = sapply(census_long_str_man,length))) %>% group_by(ReferenceShort,Cave_ID,CaveName,Species,Census_id) %>% summarise(n=n()) %>% ungroup() %>% mutate(Species=trimws(Species,"r"))


#' 
#' 
#' ## Caves
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------

caves_crete <- caves %>% filter(Region=="Kriti")

caves_crete_subregion <- caves_crete %>% group_by(Subregion) %>% summarise(number_of_caves=n()) %>% mutate(frequency=round(number_of_caves/sum(number_of_caves),digits = 3))

caves_crete_subregion$Subregion <- factor(x = caves_crete_subregion$Subregion,levels = caves_crete_subregion$Subregion[c(1,4,2,3)])

ggplot()+
  geom_col(data = caves_crete_subregion, aes(x=Subregion, y= number_of_caves, fill=Subregion),show.legend = F)+
  geom_text(data = caves_crete_subregion,aes(x =Subregion,y= number_of_caves, label=number_of_caves), position=position_dodge(width=0.7), vjust=-0.25,size=2.8)+
  scale_y_continuous(breaks = seq(0,100,20),limits = c(0,100))+
  ggtitle("Caves of Crete")+
  labs(x="Subregion", y= "Number of caves")+
  theme_bw()+
  scale_fill_brewer(palette="Set3")+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("caves_crete_subregion.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")

ggplot()+
  geom_col(data = caves_crete_subregion, aes(x=Subregion, y= frequency, fill=Subregion),show.legend = F)+
  geom_text(data = caves_crete_subregion,aes(x =Subregion,y= frequency, label=frequency), position=position_dodge(width=0.7), vjust=-0.25,size=2.8)+
  scale_y_continuous(breaks = seq(0,1,0.1),limits = c(0,1))+
  ggtitle("Caves of Crete")+
  labs(x="Subregion", y= "Frequency of caves")+
  theme_bw()+
  scale_fill_brewer(palette="Set3")+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("caves_crete_subregion_frequency.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")

ggplot()+
  geom_col(data = caves_crete_subregion, aes(x="", y= frequency, fill=Subregion),width = 1,show.legend = T)+
  labs(x = "", y = "")+ 
  #geom_text(aes(x = 3.5, y = ((ymin+ymax)/2), label = Greek_Red_Data_Book)) +
  #scale_y_continuous(breaks = 0:10)+  #labs(x="Greek red data list", y= "Frequency")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank(),panel.border = element_blank(),axis.ticks=element_blank())+
coord_polar(theta = "y")

ggsave("caves_crete_subregion_pie.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")


#' 
#' 
#' ## All species
#' 
#' 
#' ### Class 
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------
crete_census <- census_all_species_all_caves %>% filter(Region=="Kriti")

crete_species <- crete_census %>% filter(species_epithet!="sp.") %>% distinct(Species, Class,Classification) %>% mutate(dupl=duplicated(Species))

crete_species_endemic_greece <- crete_census %>% filter(Distribution=="Endemic to Greece") %>% distinct(Species,Class, Classification, Municipality, Subregion)

crete_species_class <- crete_census %>% filter(species_epithet!="sp.") %>% distinct(Species, Class) %>% group_by(Class) %>% summarise(number_of_species=n()) %>% na.omit() %>% mutate(frequency=round(number_of_species/sum(number_of_species),digits = 3))

crete_species_municipality <- crete_census %>% filter(species_epithet!="sp.") %>% distinct(Species, Municipality) %>% group_by(Municipality) %>% summarise(number_of_species=n()) %>% na.omit()

crete_census_subregion <- crete_census %>% filter(species_epithet!="sp.") %>% distinct(Species, Subregion,Class,Classification)

ggplot()+
  geom_col(data = crete_species_class, aes(x=Class, y= number_of_species, fill=Class),show.legend = F)+
  geom_text(data = crete_species_class,aes(x =Class,y= number_of_species, label=number_of_species), position=position_dodge(width=0.7), vjust=-0.25,size=2.8)+
  scale_y_continuous(breaks = seq(0,100,20),limits = c(0,100))+
  ggtitle("Species that appear in caves of Crete")+
  labs(x="Class", y= "Number of species")+
  theme_bw()+
  scale_fill_brewer(palette="Set3")+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("crete_species_class_barplot.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/") 


ggplot()+
  geom_col(data = crete_species_class, aes(x=Class, y= frequency, fill=Class),show.legend = F)+
  geom_text(data = crete_species_class,aes(x =Class,y= frequency, label=frequency), position=position_dodge(width=0.7), vjust=-0.25,size=2.8)+
  scale_y_continuous(breaks = seq(0,1,0.1),limits = c(0,1))+
  ggtitle("Species that appear in caves of Crete")+
  labs(x="Class", y= "Frequency of species")+
  theme_bw()+
  scale_fill_brewer(palette="Set3")+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("crete_species_class_barplot_frequency.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/") 

ggplot()+
  geom_col(data = crete_species_class, aes(x="", y= frequency, fill=Class),width = 1,show.legend = T)+
  labs(x = "", y = "")+ 
  #geom_text(aes(x = 3.5, y = ((ymin+ymax)/2), label = Greek_Red_Data_Book)) +
  #scale_y_continuous(breaks = 0:10)+  #labs(x="Greek red data list", y= "Frequency")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank(),panel.border = element_blank(),axis.ticks=element_blank())+
coord_polar(theta = "y")

ggsave("crete_species_class_pie.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")



#' 
#' Classes of species in different subregions of Crete.
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------

crete_census_subregion_class <- crete_census_subregion %>% group_by(Subregion,Class) %>% summarise(number_of_species=n()) %>% mutate(frequency=round(number_of_species/sum(number_of_species),digits = 3))

crete_census_subregion_class$Subregion <- factor(crete_census_subregion_class$Subregion,levels=c("Chania","Rethymno","Irakleio","Lasithi"))

ggplot()+
  geom_col(data = crete_census_subregion_class, aes(x=Class, y= number_of_species, fill=Class),show.legend = F)+
  geom_text(data = crete_census_subregion_class,aes(x =Class,y= number_of_species, label=number_of_species), position=position_dodge(width=0.7), vjust=-0.25,size=2.8)+
  scale_y_continuous(breaks = seq(0,100,20),limits = c(0,100))+
  ggtitle("Species that appear in caves of Crete")+
  labs(x="Class", y= "Number of species")+
  theme_bw()+
  scale_fill_brewer(palette="Set3")+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~ Subregion,scales = "free", ncol=2)

ggsave("crete_species_class_subregion_barplot.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/") 

ggplot()+
  geom_col(data = crete_census_subregion_class, aes(x=Class, y= frequency, fill=Class),show.legend = F)+
  geom_text(data = crete_census_subregion_class,aes(x =Class,y= frequency, label=frequency), position=position_dodge(width=0.7), vjust=-0.25,size=2.8)+
  scale_y_continuous(breaks = seq(0,1,0.1),limits = c(0,1))+
  ggtitle("Species that appear in caves of Crete")+
  labs(x="Class", y= "Frequency of species")+
  theme_bw()+
  scale_fill_brewer(palette="Set3")+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~ Subregion,scales = "free", ncol=2)

ggsave("crete_species_class_subregion_barplot_frequency.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/") 


ggplot()+
  geom_col(data = crete_census_subregion_class, aes(x="", y= frequency, fill=Class),width = 1,show.legend = T)+
  labs(x = "", y = "")+ 
  #geom_text(aes(x = 3.5, y = ((ymin+ymax)/2), label = Greek_Red_Data_Book)) +
  #scale_y_continuous(breaks = 0:10)+  #labs(x="Greek red data list", y= "Frequency")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank(),panel.border = element_blank(),axis.ticks=element_blank())+
coord_polar(theta = "y")+
  facet_wrap(~ Subregion, ncol=2)

ggsave("crete_species_class_subregion_pie.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")


#' 
#' Orders of the most abundant classes.
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------

crete_species_order <- crete_census %>% filter(species_epithet!="sp.") %>% distinct(Species, Class,Order) %>% mutate(dupl=duplicated(Species))

most_abudant_classes_crete <- crete_species_order %>% filter(Class=="Arachnida" | Class=="Insecta" | Class=="Malacostraca") %>% group_by(Class,Order) %>% summarise(number_of_species=n()) %>% mutate(frequency=round(number_of_species/sum(number_of_species),digits = 3))

ggplot()+
  geom_col(data = most_abudant_classes_crete, aes(x=Order, y= number_of_species, fill=Order),show.legend = F)+
  geom_text(data = most_abudant_classes_crete,aes(x =Order,y= number_of_species, label=number_of_species), position=position_dodge(width=0.7), vjust=-0.25,size=2.8)+
  scale_y_continuous(breaks = seq(0,200,50),limits = c(0,200))+
  ggtitle("Arachnida, Malacostraca, Insecta species abundance in caves of Crete")+
  labs(x="Order", y= "Number of species")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~ Class,scales = "free", ncol=2)
  

ggsave("most_abudant_classes_crete.jpeg", plot = last_plot(), width = 15,height = 20,units = "cm",device = "jpeg", dpi = 300,path = "Plots/")

ggplot()+
  geom_col(data = most_abudant_classes_crete, aes(x=Order, y= frequency, fill=Order),show.legend = F)+
  geom_text(data = most_abudant_classes_crete,aes(x =Order,y= frequency, label=frequency), position=position_dodge(width=0.7), vjust=-0.25,size=2.8)+
  scale_y_continuous(breaks = seq(0,1,0.1),limits = c(0,1))+
  ggtitle("Arachnida, Malacostraca, Insecta species abundance in caves of Crete")+
  labs(x="Order", y= "Frequency of species")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~ Class,scales = "free", ncol=2)
  

ggsave("most_abudant_classes_crete_frequency.jpeg", plot = last_plot(), width = 15,height = 20,units = "cm",device = "jpeg", dpi = 300,path = "Plots/")

ggplot()+
  geom_col(data = most_abudant_classes_crete, aes(x="", y= frequency, fill=Order),width = 1,show.legend = T)+
  labs(x = "", y = "")+ 
  #geom_text(aes(x = 3.5, y = ((ymin+ymax)/2), label = Greek_Red_Data_Book)) +
  #scale_y_continuous(breaks = 0:10)+  #labs(x="Greek red data list", y= "Frequency")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank(),panel.border = element_blank(),axis.ticks=element_blank())+
coord_polar(theta = "y")+
  facet_wrap(~ Class, ncol=2)

ggsave("most_abudant_classes_crete_pie.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")



#' 
#' ### Ecological Classification
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------

crete_species_classification <- crete_census %>% filter(species_epithet!="sp.") %>% distinct(Species, Classification) %>% group_by(Classification) %>% summarise(number_of_species=n()) %>% na.omit() %>% mutate(frequency=round(number_of_species/sum(number_of_species),digits = 3))

ggplot()+
  geom_col(data = crete_species_classification, aes(x=Classification, y= number_of_species, fill=Classification),show.legend = F)+
  geom_text(data = crete_species_classification,aes(x =Classification,y= number_of_species, label=number_of_species), position=position_dodge(width=0.7), vjust=-0.25,size=2.8)+
  scale_y_continuous(breaks = seq(0,300,50),limits = c(0,300))+
  ggtitle("Species that appear in caves of Crete")+
  labs(x="Ecological Classification", y= "Number of species")+
  theme_bw()+
  scale_fill_brewer(palette="Set3")+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("crete_species_classification_barplot.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/") 

ggplot()+
  geom_col(data = crete_species_classification, aes(x=Classification, y= frequency, fill=Classification),show.legend = F)+
  geom_text(data = crete_species_classification,aes(x =Classification,y= frequency, label=frequency), position=position_dodge(width=0.7), vjust=-0.25,size=2.8)+
  scale_y_continuous(breaks = seq(0,1,0.1),limits = c(0,1))+
  ggtitle("Species that appear in caves of Crete")+
  labs(x="Ecological Classification", y= "Frequency of species")+
  theme_bw()+
  scale_fill_brewer(palette="Set3")+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("crete_species_classification_barplot_frequency.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/") 

ggplot()+
  geom_col(data = crete_species_classification, aes(x="", y= frequency, fill=Classification),width = 1,show.legend = T)+
  labs(x = "", y = "")+ 
  #geom_text(aes(x = 3.5, y = ((ymin+ymax)/2), label = Greek_Red_Data_Book)) +
  #scale_y_continuous(breaks = 0:10)+  #labs(x="Greek red data list", y= "Frequency")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank(),panel.border = element_blank(),axis.ticks=element_blank())+
coord_polar(theta = "y")

ggsave("crete_species_classification_pie.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")



#' 
#' Classification for species in different subregions of Crete.
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------

crete_census_subregion_classification <- crete_census_subregion %>% group_by(Subregion,Classification) %>% summarise(number_of_species=n()) %>% mutate(frequency=round(number_of_species/sum(number_of_species),digits = 3))

crete_census_subregion_classification$Subregion <- factor(crete_census_subregion_classification$Subregion,levels=c("Chania","Rethymno","Irakleio","Lasithi"))


ggplot()+
  geom_col(data = crete_census_subregion_classification, aes(x=Classification, y= number_of_species, fill=Classification),show.legend = F)+
  geom_text(data = crete_census_subregion_classification,aes(x =Classification,y= number_of_species, label=number_of_species), position=position_dodge(width=0.7), vjust=-0.25,size=2.8)+
  scale_y_continuous(breaks = seq(0,100,20),limits = c(0,100))+
  ggtitle("Cave fauna of Crete")+
  labs(x="Ecological Classification", y= "Number of species")+
  theme_bw()+
  scale_fill_brewer(palette="Set3")+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~ Subregion,scales = "free", ncol=2)


ggsave("crete_census_subregion_classification_barplot.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/") 

ggplot()+
  geom_col(data = crete_census_subregion_classification, aes(x=Classification, y= frequency, fill=Classification),show.legend = F)+
  geom_text(data = crete_census_subregion_classification,aes(x =Classification,y= frequency, label=frequency), position=position_dodge(width=0.7), vjust=-0.25,size=2.8)+
  scale_y_continuous(breaks = seq(0,1,0.1),limits = c(0,1))+
  ggtitle("Cave fauna of Crete")+
  labs(x="Ecological Classification", y= "Frequency of species")+
  theme_bw()+
  scale_fill_brewer(palette="Set3")+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~ Subregion,scales = "free", ncol=2)


ggsave("crete_census_subregion_classification_barplot_frequency.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/") 


ggplot()+
  geom_col(data = crete_census_subregion_classification, aes(x="", y= frequency, fill=Classification),width = 1,show.legend = T)+
  labs(x = "", y = "")+ 
  #geom_text(aes(x = 3.5, y = ((ymin+ymax)/2), label = Greek_Red_Data_Book)) +
  #scale_y_continuous(breaks = 0:10)+  #labs(x="Greek red data list", y= "Frequency")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank(),panel.border = element_blank(),axis.ticks=element_blank())+
coord_polar(theta = "y")+
  facet_wrap(~ Subregion, ncol=2)

ggsave("crete_census_subregion_classification_pie.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")



#' 
#' 
#' ### Municipality
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------

crete_species_municipality$Municipality <- factor(x = crete_species_municipality$Municipality,levels = crete_species_municipality$Municipality[c(13,12,18,20,6,4,19,2,16,3,8,14,9,11,5,15,7,22,17,1,10,21)])

ggplot()+
  geom_col(data = crete_species_municipality, aes(x=Municipality, y= number_of_species, fill=factor(Municipality)),show.legend = F)+
  geom_text(data = crete_species_municipality,aes(x =Municipality,y= number_of_species, label=number_of_species), position=position_dodge(width=0.7), vjust=-0.25,size=2.8)+
  scale_y_continuous(breaks = seq(0,300,50),limits = c(0,300))+
  ggtitle("Cave fauna of Crete")+
  labs(x="Municipality", y= "Number of species")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("crete_species_municipality_barplot.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/") 
 

#' 
#' ### Subregion
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------

crete_census_subregion_species <- crete_census_subregion %>% distinct(Subregion,Species) %>% group_by(Subregion) %>% summarise(number_of_species=n())

crete_census_subregion_species$Subregion <- factor(x = crete_census_subregion_species$Subregion,levels = crete_census_subregion_species$Subregion[c(1,4,2,3)])

ggplot()+
  geom_col(data = crete_census_subregion_species, aes(x=Subregion, y= number_of_species, fill=factor(Subregion)),show.legend = F)+
  geom_text(data = crete_census_subregion_species,aes(x =Subregion,y= number_of_species, label=number_of_species), position=position_dodge(width=0.7), vjust=-0.25,size=2.8)+
  scale_y_continuous(breaks = seq(0,300,50),limits = c(0,300))+
  ggtitle("Cave fauna of Crete")+
  labs(x="Subregion", y= "Number of species")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("crete_census_subregion_species_barplot.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/") 
 

#' 
#' 
#' ## Endemic to Greece species
#' 
#' ### Class
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------

crete_species_class_endemic <- crete_species_endemic_greece %>% group_by(Class) %>% summarise(number_of_species=n()) %>% na.omit()

ggplot()+
  geom_col(data = crete_species_class_endemic, aes(x=Class, y= number_of_species, fill=Class),show.legend = F)+
  geom_text(data = crete_species_class_endemic,aes(x =Class,y= number_of_species, label=number_of_species), position=position_dodge(width=0.7), vjust=-0.25,size=2.8)+
  scale_y_continuous(breaks = seq(0,300,50),limits = c(0,300))+
  ggtitle("Species that are endemic to Greece and appear in caves of Crete")+
  labs(x="Class", y= "Number of species")+
  theme_bw()+
  scale_fill_brewer(palette="Set3")+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("crete_species_class_barplot_endemic.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")


#' 
#' 
#' ### Ecological Classification
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------

crete_species_classification_endemic <- crete_species_endemic_greece %>% group_by(Classification) %>% summarise(number_of_species=n()) %>% na.omit()

ggplot()+
  geom_col(data = crete_species_classification_endemic, aes(x=Classification, y= number_of_species, fill=Classification),show.legend = F)+
  geom_text(data = crete_species_classification_endemic,aes(x =Classification,y= number_of_species, label=number_of_species), position=position_dodge(width=0.7), vjust=-0.25,size=2.8)+
  scale_y_continuous(breaks = seq(0,300,50),limits = c(0,300))+
  ggtitle("Species that are endemic to Greece and appear in caves of Crete")+
  labs(x="Ecological Classification", y= "Number of species")+
  theme_bw()+
  scale_fill_brewer(palette="Set3")+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("crete_species_classification_endemic_barplot_endemic.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")


#' 
#' ### Municipality
#' 
## ----warning=FALSE, message=FALSE, echo=FALSE----------------------------
crete_species_municipality_endemic <- crete_species_endemic_greece %>% group_by(Municipality) %>% summarise(number_of_species=n()) %>% na.omit()

crete_species_municipality_endemic$Municipality <- factor(x = crete_species_municipality_endemic$Municipality,levels = crete_species_municipality_endemic$Municipality[c(11,10,16,18,5,4,17,2,14,3,7,12,9,13,6,15,1,8,19)])


ggplot()+
  geom_col(data = crete_species_municipality_endemic, aes(x=Municipality, y= number_of_species, fill=Municipality),show.legend = F)+
  geom_text(data = crete_species_municipality_endemic,aes(x =Municipality,y= number_of_species, label=number_of_species), position=position_dodge(width=0.7), vjust=-0.25,size=2.8)+
  scale_y_continuous(breaks = seq(0,300,50),limits = c(0,300))+
  ggtitle("Species that are endemic to Greece and appear in caves of Crete")+
  labs(x="Municipality", y= "Number of species")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("crete_species_Municipality_barplot_endemic.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")


#' 
#' ### Subregion
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------

crete_census_subregion_species <- crete_species_endemic_greece %>% distinct(Subregion,Species) %>% group_by(Subregion) %>% summarise(number_of_species=n())

crete_census_subregion_species$Subregion <- factor(x = crete_census_subregion_species$Subregion,levels = crete_census_subregion_species$Subregion[c(1,4,2,3)])

ggplot()+
  geom_col(data = crete_census_subregion_species, aes(x=Subregion, y= number_of_species, fill=factor(Subregion)),show.legend = F)+
  geom_text(data = crete_census_subregion_species,aes(x =Subregion,y= number_of_species, label=number_of_species), position=position_dodge(width=0.7), vjust=-0.25,size=2.8)+
  scale_y_continuous(breaks = seq(0,100,20),limits = c(0,100))+
  ggtitle("Species that are endemic to Greece and appear in caves of Crete")+
  labs(x="Subregion", y= "Number of species")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("crete_census_subregion_species_barplot.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/") 
 

#' 
#' ## Endemic to Crete
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------

endemic_to_crete <- census_all_species_all_caves  %>% filter(species_epithet!="sp.") %>% filter(Distribution=="Endemic to Greece") %>% distinct( Species,Region) %>% group_by(Species) %>% mutate(number_of_regions=n()) %>% filter(number_of_regions==1 & Region=="Kriti")

endemic_to_crete_all_data <- endemic_to_crete %>% left_join(species,by=c("Species"="Species_Full_Name"))


#' 
#' 
#' ### Class
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------

crete_only_species_region_endemic <- census_all_species_all_caves %>% filter(Distribution=="Endemic to Greece") %>% distinct( Species,Class,Region) %>% group_by(Species) %>% mutate(number_of_regions=n()) %>% filter(number_of_regions==1 & Region=="Kriti") %>% group_by(Class) %>% summarise(number_of_species=n()) %>% na.omit()

ggplot()+
  geom_col(data = crete_only_species_region_endemic, aes(x=Class, y= number_of_species, fill=Class),show.legend = F)+
  geom_text(data = crete_only_species_region_endemic,aes(x =Class,y= number_of_species, label=number_of_species), position=position_dodge(width=0.7), vjust=-0.25,size=2.8)+
  scale_y_continuous(breaks = seq(0,300,50),limits = c(0,300))+
  ggtitle("Species that are endemic to Greece and appear only in caves of Crete")+
  labs(x="Class", y= "Number of species")+
  theme_bw()+
  scale_fill_brewer(palette="Set3")+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("crete_only_species_class_barplot_endemic.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")



#' 
## ---- eval=FALSE---------------------------------------------------------
## crete_only_species <- census_all_species_all_caves %>% filter(Distribution=="Endemic to Greece") %>% distinct( Species,Class,Region) %>% group_by(Species) %>% mutate(number_of_regions=n()) %>% filter(number_of_regions==1 & Region=="Kriti")
## 
## #write_delim(crete_only_species,"crete_only_species.tsv",col_names = T,delim = "\t")

#' 
#' 
#' ### Ecological classification
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------

crete_only_species_region_endemic_classification <- census_all_species_all_caves %>% filter(Distribution=="Endemic to Greece") %>% distinct( Species,Classification,Region) %>% group_by(Species) %>% mutate(number_of_regions=n()) %>% filter(number_of_regions==1 & Region=="Kriti") %>% group_by(Classification) %>% summarise(number_of_species=n()) %>% na.omit()

ggplot()+
  geom_col(data = crete_only_species_region_endemic_classification, aes(x=Classification, y= number_of_species, fill=Classification),show.legend = F)+
  geom_text(data = crete_only_species_region_endemic_classification,aes(x =Classification,y= number_of_species, label=number_of_species), position=position_dodge(width=0.7), vjust=-0.25,size=2.8)+
  scale_y_continuous(breaks = seq(0,300,50),limits = c(0,300))+
  ggtitle("Species that are endemic to Greece and appear only in caves of Crete")+
  labs(x="Ecological Classification", y= "Number of species")+
  theme_bw()+
  scale_fill_brewer(palette="Set3")+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("crete_only_species_region_endemic_classification_barplot_endemic.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")



#' 
#' ### Municipality
#' 
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------

crete_species_municipality_endemic_crete <- census_all_species_all_caves %>% filter(Distribution=="Endemic to Greece") %>% distinct(Species,Municipality) %>% filter(Species %in% endemic_to_crete$Species) %>% group_by(Municipality) %>% summarise(number_of_species=n()) %>% na.omit()


#%>% group_by(Species) %>% mutate(number_of_regions=n()) %>% filter(number_of_regions==1 & Region=="Kriti")

#

crete_species_municipality_endemic_crete$Municipality <- factor(x = crete_species_municipality_endemic_crete$Municipality,levels = crete_species_municipality_endemic_crete$Municipality[c(11,10,16,18,5,4,17,2,14,3,7,12,9,13,6,15,1,8,19)])


ggplot()+
  geom_col(data = crete_species_municipality_endemic_crete, aes(x=Municipality, y= number_of_species, fill=Municipality),show.legend = F)+
  geom_text(data = crete_species_municipality_endemic_crete,aes(x =Municipality,y= number_of_species, label=number_of_species), position=position_dodge(width=0.7), vjust=-0.25,size=2.8)+
  scale_y_continuous(breaks = seq(0,300,50),limits = c(0,300))+
  ggtitle("Species that are endemic to Greece and appear only in caves of Crete")+
  labs(x="Municipality", y= "Number of species")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("crete_species_Municipality_barplot_endemic_crete.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")


#' 
#' ### Subregion
#' 
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------

crete_species_Subregion_endemic_crete <- census_all_species_all_caves %>% filter(Distribution=="Endemic to Greece") %>% distinct(Species,Subregion) %>% filter(Species %in% endemic_to_crete$Species) %>% group_by(Subregion) %>% summarise(number_of_species=n()) %>% na.omit()


crete_species_Subregion_endemic_crete$Subregion <- factor(x = crete_species_Subregion_endemic_crete$Subregion,levels = crete_species_Subregion_endemic_crete$Subregion[c(1,4,2,3)])


ggplot()+
  geom_col(data = crete_species_Subregion_endemic_crete, aes(x=Subregion, y= number_of_species, fill=Subregion),show.legend = F)+
  geom_text(data = crete_species_Subregion_endemic_crete,aes(x =Subregion,y= number_of_species, label=number_of_species), position=position_dodge(width=0.7), vjust=-0.25,size=2.8)+
  scale_y_continuous(breaks = seq(0,100,20),limits = c(0,100))+
  ggtitle("Species that are endemic to Greece and appear only in caves of Crete")+
  labs(x="Subregion", y= "Number of species")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("crete_species_Subregion_endemic_crete_barplot_endemic_crete.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")


#' 
#' 
#' Crete has `r length(unique(crete_species$Species))` species from which `r length(unique(crete_species_endemic_greece$Species))` are endemic to Greece and `r length(unique(endemic_to_crete$Species))` species that are endemic to Greece and appear only in cretan caves. Total sampled caves in Crete `r length(unique(caves_crete$Cave_ID))`.
#' 
#' ## Protection Status species Crete
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------
species_protection <- strsplit(x = species$Protection_Status,split = "|",fixed=TRUE)

species_protection_data <- data_frame(Species_Protection=unlist(species_protection),Species=rep.int(species$Species_Full_Name,times = sapply(species_protection,length)),Class=rep.int(species$Class,times = sapply(species_protection,length)),Classification=rep.int(species$Classification,times = sapply(species_protection,length)))


species_protection_data_crete <- crete_species %>% distinct(Species) %>% left_join(species_protection_data, by=c("Species"="Species"))

species_protection_data_summary_crete <- species_protection_data_crete %>% group_by(Species_Protection) %>% summarise(number_of_species=n())

kable(species_protection_data_summary_crete)


#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------
species_protection_data_classification <- species_protection_data_crete %>% mutate(Protection_Status=if_else(is.na(Species_Protection)==TRUE,"Not protected","Protected")) %>% group_by(Classification,Protection_Status) %>% summarise(number_of_species=n()) %>% ungroup() %>% spread(key = Protection_Status,value = number_of_species,fill=0) %>% gather(key =Protection_Status,value =number_of_species,  -Classification)

species_protection_data_classification$Classification <- factor(species_protection_data_classification$Classification,levels = c("Accidental","Trogloxene","Stygoxene","Stygophile","Troglophile","Stygobiont","Troglobiont"))

species_protection_data_classification_plot_crete <- ggplot()+
  geom_col(data = species_protection_data_classification, aes(x=Classification, y= number_of_species, fill=Protection_Status,group=Protection_Status),position="dodge",show.legend = T)+
  geom_text(data = species_protection_data_classification,aes(x =Classification,y= number_of_species, label=number_of_species,group=Protection_Status), position=position_dodge(width=0.93), vjust=-0.25,size=6)+
  scale_y_continuous(breaks = seq(0,125,25),limits = c(0,125),expand = c(0.01,0.4))+
  scale_fill_manual(values = c("lightcoral","lightgreen"),name="")+
  labs(x="Classification", y= "Number of species")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),legend.text = element_text(size = 18),axis.text.y=element_text(margin = margin(t = 0, r = 0, b = 0, l = 10,unit = "pt"),size = 18),axis.text.x = element_text(margin = margin(t = 0, r = 0, b = 10, l = 0,unit = "pt"),angle = 45, hjust = 1,size = 18),axis.title = element_text(size=22),panel.border = element_blank(),axis.line.x = element_line(colour = 'black', size = 0.3), axis.line.y = element_line(colour = 'black', size = 0.3),legend.position = c(0.09,0.90), legend.key.size = unit(1.5, "cm"))

ggsave("species_protection_data_classification_crete.png", plot = species_protection_data_classification_plot_crete, device = "png",width = 20,height = 11.25,units = "in", dpi = 100,path = "Plots/")


#' 
#' ![Number of protected species across their ecological classification](Plots/species_protection_data_classification_crete.png)
#' 
#' ## Protection status caves Crete
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------
caves_protection <- strsplit(x = caves$Protection_Status,split = "|",fixed=TRUE)

caves_protection_data <- data_frame(Caves_Protection=unlist(caves_protection),CaveName=rep.int(caves$Cave_Name,times = sapply(caves_protection,length)),Cave_ID=rep.int(caves$Cave_ID,times = sapply(caves_protection,length)),Region=rep.int(caves$Region,times = sapply(caves_protection,length)),Altitude=rep.int(caves$Altitude,times = sapply(caves_protection,length))) %>% mutate(Protection_Type=if_else(is.na(Caves_Protection),"Not protected", if_else(grepl("^G.",x = Caves_Protection),"Natura2000","Wildlife Refuge")))

caves_protection_data_summary_crete <- caves_protection_data %>% filter(Region=="Kriti") %>% group_by(Protection_Type) %>% summarise(number_of_caves=n())

caves_protection_data_crete <- caves_protection_data %>% filter(Region=="Kriti")

ggplot()+
  geom_col(data = caves_protection_data_summary_crete, aes(x=Protection_Type, y= number_of_caves, fill=Protection_Type),show.legend = F)+
  geom_text(data = caves_protection_data_summary_crete,aes(x =Protection_Type,y= number_of_caves, label=number_of_caves), position=position_dodge(width=0.7), vjust=-0.25,size=2.8)+
  scale_y_continuous(breaks = seq(0,250,25),limits = c(0,250))+
  ggtitle("Caves in Crete that are protected")+
  labs(x="Protection", y= "Number of caves")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("caves_protection_data_crete.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")



#' 
#' ## IUCN status
#' 
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------

iucn_species_crete <- crete_species %>% distinct(Species) %>% left_join(species,by=c("Species"="Species_Full_Name")) %>% group_by(IUCN_Red_List) %>% summarise(number_of_species=n()) %>% mutate(Red_List="IUCN Red List") %>% dplyr::rename(., Categories=IUCN_Red_List) %>% rbind(.,data_frame(Categories="EN - Endangered",number_of_species=0,Red_List="IUCN Red List"))

kable(iucn_species_crete)

ggplot()+
  geom_col(data = iucn_species_crete, aes(x=Categories, y= number_of_species, fill=Categories),show.legend = F)+
  geom_text(data = iucn_species_crete,aes(x =Categories,y= number_of_species, label=number_of_species), position=position_dodge(width=0.7), vjust=-0.25,size=2.8)+
  scale_y_continuous(breaks = seq(0,720,100),limits = c(0,720))+
  #ggtitle("Inferring methods of the Sign Score of the PPI network of Drosophila gene")+
  labs(x="IUCN status", y= "Number of species")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("iucn_species_crete.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")



#' 
#' 
#' ## Greek red data book
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------

greek_red_data_species_crete <- crete_species %>% distinct(Species) %>% left_join(species,by=c("Species"="Species_Full_Name")) %>% group_by(Greek_Red_Data_Book) %>% summarise(number_of_species=n()) %>% mutate(Red_List="Greece's Red Data Book") %>% dplyr::rename(., Categories=Greek_Red_Data_Book)

kable(greek_red_data_species_crete)

ggplot()+
  geom_col(data = greek_red_data_species_crete, aes(x=Categories, y= number_of_species, fill=Categories),show.legend = F)+
  geom_text(data = greek_red_data_species_crete,aes(x =Categories,y= number_of_species, label=number_of_species), position=position_dodge(width=0.7), vjust=-0.25,size=2.8)+
  scale_y_continuous(breaks = seq(0,720,100),limits = c(0,720))+
  #ggtitle("Inferring methods of the Sign Score of the PPI network of Drosophila gene")+
  labs(x="Greek red data list", y= "Number of species")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("greek_red_data_species_crete.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")


#' 
#' ## Both Red Lists
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------

red_lists_species_crete <- rbind(iucn_species_crete,greek_red_data_species_crete)

red_lists_species_crete$Categories <- factor(red_lists_species_crete$Categories,levels = c("NE - Not Evaluated","DD - Data Deficient","LC - Least Concern","NT - Near Threatened","VU - Vulnerable","EN - Endangered","CR - Critically Endangered"))

red_lists_data_species_plot_crete <- ggplot()+
  geom_col(data = red_lists_species_crete, aes(x=Categories, y= number_of_species, fill=Red_List,width=0.9), position = position_dodge(width = 0.9),show.legend = T)+
  geom_text(data = red_lists_species_crete,aes(x =Categories,y= number_of_species, label=number_of_species,group=Red_List), position=position_dodge(width=0.9), vjust=-0.25,size=6)+
  scale_y_continuous(breaks = seq(0,300,50),limits = c(0,300),expand = c(0.01,0.4))+
  scale_fill_manual(values = c("lightpink1","firebrick1"),name="")+
  labs(x="Categories", y= "Number of species")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),legend.text = element_text(size = 18),axis.text.y=element_text(margin = margin(t = 0, r = 0, b = 0, l = 10,unit = "pt"),size = 18),axis.text.x = element_text(angle = 45, hjust = 1,size = 18),axis.title = element_text(size=22),panel.border = element_blank(),axis.line.x = element_line(colour = 'black', size = 0.3), axis.line.y = element_line(colour = 'black', size = 0.3),legend.position = c(0.83,0.89), legend.key.size = unit(1.5, "cm"))

ggsave("red_lists_data_species_crete.png", plot = red_lists_data_species_plot_crete, device = "png",width = 20,height = 11.25,units = "in", dpi = 100,path = "Plots/")


#' 
#' ![Species assesments across Red lists categories ](Plots/red_lists_data_species_crete.png)
#' 
#' 
#' ## Species richness and altitude Crete
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------

# Caves

caves_per_altitude_crete <- caves %>% filter(Region=="Kriti") %>% distinct(Cave_ID,Altitude,Region) %>% na.omit() %>% mutate(Bins=cut(Altitude,breaks=seq(0,2200,by=100))) %>% group_by(Bins) %>%  mutate(Mean_altitude=mean(Altitude)) %>% mutate(number_of_caves=n()) %>% distinct(Mean_altitude,Bins,number_of_caves) #

# Species

species_per_altitude_crete <- census_all_species_all_caves %>% filter(species_epithet!="sp.", Region=="Kriti") %>% distinct(Species,Classification,Class,Altitude,Region) %>% mutate(Bins=cut(Altitude,breaks=seq(0,2200,by=100))) %>% group_by(Bins) %>%  mutate(Mean_altitude=mean(Altitude)) %>% distinct(Species,Classification,Class,Mean_altitude) #

species_per_altitude_crete_summary <- species_per_altitude_crete %>% group_by(Mean_altitude) %>% summarise(number_of_species=n())

ggplot()+
  geom_line(data = species_per_altitude_crete_summary, aes(x=Mean_altitude, y=number_of_species,color="Species"),show.legend = T)+
   geom_line(data = caves_per_altitude_crete, aes(x=Mean_altitude, y=number_of_caves,color="Caves"),show.legend = T)+
  #geom_freqpoly(data = species_per_altitude, aes(x=Altitude),binwidth = 100,show.legend = T)+
  scale_y_continuous(breaks = seq(0,200,25),limits = c(0,125))+
  scale_x_continuous(breaks = seq(0,2200,200),limits = c(0,2200))+
  ggtitle("Species richness and altitude in cave fauna of Crete")+
  labs(x="Altitude", y= "Abundance")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())

ggsave("species_caves_per_altitude_crete.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")


#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------




ggplot()+
  geom_line(data = caves_per_altitude_crete, aes(x=Mean_altitude, y=number_of_caves),show.legend = T)+
  #geom_freqpoly(data = species_per_altitude, aes(x=Altitude),binwidth = 100,show.legend = T)+
  scale_y_continuous(breaks = seq(0,50,5),limits = c(0,50))+
  scale_x_continuous(breaks = seq(0,2200,200),limits = c(0,2200))+
  ggtitle("Altitude of the caves of Crete")+
  labs(x="Altitude", y= "Number of caves")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())

ggsave("caves_per_altitude_crete.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")


#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------

species_per_altitude_crete_summary_classification <- species_per_altitude_crete %>% group_by(Mean_altitude,Classification) %>% summarise(number_of_species=n())

ggplot()+
  geom_line(data = species_per_altitude_crete_summary, aes(x=Mean_altitude, y=number_of_species,color="All Species"),show.legend = T)+
  geom_line(data = species_per_altitude_crete_summary_classification, aes(x=Mean_altitude,y=number_of_species,color=Classification),show.legend = T)+
  #geom_line(data = caves_per_altitude_crete, aes(x=Mean_altitude, y=number_of_caves,color="Caves"),show.legend = T)+
  #geom_freqpoly(data = species_per_altitude, aes(x=Altitude),binwidth = 100,show.legend = T)+
  scale_y_continuous(breaks = seq(0,200,25),limits = c(0,125))+
  scale_x_continuous(breaks = seq(0,2200,200),limits = c(0,2200))+
  ggtitle("Species richness and altitude for different ecological classifications of cave fauna of Crete")+
  labs(x="Altitude", y= "Number of species")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())

ggsave("species_per_altitude_classification_crete.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")




#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------
species_per_altitude_crete_summary_class <- species_per_altitude_crete %>% group_by(Mean_altitude,Class) %>% summarise(number_of_species=n())


ggplot()+
  geom_line(data = species_per_altitude_crete_summary, aes(x=Mean_altitude, y=number_of_species),show.legend = T)+
  geom_line(data = species_per_altitude_crete_summary_class, aes(x=Mean_altitude,y=number_of_species,color=Class),show.legend = T)+
  #geom_freqpoly(data = species_per_altitude, aes(x=Altitude),binwidth = 100,show.legend = T)+
  scale_y_continuous(breaks = seq(0,200,25),limits = c(0,125))+
  scale_x_continuous(breaks = seq(0,2200,200),limits = c(0,2200))+
  ggtitle("Species richness and altitude for different classes of cave fauna of Crete")+
  labs(x="Altitude", y= "Number of species")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())

ggsave("species_per_altitude_class_crete.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")

#' 
#' ### Scatterplot
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------
# Species

species_per_cave_altitude_crete <- census_all_species_all_caves %>% filter(species_epithet!="sp.", Region=="Kriti") %>% distinct(Species,Classification,Class,Altitude,Cave_ID,Region) %>% group_by(Cave_ID) %>% mutate(number_of_species=n()) %>% na.omit() %>% distinct(Altitude,Cave_ID,number_of_species)#%>% group_by(Bins) %>%  mutate(Mean_altitude=mean(Altitude)) %>% distinct(Species,Classification,Class,Mean_altitude) 

ggplot()+
  geom_point(data = species_per_cave_altitude_crete, aes(x=Altitude,y=number_of_species))+
  #geom_freqpoly(data = species_per_altitude, aes(x=Altitude),binwidth = 100,show.legend = T)+
  scale_y_continuous(breaks = seq(0,200,25),limits = c(0,125))+
  scale_x_continuous(breaks = seq(0,2200,200),limits = c(0,2200))+
  ggtitle("Species richness and altitude ")+
  labs(x="Altitude", y= "Number of species")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())

ggsave("species_per_altitude_class_crete_scat.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")


#' 
#' ## Species accumulation curve
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------

species_accumulation_crete <- crete_census %>% distinct(Species,Cave_ID) %>% mutate(Duplicates=duplicated(Species), species_epithet= as.character(lapply(strsplit(as.character(Species), split=" "), "[", n=2))) %>% filter(species_epithet!="sp.") %>% mutate(.,First_occurance=if_else(Duplicates=="FALSE",1,0)) %>% filter(First_occurance==1) %>% group_by(Cave_ID) %>% mutate(First_occurance_per_cave=sum(First_occurance))# %>% group_by(First_occurance_per_cave) %>% summarise(number_of_caves=n()) #mutate(Cumulative_occurance= cumsum(First_occurance))

species_accumulation_crete_wide <- crete_census %>% mutate(species_epithet= as.character(lapply(strsplit(as.character(Species), split=" "), "[", n=2))) %>% filter(species_epithet!="sp.") %>% dplyr::select(Cave_ID,Species) %>% distinct() %>% mutate(presence=1) %>% spread(value = presence,key = Species,fill = 0) %>% tibble::column_to_rownames(.,var="Cave_ID")  # %>% dplyr::select(-Cave_ID)

acc <- specaccum(species_accumulation_crete_wide,"random")

data <- data.frame(Sites=acc$sites, Richness=acc$richness, SD=acc$sd)

ggplot() +
  geom_point(data=data, aes(x=Sites, y=Richness)) +
  geom_line(data=data, aes(x=Sites, y=Richness)) +
  geom_ribbon(data=data ,aes(x=Sites, ymin=(Richness-2*SD),ymax=(Richness+2*SD)),alpha=0.2)+
  scale_y_continuous(breaks = seq(0,300,25),limits = c(0,275))+
  scale_x_continuous(breaks = seq(0,200,25),limits = c(0,175))+
  ggtitle("Cave fauna of Crete accumulation curve")+
  coord_equal()+
  labs(x="Caves", y= "Number of species")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())

ggsave("species_accumulation_crete.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")



#' 
#' 
#' ## Census references Crete
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------
census_long_man_crete <- census_long_man %>% filter(Cave_ID %in% caves_crete$Cave_ID) %>% left_join(Census_references,by=c("ReferenceShort"="Short")) %>% arrange(Year)


kable(head(census_long_man_crete,15))


#' 
#' There are `r length(unique(census_long_man_crete$ReferenceShort))` publications that contain records from Crete.
#' 
#' 
## ----warning=FALSE, message=FALSE, echo=FALSE----------------------------
census_long_man_reference <- census_long_man %>% left_join(Census_references,by=c("ReferenceShort"="Short"))


species_references_spreaded_arranged_crete <- census_long_man_reference %>% ungroup()  %>% filter(Cave_ID %in% caves_crete$Cave_ID) %>% dplyr::select(Species, ReferenceShort, Year) %>% group_by(Species,ReferenceShort, Year) %>% distinct(.) %>% arrange(.,Year) %>% ungroup() %>% mutate(Duplicates=duplicated(Species), species_epithet= as.character(lapply(strsplit(as.character(Species), split=" "), "[", n=2))) %>% filter(species_epithet!="sp.") %>% mutate(.,First_occurance=if_else(Duplicates=="FALSE",1,0)) %>% na.omit() %>% mutate(Cumulative_occurance= cumsum(First_occurance)) %>% mutate(Classification="All species") %>% dplyr::select(-c(Species,species_epithet,First_occurance)) %>% distinct(.)

endemic_cumulative_species_crete <- census_long_man_reference %>% left_join(species,by=c("Species"="Species_Full_Name")) %>% filter(Cave_ID %in% caves_crete$Cave_ID & Species %in% endemic_to_crete$Species) %>% dplyr::select(Species,Distribution, ReferenceShort, Year) %>% group_by(Species,Distribution,ReferenceShort, Year) %>% distinct(.) %>% arrange(.,Year) %>% ungroup() %>% mutate(Duplicates=duplicated(Species), species_epithet= as.character(lapply(strsplit(as.character(Species), split=" "), "[", n=2))) %>% filter(species_epithet!="sp.") %>% mutate(.,First_occurance=if_else(Duplicates=="FALSE",1,0)) %>% na.omit() %>% filter(First_occurance==1) %>% group_by(Year,Distribution) %>% summarise(Occurance_species_year= n()) %>% group_by(Distribution) %>% mutate(Cumulative_occurance= cumsum(Occurance_species_year)) %>% filter(Distribution=="Endemic to Greece") %>% mutate(Classification="Endemic species to Crete") %>% dplyr::select(-Occurance_species_year,-Distribution)

endemic_greek_cumulative_species_in_crete <- census_long_man_reference %>% left_join(species,by=c("Species"="Species_Full_Name")) %>% filter(Cave_ID %in% caves_crete$Cave_ID) %>% dplyr::select(Species,Distribution, ReferenceShort, Year) %>% group_by(Species,Distribution,ReferenceShort, Year) %>% distinct(.) %>% arrange(.,Year) %>% ungroup() %>% mutate(Duplicates=duplicated(Species), species_epithet= as.character(lapply(strsplit(as.character(Species), split=" "), "[", n=2))) %>% filter(species_epithet!="sp.") %>% mutate(.,First_occurance=if_else(Duplicates=="FALSE",1,0)) %>% na.omit() %>% filter(First_occurance==1) %>% group_by(Year,Distribution) %>% summarise(Occurance_species_year= n()) %>% group_by(Distribution) %>% mutate(Cumulative_occurance= cumsum(Occurance_species_year)) %>% filter(Distribution=="Endemic to Greece") %>% mutate(Classification="Endemic species to Greece") %>% dplyr::select(-Occurance_species_year,-Distribution)


census_long_man_reference_all_species_classification_crete <- census_long_man_reference %>% left_join(species,by=c("Species"="Species_Full_Name")) %>% filter(Cave_ID %in% caves_crete$Cave_ID) %>% dplyr::select(Species,Classification, ReferenceShort, Year) %>% group_by(Species,Classification,ReferenceShort, Year) %>% distinct(.) %>% arrange(.,Year) %>% ungroup() %>% mutate(Duplicates=duplicated(Species), species_epithet= as.character(lapply(strsplit(as.character(Species), split=" "), "[", n=2))) %>% filter(species_epithet!="sp.") %>% mutate(.,First_occurance=if_else(Duplicates=="FALSE",1,0)) %>% na.omit() %>% filter(First_occurance==1) %>% group_by(Year,Classification) %>% summarise(Occurance_species_year= n()) %>% group_by(Classification) %>% mutate(Cumulative_occurance= cumsum(Occurance_species_year)) %>% dplyr::select(-Occurance_species_year) %>% bind_rows(species_references_spreaded_arranged_crete) %>% filter(Classification %in% c("Troglobiont","Troglophile","All species")) %>% bind_rows(endemic_cumulative_species_crete) %>% bind_rows(endemic_greek_cumulative_species_in_crete)



species_occurrence_accumulation_classification_plot_crete <- ggplot()+
  geom_line(data=census_long_man_reference_all_species_classification_crete,aes(x=Year, y= Cumulative_occurance,color=Classification),size=1,show.legend = T)+
  #ggtitle("Class")+
  scale_x_continuous(breaks = seq(1860,2020,10),limits = c(1860,2020),expand=c(0.015,0))+
  scale_y_continuous(breaks = seq(0,300,25), limits = c(0,300),expand = c(0.01,0))+
  scale_color_manual(values =c("Endemic species to Crete"="lightcoral","Troglophile"="paleturquoise","Troglobiont"="darkolivegreen3","All species"="deepskyblue","Endemic species to Greece"="darkorchid4"))+
  labs(x="Years",y="Cumulative number of species in Crete")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),legend.text = element_text(size = 18),axis.text.y=element_text(margin = margin(t = 0, r = 5, b = 0, l = 15,unit = "pt"),size = 18),axis.text.x = element_text(margin = margin(t = 5, r = 0, b = 15, l = 0,unit = "pt"),size = 18),axis.title = element_text(size=22),panel.border = element_blank(),axis.line.x = element_line(colour = 'black', size = 0.3), axis.line.y = element_line(colour = 'black', size = 0.3),legend.position = c(0.13,0.87), legend.key.size = unit(1.5, "cm"), legend.title = element_blank())

ggsave("species_occurrence_accumulation_classification_crete.png", plot = species_occurrence_accumulation_classification_plot_crete, device = "png",width = 20,height = 11.25,units = "in", dpi = 100,path = "Plots/")



#' 
#' 
#' 
#' ## Spatial analysis of Crete
#' 
## ------------------------------------------------------------------------

caves_crete_Database_kml_to_txt <- caves_crete %>% dplyr::select(Cave_Name,Cave_ID,Latitude, Longitude,Subregion) %>% na.omit()

caves_crete_Database_kml_to_txt$Latitude <- as.numeric(caves_crete_Database_kml_to_txt$Latitude)
caves_crete_Database_kml_to_txt$Longitude <- as.numeric(caves_crete_Database_kml_to_txt$Longitude)

caves_crete_Database_kml_to_txt$ID <- as.character(seq(1:nrow(caves_crete_Database_kml_to_txt)))

caves_crete_Database_kml_to_txt_shapefile_wgs84 <- caves_crete_Database_kml_to_txt

coordinates(caves_crete_Database_kml_to_txt_shapefile_wgs84)<-~Longitude+Latitude
proj4string(caves_crete_Database_kml_to_txt_shapefile_wgs84) <- CRS("+proj=longlat +datum=WGS84")# CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")  # this is WGS84

caves_crete_Database_kml_to_txt_shapefile <- spTransform(caves_crete_Database_kml_to_txt_shapefile_wgs84, CRS( "+proj=tmerc +lat_0=0 +lon_0=24 +k=0.9996 +x_0=500000 +y_0=0 +datum=GGRS87 +units=m +no_defs +ellps=GRS80
+towgs84=-199.87,74.79,246.62"))

#' 
#' 
#' 
#' ### Geological maps of Crete
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------
#Rethymno
rethymno_geomap  <- rgdal::readOGR("Shapefiles/Crete_geological_map_SHP/rethymno/geo_uniRETHYMNON.shp",verbose = T,p4s = "+proj=tmerc +lat_0=0 +lon_0=24 +k=0.9996 +x_0=500000 +y_0=0 +datum=GGRS87 +units=m +no_defs", use_iconv = FALSE,encoding = "ISO-8859-7")

#,p4s = "+proj=longlat +datum=WGS84 +ellps=GRS80 +units=m +no_defs"
rethymno_geomap_wgs84 <- spTransform(rethymno_geomap, CRS(" +proj=longlat +datum=GGRS87 +no_defs"))

rethymno_geomap_names <- rethymno_geomap_wgs84@data %>% mutate(id=as.character(seq(from=0,to=(nrow(rethymno_geomap_wgs84@data)-1))))

rethymno_geomap_data <- tidy(rethymno_geomap_wgs84) %>% left_join(rethymno_geomap_names,by=c("id"="id"))

over_rethymno_geomap_data <- over( x = caves_crete_Database_kml_to_txt_shapefile , y = rethymno_geomap , returnList = T) # This is from rgeos, it contains multiple matches, thats why returnList=T. VERY Important


#Irakleio
irakleio_geomap  <- rgdal::readOGR("Shapefiles/Crete_geological_map_SHP/irakleio/geo_uniHERAKLION.shp",verbose = T,p4s = "+proj=tmerc +lat_0=0 +lon_0=24 +k=0.9996 +x_0=500000 +y_0=0 +datum=GGRS87 +units=m +no_defs", use_iconv = FALSE,encoding = "ISO-8859-7")

irakleio_geomap_wgs84 <- spTransform(irakleio_geomap, CRS(" +proj=longlat +datum=GGRS87 +no_defs"))

irakleio_geomap_names <- irakleio_geomap_wgs84@data %>% mutate(id=as.character(seq(from=0,to=(nrow(irakleio_geomap_wgs84@data)-1))))

irakleio_geomap_data <- tidy(irakleio_geomap_wgs84) %>% left_join(irakleio_geomap_names,by=c("id"="id"))

over_irakleio_geomap_data <- over( x = caves_crete_Database_kml_to_txt_shapefile , y = irakleio_geomap , returnList = T) # This is from rgeos, it contains multiple matches, thats why returnList=T. VERY Important


#Chania
chania_geomap  <- rgdal::readOGR("Shapefiles/Crete_geological_map_SHP/chania",layer = "geo_uniCHANIA",verbose = T,p4s = "+proj=tmerc +lat_0=0 +lon_0=24 +k=0.9996 +x_0=500000 +y_0=0 +datum=GGRS87 +units=m +no_defs", use_iconv = FALSE,encoding = "ISO-8859-7")

chania_geomap_wgs84 <- spTransform(chania_geomap, CRS(" +proj=longlat +datum=GGRS87 +no_defs"))

chania_geomap_names <- chania_geomap_wgs84@data %>% mutate(id=as.character(seq(from=0,to=(nrow(chania_geomap_wgs84@data)-1))))

chania_geomap_data <- tidy(chania_geomap_wgs84) %>% left_join(chania_geomap_names,by=c("id"="id"))

over_chania_geomap_data <- over( x = caves_crete_Database_kml_to_txt_shapefile , y = chania_geomap , returnList = T) # This is from rgeos, it contains multiple matches, thats why returnList=T. VERY Important

# lasithi

lasithi1_geomap  <- rgdal::readOGR("Shapefiles/Crete_geological_map_SHP/lasithi/geo_uniLASITHI_1.shp",verbose = T,p4s = "+proj=tmerc +lat_0=0 +lon_0=24 +k=0.9996 +x_0=500000 +y_0=0 +datum=GGRS87 +units=m +no_defs", use_iconv = FALSE,encoding = "ISO-8859-7")

lasithi1_geomap_wgs84 <- spTransform(lasithi1_geomap, CRS(" +proj=longlat +datum=GGRS87 +no_defs"))

lasithi1_geomap_names <- lasithi1_geomap_wgs84@data %>% mutate(id=as.character(seq(from=0,to=(nrow(lasithi1_geomap_wgs84@data)-1))))

lasithi1_geomap_data <- tidy(lasithi1_geomap_wgs84) %>% left_join(lasithi1_geomap_names,by=c("id"="id")) 

over_lasithi1_geomap_data <- over( x = caves_crete_Database_kml_to_txt_shapefile , y = lasithi1_geomap , returnList = T) # This is from rgeos, it contains multiple matches, thats why returnList=T. VERY Important


#2

lasithi2_geomap  <- rgdal::readOGR("Shapefiles/Crete_geological_map_SHP/lasithi/geo_uniLASITHI_2.shp",verbose = T,p4s = "+proj=tmerc +lat_0=0 +lon_0=24 +k=0.9996 +x_0=500000 +y_0=0 +datum=GGRS87 +units=m +no_defs", use_iconv = FALSE,encoding = "ISO-8859-7")

lasithi2_geomap_wgs84 <- spTransform(lasithi2_geomap, CRS(" +proj=longlat +datum=GGRS87 +no_defs"))

lasithi2_geomap_names <- lasithi2_geomap_wgs84@data %>% mutate(id=as.character(seq(from=0,to=(nrow(lasithi2_geomap_wgs84@data)-1))))

lasithi2_geomap_data <- tidy(lasithi2_geomap_wgs84) %>% left_join(lasithi2_geomap_names,by=c("id"="id"))

over_lasithi2_geomap_data <- over( x = caves_crete_Database_kml_to_txt_shapefile , y = lasithi2_geomap , returnList = T) # This is from rgeos, it contains multiple matches, thats why returnList=T. VERY Important


#' 
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------
#all caves underlying geology

all_rock_types_crete <- rbind(rethymno_geomap@data,irakleio_geomap@data,chania_geomap@data,lasithi1_geomap@data,lasithi2_geomap@data) %>% distinct() %>% mutate(color_manual=colorRampPalette(c("orangered2","palegreen3","skyblue1","slateblue1","pink2","sienna3"),space="Lab")( 14 ))

crete_caves_over_geomaps <- rbind(bind_rows(over_rethymno_geomap_data,.id = "ID"),bind_rows(over_irakleio_geomap_data,.id = "ID"),bind_rows(over_chania_geomap_data,.id = "ID"),bind_rows(over_lasithi1_geomap_data,.id = "ID"),bind_rows(over_lasithi2_geomap_data,.id = "ID"))

crete_caves_geomap_data <-caves_crete_Database_kml_to_txt %>% left_join(crete_caves_over_geomaps,by=c("ID"="ID"))



#' 
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------

color_geomaps <- as.character(all_rock_types_crete$color_manual)

names(color_geomaps) <- all_rock_types_crete$NEW

crete_geomap_data <- ggplot()+
  geom_polygon(data = chania_geomap_data,aes(x=long, y=lat,group = group,fill=NEW),lwd=0.082, alpha=0.6)+
  geom_polygon(data = rethymno_geomap_data,aes(x=long, y=lat,group = group,fill=NEW),lwd=0.082, alpha=0.6)+
  geom_polygon(data = irakleio_geomap_data,aes(x=long, y=lat,group = group,fill=NEW),lwd=0.082, alpha=0.6)+
  geom_polygon(data = lasithi1_geomap_data,aes(x=long, y=lat,group = group,fill=NEW),lwd=0.082, alpha=0.6)+
  geom_polygon(data = lasithi2_geomap_data,aes(x=long, y=lat,group = group,fill=NEW),lwd=0.082, alpha=0.6)+
  geom_point(data = caves,aes(x=Longitude, y=Latitude,color="Caves"),size = 2.5)+
  ggtitle("Crete Geological map")+
  labs(x="Longitude",y="Latitude")+
  scale_color_manual(name="", values = c("Caves"="red"))+
  scale_fill_manual(name = "Rock type",values =color_geomaps )+
  scale_x_continuous(breaks = seq(23,26.5,0.5),limits = c(23.2,26.5))+
  scale_y_continuous(breaks = seq(34.5,36,0.5),limits = c(34.5,36))+
  coord_map(xlim = c(23.2,26.5), ylim = c(34.5,36))+
  #coord_fixed(ratio = 1)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),legend.text = element_text(size=22),legend.title = element_text(size=14),axis.text = element_text(size=18), axis.title = element_text(size=22))
  
 ggsave("crete_geomap_data.png", plot = crete_geomap_data, device = "png",width = 20,height = 11.25,units = "in", dpi = 100 ,path = "Plots/")
 

#' 
#' ![Crete geological map](Plots/crete_geomap_data.png)
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------

crete_caves_geology_data_sum <- crete_caves_geomap_data %>% group_by(Subregion,NEW,FORMATION) %>% summarise(number_of_caves=n()) %>% ungroup() %>% na.omit() %>% left_join(all_rock_types_crete,by=c("NEW"="NEW"))

color <- as.character(crete_caves_geology_data_sum$color_manual)
names(color) <- crete_caves_geology_data_sum$NEW

crete_caves_geology_data_sum_all <- crete_caves_geology_data_sum %>% group_by(NEW) %>% summarise(number_of_caves=sum(number_of_caves))

ggplot()+
  geom_col(data = crete_caves_geology_data_sum_all, position =position_dodge(),aes(x=NEW, y= number_of_caves, fill=NEW),show.legend = T)+
  geom_text(data = crete_caves_geology_data_sum_all, stat = "identity",aes(x=NEW, y= number_of_caves,label=number_of_caves, group=NEW), position="identity", hjust=0.5,vjust=-0.25,size=3)+
  scale_y_continuous(breaks = seq(0,80,5),limits = c(0,80))+
  ggtitle("Rock formation of Caves of Crete")+
  labs(x="Rock Formation", y= "Number of caves")+
  scale_fill_manual(name="Rock Formation",values = color)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())

ggsave("crete_geology_caves_barplot.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/") 


ggplot()+
  geom_col(data = crete_caves_geology_data_sum, position =position_dodge(),aes(x=Subregion, y= number_of_caves, fill=NEW),show.legend = T)+
  geom_text(data = crete_caves_geology_data_sum, aes(x=Subregion, y= number_of_caves+0.15,label=number_of_caves, group=NEW), position=position_dodge(0.9),vjust=-0.25,size=3)+
  scale_y_continuous(breaks = seq(0,20,2),limits = c(0,20))+
  ggtitle("Rock formation of Caves of Crete per subregion")+
  labs(x="Administrative Subregion", y= "Number of caves")+
  scale_fill_manual(name="Rock Formation",values = color)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("crete_geology_caves_subregion_barplot.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/") 


#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------
kable(all_rock_types_crete)

#' 
#' 
#' ### Lithological maps of Crete
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE, eval=TRUE,cache=TRUE-----

#Rethymno
rethymno_lithomap  <- rgdal::readOGR("Shapefiles/Crete_lithologic_map/rethymno_s/hydroRETHYMNON.shp",verbose = T,p4s = "+proj=tmerc +lat_0=0 +lon_0=24 +k=0.9996 +x_0=500000 +y_0=0 +datum=GGRS87 +units=m +no_defs", use_iconv = FALSE,encoding = "ISO-8859-7")

rethymno_lithomap@data <- rethymno_lithomap@data %>% mutate(ID=rep.int(0,nrow(.)))

rethymno_lithomap_wgs84 <- spTransform(rethymno_lithomap, CRS("+proj=longlat +datum=GGRS87 +no_defs"))

rethymno_lithomap_names <- rethymno_lithomap_wgs84@data %>% mutate(id=as.character(seq(from=0,to=(nrow(rethymno_lithomap_wgs84@data)-1))))

rethymno_lithomap_data <- tidy(rethymno_lithomap_wgs84) %>% left_join(rethymno_lithomap_names,by=c("id"="id"))

over_rethymno_lithomap_data <- over( x = caves_crete_Database_kml_to_txt_shapefile , y = rethymno_lithomap , returnList = T) # This is from rgeos, it contains multiple matches, thats why returnList=T. VERY Important


#Irakleio 1
irakleio_lithomap_1  <- rgdal::readOGR("Shapefiles/Crete_lithologic_map/irakleio/hydroHERAKLION_1.shp",verbose = T,p4s = "+proj=tmerc +lat_0=0 +lon_0=24 +k=0.9996 +x_0=500000 +y_0=0 +datum=GGRS87 +units=m +no_defs", use_iconv = FALSE,encoding = "ISO-8859-7")

irakleio_lithomap_1_wgs84 <- spTransform(irakleio_lithomap_1, CRS(" +proj=longlat +datum=GGRS87 +no_defs"))

irakleio_lithomap_1_names <- irakleio_lithomap_1_wgs84@data %>% mutate(id=as.character(seq(from=0,to=(nrow(irakleio_lithomap_1_wgs84@data)-1))))

irakleio_lithomap_1_data <- tidy(irakleio_lithomap_1_wgs84) %>% left_join(irakleio_lithomap_1_names,by=c("id"="id")) 

over_irakleio_lithomap_1_data <- over( x = caves_crete_Database_kml_to_txt_shapefile , y = irakleio_lithomap_1 , returnList = T) # This is from rgeos, it contains multiple matches, thats why returnList=T. VERY Important


#Irakleio 2
irakleio_lithomap_2  <- rgdal::readOGR("Shapefiles/Crete_lithologic_map/irakleio/hydroHERAKLION_2.shp",verbose = T,p4s = "+proj=tmerc +lat_0=0 +lon_0=24 +k=0.9996 +x_0=500000 +y_0=0 +datum=GGRS87 +units=m +no_defs", use_iconv = FALSE,encoding = "ISO-8859-7")

irakleio_lithomap_2_wgs84 <- spTransform(irakleio_lithomap_2, CRS(" +proj=longlat +datum=GGRS87 +no_defs"))

irakleio_lithomap_2_names <- irakleio_lithomap_2_wgs84@data %>% mutate(id=as.character(seq(from=0,to=(nrow(irakleio_lithomap_2_wgs84@data)-1))))

irakleio_lithomap_2_data <- tidy(irakleio_lithomap_2_wgs84) %>% left_join(irakleio_lithomap_2_names,by=c("id"="id")) 

over_irakleio_lithomap_2_data <- over( x = caves_crete_Database_kml_to_txt_shapefile , y = irakleio_lithomap_2 , returnList = T) # This is from rgeos, it contains multiple matches, thats why returnList=T. VERY Important

#Chania 1
chania_lithomap_1  <- rgdal::readOGR("Shapefiles/Crete_lithologic_map/chania/hydroCHANIA_1.shp",verbose = T,p4s = "+proj=tmerc +lat_0=0 +lon_0=24 +k=0.9996 +x_0=500000 +y_0=0 +datum=GGRS87 +units=m +no_defs", use_iconv = FALSE,encoding = "ISO-8859-7")

chania_lithomap_1_wgs84 <- spTransform(chania_lithomap_1, CRS(" +proj=longlat +datum=GGRS87 +no_defs"))

chania_lithomap_1_names <- chania_lithomap_1_wgs84@data %>% mutate(id=as.character(seq(from=0,to=(nrow(chania_lithomap_1_wgs84@data)-1))))

chania_lithomap_1_data <- tidy(chania_lithomap_1_wgs84) %>% left_join(chania_lithomap_1_names,by=c("id"="id"))

over_chania_lithomap_1_data <- over( x = caves_crete_Database_kml_to_txt_shapefile , y = chania_lithomap_1 , returnList = T) # This is from rgeos, it contains multiple matches, thats why returnList=T. VERY Important

#Chania 2
chania_lithomap_2  <- rgdal::readOGR("Shapefiles/Crete_lithologic_map/chania/hydroCHANIA_2.shp",verbose = T,p4s = "+proj=tmerc +lat_0=0 +lon_0=24 +k=0.9996 +x_0=500000 +y_0=0 +datum=GGRS87 +units=m +no_defs", use_iconv = FALSE,encoding = "ISO-8859-7")

chania_lithomap_2_wgs84 <- spTransform(chania_lithomap_2, CRS(" +proj=longlat +datum=GGRS87 +no_defs"))

chania_lithomap_2_names <- chania_lithomap_2_wgs84@data %>% mutate(id=as.character(seq(from=0,to=(nrow(chania_lithomap_2_wgs84@data)-1))))

chania_lithomap_2_data <- tidy(chania_lithomap_2_wgs84) %>% left_join(chania_lithomap_2_names,by=c("id"="id"))

over_chania_lithomap_2_data <- over( x = caves_crete_Database_kml_to_txt_shapefile , y = chania_lithomap_2 , returnList = T) # This is from rgeos, it contains multiple matches, thats why returnList=T. VERY Important

#Lasithi 1
lasithi_lithomap_1  <- rgdal::readOGR("Shapefiles/Crete_lithologic_map/lasithi/hydroLASSITHI_1.shp",verbose = T,p4s = "+proj=tmerc +lat_0=0 +lon_0=24 +k=0.9996 +x_0=500000 +y_0=0 +datum=GGRS87 +units=m +no_defs", use_iconv = FALSE,encoding = "ISO-8859-7")

lasithi_lithomap_1_wgs84 <- spTransform(lasithi_lithomap_1, CRS(" +proj=longlat +datum=GGRS87 +no_defs"))

lasithi_lithomap_1_names <- lasithi_lithomap_1_wgs84@data %>% mutate(id=as.character(seq(from=0,to=(nrow(lasithi_lithomap_1_wgs84@data)-1))))

lasithi_lithomap_1_data <- tidy(lasithi_lithomap_1_wgs84) %>% left_join(lasithi_lithomap_1_names,by=c("id"="id"))

over_lasithi_lithomap_1_data <- over( x = caves_crete_Database_kml_to_txt_shapefile , y = lasithi_lithomap_1 , returnList = T) # This is from rgeos, it contains multiple matches, thats why returnList=T. VERY Important

#Lasithi 2
lasithi_lithomap_2  <- rgdal::readOGR("Shapefiles/Crete_lithologic_map/lasithi/hydroLASSITHI_2.shp",verbose = T,p4s = "+proj=tmerc +lat_0=0 +lon_0=24 +k=0.9996 +x_0=500000 +y_0=0 +datum=GGRS87 +units=m +no_defs", use_iconv = FALSE,encoding = "ISO-8859-7")
lasithi_lithomap_2@data <- lasithi_lithomap_2@data %>% dplyr::select(-Shape_Area,-Shape_Leng)


lasithi_lithomap_2_wgs84 <- spTransform(lasithi_lithomap_2, CRS(" +proj=longlat +datum=GGRS87 +no_defs"))

lasithi_lithomap_2_names <- lasithi_lithomap_2_wgs84@data %>% mutate(id=as.character(seq(from=0,to=(nrow(lasithi_lithomap_2_wgs84@data)-1))))

lasithi_lithomap_2_data <- tidy(lasithi_lithomap_2_wgs84) %>% left_join(lasithi_lithomap_2_names,by=c("id"="id"))

over_lasithi_lithomap_2_data <- over( x = caves_crete_Database_kml_to_txt_shapefile , y = lasithi_lithomap_2 , returnList = T) # This is from rgeos, it contains multiple matches, thats why returnList=T. VERY Important


#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------
#all caves underlying lithology

all_lithology_crete <- do.call("rbind",list(rethymno_lithomap@data,irakleio_lithomap_1@data,irakleio_lithomap_2@data,chania_lithomap_1@data,chania_lithomap_2@data,lasithi_lithomap_1@data,lasithi_lithomap_2@data)) %>% distinct()  %>% mutate(color_manual=colorRampPalette(c("orangered2","palegreen3","skyblue1","slateblue1","pink2","sienna3"),space="Lab")( 11 ))


crete_caves_over_lithomaps <- rbind(bind_rows(over_rethymno_lithomap_data,.id = "ID_cave"),bind_rows(over_irakleio_lithomap_1_data,.id = "ID_cave"),bind_rows(over_irakleio_lithomap_2_data,.id = "ID_cave"),bind_rows(over_chania_lithomap_1_data,.id = "ID_cave"),bind_rows(over_chania_lithomap_2_data,.id = "ID_cave"),bind_rows(over_lasithi_lithomap_1_data,.id = "ID_cave"),bind_rows(over_lasithi_lithomap_2_data,.id = "ID_cave"))

crete_caves_lithology_data <-caves_crete_Database_kml_to_txt %>% left_join(crete_caves_over_lithomaps,by=c("ID"="ID_cave")) %>% na.omit()



#' 
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------

color_lith <- as.character(all_lithology_crete$color_manual)
names(color_lith) <- all_lithology_crete$Code_1

crete_lithomap_data <- ggplot()+
  geom_polygon(data = chania_lithomap_1_data,aes(x=long, y=lat,group = group,fill=Code_1),lwd=0.082, alpha=0.6)+
  geom_polygon(data = chania_lithomap_2_data,aes(x=long, y=lat,group = group,fill=Code_1),lwd=0.082, alpha=0.6)+
  geom_polygon(data = rethymno_lithomap_data,aes(x=long, y=lat,group = group,fill=Code_1),lwd=0.082, alpha=0.6)+
  geom_polygon(data = irakleio_lithomap_1_data,aes(x=long, y=lat,group = group,fill=Code_1),lwd=0.082, alpha=0.6)+
  geom_polygon(data = irakleio_lithomap_2_data,aes(x=long, y=lat,group = group,fill=Code_1),lwd=0.082, alpha=0.6)+
  geom_polygon(data = lasithi_lithomap_1_data,aes(x=long, y=lat,group = group,fill=Code_1),lwd=0.082, alpha=0.6)+
  geom_polygon(data = lasithi_lithomap_2_data,aes(x=long, y=lat,group = group,fill=Code_1),lwd=0.082, alpha=0.6)+
  geom_point(data = caves,aes(x=Longitude, y=Latitude,color="Caves"),size = 2.5)+
  ggtitle("Crete Lithological map")+
  labs(x="Longitude",y="Latitude")+
  scale_color_manual(name="", values = c("Caves"="red"))+
  scale_fill_manual(name = "Rock type",values = color_lith)+
  scale_x_continuous(breaks = seq(23,26.5,0.5),limits = c(23.2,26.5))+
  scale_y_continuous(breaks = seq(34.5,36,0.5),limits = c(34.5,36))+
  coord_map(xlim = c(23.2,26.5), ylim = c(34.5,36))+
  #coord_fixed(ratio = 1)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),legend.text = element_text(size=22),legend.title = element_text(size=22),axis.text = element_text(size=18), axis.title = element_text(size=22))
  
 ggsave("crete_lithomap_data.png", plot = crete_lithomap_data,device = "png",width = 20,height = 11.25,units = "in", dpi = 100 ,path = "Plots/")
 

#' 
#' ![Crete lithological map](Plots/crete_lithomap_data.png)
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------

crete_caves_lithology_data_sum <- crete_caves_lithology_data %>% group_by(Subregion,Code_1,Category_1) %>% summarise(number_of_caves=n()) %>% ungroup() 

crete_caves_lithology_data_sum_all <- crete_caves_lithology_data_sum %>% group_by(Code_1,Category_1) %>% summarise(number_of_caves=sum(number_of_caves))

ggplot()+
  geom_col(data = crete_caves_lithology_data_sum_all, position =position_dodge(),aes(x=Code_1, y= number_of_caves, fill=Code_1),show.legend = T)+
  geom_text(data = crete_caves_lithology_data_sum_all, stat = "identity",aes(x=Code_1, y= number_of_caves,label=number_of_caves, group=Code_1), position="identity", hjust=0.5,vjust=-0.25,size=3)+
  scale_y_continuous(breaks = seq(0,80,5),limits = c(0,80))+
  #ggtitle("Caves Greece")+
  labs(x="Lithology", y= "Number of caves")+
  scale_fill_manual(name="Lithology",values = color_lith)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())

ggsave("crete_lithology_caves_barplot.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/") 

ggplot()+
  geom_col(data = crete_caves_lithology_data_sum, position =position_dodge(),aes(x=Subregion, y= number_of_caves, fill=Code_1),show.legend = T)+
  geom_text(data = crete_caves_lithology_data_sum, aes(x=Subregion, y= number_of_caves+0.15,label=number_of_caves, group=Code_1), position=position_dodge(0.9),vjust=-0.25,size=3)+
  scale_y_continuous(breaks = seq(0,20,2),limits = c(0,20))+
  #ggtitle("Caves Greece")+
  labs(x="Administrative Subregion", y= "Number of caves")+
  scale_fill_manual(name="Lithology",values = color_lith)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("crete_lithology_caves_subregion_barplot.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/") 



#' 
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------
kable(all_lithology_crete)


#' 
#' 
#' ## Interactive map
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE, eval=FALSE---------------
## m <- leaflet() %>% addTiles() %>% addPolygons(data = rethymno_lithomap, fillColor =heat.colors(3, alpha = NULL) ) %>% setView(25, 35.5, zoom = 8) %>% addLegend(position = 'topright', colors = "blue" , labels = "Lithomap", opacity = 0.4,title = 'Legend')
## 
## 
## leaflet() %>% addTiles() %>% addPolygons(data=rethymno_lithomap) %>% setView(25, 35.5, zoom = 8) %>% addMarkers(data = caves_crete,~Longitude, ~Latitude, popup = ~as.character(Cave_ID), label = ~as.character(Cave_Name))
## 
## 
## 

#' 
