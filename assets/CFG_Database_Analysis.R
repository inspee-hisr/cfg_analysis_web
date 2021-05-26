#!/usr/bin/env Rscript

# Data manipulation packages
library(gridExtra)
library(grid)
library(tidyverse)
library(kableExtra)
## Data for species

library(rredlist)
library(taxize)
#library(rgbif)
#library(ISOcodes)
#library(spocc)

# Spatial analysis packages

library(RColorBrewer)
#library(ggmap)
library(rgdal)
#library(geosphere)
#library(GISTools)
#library(leaflet)
#library(rgeos)
library(maptools)
#library(tmap)
library(Rcpp)
library(sp)
library(raster) ##Load the Raster Library

# Species statistics

#library(red)
library(vegan)

data_files <- list.files(path = "Data")
 
# Data import from Database Export, the files are choosen automatically based on their name. The folder Data must contain only the latest data files.
Cave_References <- read_delim(file = paste0("Data/",grep("Cave_References",data_files,value = TRUE)),delim = "\t")
 
caves <- read_delim(file = paste0("Data/",grep("Caves",data_files,value = TRUE)),delim = "\t")
caves$Longitude <- as.numeric(caves$Longitude)
caves$Latitude <- as.numeric(caves$Latitude)

census <- read_delim(file = paste0("Data/",grep("Census_\\d",data_files,value = TRUE)),delim = "\t")
 
Census_references <- read_delim(file = paste0("Data/",grep("Census_references",data_files,value = TRUE)),delim = "\t")
 
species <- read_delim(file = paste0("Data/",grep("Species_",data_files,value = TRUE)),delim = "\t") %>% mutate(Classification=gsub(pattern="\\?",replacement = "",x = Classification))# Data import from Database Export


census$species_epithet <- as.character(lapply(strsplit(as.character(census$Species), split=" "), "[", n=2))

census_all_species <- census %>% left_join(species,by=c("Species"="Species_Full_Name"))

census_all_species_all_caves <- census_all_species %>% dplyr::select(-Cave_Name) %>% left_join(caves, by=c("Cave_ID"="Cave_ID"))

census_long_str_man <- strsplit(x = census_all_species$Reference_Short,split = "|",fixed=TRUE)
census_long_str_man_id <- strsplit(x = census_all_species$Reference_ID,split = "|",fixed=TRUE)

census_long_man <- data_frame(ReferenceShort=unlist(census_long_str_man),reference_id=unlist(census_long_str_man_id),CaveName=rep.int(census_all_species$Cave_Name,times = sapply(census_long_str_man,length)),Cave_ID=rep.int(census_all_species$Cave_ID,times = sapply(census_long_str_man,length)),Census_id=rep.int(census_all_species$Census_ID,times = sapply(census_long_str_man,length)),Species=rep.int(census_all_species$Species,times = sapply(census_long_str_man,length))) %>% group_by(ReferenceShort,Cave_ID,CaveName,Species,Census_id) %>% summarise(n=n()) %>% ungroup() %>% mutate(Species=trimws(Species,"r"))

## Regions

caves_Region <- caves %>% dplyr::select(Cave_ID, Region) %>% distinct() %>% group_by(Region) %>% summarize(number_of_caves=n()) %>% na.omit() %>% mutate(color_manual=colorRampPalette(c("orangered2","palegreen3","skyblue1","slateblue1","pink2","sienna3"),space="Lab")( 14 ))

ggplot()+
  geom_col(data = caves_Region, aes(x=Region, y= number_of_caves, fill=Region),show.legend = F)+
  geom_text(data = caves_Region,aes(x =Region,y= number_of_caves, label=number_of_caves), position=position_dodge(width=0.7), vjust=-0.25,size=2.8)+
  scale_y_continuous(breaks = seq(0,200,50),limits = c(0,200))+
  #ggtitle("Caves Greece")+
  labs(x="Administrative Region", y= "Number of caves")+
  scale_fill_manual(values = caves_Region$color_manual)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1))


ggsave("caves_Region_barplot.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")

#' Both species and caves per region.
species_region_endemic <- census_all_species_all_caves %>% filter(species_epithet!="sp.") %>% filter(Distribution=="Endemic to Greece") %>% dplyr::select(Species,Distribution, Region) %>% distinct(.) %>% group_by(Region) %>% summarise(number_of_endemic_species=n()) %>% na.omit()

species_Region <- census_all_species_all_caves %>% filter(species_epithet!="sp.") %>% dplyr::select(Species,Region) %>% distinct() %>% group_by(Region) %>% summarise(number_of_species=n()) %>% na.omit()

species_troglobiont_Region <- census_all_species_all_caves %>% filter(species_epithet!="sp.") %>% dplyr::select(Species,Region, Classification) %>% distinct() %>% filter(Classification=="Troglobiont") %>% group_by(Region) %>% summarise(number_of_troglobiont_species=n()) %>% na.omit()

caves_species_region <- species_Region %>% left_join(caves_Region, by=c("Region"="Region")) %>% left_join(species_region_endemic, by=c("Region"="Region")) %>% left_join(species_troglobiont_Region, by=c("Region"="Region")) %>% gather(key = Variable,value = number,-Region,-color_manual) %>% replace(is.na(.),0)

caves_species_region$Variable <- factor(caves_species_region$Variable, levels = c("number_of_caves","number_of_species","number_of_endemic_species","number_of_troglobiont_species"))

caves_species_region_plot <- ggplot()+
  geom_col(data = caves_species_region, aes(x=Region, y= number, fill=Variable),width=0.82, position = position_dodge(width = 0.82),show.legend = T)+
  geom_text(data = caves_species_region,aes(x =Region,y= number, label=number,group=Variable), position=position_dodge(width = 0.87), vjust=-0.25,size=5)+
  scale_y_continuous(breaks = seq(0,280,20),limits = c(0,285),expand = c(0.01,0.4))+
  scale_x_discrete(expand = c(0.01,0.4))+
  scale_fill_manual(label=c("Caves","All species","Species endemic to Greece","Troglobiont species"),values = c("coral1","lightgoldenrod2","lightpink1","lightblue1"),name="")+
  labs(x="Region", y= "Count")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),legend.text = element_text(size = 18),axis.text.y=element_text(margin = margin(t = 0, r = 0, b = 0, l = 10,unit = "pt"),size = 18),axis.text.x = element_text(angle = 45, hjust = 1,size = 18),axis.title = element_text(size=22),panel.border = element_blank(),axis.line.x = element_line(colour = 'black', size = 0.3), axis.line.y = element_line(colour = 'black', size = 0.3),legend.position = c(0.13,0.87), legend.key.size = unit(1, "cm"))

ggsave("caves_species_region.png", plot = caves_species_region_plot, device = "png",width = 20,height = 11.25,units = "in", dpi = 100,path = "Website_plots/")


## Species taxonomy

species_class <- species %>% dplyr::select(Species_Full_Name,Class) %>% distinct() %>% group_by(Class) %>% summarise(number_of_species=n()) %>% na.omit() %>% mutate(color_manual=colorRampPalette(c("orangered2","palegreen3","skyblue1","slateblue1","pink2","goldenrod1","slategray2"),space="Lab")( 20 ))

species_class_barplot <- ggplot()+
  geom_col(data = species_class, aes(x=Class, y= number_of_species, fill=Class),show.legend = F)+
  geom_text(data = species_class,aes(x =Class,y= number_of_species, label=number_of_species), position=position_dodge(width=0.7), vjust=-0.25,size=6)+
  scale_y_continuous(breaks = seq(0,300,25),limits = c(0,300))+
  labs(x="Class", y= "Number of species")+
  scale_fill_manual(values = species_class$color_manual)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),legend.text = element_text(size = 22),axis.text.y=element_text(size = 18),axis.text.x = element_text(angle = 45, hjust = 1,size = 18),axis.title = element_text(size=22),panel.border = element_blank(),axis.line.x = element_line(colour = 'black', size = 0.3), axis.line.y = element_line(colour = 'black', size = 0.3),legend.position = c(0.15,0.91), legend.key.size = unit(1, "cm"))


ggsave("species_class_barplot.png", plot = species_class_barplot, device = "png",width = 20,height = 15,units = "in", dpi = 100,path = "Website_plots/")

# Table for statistics tab on the website: for each phylum, for each class, for each order count number of families, genera, species
species_na_order <- species %>% filter(is.na(Order))

species_taxonomy_table <- species %>% dplyr::select(Species_Full_Name,Genus,Family,Order,Class,Phylum) %>% group_by(Genus,Family,Order,Class,Phylum) %>% summarise(Species=n()) %>% group_by(Family,Order,Class,Phylum) %>% summarise(Genera=n(),Species=sum(Species)) %>% group_by(Order,Class,Phylum) %>% summarise(Families=n(),Genera=sum(Genera),Species=sum(Species)) %>% group_by(Class,Phylum) %>% mutate(Orders=n()) %>% ungroup()

# Create Taxonomic Summary Table for The Database Statistics

database_taxonomic_summary <- as_tibble(matrix(ncol = 5))
colnames(database_taxonomic_summary)<- c("TAXA","Orders","Families","Genera","Species")
phyla <- unique(species_taxonomy_table$Phylum)

for(i in 1:length(phyla)) {
  
  phylum <- as_tibble(matrix(c(paste0("Phylum ",phyla[i]),NA,NA,NA,NA),ncol = 5))
  colnames(phylum)<- c("TAXA","Orders","Families","Genera","Species")
  
  database_taxonomic_summary <- rbind(database_taxonomic_summary,phylum)
  
  class <- species_taxonomy_table %>% filter(Phylum==phyla[i]) %>%
    group_by(Class,Orders) %>%   summarise(Families=sum(Families),Genera=sum(Genera),Species=sum(Species)) %>% dplyr::rename(TAXA=Class) %>% ungroup()
  
  for(j  in 1:nrow(class)) {

    database_taxonomic_summary <- rbind(database_taxonomic_summary,class[j,])
    
    orders <- species_taxonomy_table %>% filter(Class==as.character(class[j,1])) %>% dplyr::select(Order,Orders,Families,Genera,Species) %>% dplyr::rename(TAXA=Order) %>% mutate(Orders=NA) %>% ungroup()
      
    database_taxonomic_summary <- rbind(database_taxonomic_summary,orders)

  }

}

# Create summary, totals
TOTAL <- as_tibble(matrix(c(NA,NA,NA,NA,NA,"TOTAL",length(unique(species$Order)),length(unique(species$Family)),length(unique(species$Genus)),length(unique(species$Species_Full_Name))),ncol = 5,nrow = 2,byrow = T))
  colnames(TOTAL)<- c("TAXA","Orders","Families","Genera","Species")

database_taxonomic_summary <- database_taxonomic_summary[-1,] %>% rbind(.,TOTAL)

write_delim(database_taxonomic_summary,delim = "\t",col_names = T,path = "database_taxonomic_summary.tsv",na = " ")

database_taxonomic_summary[is.na(database_taxonomic_summary)] <- " " # replace NA with space


# HTML formatting and exporting
database_taxonomic_summary %>% mutate(TAXA = cell_spec(TAXA, "html", bold = ifelse(grepl(pattern = "^Phylum",x = database_taxonomic_summary$TAXA), "TRUE", ifelse(database_taxonomic_summary$Orders!=" ", "TRUE", "FALSE")),underline = ifelse(grepl(pattern = "^Phylum",x = database_taxonomic_summary$TAXA), "TRUE", "FALSE"))) %>% kable(format = "html", escape = F) %>% kable_styling(bootstrap_options = c("hover","condensed"),font_size = 15) %>% cat(., file = "Website_plots/database_taxonomic_summary.html")

# check the font type
# import in Website: paste the html code in source code of the edit stats page, 
#
### Taxonomy endemic 

species_class_endemic <- species %>% filter(Distribution=="Endemic to Greece") %>% dplyr::select(Species_Full_Name,Class) %>% distinct() %>% group_by(Class) %>% summarise(number_of_species=n()) %>% na.omit()

ggplot()+
  geom_col(data = species_class_endemic, aes(x=Class, y= number_of_species, fill=Class),show.legend = F)+
  geom_text(data = species_class_endemic,aes(x =Class,y= number_of_species, label=number_of_species), position=position_dodge(width=0.7), vjust=-0.25,size=2.8)+
  scale_y_continuous(breaks = seq(0,300,50),limits = c(0,300))+
  ggtitle("Species endemic to Greece")+
  labs(x="Class", y= "Number of species")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("species_class_barplot_endemic.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")

# bar plots with Arachnida, Insecta, Malacostrata orders

most_abudant_classes <- species %>% filter(Class=="Arachnida" | Class=="Insecta" | Class=="Malacostraca") %>% group_by(Class,Order) %>% summarise(number_of_species=n())

ggplot()+
  geom_col(data = most_abudant_classes, aes(x=Order, y= number_of_species, fill=Order),show.legend = F)+
  geom_text(data = most_abudant_classes,aes(x =Order,y= number_of_species, label=number_of_species), position=position_dodge(width=0.7), vjust=-0.25,size=2.8)+
  scale_y_continuous(breaks = seq(0,200,50),limits = c(0,200))+
  #ggtitle("Inferring methods of the Sign Score of the PPI network of Drosophila gene")+
  labs(x="Order", y= "Number of species")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~ Class,scales = "free", ncol=1)
  

ggsave("most_abudant_classes.jpeg", plot = last_plot(), width = 15,height = 20,units = "cm",device = "jpeg", dpi = 300,path = "Plots/")

## Per administrative division
### Species
species_Region <- census_all_species_all_caves %>% filter(species_epithet!="sp.") %>% dplyr::select(Species,Region) %>% distinct() %>% group_by(Region) %>% summarise(number_of_species=n()) %>% na.omit()

species_Municipality <- census_all_species_all_caves %>% filter(species_epithet!="sp.") %>% dplyr::select(Species,Municipality) %>% distinct() %>% group_by(Municipality) %>% summarise(number_of_species=n()) %>% na.omit()

### Caves

caves_municipality <- caves %>% dplyr::select(Cave_ID,Cave_Name,Municipality) %>% distinct() %>% group_by(Municipality) %>% summarise(number_of_caves=n()) %>% na.omit()

## Ecological classification

species_classification <- species %>% dplyr::select(Species_Full_Name, Classification)

species_classification_summary <- species %>% group_by(Classification) %>% summarise(number_of_species=n()) %>% mutate(frequency=round(number_of_species/sum(number_of_species),digits = 3)) %>% mutate(Species_status="All species")

ggplot()+
  geom_col(data = species_classification_summary, aes(x=Classification, y= number_of_species, fill=Classification),show.legend = F)+
  geom_text(data = species_classification_summary,aes(x =Classification,y= number_of_species, label=number_of_species), position=position_dodge(width=0.7), vjust=-0.25,size=2.8)+
  scale_y_continuous(breaks = seq(0,350,50),limits = c(0,350))+
  labs(x="Ecological Classification", y= "Number of species")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("species_ecological_classification.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")

ggplot()+
  geom_col(data = species_classification_summary, aes(x=Classification, y= frequency, fill=Classification),show.legend = F)+
  geom_text(data = species_classification_summary,aes(x =Classification,y= frequency, label=frequency), position=position_dodge(width=0.7), vjust=-0.25,size=2.8)+
  scale_y_continuous(breaks = seq(0,1,0.1),limits = c(0,1))+
  labs(x="Ecological Classification", y= "Frequency")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("species_ecological_classification_frequency.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")

ggplot()+
  geom_col(data = species_classification_summary, aes(x="", y= frequency, fill=Classification),width = 1,show.legend = T)+
  labs(x = "", y = "")+ 
  #geom_text(aes(x = 3.5, y = ((ymin+ymax)/2), label = Greek_Red_Data_Book)) +
  #scale_y_continuous(breaks = 0:10)+  #labs(x="Greek red data list", y= "Frequency")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank(),panel.border = element_blank(),axis.ticks=element_blank())+
coord_polar(theta = "y")

ggsave("species_classification_summary_pie.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")


# For endemic species. 

species_classification_categories <- data.frame(Classification=unique(species$Classification))

species_classification_summary_endemic <- species %>% filter(Distribution=="Endemic to Greece") %>% group_by(Classification) %>% summarise(number_of_species=n()) %>% mutate(frequency=round(number_of_species/sum(number_of_species),digits = 3)) %>% mutate(Species_status="Endemic to Greece")

species_classification_summary_endemic_all_categories <- species_classification_categories %>% left_join(species_classification_summary_endemic,by=c("Classification"="Classification")) %>% mutate(number_of_species=if_else(is.na(number_of_species),0,as.numeric(number_of_species))) %>% mutate(frequency=round(number_of_species/sum(number_of_species),digits = 3)) %>% mutate(Species_status="Endemic to Greece")

ggplot()+
  geom_col(data = species_classification_summary_endemic, aes(x=Classification, y= number_of_species, fill=Classification),show.legend = F)+
  geom_text(data = species_classification_summary_endemic,aes(x =Classification,y= number_of_species, label=number_of_species), position=position_dodge(width=0.7), vjust=-0.25,size=2.8)+
  scale_y_continuous(breaks = seq(0,300,50),limits = c(0,300))+
  #ggtitle("Inferring methods of the Sign Score of the PPI network of Drosophila gene")+
  labs(x="Ecological Classification", y= "Number of species endemic to Greece")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("species_ecological_classification_only_endemic.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")


classification_all_and_endemic <- rbind(species_classification_summary_endemic_all_categories,species_classification_summary)

classification_all_and_endemic$Classification <- factor(classification_all_and_endemic$Classification,levels = c("Accidental","Trogloxene","Stygoxene","Stygophile","Troglophile","Stygobiont","Troglobiont"))

species_ecological_classification_all_and_endemic <- ggplot()+
  geom_col(data = classification_all_and_endemic, aes(x=Classification, y= number_of_species, fill=Species_status,width=0.8), position = position_dodge(width = 0.8),show.legend = T)+
  geom_text(data = classification_all_and_endemic,aes(x =Classification,y= number_of_species, label=number_of_species,group=Species_status), position=position_dodge(width=0.9), vjust=-0.25,size=6)+
  scale_y_continuous(breaks = seq(0,350,25),limits = c(0,350),expand = c(0.01,0.4))+
  scale_fill_manual(labels=c("All species","Species endemic to Greece"),values = c("lightgoldenrod2","lightpink1"),name="")+
  labs(x="Classification", y= "Number of species")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),legend.text = element_text(size = 18),axis.text.y=element_text(margin = margin(t = 0, r = 0, b = 0, l = 10,unit = "pt"),size = 18),axis.text.x = element_text(margin = margin(t = 0, r = 0, b = 12, l = 0,unit = "pt"),angle = 45, hjust = 1,size = 18),axis.title = element_text(size=22),panel.border = element_blank(),axis.line.x = element_line(colour = 'black', size = 0.3), axis.line.y = element_line(colour = 'black', size = 0.3),legend.position = c(0.132,0.93), legend.key.size = unit(1.5, "cm"))

ggsave("species_ecological_classification_all_and_endemic.png", plot = species_ecological_classification_all_and_endemic, device = "png",width = 20,height = 11.25,units = "in", dpi = 100,path = "Website_plots/")


## Locus Typicus
species_class_locus <- species %>% filter(!(is.na(Locus_Typicus_Cave))) %>% dplyr::select(Species_Full_Name,Class) %>% distinct() %>% group_by(Class) %>% summarise(number_of_species=n()) %>% na.omit()

ggplot()+
  geom_col(data = species_class_locus, aes(x=Class, y= number_of_species, fill=Class),show.legend = F)+
  geom_text(data = species_class_locus,aes(x =Class,y= number_of_species, label=number_of_species), position=position_dodge(width=0.7), vjust=-0.25,size=2.8)+
  scale_y_continuous(breaks = seq(0,150,25),limits = c(0,125))+
  #ggtitle("Inferring methods of the Sign Score of the PPI network of Drosophila gene")+
  labs(x="Class", y= "Number of species with Locus Typicus Greek caves")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("species_class_barplot_locus_typicus.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")

species_class_locus_typicus_count <- species %>% filter(!(is.na(Locus_Typicus_Cave))) %>% dplyr::select(Species_Full_Name,Class,Locus_Typicus_Cave,Locus_Typicus_Cave_ID) %>% distinct() %>% group_by(Locus_Typicus_Cave_ID,Locus_Typicus_Cave) %>% summarise(number_of_species=n()) %>% na.omit() %>% arrange(desc(number_of_species))

species_class_locus_typicus_count_dist <- species_class_locus_typicus_count %>% group_by(number_of_species) %>% summarise(number_of_caves=n())

 ggplot(data=species_class_locus_typicus_count_dist)+
  geom_line(aes(x=number_of_species, y= number_of_caves),color="dodgerblue2",show.legend = F)+
  geom_point(aes(x=number_of_species, y= number_of_caves),color="dodgerblue2",show.legend = F, size=1)+
  #ggtitle("Species")+
  scale_y_continuous(breaks = seq(0,75,5),limits = c(0,75))+
  scale_x_continuous(breaks = seq(0,12,1), limits = c(0,12))+
  labs(x="Number of species", y= "Number of Locus Typicus caves")+
  theme_bw()+theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())
  
ggsave("cave_dist_locus_typicus.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")
 

kable(x = head(species_class_locus_typicus_count,15))

# Species Richness and Altitude

caves_per_altitude <- caves %>% distinct(Cave_ID,Altitude,Region) %>% na.omit() %>% mutate(Bins=cut(Altitude,breaks=seq(0,2200,by=100))) %>% group_by(Bins) %>%  mutate(Mean_altitude=mean(Altitude)) %>% mutate(number_of_caves=n()) %>% distinct(Mean_altitude,Bins,number_of_caves) #

ggplot()+
  geom_line(data = caves_per_altitude, aes(x=Mean_altitude, y=number_of_caves),show.legend = T)+
  #geom_freqpoly(data = species_per_altitude, aes(x=Altitude),binwidth = 100,show.legend = T)+
  scale_y_continuous(breaks = seq(0,150,20),limits = c(0,150))+
  scale_x_continuous(breaks = seq(0,2200,200),limits = c(0,2200))+
  ggtitle("Altitude of the caves of Crete")+
  labs(x="Altitude", y= "Number of caves")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())

ggsave("caves_per_altitude.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")


species_per_altitude <- census_all_species_all_caves %>% filter(species_epithet!="sp.") %>% distinct(Species,Classification,Class,Altitude) %>% mutate(Classification_short=gsub(pattern="\\?",replacement = "",x = Classification)) %>% mutate(Bins=cut(Altitude,breaks=seq(0,2200,by=100))) %>% group_by(Bins) %>% mutate(Mean_altitude=mean(Altitude)) %>% distinct(Species,Classification,Class,Mean_altitude) #

species_per_altitude_summary <- species_per_altitude %>% group_by(Mean_altitude) %>% summarise(number_of_species=n())

ggplot()+
  geom_line(data = species_per_altitude_summary, aes(x=Mean_altitude,y=number_of_species))+
  #geom_freqpoly(data = species_per_altitude, aes(x=Altitude),binwidth = 100,show.legend = T)+
  scale_y_continuous(breaks = seq(0,400,50),limits = c(0,300))+
  scale_x_continuous(breaks = seq(0,2200,200),limits = c(0,2200))+
  ggtitle("Species richness and altitude")+
  labs(x="Altitude", y= "Number of species")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("species_per_altitude.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")


species_per_altitude_summary_classification <- species_per_altitude %>% group_by(Mean_altitude,Classification) %>% summarise(number_of_species=n())

ggplot()+
  geom_line(data = species_per_altitude_summary, aes(x=Mean_altitude,y=number_of_species),color="black")+
  geom_line(data = species_per_altitude_summary_classification, aes(x=Mean_altitude,y=number_of_species,color=Classification),show.legend = T)+
  scale_y_continuous(breaks = seq(0,300,25),limits = c(0,300))+
  scale_x_continuous(breaks = seq(0,2200,200),limits = c(0,2200))+
  ggtitle("Species richness and altitude for different ecological classifications")+
  labs(x="Altitude", y= "Number of species")+
  theme_bw()+
  scale_color_brewer(palette="Set3")+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())

ggsave("species_per_altitude_classification.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")


species_per_altitude_summary_class <- species_per_altitude %>% group_by(Mean_altitude,Class) %>% summarise(number_of_species=n()) 

tol21rainbow= c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#117744", "#44AA77", "#88CCAA", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788")

ggplot()+
  geom_line(data = species_per_altitude_summary, aes(x=Mean_altitude,y=number_of_species))+
  geom_line(data = species_per_altitude_summary_class, aes(x=Mean_altitude,y=number_of_species,color=Class),show.legend = T)+
  #geom_freqpoly(data = species_per_altitude, aes(x=Altitude),binwidth = 100,show.legend = T)+
  scale_y_continuous(breaks = seq(0,300,25),limits = c(0,300))+
  scale_x_continuous(breaks = seq(0,2200,200),limits = c(0,2200))+
  ggtitle("Species richness and altitude for different classes")+
  labs(x="Altitude", y= "Number of species")+
  #scale_fill_manual(values = getPalette(length(unique(species_per_altitude_summary_class$Class))))+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())

ggsave("species_per_altitude_class.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")

# Red Lists
## IUCN status
iucn_species <- species %>% group_by(IUCN_Red_List) %>% summarise(number_of_species=n()) %>% mutate(frequency=round(number_of_species/sum(number_of_species),digits = 3)) %>% mutate(Red_List="IUCN Red List") %>% dplyr::rename(., Categories=IUCN_Red_List) %>% rbind(.,data_frame(Categories="EN - Endangered",number_of_species=0,frequency=0,Red_List="IUCN Red List"))

kable(iucn_species)

ggplot()+
  geom_col(data = iucn_species, aes(x=Categories, y= number_of_species, fill=Categories),show.legend = F)+
  geom_text(data = iucn_species,aes(x =Categories,y= number_of_species, label=number_of_species), position=position_dodge(width=0.7), vjust=-0.25,size=2.8)+
  scale_y_continuous(breaks = seq(0,820,100),limits = c(0,820))+
  labs(x="IUCN status", y= "Number of species")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("iucn_species.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")

ggplot()+
  geom_col(data = iucn_species, aes(x=Categories, y= frequency, fill=Categories),show.legend = F)+
  geom_text(data = iucn_species,aes(x =Categories,y= frequency, label=frequency), position=position_dodge(width=0.7), vjust=-0.25,size=2.8)+
  scale_y_continuous(breaks = seq(0,1,0.1),limits = c(0,1))+
  labs(x="IUCN status", y= "Frequency")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("iucn_species_frequency.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")

ggplot()+
  geom_col(data = iucn_species, aes(x="", y= frequency, fill=Categories),width = 1,show.legend = T)+
  labs(x = "", y = "")+ 
  #geom_text(aes(x = 3.5, y = ((ymin+ymax)/2), label = Greek_Red_Data_Book)) +
  #scale_y_continuous(breaks = 0:10)+  #labs(x="Greek red data list", y= "Frequency")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank(),panel.border = element_blank(),axis.ticks=element_blank())+
coord_polar(theta = "y")

ggsave("IUCN_Red_List_pie.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")


### IUCN and ecological classification
iucn_species_classification <- species %>% group_by(IUCN_Red_List, Classification) %>% summarise(number_of_species=n()) %>% mutate(frequency=round(number_of_species/sum(number_of_species),digits = 3)) %>% mutate(Red_List="IUCN Red List") %>% dplyr::rename(., Categories=IUCN_Red_List) #%>% rbind(.,data_frame(Categories="EN - Endangered",number_of_species=0,frequency=0,Red_List="IUCN Red List"))

ggplot()+
  geom_col(data = iucn_species_classification, aes(x=Categories, y= number_of_species, fill=Classification),show.legend = T)+
  #geom_text(data = iucn_species_classification,aes(x =Categories,y= number_of_species, label=number_of_species), position=position_dodge(width=0.7), vjust=-0.25,size=2.8)+
  scale_y_continuous(breaks = seq(0,720,100),limits = c(0,720))+
  labs(x="IUCN status", y= "Number of species")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1))
  #facet_grid( ~ Classification,scales = "free")

ggsave("iucn_species_classification_barplot.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")

ggplot()+
  geom_col(data = iucn_species_classification, aes(x=Classification, y= number_of_species, fill=Categories),show.legend = T, position = position_dodge())+
  geom_text(data = iucn_species_classification,aes(x=Classification, y=number_of_species, label= number_of_species,group= Categories), position = position_dodge(width = 0.8), vjust=-0.25,size=2.8)+
  scale_y_continuous(breaks = seq(0,720,100),limits = c(0,720))+
  labs(x="IUCN status", y= "Number of species")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1))
  #facet_wrap( ~ Classification,scales = "free")

ggsave("iucn_species_classification_barplot_categories.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")

ggplot()+
  geom_col(data = iucn_species_classification,aes(x=Categories, y= number_of_species ))+
  geom_text(data = iucn_species_classification,aes(x =Categories,y= number_of_species, label=number_of_species), position=position_dodge(width=0.7), vjust=-0.25,size=2.8)+
  scale_y_continuous(breaks = seq(0,275,25),limits = c(0,275))+
  labs(x="IUCN status", y= "Number of species")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1,size=8))+
  facet_grid(.~ Classification,scales = "free")

ggsave("iucn_species_classification_barplot_grid.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")


## Greek red data book
greek_red_data_species <- species %>% group_by(Greek_Red_Data_Book) %>% summarise(number_of_species=n()) %>% mutate(frequency=round(number_of_species/sum(number_of_species),digits = 3)) %>% mutate(Red_List="Greece's Red Data Book") %>% dplyr::rename(., Categories=Greek_Red_Data_Book)

kable(greek_red_data_species)

ggplot()+
  geom_col(data = greek_red_data_species, aes(x=Categories, y= number_of_species, fill=Categories),show.legend = F)+
  geom_text(data = greek_red_data_species,aes(x =Categories,y= number_of_species, label=number_of_species), position=position_dodge(width=0.7), vjust=-0.25,size=2.8)+
  scale_y_continuous(breaks = seq(0,800,100),limits = c(0,800))+
  labs(x="Greek red data list", y= "Number of species")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("greek_red_data_species.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")

ggplot()+
  geom_col(data = greek_red_data_species, aes(x=Categories, y= frequency, fill=Categories),show.legend = F)+
  geom_text(data = greek_red_data_species,aes(x =Categories,y= frequency, label=frequency), position=position_dodge(width=0.7), vjust=-0.25,size=2.8)+
  scale_y_continuous(breaks = seq(0,1,0.1),limits = c(0,1))+
  labs(x="Greek red data list", y= "Frequency")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("greek_red_data_species_frequency.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")

ggplot()+
  geom_col(data = greek_red_data_species, aes(x="", y= frequency, fill=Categories),width = 1,show.legend = T)+
  labs(x = "", y = "")+ 
  #geom_text(aes(x = 3.5, y = ((ymin+ymax)/2), label = Categories)) +
  #scale_y_continuous(breaks = 0:10)+  #labs(x="Greek red data list", y= "Frequency")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank(),panel.border = element_blank(),axis.ticks=element_blank())+
coord_polar(theta = "y")

ggsave("greek_red_data_species_pie.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")



## Both Red Lists

red_lists_species <- rbind(greek_red_data_species,iucn_species)

red_lists_species$Categories <- factor(red_lists_species$Categories,levels = c("NE - Not Evaluated","DD - Data Deficient","LC - Least Concern","NT - Near Threatened","VU - Vulnerable","EN - Endangered","CR - Critically Endangered"))

red_lists_data_species_plot <- ggplot()+
  geom_col(data = red_lists_species, aes(x=Categories, y= number_of_species, fill=Red_List,width=0.9), position = position_dodge(width = 0.9),show.legend = T)+
  geom_text(data = red_lists_species,aes(x =Categories,y= number_of_species, label=number_of_species,group=Red_List), position=position_dodge(width=0.9), vjust=-0.25,size=6)+
  scale_y_continuous(breaks = seq(0,800,50),limits = c(0,800),expand = c(0.01,0.4))+
  scale_fill_manual(values = c("lightpink1","firebrick1"),name="")+
  labs(x="Categories", y= "Number of species")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),legend.text = element_text(size = 18),axis.text.y=element_text(margin = margin(t = 0, r = 0, b = 0, l = 10,unit = "pt"),size = 18),axis.text.x = element_text(angle = 45, hjust = 1,size = 18),axis.title = element_text(size=22),panel.border = element_blank(),axis.line.x = element_line(colour = 'black', size = 0.3), axis.line.y = element_line(colour = 'black', size = 0.3),legend.position = c(0.83,0.89), legend.key.size = unit(1.5, "cm"))

ggsave("red_lists_data_species.png", plot = red_lists_data_species_plot, device = "png",width = 20,height = 11.25,units = "in", dpi = 100,path = "Website_plots/")


# Protection status caves

caves_protection <- strsplit(x = caves$Protection_Status,split = "|",fixed=TRUE)

caves_protection_data <- data_frame(Caves_Protection=unlist(caves_protection),CaveName=rep.int(caves$Cave_Name,times = sapply(caves_protection,length)),Cave_ID=rep.int(caves$Cave_ID,times = sapply(caves_protection,length)),Region=rep.int(caves$Region,times = sapply(caves_protection,length)),Altitude=rep.int(caves$Altitude,times = sapply(caves_protection,length))) %>% 
mutate(Protection_Type_ab=substr(x = Caves_Protection,start = 1,stop = 1)) %>% group_by(Protection_Type_ab) %>% left_join(.,data_frame(ab=c("G","K",NA,"H","A","L"),Protection_Type=c("Natura2000","Wildlife Refuge","Not Protected","Historical Monument","Archaeological Site","Landscape of Outstanding Natural Beauty")),by=c("Protection_Type_ab"="ab")) %>% dplyr::select(-Protection_Type_ab)

caves_protection_data_summary_type <- caves_protection_data %>% group_by(Protection_Type) %>% summarise(number_of_caves=n()) %>% mutate(frequency=round(number_of_caves/sum(number_of_caves),digits = 3))

# Define the order of the columns but simultaniously wrap the text of the labels of the columns by replacing space with \n

caves_protection_data_summary_type$Protection_Type_l <- factor(gsub(pattern = " ",replacement = "\n",x = caves_protection_data_summary_type$Protection_Type),levels = c("Archaeological\nSite","Historical\nMonument","Landscape\nof\nOutstanding\nNatural\nBeauty","Natura2000","Wildlife\nRefuge","Not\nProtected"))

ggplot()+
  geom_col(data = caves_protection_data_summary_type, aes(x=Protection_Type_l, y= number_of_caves, fill=Protection_Type_l),width=0.8,show.legend = F)+
  geom_text(data = caves_protection_data_summary_type,aes(x =Protection_Type_l,y= number_of_caves, label=number_of_caves), position=position_dodge(width=0.7), vjust=-0.25,size=5)+
  scale_y_continuous(breaks = seq(0,350,50),limits = c(0,350))+
  #scale_x_discrete(labels=Protection_Type_label)+
  labs(x="Protection", y= "Number of caves")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),legend.text = element_text(size = 18),axis.text.y=element_text(margin = margin(t = 0, r = 0, b = 0, l = 10,unit = "pt"),size = 18),axis.text.x = element_text(size = 18,angle = 45, hjust = 1),axis.title = element_text(size=22),panel.border = element_blank(),axis.line.x = element_line(colour = 'black', size = 0.3), axis.line.y = element_line(colour = 'black', size = 0.3),legend.position = c(0.132,0.93), legend.key.size = unit(1.5, "cm"))

ggsave("caves_protection_data_type.png", plot = last_plot(), device = "png",width = 20,height = 11.25,units = "in", dpi = 100,path = "Website_plots/")



ggplot()+
  geom_col(data = caves_protection_data_summary_type, aes(x=Protection_Type, y= frequency, fill=Protection_Type),show.legend = F)+
  geom_text(data = caves_protection_data_summary_type,aes(x =Protection_Type,y= frequency, label=frequency), position=position_dodge(width=0.7), vjust=-0.25,size=2.8)+
  scale_y_continuous(breaks = seq(0,1,0.1),limits = c(0,1))+
  labs(x="Protection", y= "Frequency of caves")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("caves_protection_data_type_frequency.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")

# Protection status species
species_protection <- strsplit(x = species$Protection_Status,split = "|",fixed=TRUE)

species_protection_data <- data_frame(Species_Protection=unlist(species_protection),Species=rep.int(species$Species_Full_Name,times = sapply(species_protection,length)),Class=rep.int(species$Class,times = sapply(species_protection,length)),Classification=rep.int(species$Classification,times = sapply(species_protection,length)))

species_protection_data_summary <- species_protection_data %>% group_by(Species_Protection) %>% summarise(number_of_species=n())

# ggplot()+
#   geom_col(data = species_protection_data_summary, aes(x=Species_Protection, y= number_of_species, fill=Species_Protection),show.legend = F)+
#   geom_text(data = species_protection_data_summary,aes(x =Species_Protection,y= number_of_species, label=number_of_species), position=position_dodge(width=0.7), vjust=-0.25,size=2.8)+
#   scale_y_continuous(breaks = seq(0,780,50),limits = c(0,780))+
#   #ggtitle("Inferring methods of the Sign Score of the PPI network of Drosophila gene")+
#   labs(x="Protection", y= "Number of species")+
#   theme_bw()+
#   theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1))
# 
# ggsave("species_protection_data_summary.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")
# 

kable(x = species_protection_data_summary)


species_protection_data_classification <- species_protection_data %>% mutate(Protection_Status=if_else(is.na(Species_Protection)==TRUE,"Not protected","Protected")) %>% distinct(Classification,Protection_Status,Species) %>% group_by(Classification,Protection_Status) %>% summarise(number_of_species=n()) %>% ungroup() %>% spread(key = Protection_Status,value = number_of_species,fill=0) %>% gather(key =Protection_Status,value =number_of_species,  -Classification)

species_protection_data_classification$Classification <- factor(species_protection_data_classification$Classification,levels = c("Accidental","Trogloxene","Stygoxene","Stygophile","Troglophile","Stygobiont","Troglobiont"))

species_protection_data_classification_plot <- ggplot()+
  geom_col(data = species_protection_data_classification, aes(x=Classification, y= number_of_species, fill=Protection_Status,group=Protection_Status),position="dodge",show.legend = T)+
  geom_text(data = species_protection_data_classification,aes(x =Classification,y= number_of_species, label=number_of_species,group=Protection_Status), position=position_dodge(width=0.93), vjust=-0.25,size=6)+
  scale_y_continuous(breaks = seq(0,350,25),limits = c(0,350),expand = c(0.01,0.4))+
  scale_fill_manual(values = c("lightcoral","lightgreen"),name="")+
  labs(x="Classification", y= "Number of species")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),legend.text = element_text(size = 18),axis.text.y=element_text(margin = margin(t = 0, r = 0, b = 0, l = 10,unit = "pt"),size = 18),axis.text.x = element_text(margin = margin(t = 0, r = 0, b = 10, l = 0,unit = "pt"),angle = 45, hjust = 1,size = 18),axis.title = element_text(size=22),panel.border = element_blank(),axis.line.x = element_line(colour = 'black', size = 0.3), axis.line.y = element_line(colour = 'black', size = 0.3),legend.position = c(0.09,0.90), legend.key.size = unit(1.5, "cm"))

ggsave("species_protection_data_classification.png", plot = species_protection_data_classification_plot, device = "png",width = 20,height = 11.25,units = "in", dpi = 100,path = "Website_plots/")


species_protection_data_classification %>% spread(key = Protection_Status,value = number_of_species,fill=0) %>% kable()

# Species per cave

census_caves_per_species <- census_all_species %>% filter(species_epithet!="sp.") %>% distinct(Species,Cave_ID, Cave_Name,Classification) %>% group_by(Species) %>% summarise(number_of_caves=n()) %>% arrange(desc(number_of_caves))


census_species_per_cave <- census_all_species %>% filter(species_epithet!="sp.") %>% distinct(Species,Cave_ID, Cave_Name,Classification) %>% group_by(Cave_ID,Cave_Name) %>% summarise(number_of_species=n()) %>% arrange(desc(number_of_species))

census_species_per_cave_dist <- census_species_per_cave %>% group_by(number_of_species) %>% summarise(number_of_caves=n())

ggplot(data=census_species_per_cave_dist)+
  #=geom_line(aes(x=number_of_caves, y= number_of_species),color="dodgerblue2",show.legend = F)+
  geom_point(aes(x=number_of_caves, y= number_of_species),color="dodgerblue2",show.legend = F, size=1)+
  #ggtitle("Species")+
  scale_y_continuous(breaks = seq(0,75,5),limits = c(0,75))+
  scale_x_continuous(breaks = seq(0,200,25), limits = c(0,200))+
  labs(x="Number of caves", y= "Number of species")+
  theme_bw()+theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())
  
ggsave("census_species_per_cave_dist.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")
 


kable(x = head(census_species_per_cave,20))

## Endemic species per region

species_region_endemic <- census_all_species_all_caves %>% filter(species_epithet!="sp.") %>% filter(Distribution=="Endemic to Greece") %>% distinct( Species,Region) %>% group_by(Species) %>% summarize(number_of_regions=n()) %>% group_by(number_of_regions) %>% summarise(number_of_species=n())

ggplot(data=species_region_endemic)+
  geom_line(aes(x=number_of_regions, y= number_of_species),color="dodgerblue2",show.legend = F)+
  geom_point(aes(x=number_of_regions, y= number_of_species),color="dodgerblue2",show.legend = F, size=1)+
  ggtitle("Endemic to Greece species appearance in regions")+
  scale_y_continuous(breaks = seq(0,375,25),limits = c(0,375))+
  scale_x_continuous(breaks = seq(0,8,1), limits = c(0,8))+
  labs(x="Number of regions", y= "Number of endemic to Greece species")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),plot.title = element_text(hjust = 0.5, size = 11))

ggsave("dist_species_region_endemic.jpeg", plot = last_plot(), device = "jpeg",width = 13,height = 18,units = "cm", dpi = 300,path = "Plots/")


## Ecological classification

#census_species_per_cave_classification <- census_all_species %>% filter(species_epithet!="sp.") %>% distinct(Species,Cave_ID, Cave_Name,Classification) %>% group_by(Cave_ID,Cave_Name,Classification) %>% summarise(number_of_species=n()) %>% arrange(desc(number_of_species)) %>% na.omit() %>% spread(Classification,number_of_species,fill = 0) %>% ungroup() %>% mutate(Troglobiont_species=rowSums(.[9:10]))

census_species_per_cave_classification <- census_all_species %>% filter(species_epithet!="sp.") %>% distinct(Species,Cave_ID, Cave_Name,Classification) %>% group_by(Cave_ID,Cave_Name,Classification) %>% summarise(number_of_species=n()) %>% arrange(desc(number_of_species)) %>% na.omit()

census_species_per_cave_classification_trogl <- census_species_per_cave_classification %>% filter(Classification=="Troglobiont")%>% distinct(Cave_ID, Cave_Name, Classification,number_of_species) %>% arrange(desc(Classification))

kable(x = head(census_species_per_cave_classification_trogl,20))


# Taxon Occurencies Distribution
# Species, Genera, Families abudance in caves of Greece

# Species occurencies distributions
species_occurencies_caves <- census_all_species %>% group_by(Species) %>% filter(species_epithet!="sp.") %>% summarise(taxon_occurences=n()) %>% group_by(taxon_occurences) %>% summarise(number_of_taxon=n()) %>% mutate(taxon="Species")

dist_species_occurencies_caves <- ggplot(data=species_occurencies_caves)+
  geom_line(aes(x=taxon_occurences, y= number_of_taxon),color="dodgerblue2",show.legend = F)+
  geom_point(aes(x=taxon_occurences, y= number_of_taxon),color="dodgerblue2",show.legend = F, size=1)+
  ggtitle("Species")+
  scale_y_continuous(breaks = seq(0,600,100),limits = c(0,600))+
  scale_x_continuous(breaks = seq(0,90,10), limits = c(0,90))+
  #labs(x="Number of species", y= "Number of taxa")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),plot.title = element_text(hjust = 0.5, size = 11))

# Genera occurencies distributions
genera_occurencies_caves <- census_all_species %>% group_by(Genus) %>% summarise(taxon_occurences=n()) %>% group_by(taxon_occurences) %>% summarise(number_of_taxon=n()) %>% mutate(taxon="Genus")

dist_genera_occurencies_caves <-  ggplot(data=genera_occurencies_caves)+
  geom_line(aes(x=taxon_occurences, y= number_of_taxon),color="orange",show.legend = F)+
  geom_point(aes(x=taxon_occurences, y= number_of_taxon),color="orange",show.legend = F, size=1)+
  ggtitle("Genus")+
  scale_y_continuous(breaks = seq(0,210,50),limits = c(0,210))+
  scale_x_continuous(breaks = seq(0,210,50), limits = c(0,210))+
  #labs(x="Number of species", y= "Number of taxa")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),plot.title = element_text(hjust = 0.5, size = 11))

# Family occurencies distribution

family_occurencies_caves <- census_all_species %>% group_by(Family) %>% summarise(taxon_occurences=n()) %>% group_by(taxon_occurences) %>% summarise(number_of_taxon=n()) %>% mutate(taxon="Family")

dist_family_occurencies_caves <- ggplot(data=family_occurencies_caves)+
  geom_line(aes(x=taxon_occurences, y= number_of_taxon),color="turquoise",show.legend = F)+
  geom_point(aes(x=taxon_occurences, y= number_of_taxon),color="turquoise",show.legend = F, size=1)+
  ggtitle("Family")+
  scale_x_continuous(breaks = seq(0,250,50),limits = c(0,250))+
  scale_y_continuous(breaks = seq(0,90,20), limits = c(0,90))+
  #labs(x="Number of species", y= "Number of taxa")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),plot.title = element_text(hjust = 0.5, size = 11))

# Order occurencies distributions

order_occurencies_caves <- census_all_species %>% group_by(Order) %>% summarise(taxon_occurences=n()) %>% group_by(taxon_occurences) %>% summarise(number_of_taxon=n()) %>% mutate(taxon="Order")

dist_order_occurencies_caves <- ggplot(data=order_occurencies_caves)+
  geom_line(aes(x=taxon_occurences, y= number_of_taxon),color="purple",show.legend = F)+
  geom_point(aes(x=taxon_occurences, y= number_of_taxon),color="purple",show.legend = F, size=1)+
  ggtitle("Order")+
  scale_x_continuous(breaks = seq(0,600,100),limits = c(0,600))+
  scale_y_continuous(breaks = seq(0,20,5), limits = c(0,20))+
  #labs(x="Number of species", y= "Number of taxa")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),plot.title = element_text(hjust = 0.5, size = 11))


## Class occurencies
class_occurencies_caves <- census_all_species %>% group_by(Class) %>% summarise(taxon_occurences=n()) %>% group_by(taxon_occurences) %>% summarise(number_of_taxon=n()) %>% mutate(taxon="Class")

dist_class_occurencies_caves <- ggplot(data=class_occurencies_caves)+
  geom_line(aes(x=taxon_occurences, y= number_of_taxon),color="red",show.legend = F)+
  geom_point(aes(x=taxon_occurences, y= number_of_taxon),color="red",show.legend = F, size=1)+
  ggtitle("Class")+
  scale_y_continuous(breaks = seq(0,5,1),limits = c(0,4))+
  scale_x_continuous(breaks = seq(0,900,150), limits = c(0,900))+
  labs(x="Number of species", y= "Number of taxa")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),plot.title = element_text(hjust = 0.5, size = 11))

## Merge distributions

occurences_taxa <- do.call("rbind",list(species_occurencies_caves,genera_occurencies_caves,family_occurencies_caves,order_occurencies_caves,class_occurencies_caves))


 ### Arrange plots
distributions_occurences_taxa <- grid.arrange(dist_species_occurencies_caves,arrangeGrob(dist_genera_occurencies_caves,dist_family_occurencies_caves,dist_order_occurencies_caves,dist_class_occurencies_caves,ncol=2),nrow = 2,bottom=textGrob("Number of occurencies in caves", gp=gpar(fontface="plain", col="black", fontsize=11)),left=textGrob("Number of taxa", gp=gpar(fontface="plain", col="black",fontsize=11), rot=90), heights=c(0.35,0.65))


ggsave("distributions_distributions_occurences_taxa.jpeg", plot = distributions_occurences_taxa, device = "jpeg",width = 13,height = 18,units = "cm", dpi = 300,path = "Plots/")


## Without chiroptera

species_occurencies_no_chiroptera <- census_all_species %>% filter(Order!="Chiroptera",species_epithet!="sp.")

# Species occurencies distributions
species_occurencies_caves <- species_occurencies_no_chiroptera %>% group_by(Species) %>% summarise(taxon_occurences=n()) %>% group_by(taxon_occurences) %>% summarise(number_of_taxon=n()) %>% mutate(taxon="Species")

dist_species_occurencies_caves <- ggplot(data=species_occurencies_caves)+
  geom_line(aes(x=taxon_occurences, y= number_of_taxon),color="dodgerblue2",show.legend = F)+
  geom_point(aes(x=taxon_occurences, y= number_of_taxon),color="dodgerblue2",show.legend = F, size=1)+
  ggtitle("Species")+
  scale_y_continuous(breaks = seq(0,600,100),limits = c(0,600))+
  scale_x_continuous(breaks = seq(0,50,5), limits = c(0,50))+
  #labs(x="Number of species", y= "Number of taxa")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),plot.title = element_text(hjust = 0.5, size = 11))

# Genera occurencies distributions
genera_occurencies_caves <- species_occurencies_no_chiroptera %>% group_by(Genus) %>% summarise(taxon_occurences=n()) %>% group_by(taxon_occurences) %>% summarise(number_of_taxon=n()) %>% mutate(taxon="Genus")

dist_genera_occurencies_caves <-  ggplot(data=genera_occurencies_caves)+
  geom_line(aes(x=taxon_occurences, y= number_of_taxon),color="orange",show.legend = F)+
  geom_point(aes(x=taxon_occurences, y= number_of_taxon),color="orange",show.legend = F, size=1)+
  ggtitle("Genus")+
  scale_y_continuous(breaks = seq(0,210,50),limits = c(0,210))+
  scale_x_continuous(breaks = seq(0,210,25), limits = c(0,150))+
  #labs(x="Number of species", y= "Number of taxa")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),plot.title = element_text(hjust = 0.5, size = 11))

# Family occurencies distribution

family_occurencies_caves <- species_occurencies_no_chiroptera %>% group_by(Family) %>% summarise(taxon_occurences=n()) %>% group_by(taxon_occurences) %>% summarise(number_of_taxon=n()) %>% mutate(taxon="Family")

dist_family_occurencies_caves <- ggplot(data=family_occurencies_caves)+
  geom_line(aes(x=taxon_occurences, y= number_of_taxon),color="turquoise",show.legend = F)+
  geom_point(aes(x=taxon_occurences, y= number_of_taxon),color="turquoise",show.legend = F, size=1)+
  ggtitle("Family")+
  scale_x_continuous(breaks = seq(0,150,25),limits = c(0,150))+
  scale_y_continuous(breaks = seq(0,90,20), limits = c(0,90))+
  #labs(x="Number of species", y= "Number of taxa")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),plot.title = element_text(hjust = 0.5, size = 11))

# Order occurencies distributions

order_occurencies_caves <- species_occurencies_no_chiroptera %>% group_by(Order) %>% summarise(taxon_occurences=n()) %>% group_by(taxon_occurences) %>% summarise(number_of_taxon=n()) %>% mutate(taxon="Order")

dist_order_occurencies_caves <- ggplot(data=order_occurencies_caves)+
  geom_line(aes(x=taxon_occurences, y= number_of_taxon),color="purple",show.legend = F)+
  geom_point(aes(x=taxon_occurences, y= number_of_taxon),color="purple",show.legend = F, size=1)+
  ggtitle("Order")+
  scale_x_continuous(breaks = seq(0,600,100),limits = c(0,600))+
  scale_y_continuous(breaks = seq(0,20,5), limits = c(0,20))+
  #labs(x="Number of species", y= "Number of taxa")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),plot.title = element_text(hjust = 0.5, size = 11))


## Class occurencies
class_occurencies_caves <- species_occurencies_no_chiroptera %>% group_by(Class) %>% summarise(taxon_occurences=n()) %>% group_by(taxon_occurences) %>% summarise(number_of_taxon=n()) %>% mutate(taxon="Class")

dist_class_occurencies_caves <- ggplot(data=class_occurencies_caves)+
  geom_line(aes(x=taxon_occurences, y= number_of_taxon),color="red",show.legend = F)+
  geom_point(aes(x=taxon_occurences, y= number_of_taxon),color="red",show.legend = F, size=1)+
  ggtitle("Class")+
  scale_y_continuous(breaks = seq(0,5,1),limits = c(0,4))+
  scale_x_continuous(breaks = seq(0,900,150), limits = c(0,900))+
  labs(x="Number of species", y= "Number of taxa")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),plot.title = element_text(hjust = 0.5, size = 11))

## Merge distributions

occurences_taxa <- do.call("rbind",list(species_occurencies_caves,genera_occurencies_caves,family_occurencies_caves,order_occurencies_caves,class_occurencies_caves))


 ### Arrange plots
distributions_occurences_taxa <- grid.arrange(dist_species_occurencies_caves,arrangeGrob(dist_genera_occurencies_caves,dist_family_occurencies_caves,dist_order_occurencies_caves,dist_class_occurencies_caves,ncol=2),nrow = 2,bottom=textGrob("Number of occurencies in caves", gp=gpar(fontface="plain", col="black", fontsize=11)),left=textGrob("Number of taxa (no chiroptera)", gp=gpar(fontface="plain", col="black",fontsize=11), rot=90), heights=c(0.35,0.65))


ggsave("distributions_distributions_occurences_taxa_no_chiroptera.jpeg", plot = distributions_occurences_taxa, device = "jpeg",width = 13,height = 18,units = "cm", dpi = 300,path = "Plots/")


## Only chiroptera

species_occurencies_only_chiroptera <- census_all_species %>% filter(Order=="Chiroptera", species_epithet!="sp.")
species_occurencies_only_chiroptera2 <- species_occurencies_only_chiroptera %>% dplyr::select(Order,Family,Genus,Species) %>% distinct()

# Species occurencies distributions
species_occurencies_caves <- species_occurencies_only_chiroptera %>% group_by(Species) %>% summarise(taxon_occurences=n()) %>% group_by(taxon_occurences) %>% summarise(number_of_taxon=n()) %>% mutate(taxon="Species")

dist_species_occurencies_caves <- ggplot(data=species_occurencies_caves)+
  geom_line(aes(x=taxon_occurences, y= number_of_taxon),color="dodgerblue2",show.legend = F)+
  geom_point(aes(x=taxon_occurences, y= number_of_taxon),color="dodgerblue2",show.legend = F, size=1)+
  ggtitle("Species")+
  #scale_y_continuous(breaks = seq(0,600,100),limits = c(0,600))+
  scale_x_continuous(breaks = seq(0,50,5), limits = c(0,50))+
  #labs(x="Number of species", y= "Number of taxa")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),plot.title = element_text(hjust = 0.5, size = 11))

# Genera occurencies distributions
genera_occurencies_caves <- species_occurencies_only_chiroptera %>% group_by(Genus) %>% summarise(taxon_occurences=n()) %>% group_by(taxon_occurences) %>% summarise(number_of_taxon=n()) %>% mutate(taxon="Genus")

dist_genera_occurencies_caves <-  ggplot(data=genera_occurencies_caves)+
  geom_line(aes(x=taxon_occurences, y= number_of_taxon),color="orange",show.legend = F)+
  geom_point(aes(x=taxon_occurences, y= number_of_taxon),color="orange",show.legend = F, size=1)+
  ggtitle("Genus")+
  scale_y_continuous(breaks = seq(0,3,1),limits = c(0,3))+
  scale_x_continuous(breaks = seq(0,210,25), limits = c(0,150))+
  #labs(x="Number of species", y= "Number of taxa")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),plot.title = element_text(hjust = 0.5, size = 11))

# Family occurencies distribution

family_occurencies_caves <- species_occurencies_only_chiroptera %>% group_by(Family) %>% summarise(taxon_occurences=n()) %>% group_by(taxon_occurences) %>% summarise(number_of_taxon=n()) %>% mutate(taxon="Family")

# dist_family_occurencies_caves <- ggplot(data=family_occurencies_caves)+
#   geom_line(aes(x=taxon_occurences, y= number_of_taxon),color="turquoise",show.legend = F)+
#   geom_point(aes(x=taxon_occurences, y= number_of_taxon),color="turquoise",show.legend = F, size=1)+
#   ggtitle("Family")+
#   scale_x_continuous(breaks = seq(0,150,25),limits = c(0,150))+
#   scale_y_continuous(breaks = seq(0,90,20), limits = c(0,90))+
#   #labs(x="Number of species", y= "Number of taxa")+
#   theme_bw()+
#   theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),plot.title = element_text(hjust = 0.5, size = 11))

# Order occurencies distributions

order_occurencies_caves <- species_occurencies_only_chiroptera %>% group_by(Order) %>% summarise(taxon_occurences=n()) %>% group_by(taxon_occurences) %>% summarise(number_of_taxon=n()) %>% mutate(taxon="Order")

# dist_order_occurencies_caves <- ggplot(data=order_occurencies_caves)+
#   geom_line(aes(x=taxon_occurences, y= number_of_taxon),color="purple",show.legend = F)+
#   geom_point(aes(x=taxon_occurences, y= number_of_taxon),color="purple",show.legend = F, size=1)+
#   ggtitle("Order")+
#   scale_x_continuous(breaks = seq(0,600,100),limits = c(0,600))+
#   scale_y_continuous(breaks = seq(0,20,5), limits = c(0,20))+
#   #labs(x="Number of species", y= "Number of taxa")+
#   theme_bw()+
#   theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),plot.title = element_text(hjust = 0.5, size = 11))


## Class occurencies
class_occurencies_caves <- species_occurencies_only_chiroptera %>% group_by(Class) %>% summarise(taxon_occurences=n()) %>% group_by(taxon_occurences) %>% summarise(number_of_taxon=n()) %>% mutate(taxon="Class")

# dist_class_occurencies_caves <- ggplot(data=class_occurencies_caves)+
#   geom_line(aes(x=taxon_occurences, y= number_of_taxon),color="red",show.legend = F)+
#   geom_point(aes(x=taxon_occurences, y= number_of_taxon),color="red",show.legend = F, size=1)+
#   ggtitle("Class")+
#   scale_y_continuous(breaks = seq(0,5,1),limits = c(0,4))+
#   scale_x_continuous(breaks = seq(0,900,150), limits = c(0,900))+
#   labs(x="Number of species", y= "Number of taxa")+
#   theme_bw()+
#   theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),plot.title = element_text(hjust = 0.5, size = 11))

## Merge distributions

occurences_taxa <- do.call("rbind",list(species_occurencies_caves,genera_occurencies_caves,family_occurencies_caves,order_occurencies_caves,class_occurencies_caves))


 ### Arrange plots
distributions_occurences_taxa <- grid.arrange(dist_species_occurencies_caves,dist_genera_occurencies_caves,nrow = 2,bottom=textGrob("Number of occurencies in caves", gp=gpar(fontface="plain", col="black", fontsize=11)),left=textGrob("Number of taxa (only chiroptera)", gp=gpar(fontface="plain", col="black",fontsize=11), rot=90), heights=c(0.35,0.65))


ggsave("distributions_distributions_occurences_taxa_only_chiroptera.jpeg", plot = distributions_occurences_taxa, device = "jpeg",width = 13,height = 18,units = "cm", dpi = 300,path = "Plots/")


## Taxon - Subtaxon Distribution
# Genus
species_occurencies_unique_species_genus_dist <- species %>% dplyr::select(Species_Full_Name,Genus) %>% distinct() %>% group_by(Genus) %>% summarise(number_of_species=n()) %>% group_by(number_of_species) %>% summarise(number_of_taxon=n()) %>% mutate(taxon="Genus")

# Family
species_occurencies_unique_species_Family_dist <- species %>% dplyr::select(Species_Full_Name,Family) %>% distinct() %>% group_by(Family) %>% summarise(number_of_species=n()) %>% group_by(number_of_species) %>% summarise(number_of_taxon=n()) %>% mutate(taxon="Family")

# Order
species_occurencies_unique_species_Order_dist <- species %>% dplyr::select(Species_Full_Name,Order) %>% distinct() %>% group_by(Order) %>% summarise(number_of_species=n()) %>% group_by(number_of_species) %>% summarise(number_of_taxon=n()) %>% mutate(taxon="Order")

# Class
species_occurencies_unique_species_Class_dist <- species %>% dplyr::select(Species_Full_Name,Class) %>% distinct() %>% group_by(Class) %>% summarise(number_of_species=n()) %>% group_by(number_of_species) %>% summarise(number_of_taxon=n()) %>% mutate(taxon="Class")

species_taxon_distributions <- do.call("rbind", list(species_occurencies_unique_species_genus_dist, species_occurencies_unique_species_Family_dist, species_occurencies_unique_species_Order_dist,species_occurencies_unique_species_Class_dist))


ggplot()+
  geom_point(data = species_taxon_distributions, aes(x=number_of_species, y= number_of_taxon, color=taxon),show.legend = T)+
  #ggtitle("Inferring methods of the Sign Score of the PPI network of Drosophila gene")+
  scale_y_continuous(breaks = seq(0,320,20),limits = c(0,320))+
  scale_x_continuous(breaks = seq(0,280,20),limits = c(0,280))+
  labs(x="Number of species", y= "Number of taxa")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())
ggsave("species_taxon_distributions_plot.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")


dist_class <- species_taxon_distributions %>% filter(taxon=="Class") %>% 
  ggplot(data=.)+
  geom_line(aes(x=number_of_species, y= number_of_taxon),color="red",show.legend = F)+
  geom_point(aes(x=number_of_species, y= number_of_taxon),color="red",show.legend = F, size=1)+
  ggtitle("Class")+
  scale_y_continuous(breaks = seq(0,5,1),limits = c(0,4))+
  scale_x_continuous(breaks = seq(0,300,50), limits = c(0,300))+
  labs(x="Number of species", y= "Number of taxa")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),plot.title = element_text(hjust = 0.5, size = 11))

dist_order <- species_taxon_distributions %>% filter(taxon=="Order") %>% 
  ggplot(data=.)+
  geom_line(aes(x=number_of_species, y= number_of_taxon),color="purple",show.legend = F)+
  geom_point(aes(x=number_of_species, y= number_of_taxon),color="purple",show.legend = F, size=1)+
  ggtitle("Order")+
  scale_y_continuous(breaks = seq(0,20,5),limits = c(0,20))+
  scale_x_continuous(breaks = seq(0,180,20), limits = c(0,180))+
  #labs(x="Number of species", y= "Number of taxa")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),plot.title = element_text(hjust = 0.5, size = 11))

dist_family <- species_taxon_distributions %>% filter(taxon=="Family") %>% 
  ggplot(data=.)+
  geom_line(aes(x=number_of_species, y= number_of_taxon),color="turquoise",show.legend = F)+
  geom_point(aes(x=number_of_species, y= number_of_taxon),color="turquoise",show.legend = F, size=1)+
  ggtitle("Family")+
  scale_y_continuous(breaks = seq(0,120,20),limits = c(0,120))+
  scale_x_continuous(breaks = seq(0,80,10), limits = c(0,70))+
  #labs(x="Number of species", y= "Number of taxa")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),plot.title = element_text(hjust = 0.5, size = 11))

dist_genus <- species_taxon_distributions %>% filter(taxon=="Genus") %>% 
  ggplot(data=.)+
  geom_line(aes(x=number_of_species, y= number_of_taxon),color="orange",show.legend = F)+
  geom_point(aes(x=number_of_species, y= number_of_taxon),color="orange",show.legend = F, size=1)+
  ggtitle("Genus")+
  scale_y_continuous(breaks = seq(0,300,50),limits = c(0,300))+
  scale_x_continuous(breaks = seq(0,30,5), limits = c(0,30))+
  #labs(x="Number of species", y= "Number of taxa")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),plot.title = element_text(hjust = 0.5, size = 11))

distributions_species_taxon <- grid.arrange(arrangeGrob(grobs = list(dist_class,dist_order,dist_family,dist_genus)), ncol=2,bottom=textGrob("Number of species", gp=gpar(fontface="plain", col="black", fontsize=11)), left=textGrob("Number of taxa", gp=gpar(fontface="plain", col="black",fontsize=11), rot=90),widths=c(9,0.5))


ggsave("distributions_species_taxon.jpeg", plot = distributions_species_taxon, device = "jpeg", dpi = 300,path = "Plots/")



#determine order of appearance in facet_wrap
## species_taxon_distributions$taxon_order <- factor(species_taxon_distributions$taxon, levels = c("Class","Order","Family","Genus"))
## 
## ggplot()+
##   geom_line(data = species_taxon_distributions, aes(x=number_of_species, y= number_of_taxon,colour = factor(taxon)),show.legend = F)+
##   geom_point(data = species_taxon_distributions, aes(x=number_of_species, y= number_of_taxon,colour = factor(taxon)),show.legend = F, size=1)+
##   #scale_y_continuous(breaks = )+
##   #scale_x_continuous(breaks = seq(0,280,20))+
##   labs(x="Number of species", y= "Number of taxa")+
##   theme_bw()+
##   theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
##   facet_wrap( ~ taxon_order,scales = "free")
## 
## ggsave("species_taxon_distributions_plot_facet.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")

# Links to other databases
## Cave Fauna of Greece database includes links of species to GBIF, Fauna Europea, IUCN, PESI and NCBI Taxonomy.  

species_links <- species %>% dplyr::select(Link_IUCN,Link_GBIF,Link_Fauna_Europaea,Link_NCBI,Link_PESI)

species_links_summary <- data_frame(Source=c("Fauna Europaea","NCBI Taxonomy","GBIF","PESI","IUCN", "Total Species"), Number_of_links=c(sum(!is.na(species_links$Link_Fauna_Europaea)),sum(!is.na(species_links$Link_NCBI)),sum(!is.na(species_links$Link_GBIF)),sum(!is.na(species_links$Link_PESI)),sum(!is.na(species_links$Link_IUCN)), nrow(species_links)), Missing_links=c(nrow(species)-sum(!is.na(species_links$Link_Fauna_Europaea)),nrow(species)-sum(!is.na(species_links$Link_NCBI)),nrow(species)-sum(!is.na(species_links$Link_GBIF)),nrow(species)-sum(!is.na(species_links$Link_PESI)),nrow(species)-sum(!is.na(species_links$Link_IUCN)),nrow(species_links)-nrow(species_links))) %>% mutate(frequency=round(Number_of_links/nrow(species),digits = 3))


kable(species_links_summary)

# Bar plots of the links. 

ggplot()+
  geom_col(data = species_links_summary, aes(x=Source, y= Number_of_links, fill=Source),show.legend = F)+
  geom_text(data = species_links_summary,aes(x =Source,y= Number_of_links, label=Number_of_links), position=position_dodge(width=0.7), vjust=-0.25,size=2.8)+
  scale_y_continuous(breaks = seq(0,nrow(species),100),limits = c(0,nrow(species)))+
  labs(x="Database", y= "Number of species links")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("species_links_summary.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")

ggplot()+
  geom_col(data = species_links_summary, aes(x=Source, y= frequency, fill=Source),show.legend = F)+
  geom_text(data = species_links_summary,aes(x =Source,y= frequency, label=frequency), position=position_dodge(width=0.7), vjust=-0.25,size=2.8)+
  scale_y_continuous(breaks = seq(0,1,0.1),limits = c(0,1))+
  labs(x="Database", y= "Frequency of species links")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("species_links_summary_frequency.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")



PESI_vs_Fauna_E <- species %>% filter((is.na(Link_PESI) & !is.na(Link_Fauna_Europaea)) | (is.na(Link_Fauna_Europaea) & !is.na(Link_PESI))) %>% dplyr::select(Species_Full_Name,Link_Fauna_Europaea,Link_PESI)

# Census References

species_references_year_dist <- Census_references %>% group_by(Year) %>% summarise(publications_per_year=n())

ggplot()+
  geom_line(data=species_references_year_dist,aes(x=Year, y= publications_per_year),color="red",show.legend = F)+
  scale_x_continuous(breaks = seq(1860,2020,10),limits = c(1860,2020))+
  scale_y_continuous(breaks = seq(0,20,2), limits = c(0,20))+
  labs(x="Years",y="Number of new publications")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())

ggsave("species_references_year_dist.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")

species_references_year_dist$Cumulative_publications <- cumsum(species_references_year_dist$publications_per_year)

ggplot(data=species_references_year_dist)+
  geom_line(aes(x=Year, y= Cumulative_publications),color="mediumseagreen",show.legend = F)+
  #ggtitle("Class")+
  scale_x_continuous(breaks = seq(1860,2020,10),limits = c(1860,2020))+
  scale_y_continuous(breaks = seq(0,750,100), limits = c(0,750))+
  labs(x="Years",y="Cumulative publications")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())

ggsave("species_references_year_dist_cumulative.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")


# New species discovered in time. (references)

census_long_man_reference <- census_long_man %>% left_join(Census_references,by=c("ReferenceShort"="Short"))

census$species_epithet <- as.character(lapply(strsplit(as.character(census$Species), split=" "), "[", n=2))

species_references_spreaded_arranged <- census_long_man_reference %>% ungroup() %>% dplyr::select(Species, ReferenceShort, Year) %>% group_by(Species,ReferenceShort, Year) %>% distinct(.) %>% arrange(.,Year) %>% ungroup() %>% mutate(Duplicates=duplicated(Species), species_epithet= as.character(lapply(strsplit(as.character(Species), split=" "), "[", n=2))) %>% filter(species_epithet!="sp.") %>% mutate(.,First_occurance=if_else(Duplicates=="FALSE",1,0)) %>% na.omit() %>% mutate(Cumulative_occurance= cumsum(First_occurance)) %>% mutate(Classification="All species") %>% dplyr::select(-c(Species,species_epithet,First_occurance)) %>% distinct(.)

ggplot()+
  geom_line(data=species_references_spreaded_arranged,aes(x=Year, y= Cumulative_occurance),color="violetred1",show.legend = F)+
  #ggtitle("Class")+
  scale_x_continuous(breaks = seq(1860,2020,10),limits = c(1860,2020))+
  scale_y_continuous(breaks = seq(0,950,100), limits = c(0,950))+
  labs(x="Years",y="Cumulative species in Greek caves")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())

ggsave("species_occurrence_accumulation.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")


qq <- species %>% filter(Distribution=="Endemic to Greece")


endemic_cumulative_species <- census_long_man_reference %>% left_join(species,by=c("Species"="Species_Full_Name")) %>% dplyr::select(Species,Distribution, ReferenceShort, Year) %>% group_by(Species,Distribution,ReferenceShort, Year) %>% distinct(.) %>% arrange(.,Year) %>% ungroup() %>% mutate(Duplicates=duplicated(Species), species_epithet= as.character(lapply(strsplit(as.character(Species), split=" "), "[", n=2))) %>% filter(species_epithet!="sp.") %>% mutate(.,First_occurance=if_else(Duplicates=="FALSE",1,0)) %>% na.omit() %>% filter(First_occurance==1) %>% group_by(Year,Distribution) %>% summarise(Occurance_species_year= n()) %>% group_by(Distribution) %>% mutate(Cumulative_occurance= cumsum(Occurance_species_year)) %>% filter(Distribution=="Endemic to Greece") %>% mutate(Classification="Endemic species to Greece") %>% dplyr::select(-Occurance_species_year,-Distribution)

census_long_man_reference_all_species_classification <- census_long_man_reference %>% left_join(species,by=c("Species"="Species_Full_Name")) %>% dplyr::select(Species,Classification, ReferenceShort, Year) %>% group_by(Species,Classification,ReferenceShort, Year) %>% distinct(.) %>% arrange(.,Year) %>% ungroup() %>% mutate(Duplicates=duplicated(Species), species_epithet= as.character(lapply(strsplit(as.character(Species), split=" "), "[", n=2))) %>% filter(species_epithet!="sp.") %>% mutate(.,First_occurance=if_else(Duplicates=="FALSE",1,0)) %>% na.omit() %>% filter(First_occurance==1) %>% group_by(Year,Classification) %>% summarise(Occurance_species_year= n()) %>% group_by(Classification) %>% mutate(Cumulative_occurance= cumsum(Occurance_species_year)) %>% dplyr::select(-Occurance_species_year) %>% bind_rows(species_references_spreaded_arranged) %>% filter(Classification %in% c("Troglobiont","Troglophile","All species")) %>% bind_rows(endemic_cumulative_species)


species_occurrence_accumulation_classification_plot <- ggplot()+
  geom_line(data=census_long_man_reference_all_species_classification,aes(x=Year, y= Cumulative_occurance,color=Classification),size=1,show.legend = T)+
  #ggtitle("Class")+
  scale_x_continuous(breaks = seq(1860,2030,10),limits = c(1860,2030),expand=c(0.015,0))+
  scale_y_continuous(breaks = seq(0,950,100), limits = c(0,913),expand = c(0.01,0))+
  scale_color_manual(values =c("Endemic species to Greece"="lightcoral","Troglophile"="paleturquoise","Troglobiont"="darkolivegreen3","All species"="deepskyblue"))+
  labs(x="Years",y="Cumulative number of species")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),legend.text = element_text(size = 18),axis.text.y=element_text(margin = margin(t = 0, r = 5, b = 0, l = 15,unit = "pt"),size = 18),axis.text.x = element_text(margin = margin(t = 5, r = 0, b = 15, l = 0,unit = "pt"),size = 18),axis.title = element_text(size=22),panel.border = element_blank(),axis.line.x = element_line(colour = 'black', size = 0.3), axis.line.y = element_line(colour = 'black', size = 0.3),legend.position = c(0.13,0.87), legend.key.size = unit(1.5, "cm"), legend.title = element_blank())

ggsave("species_occurrence_accumulation_classification.png", plot = species_occurrence_accumulation_classification_plot, device = "png",width = 20,height = 11.25,units = "in", dpi = 100,path = "Website_plots/")


species_references_spreaded_arranged_article <- species_references_spreaded_arranged %>% filter(Duplicates=="FALSE") %>% group_by(ReferenceShort, Year) %>% summarise(n_new_species=n()) %>% group_by(n_new_species) %>% summarise(n_references=n())

ggplot()+
  geom_line(data=species_references_spreaded_arranged_article,aes(y=n_references, x= n_new_species),color="violetred1",show.legend = F)+
  #ggtitle("Class")+
  scale_x_continuous(breaks = seq(0,180,10),limits = c(0,100))+
  scale_y_continuous(breaks = seq(0,180,20),limits = c(0,180))+
  labs(x="Number of references",y="Number of new species additions in Greek caves")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())

ggsave("species_references_spreaded_arranged_article.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")

## Classes growth 

census_long_man_reference_all_species <- census_long_man_reference %>% left_join(species,by=c("Species"="Species_Full_Name")) %>% dplyr::select(Species,Class, ReferenceShort, Year) %>% group_by(Species,Class,ReferenceShort, Year) %>% distinct(.) %>% arrange(.,Year) %>% ungroup() %>% mutate(Duplicates=duplicated(Species), species_epithet= as.character(lapply(strsplit(as.character(Species), split=" "), "[", n=2))) %>% filter(species_epithet!="sp.") %>% mutate(.,First_occurance=if_else(Duplicates=="FALSE",1,0)) %>% na.omit() %>% filter(First_occurance==1) %>% mutate(Cumulative_occurance_species= cumsum(First_occurance)) %>% group_by(Year,Class) %>% summarise(Occurance_species_class_year= n()) %>% group_by(Class) %>% mutate(Cumulative_occurance_class= cumsum(Occurance_species_class_year)) %>% filter(Class %in% c("Insecta","Mammalia","Arachnida","Malacostraca"))

ggplot()+
  geom_line(data=census_long_man_reference_all_species,aes(y=Cumulative_occurance_class, x= Year, color=Class),show.legend = T)+
  #ggtitle("Class")+
  scale_x_continuous(breaks = seq(1860,2022,10),limits = c(1860,2025))+
  scale_y_continuous(breaks = seq(0,250,25),limits = c(0,250))+
  labs(x="Year",y="Cumulative number of species per year")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), legend.position = c(0.15,0.8))

ggsave("species_class_occurence_accumulation.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")



# Spatial Analysis

#' # Spatial analysis of Caves
#' 
#' For the creation of the caves geodatabase in r we used multiple packages. 
#' 
#' **For spatial analysis**
#' 
#' 1. sp package [@R-sp]
#' 
#' 2. rgeos package [@R-rgeos]
#' 
#' 3. rgdal package [@R-raster]
#' 
#' 4. raster package [@R-raster]
#' 
#' **For spatial visualisations**
#' 
#' 1. maptools package [@R-maptools]
#' 
#' 2. tmaps package [@R-tmap]
#' 
#' 3. ggmap package [@R-ggmap]
#' 
#' 
#' Each cave location was manually imported to Google Earth. All locations were then exported to a single kml file. In order to handle the data the kml file should be converted to a different format like xlsx. The procedure suggested from google forum:
#' 
#' 1. Download the kml file
#' 2. rename the file to .xml 
#' 3. from excel 2007 go to Data > From Other Sources > From XML Data Source 
#' 4. Browse to where you saved the file to impoort into excel. 
#' 5. Excel will prompt that it can find the schema and will try to make it by it's own, accept it and you should see your data imported successfully.
#' 
#' This procedure resulted to a xlsx file with 67 columns and tha names and coordinates of caves were spread across them for some reason.
#' 
#' So we used the [online convertor](www.gpsvisualizer.com) which resulted to a txt file with a consistent format.
#' 
#' 
#' ## Caves geographical data
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------
# Caves shape file! Caves Coordinates

# Caves_Database_kml_to_txt <- read_delim(file = "Caves_Database_kml_to_txt_25_09_2017.csv",delim = ";",col_names = T) %>% dplyr::select("name","latitude","longitude")

Caves_Database_kml_to_txt <- caves %>% dplyr::select(Cave_Name,Cave_ID,Latitude, Longitude) %>% na.omit()

Caves_Database_kml_to_txt$Latitude <- as.numeric(Caves_Database_kml_to_txt$Latitude)
Caves_Database_kml_to_txt$Longitude <- as.numeric(Caves_Database_kml_to_txt$Longitude)

Caves_Database_kml_to_txt$ID <- as.character(seq(1:nrow(Caves_Database_kml_to_txt)))

Caves_Database_kml_to_txt_shapefile_wgs84 <- Caves_Database_kml_to_txt

coordinates(Caves_Database_kml_to_txt_shapefile_wgs84)<-~Longitude+Latitude
proj4string(Caves_Database_kml_to_txt_shapefile_wgs84) <- CRS("+proj=longlat +datum=WGS84")# CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")  # this is WGS84

Caves_Database_kml_to_txt_shapefile <- spTransform(Caves_Database_kml_to_txt_shapefile_wgs84, CRS( "+proj=longlat +datum=GGRS87 +no_defs"))

species_occurencies_unique_caves_without <- caves[which((is.na(caves$Latitude))),]


#' 
#' Are the cave names in the species occurencies file the same with the Google Earth data?
#' 
#' There are `r nrow(caves)` caves that have been sampled.
#' 
#' ## Administrative data of Greece
#' 
#' We downloaded the greek municipality boundaries [Kallikratis plan](http://geodata.gov.gr/en/dataset/oria-demon-kallikrates) in the epsg 4326 format. In this format the axis order is Latitude followed by Longitude. 
#' The Greek names of municipalities were converted using the ISO 843 traslitaration system from this [online portal](http://www.passport.gov.gr/elot-743.html).
#' 
#' It is preferable to download shapefiles because they are immediatly imported into the Spatial objects. 
#' 
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------
# Hellenic settlements 

# hellenic_settlements <- maptools::readShapePoly("hellenic_settlements/hellenic_settlements",verbose=TRUE)

# proj4string(hellenic_settlements) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")  # this is WGS84

# Municipalities shape file


municipalities_shape_file_original <- rgdal::readOGR("Shapefiles/municipalities_shape_file/municipalities_Kallikratis_plan_Greece.shp",verbose=TRUE)

municipalities_shape_file <- spTransform(municipalities_shape_file_original, CRS("+proj=longlat +datum=GGRS87 +no_defs"))

#proj4string(municipalities_shape_file) <- CRS("+proj=longlat +datum=WGS84")# CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")  # this is WGS84

municipalities_greece_long_names_eng <- readxl::read_xlsx("Shapefiles/municipalities_shape_file/names_municipalities_gr_eng.xlsx",col_names = T)
municipalities_greece_long_names_eng$KWD_YPES <- as.character(municipalities_greece_long_names_eng$KWD_YPES)

municipalities_shape_file@data <- municipalities_shape_file@data %>% left_join(., municipalities_greece_long_names_eng, by=c("KWD_YPES"="KWD_YPES"))
rownames(municipalities_shape_file@data) <- as.character(seq(1:nrow(municipalities_shape_file@data)))



over_municipality <- sp::over( x = Caves_Database_kml_to_txt_shapefile , y = municipalities_shape_file , fn = NULL)

caves_in_municipa <- over_municipality

caves_in_municipa$ID <- as.character(seq(1:nrow(caves_in_municipa)))

caves_in_municipa2 <- caves_in_municipa %>% left_join(., Caves_Database_kml_to_txt, by=c("ID"="ID"))

caves_municipality_join <- caves %>% dplyr::select(Cave_ID,Cave_Name,Municipality) %>% distinct() %>% group_by(Municipality) %>% summarise(number_of_caves=n()) %>% na.omit() %>% left_join(.,municipalities_greece_long_names_eng, by=c("Municipality"="Municipalities_ISO_843"),copy=TRUE) %>% left_join(.,municipalities_greece_long_names_eng, by=c("KWD_YPES"="KWD_YPES"),copy=TRUE) %>% dplyr::select(Municipalities_ISO_843,KWD_YPES,number_of_caves) %>% ungroup() %>% filter(!(KWD_YPES=="9170" & !is.na(KWD_YPES)))

# Irakleio is 2 times but they are different municipalities. Irakleio Attikis has 9170 code so we removed it.

species_Municipality <- census_all_species_all_caves %>% dplyr::select(Species,Municipality) %>% distinct() %>% group_by(Municipality) %>% summarise(number_of_species=n()) %>% na.omit()


caves_species_municipality_join <-  caves_municipality_join %>% left_join(.,species_Municipality, by=c("Municipalities_ISO_843"="Municipality"))



## Protected areas of Greece
## Function capitals to lower

### caves with all protected areas
capwords <- function(s, strict = FALSE) {
    cap <- function(s) paste(toupper(substring(s, 1, 1)),
                  {s <- substring(s, 2); if(strict) tolower(s) else s},
                             sep = "", collapse = " " )
    sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

# All new Natura

natura2000_new_shapefile_v30  <- rgdal::readOGR("Shapefiles/GR_Natura2000_v30/gr_natura_v30.shp",verbose = T) #,p4s = "+proj=tmerc +lat_0=0 +lon_0=24 +k=0.9996 +x_0=500000 +y_0=0 +datum=GGRS87 +units=m +no_defs"
#,p4s = "+proj=longlat +datum=WGS84 +ellps=GRS80 +units=m +no_defs"
natura2000_new_shapefile_v30_wgs84 <- spTransform(natura2000_new_shapefile_v30, CRS("+proj=longlat +datum=WGS84")) #+proj=longlat +datum=GGRS87 +no_defs

names_natura2000_new_shapefile_v30 <- natura2000_new_shapefile_v30@data %>% mutate(id=as.character(seq(from=0,to=(nrow(.)-1))))

natura2000_new_shapefile_v30_dataframe <- broom::tidy(natura2000_new_shapefile_v30_wgs84) %>% left_join(., names_natura2000_new_shapefile_v30, by=c("id"="id"))

over_natura_NEW_v30 <- over( x = Caves_Database_kml_to_txt_shapefile_wgs84 , y = natura2000_new_shapefile_v30_wgs84 , returnList = T) # This is from rgeos, it contains multiple matches, thats why returnList=T. VERY Important

over_natura_NEW_v30_d <-Caves_Database_kml_to_txt %>% left_join(bind_rows(over_natura_NEW_v30,.id = "ID"),by=c("ID"="ID"))

## Only the new parts
natura2000_NEW_shapefile <- maptools::readShapePoly("Shapefiles/Natura2000_2017_NEW_shp/Kaloust/Nees_Natura", verbose = TRUE)

natura2000_NEW_shapefile_INFO <- maptools::readShapePoly("Shapefiles/Natura2000_2017_NEW_shp/NEES_FINAL_V10", verbose = TRUE)

natura2000_NEW_shapefile_INFO_df <- natura2000_NEW_shapefile_INFO@data %>% mutate(id=as.character(seq(from=0,to=(nrow(.)-1))))

natura2000_NEW_shapefile@data <-  natura2000_NEW_shapefile@data %>% mutate(id=as.character(seq(from=0,to=(nrow(.)-1)))) %>% left_join(natura2000_NEW_shapefile_INFO_df, by=c("id"="id")) %>% dplyr::select(-c(descriptio,timestamp,begin,end,altitudeMo,tessellate,extrude,visibility,drawOrder,icon))

proj4string(natura2000_NEW_shapefile) <- CRS("+proj=longlat +datum=WGS84")# CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") # this is WGS84


# shapefiles to dataframes for plotting
## New Natura
natura2000_NEW_shapefile_names <- natura2000_NEW_shapefile@data %>% mutate(id=as.character(seq(from=0,to=(nrow(.)-1))))

natura2000_NEW_shapefile_dataframe <- broom::tidy(natura2000_NEW_shapefile) %>% left_join(., natura2000_NEW_shapefile_names, by=c("id"="id"))

##### Natura 2000
natura2000shapefile <- maptools::readShapePoly("Shapefiles/natura2000shapefile/natura2000shapefile",verbose=TRUE)

proj4string(natura2000shapefile) <- CRS("+proj=longlat +datum=WGS84") #CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")  # this is WGS84

natura2000shapefile_data <- natura2000shapefile@data %>% dplyr::select(CODE,NAME_LATIN)
### Katafygia agrias zois
#katafygia_agrias_zwhs <- maptools::readShapePoly("katafygia_agrias_zwhs/katafygia_agrias_zwhs",verbose=TRUE)

#proj4string(katafygia_agrias_zwhs) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")  # this is WGS84

### Natura 2000 TIDY
names_natura2000shapefile <- natura2000shapefile@data %>% mutate(id=as.character(seq(from=0,to=(nrow(.)-1)))) %>% mutate(NAME_LATIN_lower_letters=capwords(tolower(NAME_LATIN))) %>% mutate(SITETYPE_NATURA=ifelse(is.na(SITETYPE), NA_character_, ifelse(SITETYPE=="SPA","Special Protection Area", ifelse(SITETYPE=="SCI","Special Area of Conservation ", "Special Protection Area - Special Area of Conservation"))))

natura2000shapefile_dataframe <- broom::tidy(natura2000shapefile) %>% left_join(., names_natura2000shapefile, by=c("id"="id"))


####
katafygia_agrias_zwhs <- maptools::readShapePoly("Shapefiles/KAZ_data/KAZ_data",verbose=TRUE)

proj4string(katafygia_agrias_zwhs) <- CRS("+proj=longlat +datum=WGS84")# CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")  # this is WGS84



#ssss <- katafygia_agrias_zwhs@data

KAZ_data_translated <- readxl::read_xlsx("Shapefiles/KAZ_data/KAZ_table_translated.xlsx",col_names = T)

KAZ_data_KODE <- read_csv("Shapefiles/KAZ_data/KAZ_data.csv",col_names = T) %>% dplyr::select(-the_geom) %>% left_join(KAZ_data_translated, by=c("KODE"="KODE"))

### katafygia_agrias_zwhs TIDY
#katafygia_agrias_zwhs_names$id <- as.character(seq(from=0,to=(nrow(katafygia_agrias_zwhs_names)-1)))

katafygia_agrias_zwhs_dataframe <- broom::tidy(katafygia_agrias_zwhs)
#%>% left_join(., katafygia_agrias_zwhs_names, by=c("id"="id"))


### New Natura Over
over_NEW_natura <- sp::over( x = Caves_Database_kml_to_txt_shapefile_wgs84, y = natura2000_NEW_shapefile , fn = NULL)

caves_in_over_NEW_natura <- over_NEW_natura %>% mutate(ID=as.character(seq(1:nrow(over_NEW_natura)))) %>% filter(SITE_TYPE=="SCI") %>% mutate(TYPE=gsub(pattern = "TROP",replacement = "Modified:",x =TYPE),Name_New_NATURA2=gsub(pattern = "TROP ",replacement = "",x = Name)) %>% left_join(Caves_Database_kml_to_txt,by=c("ID"="ID")) %>% dplyr::select(Cave_ID,TYPE,SITE_TYPE,Name_New_NATURA2) %>% left_join(natura2000shapefile_data, by=c("Name_New_NATURA2"="CODE")) %>% dplyr::select(-c(Name_New_NATURA2))

colnames(caves_in_over_NEW_natura) <- c("Cave_ID","CODE_NEW_NATURA","SITETYPE_NEW_NATURA", "NAME_LATIN_NEW_NATURA")

### over NATURA

over_natura <- sp::over( x = Caves_Database_kml_to_txt_shapefile_wgs84 , y = natura2000shapefile , fn = NULL)

# create file with the old and new natura2000 areas combined

caves_in_over_natura <- over_natura %>% mutate(ID=as.character(seq(1:nrow(over_natura)))) %>% left_join(Caves_Database_kml_to_txt,by=c("ID"="ID")) %>% dplyr::select(Cave_ID,CODE,SITETYPE,NAME_LATIN)

colnames(caves_in_over_natura) <- c("Cave_ID","CODE_NATURA","SITETYPE_NATURA", "NAME_LATIN_NATURA")

### over Katafygia agrias zwis

over_katafygia_agrias_zwhs <- sp::over( x = Caves_Database_kml_to_txt_shapefile_wgs84 , y = katafygia_agrias_zwhs , fn = NULL)

over_katafygia_agrias_zwhs$ID_cave <- as.character(seq(1:nrow(over_katafygia_agrias_zwhs)))

over_katafygia_agrias_zwhs_info <- over_katafygia_agrias_zwhs %>% dplyr::select(KODE,ID_cave) %>% left_join(KAZ_data_KODE, by=c("KODE"="KODE"))

caves_in_over_katafygia_agrias_zwhs <- Caves_Database_kml_to_txt %>% left_join(over_katafygia_agrias_zwhs_info,by=c("ID"="ID_cave")) %>% dplyr::select(Cave_ID,KODE,KAZ_NAME_GR,KAZ_Official_Government_Gazette)


#  left_join(.,over_natura_NEW_v30_d, by=c("Cave_ID"="Cave_ID")) %>% 
caves_protection <- caves %>% dplyr::select(Cave_ID) %>% left_join(.,caves_in_over_natura, by=c("Cave_ID"="Cave_ID")) %>% left_join(.,caves_in_over_NEW_natura, by=c("Cave_ID"="Cave_ID")) %>% left_join(., caves_in_over_katafygia_agrias_zwhs, by=c("Cave_ID"="Cave_ID")) %>% mutate(NAME_LATIN_NATURA=gsub("NANA",NA_character_,gsub(" Kai "," and ",capwords(tolower(NAME_LATIN))))) %>% mutate(NAME_LATIN_NEW_NATURA=gsub("NANA",NA_character_,gsub(" Kai "," and ",capwords(tolower(NAME_LATIN_NEW_NATURA))))) %>% mutate(SITETYPE_NATURA_ALL=ifelse(is.na(SITETYPE), NA_character_, ifelse(SITETYPE=="SPA","Special Protection Area", ifelse(SITETYPE=="SCI","Special Area of Conservation", "Special Protection Area - Special Area of Conservation"))),SITETYPE_NEW_NATURA_ALL=ifelse(is.na(SITETYPE_NEW_NATURA), NA_character_, ifelse(SITETYPE_NEW_NATURA=="SPA","Special Protection Area", ifelse(SITETYPE_NEW_NATURA=="SCI","Special Area of Conservation", "Special Protection Area - Special Area of Conservation"))))

## some names in () had lower case first letters so i changed them manually.
#write_delim(caves_protection,path = "caves_protection.txt",delim = "\t",col_names = T)

#### JOIN FILES ##########

caves_all_shapefiles <- caves_in_municipa2 %>% dplyr::select(-Cave_Name,-Longitude,-Latitude)%>% left_join(.,caves_protection, by=c("Cave_ID"="Cave_ID"))

# Caves Ready!!!

caves_all_info <- caves %>% left_join(.,caves_all_shapefiles, by=c("Cave_ID"="Cave_ID")) %>% left_join(.,over_natura_NEW_v30_d, by=c("Cave_ID"="Cave_ID"))



#' ## New Natura2000 v30
# Which are different

caves_protection <- strsplit(x = caves$Protection_Status,split = "|",fixed=TRUE)

caves_protection_data <- data_frame(Caves_Protection=unlist(caves_protection),CaveName=rep.int(caves$Cave_Name,times = sapply(caves_protection,length)),Cave_ID=rep.int(caves$Cave_ID,times = sapply(caves_protection,length)),Region=rep.int(caves$Region,times = sapply(caves_protection,length)),Altitude=rep.int(caves$Altitude,times = sapply(caves_protection,length))) %>% mutate(Protection_Type=if_else(is.na(Caves_Protection),"Not protected", if_else(grepl("^G.",x = Caves_Protection),"Natura2000","Wildlife Refuge"))) %>% mutate(Caves_Protection_Code=gsub(" - .*","",Caves_Protection))

caves_protection_data_natura <- caves_protection_data %>% filter(Protection_Type=="Natura2000") %>% left_join(names_natura2000shapefile, by=c("Caves_Protection_Code"="CODE")) %>% dplyr::select(Cave_ID,Region,Caves_Protection_Code,Caves_Protection, NAME_LATIN_lower_letters,SITETYPE_NATURA) %>% mutate(Law= gsub(".*Law *(.*?) *<a.*","Law \\1",Caves_Protection),Link=paste0(' <a href="http://natura2000.eea.europa.eu/Natura2000/SDF.aspx?site=',Caves_Protection_Code,'" target="_blank"> Check site’s Standard Data Form</a>'))

caves_in_over_NEW_natura_v30 <- over_natura_NEW_v30_d %>% dplyr::select(-c(Cave_Name,Latitude,Longitude,ID),SITECODE_v30=SITECODE) %>% left_join(Caves_Database_kml_to_txt,by=c("Cave_ID"="Cave_ID")) %>% left_join(caves_protection_data_natura, by=c("Cave_ID"="Cave_ID")) %>% mutate(SITECODE_v30=trimws(SITECODE_v30,which = "both"))# %>% mutate(CODE_NEW_NATURA=gsub(pattern = "Modified: ",replacement = "",x=CODE_NEW_NATURA))

natura_v30_vs_natura <- caves_in_over_NEW_natura_v30 %>% distinct(Cave_ID,Cave_Name,Region,SITECODE_v30,Caves_Protection_Code) %>% distinct() %>% mutate(SITECODE_v30=trimws(SITECODE_v30,which = "both"),Caves_Protection_Code=trimws(Caves_Protection_Code,which = "both")) %>% filter(SITECODE_v30!=Caves_Protection_Code)

natura_v30_vs_natura_codes <- natura_v30_vs_natura %>% distinct(SITECODE_v30,Caves_Protection_Code)


#new_natura_codes <- natura_v30_vs_natura[which(!(natura_v30_vs_natura$SITECODE_v30 %in% caves_protection_data_natura$Caves_Protection_Code)),][,2] %>% distinct(.) %>% left_join(names_natura2000shapefile, by=c("SITECODE_v30"="CODE")) %>% mutate(Law="Law 3937/2011 (OGG/Α 60/31.03.2011), Joint Ministerial Decision 50743/2017 (OGG 4432/B/15.12.17).",Link=paste0(' <a href="http://natura2000.eea.europa.eu/Natura2000/SDF.aspx?site=',SITECODE_v30,'" target="_blank"> Check site’s Standard Data Form</a>')) %>% mutate(Site_Description=paste0(SITECODE_v30," - ",NAME_LATIN_lower_letters," (",SITETYPE_NATURA,"): ",Law," ",Link))


#' 
## ------------------------------------------------------------------------

kable(table(over_natura_NEW_v30_d$SITETYPE),col.names = c("Natura2000 Site type","Caves"),caption = "Natura2000 v30 and caves")

#' 
#' 
#' # Geospatial data visualisation
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------



ggplot()+
  geom_freqpoly(data = caves, aes(x=Altitude),binwidth = 100,show.legend = F)+
  #geom_freqpoly(data = species_per_altitude, aes(x=Altitude),binwidth = 100,show.legend = T)+
  scale_y_continuous(breaks = seq(0,100,25),limits = c(0,100))+
  scale_x_continuous(breaks = seq(0,2200,200),limits = c(0,2200))+
  ggtitle("Caves in different altitude with 100m bins")+
  labs(x="Altitude", y= "Number of caves")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())

ggsave("caves_per_altitude.png", plot = last_plot(), device = "png",width = 20,height = 20,units = "cm",dpi = 300 ,path = "Plots/")


#' ## Species and caves per region
greece_level_2_shape <-getData('GADM', country='GRC', level=2)  ##Get the Province Shapefile for France

greece_level_2 <- spTransform(greece_level_2_shape, CRS("+proj=longlat +datum=WGS84"))

greece_regions <- c("Athos","East Macedonia and Thrace","Attica ","West Greece","West Macedonia","Ionian Islands ","Epirus ","Central Macedonia","Crete","South Aegean","Peloponnese ","Central Greece ","Thessaly","North Aegean")

caves_Region <- caves %>% dplyr::select(Cave_ID, Region) %>% distinct() %>% group_by(Region) %>% summarize(number_of_caves=n()) %>% na.omit() %>% mutate(color_manual=colorRampPalette(c("orangered2","palegreen3","skyblue1","slateblue1","pink2","sienna3"),space="Lab")( 14 ))

caves_Region$regions <- greece_regions

species_Region <- census_all_species_all_caves %>% dplyr::select(Species,Region) %>% distinct() %>% group_by(Region) %>% summarise(number_of_species=n()) %>% na.omit()

species_Region$regions <- greece_regions


# https://www.r-bloggers.com/using-r-working-with-geospatial-data-and-ggplot2/

greece_level_2@data$id <- rownames(greece_level_2@data)

greece_level_2 <- spTransform(greece_level_2, CRS("+proj=longlat +datum=WGS84"))

greece_level_2_fortify <- broom::tidy(greece_level_2)
#greece_level_2_fortify <- fortify(greece_level_2, region = "id")

greece_level_2_dataframe <- merge(greece_level_2_fortify, greece_level_2@data, by = "id")

greece_level_2_dataframe <- greece_level_2_dataframe %>% left_join(., caves_Region, by=c("NAME_2"="regions")) %>% dplyr::select(-Region) %>% left_join(., species_Region, by=c("NAME_2"="regions"))

cnames <- aggregate(cbind(long, lat) ~ NAME_2, data=greece_level_2_dataframe, FUN=function(x)mean(range(x))) %>% left_join(., caves_Region, by=c("NAME_2"="regions"))

cnames_species <- aggregate(cbind(long, lat) ~ NAME_2, data=greece_level_2_dataframe, FUN=function(x)mean(range(x))) %>% left_join(., species_Region, by=c("NAME_2"="regions"))


#' 
#' 
#' Caves distribution across all regions in Greece.
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE, eval=TRUE,cache=TRUE-----


gg_region_caves <- ggplot() +
  geom_polygon(data = greece_level_2_dataframe,aes(x=long, y=lat,group = group,fill = number_of_caves),color="white",lwd=0.2) +
  #geom_path(size= 0.2,color = "white") +
  #coord_equal() +
  #geom_text(data=cnames,aes(label = number_of_caves, x = long, y = lat)) + 
  scale_fill_gradient(low="blue", high="red",breaks=seq(0,200,50), limits=c(0,200),name="Caves")+
  scale_x_continuous(breaks = seq(18,30,1),limits = c(18,30))+
  scale_y_continuous(breaks = seq(35,42,1),limits = c(34.5,42))+
  coord_map(xlim = c(19,30.1), ylim = c(34.5,42))+
  theme_bw()+
  theme(legend.position = c(0.85, 0.80),panel.border = element_blank(),panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.title = element_blank(), axis.text = element_blank(),axis.ticks = element_blank())

ggsave("caves_spatial_dist_per_region_no_text.png", plot = gg_region_caves, device = "png",width = 30,height = 30,units = "cm",dpi = 300 ,path = "Plots/")


#' 
#' ![Samplings from caves in Greece.](Plots/caves_spatial_dist_per_region_no_text.png)
#' 
#' 
#' Regions with caves coordinates.
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------

gg_region_caves_color <- ggplot() +
  geom_polygon(data = greece_level_2_dataframe,aes(x=long, y=lat,group = group,fill = color_manual),color="white",lwd=0.2,show.legend = F, alpha=0.5) +
  #geom_path(size= 0.2,color = "white") +
  #coord_equal() +
  #geom_text(data=caves,aes(x=Longitude, y=Latitude,label = Cave_ID)) + 
  geom_point(data = caves,aes(x=Longitude, y=Latitude,color=Cave_Type),size = 3)+
  scale_color_manual(name="Cave Types", values = c("Natural"="red","Artificial"="black", "Natural Modified"= "orange"))+
  #scale_fill_manual(values = caves_Region$color_manual)+
  scale_x_continuous(breaks = seq(18,30,1),limits = c(18,30))+
  scale_y_continuous(breaks = seq(35,42,1),limits = c(34.5,42))+
  coord_map(xlim = c(19,30.1), ylim = c(34.5,42))+
  theme_bw()+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.8),panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.title = element_blank(), axis.text = element_text(size = 18))

ggsave("caves_in_region_no_text_color.png", plot = gg_region_caves_color, device = "png",width = 28,height = 28,units = "in",dpi = 100 ,path = "Plots/")


#' 
#' ![Regions with caves coordinates in Greece.](Plots/caves_in_region_no_text_color.png)
#' 
#' Species distribution across all regions in Greece.
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE, eval=TRUE,cache=TRUE-----

gg_region_species <- ggplot(data = greece_level_2_dataframe,aes(x=long, y=lat)) +
  geom_polygon(data = greece_level_2_dataframe,aes(x=long, y=lat,group = group,fill = number_of_species),color="white",lwd=0.2) +
  #geom_path(size= 0.2,color = "white") +
  #coord_equal() +
  #geom_text(data=cnames_species,aes(label = number_of_species, x = long, y = lat)) + 
  scale_fill_gradient(low="grey", high="red",breaks=seq(0,300,50), limits=c(0,300),name="Species")+
  scale_x_continuous(breaks = seq(18,30,1),limits = c(18,30))+
  scale_y_continuous(breaks = seq(35,42,1),limits = c(34.5,42))+
  coord_map(xlim = c(19,30.1), ylim = c(34.5,42))+
  theme_bw()+
  theme(legend.position = c(0.85, 0.85),panel.border = element_blank(),panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.title = element_blank(), axis.text = element_blank(),axis.ticks = element_blank())

ggsave("species_spatial_dist_per_region_no_text.png", plot = gg_region_species, device = "png",width = 30,height = 30,units = "cm",dpi = 300 ,path = "Plots/")


#' 
#' ![Cave fauna distribution in Greece.](Plots/species_spatial_dist_per_region_no_text.png)
#' 
#' ## Species and caves distribution across Greek municipalities
#' 
#' 
## ----warning=FALSE, message=FALSE, echo=FALSE,eval=TRUE------------------
# Transform to dataframe
# https://www.r-bloggers.com/using-r-working-with-geospatial-data-and-ggplot2/

municipalities_shape_file_dataframe <- broom::tidy(municipalities_shape_file)

municipalities_greece_long_names_eng$id <- as.character(seq(from=0, to=(nrow(municipalities_greece_long_names_eng)-1)))

watershedDF_muni <- municipalities_shape_file_dataframe %>% left_join(., municipalities_greece_long_names_eng, by=c("id"="id")) %>% left_join(., caves_species_municipality_join, by=("Municipalities_ISO_843"="Municipalities_ISO_843"))

cnames <- aggregate(cbind(long, lat) ~ Municipalities_ISO_843, data=watershedDF_muni, FUN=function(x)mean(range(x))) %>% left_join(., caves_species_municipality_join, by=c("Municipalities_ISO_843"="Municipalities_ISO_843"))

cnames_species <- aggregate(cbind(long, lat) ~ Municipalities_ISO_843, data=watershedDF_muni, FUN=function(x)mean(range(x))) %>% left_join(., caves_species_municipality_join, by=c("Municipalities_ISO_843"="Municipalities_ISO_843"))

watershedDF_muni$number_of_caves[is.na(watershedDF_muni$number_of_caves)] <- 0
watershedDF_muni$number_of_species[is.na(watershedDF_muni$number_of_species)] <- 0


#' 
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE, eval=TRUE,cache=TRUE-----

gg_municipalities_caves <- ggplot() +
  geom_polygon(data = watershedDF_muni,aes(x=long, y=lat,group = group,fill = number_of_caves),color="white",lwd=0.08) +
  #geom_path(size= 0.2,color = "white") +
  #coord_equal() +
  #geom_text(data=cnames,aes(label = number_of_caves, x = long, y = lat)) + 
  scale_fill_gradient(low="blue", high="red",breaks=seq(0,20,5), limits=c(0,20),name="Caves")+
  scale_x_continuous(breaks = seq(18,30,1),limits = c(18,30))+
  scale_y_continuous(breaks = seq(35,42,1),limits = c(34.5,42))+
  coord_map(xlim = c(19,30.1), ylim = c(34.5,42))+
  theme_bw()+
  theme(legend.position = c(0.85, 0.85),panel.border = element_blank(),panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.title = element_blank(), axis.text = element_blank(),axis.ticks = element_blank())

ggsave("caves_spatial_dist_per_municipality_no_text.png", plot = gg_municipalities_caves, device = "png",width = 30,height = 30,units = "cm",dpi = 300 ,path = "Plots/")



#' 
#' ![Caves that have been sampled in Greece](Plots/caves_spatial_dist_per_municipality_no_text.png)
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE, eval=TRUE,cache=TRUE-----

gg_municipalities_species <- ggplot() +
  geom_polygon(data = watershedDF_muni,aes(x=long, y=lat,group = group,fill = number_of_species),color="white",lwd=0.082) +
  #geom_path(size= 0.2,color = "white") +
  #coord_equal() +
  #geom_text(data=cnames,aes(label = number_of_caves, x = long, y = lat)) + 
  scale_fill_gradient(low="grey", high="red",breaks=seq(0,80,20), limits=c(0,80),name="Species")+
  scale_x_continuous(breaks = seq(18,30,1),limits = c(18,30))+
  scale_y_continuous(breaks = seq(35,42,1),limits = c(34.5,42))+
  coord_map(xlim = c(19,30.1), ylim = c(34.5,42))+
  theme_bw()+
  theme(legend.position = c(0.85, 0.85),panel.border = element_blank(),panel.grid.minor = element_blank(), panel.grid.major =element_blank(),axis.title = element_blank(), axis.text = element_blank(),axis.ticks = element_blank())

ggsave("species_spatial_dist_per_municipality_no_text.png", plot = gg_municipalities_species, device = "png",width = 30,height = 30,units = "cm",dpi = 300 ,path = "Plots/")



#' 
#' ![Species sampled per municipality in Greece](Plots/species_spatial_dist_per_municipality_no_text.png)
#' 
#' ## Greece caves and protected areas
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------

caves_in_SCI <- over_natura_NEW_v30_d %>% dplyr::filter(SITETYPE=="SCI") %>% nrow()
caves_in_SPA <- over_natura_NEW_v30_d %>% dplyr::filter(SITETYPE=="SPA") %>% nrow()
caves_in_SCISPA <- over_natura_NEW_v30_d %>% dplyr::filter(SITETYPE=="SCISPA") %>% nrow()
caves_in_KAZ <- caves_all_info %>% dplyr::filter(!is.na(KODE))%>% nrow()

caves_in_areas <- data.frame(Areas=c("NATURA2000 SCI","NATURA2000 SPA", "NATURA2000 SPASCI", "Wildlife Refuge"), Caves=c(caves_in_SCI,caves_in_SPA,caves_in_SCISPA,caves_in_KAZ))

caves_in_areas_caption <- "Number of caves per protected area type"
kable(caves_in_areas,caption=caves_in_areas_caption, align = 'l')


#' 
## ------------------------------------------------------------------------

caves_protection_data %>% group_by(Protection_Type) %>% summarise(number_of_caves=n()) %>%
ggplot(.)+
  geom_col(aes(x=Protection_Type, y= number_of_caves, fill=Protection_Type),show.legend = F)+
  geom_text(aes(x =Protection_Type,y= number_of_caves, label=number_of_caves), position=position_dodge(width=0.7), vjust=-0.25,size=2.8)+
  scale_y_continuous(breaks = seq(0,350,25),limits = c(0,350))+
  ggtitle("Caves in Greece that are protected")+
  labs(x="Protection", y= "Number of caves")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("caves_protection_data_greece.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")


#' 
#' 
#' With google earth background.
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------
## load greek borders

hellenic_borders_shapefile <- maptools::readShapeLines("Shapefiles/hellenic_borders/hellenic_borders",verbose=TRUE)

proj4string(hellenic_borders_shapefile) <- CRS("+proj=longlat +datum=WGS84") #CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")  # this is WGS84

hellenic_borders_shapefile_dataframe <- broom::tidy(hellenic_borders_shapefile)
bbox_hellenic_borders <- hellenic_borders_shapefile@bbox
bbox_hellenic_borders_lat <- bbox_hellenic_borders





#' 
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE, eval=FALSE,cache=TRUE----
## # plot map
## 
## ## get map
## map_greece <- get_map(location = c(lon=mean(bbox_hellenic_borders[1,]), lat=mean(bbox_hellenic_borders[2,])),zoom = 6,maptype = "terrain")
## 
## map_greece_plot <- ggmap(map_greece)+
##   geom_polygon(data = natura2000shapefile_dataframe,aes(x=long, y=lat,group = group,fill="Natura2000"),lwd=0.082, alpha=0.6)+
##   geom_polygon(data = katafygia_agrias_zwhs_dataframe,aes(x=long, y=lat,group = group,fill="Wildlife Refuge"),lwd=0.082,alpha=0.8)+
##   geom_point(data = caves,aes(x=Longitude, y=Latitude,color="Caves"),size = 0.2)+
##   labs(x="Longitude",y="Latitude")+
##   scale_fill_manual(values = c("Natura2000"="chartreuse3","Wildlife Refuge"="chocolate2"),name="Protected areas")+
##   scale_color_manual(name="", values = c("Caves"="red"))+
##   scale_x_continuous(breaks = seq(20,30,2),limits = c(20,30))+
##   scale_y_continuous(breaks = seq(35,42,2),limits = c(34.5,42))+
##   coord_map(xlim = c(19,30.1), ylim = c(34.5,42))+
##   theme_bw()
## #geom_text(data = sisquoc, aes(label = paste("  ", as.character(name), sep="")), angle = 60, hjust = 0, color = "yellow")
## 
## ggsave("map_greece_plot_protected_areas.png", plot = map_greece_plot, device = "png",width = 30,height = 30,units = "cm",dpi = 300 ,path = "Plots/")

#' 
#' Only with borders.
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE, eval=TRUE----------------

map_greece_plot_lines <- ggplot()+
  geom_polygon(data = hellenic_borders_shapefile_dataframe,aes(x=long, y=lat,group = group),lwd=0.12,color="black")+
  geom_polygon(data = natura2000_new_shapefile_v30_dataframe,aes(x=long, y=lat,group = group,fill=SITETYPE),lwd=0.082, alpha=0.6)+
  geom_polygon(data = katafygia_agrias_zwhs_dataframe,aes(x=long, y=lat,group = group,fill="Wildlife Refuge"),lwd=0.082,alpha=0.8)+
  geom_point(data = caves,aes(x=Longitude, y=Latitude,color="Caves"),size = 0.2)+
  #geom_text(data = caves,aes(x=Longitude, y=Latitude,label=Cave_ID))+
  labs(x="Longitude",y="Latitude")+
  scale_fill_manual(values = c("chartreuse3","purple","cyan3","chocolate2"),labels = c("Natura2000 v30 SCI", "Natura2000 v30 SPA", "Natura2000 v30 SCISPA","Wildlife Refuge"),name="Protected areas")+
  scale_color_manual(name="", values = c("Caves"="red"))+
  scale_x_continuous(breaks = seq(18,30,1),limits = c(18,30))+
  scale_y_continuous(breaks = seq(35,42,1),limits = c(34.5,42))+
  coord_map(xlim = c(19,30.1), ylim = c(34.5,42))+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),legend.position = c(0.87, 0.73),legend.text = element_text(size=9),legend.title = element_text(size=10))
#geom_text(data = sisquoc, aes(label = paste("  ", as.character(name), sep="")), angle = 60, hjust = 0, color = "yellow")
    
ggsave("map_greece_plot_lines.png", plot = map_greece_plot_lines, device = "png",width = 30,height = 30,units = "cm",dpi = 300 ,path = "Plots/")



#' 
#' ![Caves and protected areas in Greece (only borders)](Plots/map_greece_plot_lines.png)
#' 
#' 
#' Compare Natura2000
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE, eval=FALSE---------------
## 
## comparison_Natura2000 <- ggplot()+
##   geom_polygon(data = natura2000_NEW_shapefile_dataframe,aes(x=long, y=lat,group = group,fill=SITE_TYPE),lwd=0.082, alpha=0.6)+
##   geom_polygon(data = hellenic_borders_shapefile_dataframe,aes(x=long, y=lat,group = group),lwd=0.12,color="black")+
##   geom_polygon(data = natura2000shapefile_dataframe,aes(x=long, y=lat,group = group,fill=SITETYPE),lwd=0.082, alpha=0.6)+
##   geom_polygon(data = natura2000_new_shapefile_v30_dataframe,aes(x=long, y=lat,group = group,fill="Natura2000 v30"),lwd=0.082,alpha=0.8)+
##   geom_point(data = caves,aes(x=Longitude, y=Latitude,color="Caves"),size = 0.2)+
##   #geom_text(data = caves,aes(x=Longitude, y=Latitude,label=Cave_ID))+
##   labs(x="Longitude",y="Latitude")+
##   #scale_fill_manual(values = c("chartreuse3","purple","cyan3","chocolate2"),labels = c("Natura2000 SCI", "Natura2000 SPA", "Natura2000 SPASCI","Natura2000 v30"),name="Protected areas")+
##   #scale_fill_manual(values = c("SCI"="chartreuse3","SPA"="purple","SPASCI"="cyan3", "Wildlife Refuge"="chocolate2"),labels = c("Natura2000 SCI", "Natura2000 SPA", "Natura2000 SPASCI","Wildlife Refuge")name="Protected areas")+
##   scale_color_manual(name="", values = c("Caves"="red"))+
##   scale_x_continuous(breaks = seq(18,30,1),limits = c(18,30))+
##   scale_y_continuous(breaks = seq(35,42,1),limits = c(34.5,42))+
##   coord_map(xlim = c(19,30.1), ylim = c(34.5,42))+
##   theme_bw()+
##   theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),legend.position = c(0.87, 0.73),legend.text = element_text(size=9),legend.title = element_text(size=10))
## #geom_text(data = sisquoc, aes(label = paste("  ", as.character(name), sep="")), angle = 60, hjust = 0, color = "yellow")
## 
## ggsave("comparison_Natura2000.png", plot = comparison_Natura2000, device = "png",path = "Plots/")
## 
## 

#' 
#' 
#' ## Grids
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------
## https://gis.stackexchange.com/questions/124295/convert-coordinates-from-readshapepoly-in-r-to-long-lat-coordinates ## sotireeees

grid_100k_shapefile <- readOGR("Shapefiles/EEA_reference_grid_1_10_50_and_100_kmgr/GR_100k.shp",verbose=TRUE)


grid_100k_shapefile_wgs84 <- spTransform(grid_100k_shapefile, CRS("+proj=longlat +datum=WGS84"))
grid_100k_shapefile_dataframe <- tidy(grid_100k_shapefile_wgs84)

grid_10k_shapefile <- readOGR("Shapefiles/EEA_reference_grid_1_10_50_and_100_kmgr/GR_10k.shp",verbose=TRUE)


grid_10k_shapefile_wgs84 <- spTransform(grid_10k_shapefile, CRS("+proj=longlat +datum=WGS84"))
grid_10k_shapefile_dataframe <- broom::tidy(grid_10k_shapefile_wgs84)


grid_10k_shapefile_wgs84_data <- grid_10k_shapefile_wgs84@data %>% mutate(id=as.character(seq(from=0, to=(nrow(grid_10k_shapefile_wgs84@data)-1)))) %>% left_join(grid_10k_shapefile_dataframe,by=c("id"="id")) %>% dplyr::select(-c(EofOrigin,NofOrigin))

dim(grid_10k_shapefile_wgs84@data)
length(unique(grid_10k_shapefile_dataframe$id))
 

#grid_1k_shapefile <- readOGR("EEA_reference_grid_1_10_50_and_100_kmgr/GR_1k.shp",verbose=TRUE)


#grid_1k_shapefile_wgs84 <- spTransform(grid_1k_shapefile, CRS("+proj=longlat +datum=WGS84"))
#grid_1k_shapefile_dataframe <- tidy(grid_1k_shapefile_wgs84)

 

#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------

# caves over polygon
Caves_Database_kml_to_txt_shapefile_wgs84 <- spTransform(Caves_Database_kml_to_txt_shapefile, CRS("+proj=longlat +datum=WGS84"))


### over grid

over_grid_10k <- sp::over( x = Caves_Database_kml_to_txt_shapefile_wgs84 , y = grid_10k_shapefile_wgs84 , fn = NULL)



# 
#grid_10k_shapefile_dataframe

caves_in_over_grid_10k <- over_grid_10k %>% mutate(ID=as.character(seq(1:nrow(over_grid_10k)))) %>% left_join(Caves_Database_kml_to_txt,by=c("ID"="ID"))

#%>% 
grid_10k_caves <- grid_10k_shapefile_wgs84_data %>% left_join(caves_in_over_grid_10k,by=c("CellCode"="CellCode"))

# cave Abundance 
caves_in_over_grid_10k_abundance <- caves_in_over_grid_10k %>% group_by(CellCode) %>% summarise(number_of_caves=n())

grid_10k_caves_abundance <- grid_10k_shapefile_wgs84_data %>% left_join(caves_in_over_grid_10k_abundance,by=c("CellCode"="CellCode")) %>% mutate(number_of_caves=if_else(is.na(number_of_caves),0,as.numeric(number_of_caves)))

# species abundance

species_per_cave <- census_all_species %>% filter(species_epithet!="sp.") %>% distinct(Species,Classification,Cave_Name,Cave_ID)

caves_in_over_grid_10k_species <- caves_in_over_grid_10k %>% left_join(species_per_cave,by=c("Cave_ID"="Cave_ID")) %>% distinct(CellCode,Species) %>% group_by(CellCode) %>% summarise(number_of_species=n())

grid_10k_species_abundance <- grid_10k_caves_abundance %>% left_join(caves_in_over_grid_10k_species,by=c("CellCode"="CellCode")) %>% mutate(number_of_species=if_else(is.na(number_of_species),0,as.numeric(number_of_species)))

# Species classification

caves_in_over_grid_10k_species_classification <- caves_in_over_grid_10k %>% left_join(species_per_cave,by=c("Cave_ID"="Cave_ID")) %>% distinct(CellCode,Species,Classification) %>% group_by(CellCode,Classification) %>% summarise(number_of_species=n())

grid_10k_species_abundance_classification <- grid_10k_caves_abundance %>% left_join(caves_in_over_grid_10k_species_classification,by=c("CellCode"="CellCode")) %>% mutate(number_of_species=if_else(is.na(number_of_species),0,as.numeric(number_of_species))) %>% filter(!is.na(Classification))

# endemic species abundance

endemic_species_per_cave <- census_all_species %>% filter(species_epithet!="sp.", Distribution=="Endemic to Greece") %>% distinct(Species,Classification,Cave_Name,Cave_ID,Distribution)

caves_in_over_grid_10k_endemic_species <- caves_in_over_grid_10k %>% left_join(endemic_species_per_cave,by=c("Cave_ID"="Cave_ID")) %>% distinct(CellCode,Species) %>% group_by(CellCode) %>% summarise(number_of_species=n())

grid_10k_endemic_species_abundance <- grid_10k_caves_abundance %>% left_join(caves_in_over_grid_10k_endemic_species,by=c("CellCode"="CellCode")) %>% mutate(number_of_species=if_else(is.na(number_of_species),0,as.numeric(number_of_species)))



#' 
#' 
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE, eval=FALSE,cache=TRUE----
## # Caves
## 
## ggplot()+
##   geom_polygon(data = grid_10k_caves_abundance,aes(x=long, y=lat,group = group,fill=number_of_caves),lwd=0.082, alpha=0.6)+
##   #geom_polygon(data = grid_100k_shapefile_dataframe,aes(x=long, y=lat,group = group),lwd=0.082, alpha=0.6)+
##   geom_polygon(data = hellenic_borders_shapefile_dataframe,aes(x=long, y=lat,group = group),lwd=0.12,color="black")+
##   # geom_polygon(data = natura2000shapefile_dataframe,aes(x=long, y=lat,group = group,fill=SITETYPE),lwd=0.082, alpha=0.6)+
##   #geom_polygon(data = katafygia_agrias_zwhs_dataframe,aes(x=long, y=lat,group = group,fill="Wildlife Refuge"),lwd=0.082,alpha=0.8)+
##   geom_point(data = caves,aes(x=Longitude, y=Latitude,color="Caves"),size = 0.2)+
##   labs(x="Longitude",y="Latitude")+
##   #scale_fill_manual(values = c("chartreuse3","purple","cyan3","chocolate2"),labels = c("Natura2000 SCI", "Natura2000 SPA", "Natura2000 SPASCI","Wildlife Refuge"),name="Protected areas")+
##   #scale_fill_manual(values = c("SCI"="chartreuse3","SPA"="purple","SPASCI"="cyan3", "Wildlife Refuge"="chocolate2"),labels = c("Natura2000 SCI", "Natura2000 SPA", "Natura2000 SPASCI","Wildlife Refuge")name="Protected areas")+
##   scale_color_manual(name="", values = c("Caves"="red"))+
##   scale_fill_gradient(low="white", high="slateblue3",breaks=seq(0,15,3), limits=c(0,15),name="Caves")+
##   scale_x_continuous(breaks = seq(18,30,1),limits = c(18,30))+
##   scale_y_continuous(breaks = seq(35,42,1),limits = c(34.5,42))+
##   coord_map(xlim = c(19,30.1), ylim = c(34.5,42))+
##   theme_bw()+
##   theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),legend.position = c(0.87, 0.75),legend.text = element_text(size=9),legend.title = element_text(size=10),axis.text =element_text(size = 10))
## #geom_text(data = sisquoc, aes(label = paste("  ", as.character(name), sep="")), angle = 60, hjust = 0, color = "yellow")
## 
##  ggsave("map_greece_plot_lines_grid_caves.png", plot = last_plot(),device = "png",width = 30,height = 30,units = "cm",dpi = 300 ,path = "Plots/")
## 

#' 
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------
# species

grid_10k_species_abundance_plot <- ggplot()+
  geom_polygon(data = grid_10k_species_abundance,aes(x=long, y=lat,group = group,fill=number_of_species),lwd=0.082, alpha=0.83)+
  scale_fill_gradientn(colours = c("gray100","gray50","gray40","gray35","gray30","gray20","gray10","gray0"),name="Number of species")+
  #geom_polygon(data = grid_100k_shapefile_dataframe,aes(x=long, y=lat,group = group),lwd=0.082, alpha=0.6)+
  #geom_polygon(data = hellenic_borders_shapefile_dataframe,aes(x=long, y=lat,group = group),lwd=0.04,color="black")+
  #geom_polygon(data = natura2000shapefile_dataframe,aes(x=long, y=lat,group = group,fill=SITETYPE),lwd=0.082, alpha=0.6)+
  #geom_polygon(data = katafygia_agrias_zwhs_dataframe,aes(x=long, y=lat,group = group,fill="Wildlife Refuge"),lwd=0.082,alpha=0.8)+
  geom_polygon(data = greece_level_2_dataframe,aes(x=long, y=lat,group = group),fill=greece_level_2_dataframe$color_manual, color="white",lwd=0.2,show.legend = F, alpha=0.35)+
  geom_point(data = caves,aes(x=Longitude, y=Latitude,color=Cave_Type),size = 1.3)+
  labs(x="Longitude",y="Latitude")+
  ggtitle("Species richness")+
  #scale_fill_manual(values = c("chartreuse3","purple","cyan3","chocolate2"),labels = c("Natura2000 SCI", "Natura2000 SPA", "Natura2000 SPASCI","Wildlife Refuge"),name="Protected areas")+
  #scale_fill_manual(values = c("SCI"="chartreuse3","SPA"="purple","SPASCI"="cyan3", "Wildlife Refuge"="chocolate2"),labels = c("Natura2000 SCI", "Natura2000 SPA", "Natura2000 SPASCI","Wildlife Refuge")name="Protected areas")+
  scale_color_manual(name="Cave Types", values = c("Natural"="red","Artificial"="black", "Natural Modified"= "orange"))+  
  #scale_fill_manual(values = unique(greece_level_2_dataframe$color_manual))+
  scale_x_continuous(breaks = seq(18,30,1),limits = c(18,30))+
  scale_y_continuous(breaks = seq(35,42,1),limits = c(34.5,42))+
  coord_map(xlim = c(19,30.1), ylim = c(34.5,42))+
  theme_bw()+
  guides(colour = guide_legend(order = 1), 
              fill = guide_legend(order = 2))+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),legend.position = c(0.145, 0.15),legend.text = element_text(size=16,hjust = 0.5),legend.title = element_text(size=18,hjust = 0.5),axis.text=element_text(size = 16),plot.title = element_text(size=22),axis.title = element_text(size = 18),legend.title.align = 0.5,legend.box = "vertical")
  #guides(colour=guide_legend(override.aes=list(fill=unique(greece_level_2_dataframe$color_manual))))
#geom_text(data = sisquoc, aes(label = paste("  ", as.character(name), sep="")), angle = 60, hjust = 0, color = "yellow")

ggsave("map_greece_plot_lines_grid_species.png", plot = grid_10k_species_abundance_plot, device = "png",width = 30,height = 30,units = "cm",dpi = 300 ,path = "Plots/")
 
# gg_region_caves_color <- ggplot() +
#   geom_polygon(data = greece_level_2_dataframe,aes(x=long, y=lat,group = group,fill = Region),color="white",lwd=0.2,show.legend = F, alpha=0.8) +
#   #geom_path(size= 0.2,color = "white") +
#   #coord_equal() +
#   #geom_text(data=caves,aes(x=Longitude, y=Latitude,label = Cave_ID)) + 
#   geom_point(data = caves,aes(x=Longitude, y=Latitude,color=Cave_Type),size = 3,show.legend = F)+
#   scale_color_manual(name="", values = c("black","red"))+
#   scale_fill_manual(values = caves_Region$color_manual)+
#   scale_x_continuous(breaks = seq(18,30,1),limits = c(18,30))+
#   scale_y_continuous(breaks = seq(35,42,1),limits = c(34.5,42))+
#   coord_map(xlim = c(19,30.1), ylim = c(34.5,42))+
#   theme_bw()+
#   theme(panel.border = element_rect(colour = "black", fill=NA, size=0.8),panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.title = element_blank())
# 
# ggsave("caves_in_region_no_text_color.png", plot = gg_region_caves_color, device = "png",width = 28,height = 28,units = "in",dpi = 100 ,path = "Plots/")

#' 
#' ![Species abundance in Greece](Plots/map_greece_plot_lines_grid_species.png)
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------
# species endemic species richness

ggplot()+
  geom_polygon(data = grid_10k_endemic_species_abundance,aes(x=long, y=lat,group = group,fill=number_of_species),lwd=0.082, alpha=0.6)+
  geom_polygon(data = hellenic_borders_shapefile_dataframe,aes(x=long, y=lat,group = group),lwd=0.12,color="black")+
  geom_point(data = caves,aes(x=Longitude, y=Latitude,color="Caves"),size = 0.2)+
  labs(x="Longitude",y="Latitude")+
  ggtitle("Endemic species richness")+
  scale_color_manual(name="", values = c("Caves"="red"))+
  scale_fill_gradientn(colours = c("gray100",terrain.colors(10)),na.value = "grey50" ,name="Number of species")+ #c("gray100","gray50","gray40","gray35","gray30","gray20","gray10","gray0")
  scale_x_continuous(breaks = seq(18,30,1),limits = c(18,30))+
  scale_y_continuous(breaks = seq(35,42,1),limits = c(34.5,42))+
  coord_map(xlim = c(19,30.1), ylim = c(34.5,42))+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),legend.position = c(0.87, 0.75),legend.text = element_text(size=13),legend.title = element_text(size=14),axis.text=element_text(size = 16),plot.title = element_text(size=22),axis.title = element_text(size = 18))

 
 ggsave("map_greece_plot_lines_grid_endemic_species.png", plot = last_plot(), device = "png",width = 30,height = 30,units = "cm",dpi = 300 ,path = "Plots/")


#' 
#' ![Endemic species abundance in Greece](Plots/map_greece_plot_lines_grid_endemic_species.png)
#' 
#' 
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE, eval=TRUE----------------
# species classification

ggplot()+
  geom_polygon(data = grid_10k_species_abundance_classification,aes(x=long, y=lat,group = group,fill=number_of_species),lwd=0.082, alpha=0.6)+
  geom_polygon(data = hellenic_borders_shapefile_dataframe,aes(x=long, y=lat,group = group),lwd=0.12,color="black")+
  #geom_point(data = caves,aes(x=Longitude, y=Latitude,color="Caves"),size = 0.2)+
  labs(x="Longitude",y="Latitude")+
  #scale_color_manual(name="", values = c("Caves"="red"))+
   scale_fill_gradientn(colours = rev(rainbow(9)),
                         breaks = c(0, 5, 10, 15, 20, 25,30))+
  coord_map(xlim = c(19,30.1), ylim = c(34.5,42))+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),legend.text = element_text(size=9),legend.title = element_text(size=10)) +
  facet_wrap(~ Classification, ncol=3)

 
 ggsave("map_greece_plot_lines_grid_species_classification.png", plot = last_plot(), device = "png",width = 30,height = 30,units = "cm",dpi = 300 ,path = "Plots/")


#' 
#' ![Species abundance in Greece in respect to their classification](Plots/map_greece_plot_lines_grid_species_classification.png)
#' 
#' 
#' 
#' ## Underground systems
#' 
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------

YPOGEIA_YDATIKA_SYSTIMATA <- rgdal::readOGR("Shapefiles/YPOGEIA_YDATIKA_SYSTIMATA/GR_GWB_50K_GREECE.shp")

YPOGEIA_YDATIKA_SYSTIMATA_wgs84 <- spTransform(YPOGEIA_YDATIKA_SYSTIMATA, CRS("+proj=longlat +datum=WGS84"))

YPOGEIA_YDATIKA_SYSTIMATA_wgs84_data <- broom::tidy(YPOGEIA_YDATIKA_SYSTIMATA_wgs84)




#' 
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE, eval=FALSE,cache=TRUE----
## ggplot()+
##   geom_polygon(data = YPOGEIA_YDATIKA_SYSTIMATA_wgs84_data,aes(x=long, y=lat,group = group,fill="orange"),color="white",lwd=0.082, alpha=0.6)+
##   #geom_polygon(data = grid_100k_shapefile_dataframe,aes(x=long, y=lat,group = group),lwd=0.082, alpha=0.6)+
##   geom_polygon(data = hellenic_borders_shapefile_dataframe,aes(x=long, y=lat,group = group),lwd=0.12,color="black")+
##   #geom_polygon(data = natura2000shapefile_dataframe,aes(x=long, y=lat,group = group,fill=SITETYPE),lwd=0.082, alpha=0.6)+
##   #geom_polygon(data = katafygia_agrias_zwhs_dataframe,aes(x=long, y=lat,group = group,fill="Wildlife Refuge"),lwd=0.082,alpha=0.8)+
##   geom_point(data = caves,aes(x=Longitude, y=Latitude,color="Caves"),size = 0.2)+
##   labs(x="Longitude",y="Latitude")+
##   #scale_fill_manual(values = c("chartreuse3","purple","cyan3","chocolate2"),labels = c("Natura2000 SCI", "Natura2000 SPA", "Natura2000 SPASCI","Wildlife Refuge"),name="Protected areas")+
##  # scale_fill_manual(values = c("SCI"="chartreuse3","SPA"="purple","SPASCI"="cyan3", "Wildlife Refuge"="chocolate2"),labels = c("Natura2000 SCI", "Natura2000 SPA", "Natura2000 SPASCI","Wildlife Refuge"),name="Protected areas")+
##   scale_color_manual(name="", values = c("Caves"="red"))+
##   #scale_fill_gradient(low="white", high="red",breaks=seq(0,70,10), limits=c(0,70),name="Species")+
##   #scale_x_continuous(breaks = seq(20,30,1),limits = c(20,30))+
##   #scale_y_continuous(breaks = seq(35,42,1),limits = c(34.5,42))+
##   coord_map(xlim = c(19,30.1), ylim = c(34.5,42))+
##   theme_bw()+
##   theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),legend.position = c(0.87, 0.75),legend.text = element_text(size=9),legend.title = element_text(size=10))
## 
##  ggsave("YPOGEIA_YDATIKA_SYSTIMATA.png", plot = last_plot(), device = "png",width = 30,height = 30,units = "cm",dpi = 300 ,path = "Plots/")
## 

#' 
#' 
#' 
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE,eval=F--------------------
## # Geopark psiloritis
## 
## geopark_borders <- readOGR("Shapefiles/geopark_borders_mod/geopark_borders_mod.shp",verbose = T)
## 
## geopark_borders <- spTransform(x = geopark_borders,CRSobj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
## 
## over_geopark_borders <- over( x = Caves_Database_kml_to_txt_shapefile_wgs84 , y = geopark_borders , returnList = T) # This is from rgeos, it contains multiple matches, thats why returnList=T. VERY Important
## 
## caves_over_geopark_borders_d <-Caves_Database_kml_to_txt %>% left_join(bind_rows(over_geopark_borders,.id = "ID"),by=c("ID"="ID")) %>% filter(!is.na(Name))
## 
## geopark_census <- census_all_species_all_caves %>% filter(species_epithet!="sp.") %>% filter(Cave_ID %in% caves_over_geopark_borders_d$Cave_ID)
## 
## #write_delim(geopark_census,path = "/Users/savasp/Dropbox (INSPEE)/INSPEE Team Folder/Conservation of the cave fauna of Greece - MAVA/Cave_Fauna_database/Cave_Fauna_Database_Analysis/geopark_census.tsv",delim = "\t",col_names = T)
## 
## geopark_species <- geopark_census %>% distinct(Species,Order,Class,Classification,Locus_Typicus_Cave,Locus_Typicus_Cave_ID)
## 
## geopark_caves <- length(unique(geopark_census$Cave_ID))
## 
## geopark_greek_endemics <- geopark_census %>% filter(Distribution=="Endemic to Greece") %>% distinct(Species)
## 
## endemic_to_crete <- census_all_species_all_caves %>% filter(species_epithet!="sp.") %>% filter(Distribution=="Endemic to Greece") %>% distinct( Species,Region) %>% group_by(Species) %>% mutate(number_of_regions=n()) %>% filter(number_of_regions==1 & Region=="Kriti")
## 
## endemic_to_geopark_psiloritis <- census_all_species_all_caves %>% filter(species_epithet!="sp.") %>% filter(Distribution=="Endemic to Greece") %>% left_join(caves_over_geopark_borders_d,by=c("Cave_ID"="Cave_ID")) %>% distinct(Species,Cave_ID,Cave_Name,Name)
## 
## geopark_crete_endemics <- geopark_census %>% filter(Species %in% endemic_to_crete$Species) %>% filter(Distribution=="Endemic to Greece") %>% distinct( Species,Region) %>% group_by(Species) %>% mutate(number_of_regions=n()) %>% filter(number_of_regions==1 & Region=="Kriti")
## 
## caves_number_tr <- geopark_census %>% filter(Classification=="Troglobiont") %>% group_by(Cave_ID,Cave_Name) %>% summarise(number_of_species=n())
## 
## species_number_tr <- geopark_census %>% filter(Classification=="Troglobiont") %>% dplyr::select(Species,Cave_ID,Cave_Name)  %>% group_by(Species) %>% mutate(number_of_caves=n())
## 
## write_delim(x = species_number_tr,path = "species_number_tr_troglobionts.tsv",delim = "\t",col_names = T)
## 

#' 
#' # References

#' 
#' # IUCN Assessment
#' 
## ---- child='CFG_data_enrichment.Rmd'------------------------------------

#' 


#' With the taxize package we can resolve mispellings of species scientific names using the Global Names Resolver (GNR) service.
#' 
## library('taxizesoap')
## 
## aa <- get_pesiid((head(species_occurencies_unique_species$Species)))
## 
## species_resolve_names_Global_Name <- species_occurencies_unique_species %>% left_join(species_occurencies_unique_species_resolve_sum, by=c("Species"="user_supplied_name")) %>% dplyr::select(-species_epithet,-Species_bare_name,-submitted_name)
## 
## # write_delim(species_resolve_names_Global_Name,"species_resolve_names_Global_Names_Resolver.txt",delim = "\t",col_names = T)
#' ## Higher Taxonomy with R
## library(taxize)
## library(httr)
## 
#' ## Species occurencies-distributions from databases
#' ### From GBIF
#' Get the species ids from Gbif with rgbif package.
## library(rgbif)
## library(ISOcodes)
## library(spocc)
## keys_gbif <- sapply(splist, function(x) name_backbone(x,rank = "species"))
## keys_gbif_df <- data.frame(Data=unlist(keys_gbif), stringsAsFactors=FALSE) %>% mutate(Columns=rownames(.)) %>%
## mutate(Species=gsub("(.*)\\.(.*)", "\\1",Columns), Variables=gsub("(.*)\\.(.*)", "\\2",Columns)) %>% dplyr::select(-Columns) %>% spread(Variables,Data)
## 
#' Get the occurencies data from Gbif.
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE, eval=FALSE---------------
## keys_gbif_df <- read_delim("Species_with_Gbif_ids.txt",delim = "\t",col_names = T)
## 
## 
## start_time <- Sys.time()
## 
## gbif <- occ_data(taxonKey = na.omit(unique(keys_gbif_df$speciesKey)))
## 
## end_time <- Sys.time()
## 
## end_time-start_time
## #Time difference of 9.739269 mins
## 
## 
## list_dataframes <- list()
## 
## for(i in 1:length(gbif)){
## 
##   list_dataframes[[i]] <- gbif[[i]]$data
## 
## }
## 
## df <- do.call("bind_rows", list_dataframes)
## 
## df_in_our_database <- df %>% filter(scientificName %in% species_occurencies_unique_species$Species)
## 
## df_in_our_database_countries <- df %>% filter(scientificName %in% species_occurencies_unique_species$Species) %>% distinct(scientificName, country)%>% na.omit() %>% group_by(scientificName) %>% summarise(Countries_occurrences_GBIF=toString(country))
## 
## #write_delim(df_in_our_database_countries,"Species_with_Gbif_Occurencies.txt",delim = "\t")
## 
## sss <- species_occurencies %>% filter(species_epithet!="sp.") %>% distinct(Species,Distribution_IUCN)
## 
## sss2 <- sss %>% left_join(df_in_our_database_countries, by=c("Species"="scientificName")) %>% left_join(catalogue_life_taxa_in_Cave_fauna_Greece_dist,by=c("Species"="scientificName"))
## 
## sss3 <- sss2 %>% filter(!is.na(Countries_occurrences_GBIF) | !is.na(Distribution_IUCN) | !is.na(Distribution_CoT))
## 
## #write_delim(sss2,"Species_with_Distributions.txt",delim = "\t")
## 
## 

#' 
#' 
#' ### Species distributions IUCN
#' 
## ------------------------------------------------------------------------

species_occurencies_unique_species$speciesGSUB <- gsub(pattern = " ",replacement ="##",x = species_occurencies_unique_species$Species)



#' 
#' 
#' We downloaded the distribution countries for each species in our database from IUCN using the rredlist package.
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE, eval=FALSE---------------
## 
## IUCN_token <- "0ec43516c25669cfd876387ab7f755d1d3745a6f39f4ee5a00b96bac1579cb42"
## 
## library(rredlist)
## library(taxize)
## 
## #Time difference of 16.67948 mins
## start_time <- Sys.time()
## 
## sss <- species_occurencies_unique_species$Species_bare_name
## 
## species_IUCN <- lapply(sss,function(x) rl_search(name = x,key = IUCN_token))
## 
## 
## #iucn_species_summary <- sapply(sss,function(x) iucn_summary(sciname = x,key = IUCN_token))
## 
## #iucn_species_summary2 <- iucn_summary(sciname = sss,key = IUCN_token)
## 
## iucn_species_summary_dataframe <- as.data.frame(c())
## 
## to_df_iucn <- function(x){
## 
## y <- data.frame(Names=as.character(),Status=as.character(),Distribution=as.character(),Population_trend=as.character(),stringsAsFactors=FALSE)
## 
## for(i in 1:length(x)) {
## 
##   y[i,1] <- as.character(names(x[i]))
##   y[i,2] <- paste(as.character(x[[i]][1]),collapse = ";")
##   y[i,3] <- paste(as.character(x[[i]][3]),collapse = ";")
##   y[i,4] <- paste(as.character(x[[i]][4]),collapse = ";")
## }
##   y
## }
## 
## #iucn_species_summary_dataframe <- to_df_iucn(iucn_species_summary)
## 
## 
## x <- species_IUCN
## 
##   #y <- data.frame(Names=as.character(),Status=as.character(),Distribution=as.character(),Population_trend=as.character(),stringsAsFactors=FALSE)
## 
## dat <- data.frame(i=as.numeric(),names=as.character(),status=as.character(),stringsAsFactors=FALSE)
## 
## 
## for(i in 1:length(x)) {
## 
##   dat[i,1] <- i
##   dat[i,2] <- paste(as.character(x[[i]][1]))
##   dat[i,3] <- class(x[[i]]$result)=="data.frame"
## 
##   # y[i,1] <- as.character(names(x[i]))
##   # y[i,2] <- paste(as.character(x[[i]][1]),collapse = ";")
##   # y[i,3] <- paste(as.character(x[[i]][3]),collapse = ";")
##   # y[i,4] <- paste(as.character(x[[i]][4]),collapse = ";")
## 
## }
## 
##   #l <- unlist(x,recursive = F)
##   list_dataframes <- list(c())
##   list_with_true <- x[which(dat$status=="TRUE")]
## 
##   for (j in 1:length(list_with_true)) {
## 
##     list_dataframes[[j]] <- as.data.frame(list_with_true[[j]][2])
## 
##   }
## 
## Species_in_IUCN <- do.call("rbind",list_dataframes)
## 
## 
## end_time <- Sys.time()
## 
## end_time - start_time
## 
## 
## #write_delim(Species_in_IUCN,path = "Species_in_IUCN.txt",delim = "\t",col_names = T)
## 

#' 
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------
iucn_species_summary_dataframe <- read_csv(file = "iucn_species_summary_dataframe.csv",col_names = T) %>% distinct()
colnames(iucn_species_summary_dataframe)<-c("IUCN_Status","Distribution_IUCN","Population_trend_IUCN","Species_Name")

species_IUCN_enrichment <- species_occurencies_unique_species %>% left_join(.,iucn_species_summary_dataframe,by=c("Species_bare_name"="Species_Name")) %>% dplyr::select(Species,IUCN_Status,Distribution_IUCN,Population_trend_IUCN)

species_occurencies_unique_species_in_IUCN <- species_IUCN_enrichment %>% na.omit(IUCN_Status)

species_occurencies_unique_species_in_IUCN_table <- species_occurencies_unique_species_in_IUCN %>% left_join(species_occurencies_unique_species, by=c("Species"="Species")) %>% group_by(Order) %>% summarise(n=n())



#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------
species_occurencies_unique_species_in_IUCN_table_caption <- "Only 43 species of Cave Fauna of Greece database are evaluated in IUCN database"
kable(species_occurencies_unique_species_in_IUCN_table,caption=species_occurencies_unique_species_in_IUCN_table_caption, align = 'l')


#' 
#' 
#' IUCN database has `r nrow(species_occurencies_unique_species_in_IUCN)` species from our database.  
#' 
#' ### Species distributions from Catalogue of Life
#' 
#' From the Catalogue of Life.
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE, eval=FALSE---------------
## ## Load the data Catalogue of Life
## 
## 
## catalogue_life_description <- read_delim(file = "/Users/savasp/Desktop/archive-kingdom-animalia-bl3-2/description.txt",delim = "\t",col_names = TRUE)
## 
## catalogue_life_reference <- read_delim(file = "/Users/savasp/Desktop/archive-kingdom-animalia-bl3-2/reference.txt",delim = "\t",col_names = TRUE)
## 
## catalogue_life_distribution <- read_delim(file = "/Users/savasp/Desktop/archive-kingdom-animalia-bl3-2/distribution.txt",delim = "\t",col_names = TRUE)
## 
## catalogue_life_taxa <- read_delim(file = "/Users/savasp/Desktop/archive-kingdom-animalia-bl3-2/taxa.txt",delim = "\t",col_names = TRUE)
## 
## ## From our database
## 
## catalogue_life_taxa_in_Cave_fauna_Greece <- catalogue_life_taxa %>% filter(scientificName %in% unique(species_occurencies_unique_species$Species))
## 
## 
## catalogue_life_taxa_in_Cave_fauna_Greece_dist <- catalogue_life_distribution %>% filter(taxonID %in% catalogue_life_taxa_in_Cave_fauna_Greece$taxonID) %>% left_join(catalogue_life_taxa_in_Cave_fauna_Greece,by=c("taxonID"="taxonID")) %>% group_by(taxonID) %>% summarise(Distribution_CoT=toString(locality)) %>% left_join(catalogue_life_taxa_in_Cave_fauna_Greece,by=c("taxonID"="taxonID")) %>% distinct(scientificName,Distribution_CoT)
## 
## catalogue_life_taxa_in_Cave_fauna_Greece_reference <- catalogue_life_reference %>% filter(taxonID %in% catalogue_life_taxa_in_Cave_fauna_Greece$taxonID) %>% left_join(catalogue_life_taxa_in_Cave_fauna_Greece,by=c("taxonID"="taxonID"))
## 
## catalogue_life_taxa_in_Cave_fauna_Greece_description <- catalogue_life_description %>% filter(taxonID %in% catalogue_life_taxa_in_Cave_fauna_Greece$taxonID) %>% left_join(catalogue_life_taxa_in_Cave_fauna_Greece,by=c("taxonID"="taxonID"))
## 
## 

#' 
#' 
#' ## Species protection status
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE, eval=FALSE---------------
## species_protection_status <- read_xlsx(path = "Species_Protection_Status_for_Kaloust_Working_13.12.2107.xlsx",col_names = T,sheet = "SpeciesPS") %>% dplyr::select(-c(PhylumOrDivision, Class,Order,Family,Genus,species_epithet,Species_bare_name,Protected,H))
## 
## species_protection_status[species_protection_status=="NA"] <- NA
## 
## species_protection_status_categories <- t(read_xlsx(path = "Species_Protection_Status_for_Kaloust_Working_13.12.2107.xlsx",col_names = F,sheet = "Apend"))
## 
## species_protection_status$A <- with(species_protection_status,ifelse(is.na(A),NA_character_,species_protection_status_categories[3,1]))
## 
## species_protection_status$B <- with(species_protection_status,ifelse(is.na(B),NA_character_,species_protection_status_categories[3,2]))
## 
## species_protection_status$C <- with(species_protection_status,ifelse(is.na(C),NA_character_,ifelse(C=="IV",species_protection_status_categories[3,4],species_protection_status_categories[3,3])))
## 
## species_protection_status$D <- with(species_protection_status,ifelse(is.na(D),NA_character_,species_protection_status_categories[3,5]))
## 
## species_protection_status$E <- with(species_protection_status,ifelse(is.na(E),NA_character_,ifelse(E=="II",species_protection_status_categories[3,6],species_protection_status_categories[3,7])))
## 
## species_protection_status$F <- with(species_protection_status,ifelse(is.na(F),NA_character_,species_protection_status_categories[3,8]))
## 
## species_protection_status$G <- with(species_protection_status,ifelse(is.na(G),NA_character_,ifelse(G=="I/A",species_protection_status_categories[3,9],species_protection_status_categories[3,10])))
## 
## #species_protection_status$H <- with(species_protection_status,ifelse(is.na(H),NA_character_,species_protection_status_categories[3,11]))
## 
## #species_protection_status[is.na(species_protection_status)] <- "There is no species-specific legal framework for protection"
## 
## colnames(species_protection_status) <- c("Species","Protection_Law_86/1969","Protection_Presidential_Decree 67/1981","Protection_Habitats_Directive_(92/43/EEC)","Protection_Birds_Directive_(79/409/EEC)","Protection_Bern_Convection","Protection_Bonn_Convection","Protection_CITES")
## 
## #species_occurencies2 <- species_occurencies %>% left_join(species_protection_status, by=c("Species"="Species"))
## 
## #write_delim(x = species_occurencies2,path = "Cave_Fauna_of_Greece_Census_14_12_2017.txt",delim = "\t",col_names = T)
## 

#' 
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------
species_protection_status <- species_occurencies %>% dplyr::select("Species","Protection_Law_86/1969","Protection_Presidential_Decree 67/1981","Protection_Habitats_Directive_(92/43/EEC)","Protection_Birds_Directive_(79/409/EEC)","Protection_Bern_Convection","Protection_Bonn_Convection","Protection_CITES") %>% distinct(.)


#' 
#' 
#' ## Troglofauna characterisation
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE, eval=FALSE---------------
## 
## Troglofauna_characterisation <- data_frame(Troglofauna_characterisation_AB=c("tx","tx?","tph","tph?","tb","tb?","ac","ac?"),Troglofauna_characterisation=c("Trogloxene","Trogloxene?","Troglophile", "Troglophile?","Troglobiont", "Troglobiont?","Accidental","Accidental?") )
## 
## 
## species_troglofauna_characterisation <- species_occurencies %>% dplyr::select(Species,species_epithet,Type) %>% distinct(.) %>% mutate(duplicates=duplicated(Species,fromLast = F)) %>% mutate(duplicatesLast=duplicated(Species,fromLast = T)) %>% mutate(duplicatesAll=if_else(duplicates=="TRUE","TRUE",if_else(duplicatesLast=="TRUE","TRUE","FALSE"))) %>% dplyr::select(-duplicatesLast,-duplicates) %>% filter(!(duplicatesAll=="TRUE" & is.na(Type))) %>% mutate(duplicates=duplicated(Species,fromLast = F)) %>% mutate(duplicatesLast=duplicated(Species,fromLast = T)) %>% mutate(duplicatesAll=if_else(duplicates=="TRUE","TRUE",if_else(duplicatesLast=="TRUE","TRUE","FALSE"))) %>% dplyr::select(-duplicatesLast,-duplicates) %>% mutate(Troglofauna_characterisation_AB=if_else(duplicatesAll=="TRUE",NA_character_,Type)) %>% distinct(Species,Troglofauna_characterisation_AB) %>% left_join(Troglofauna_characterisation, by=c("Troglofauna_characterisation_AB"="Troglofauna_characterisation_AB"))
## 
## 
## #species_occurencies_unique_species_all
## #%>% filter(!(is.na(Type) & duplicatesAll=="TRUE")) %>% distinct() %>% mutate(duplicates=duplicated(Species,fromLast = F)) %>% mutate(duplicatesLast=duplicated(Species,fromLast = T)) %>% mutate(duplicatesAll_misspell=if_else(duplicates=="TRUE","TRUE",if_else(duplicatesLast=="TRUE","TRUE","FALSE")))
## 
## # species_occurencies_unique_species2 <- species_occurencies %>% dplyr::select(Phylum, Class,Order,Family,Genus,Species,species_epithet,Species_bare_name,Synonymes,Type,Speciment_Abbreviations,Species_comment,Comments) %>% distinct(.) %>% filter(species_epithet!="sp.") %>% dplyr::select(Species,Type)
## #
## # species_occurencies_unique_species222 <- species_occurencies %>% dplyr::select(Species,species_epithet,Type) %>% distinct(.)
## # #%>% filter(species_epithet!="sp.") %>% dplyr::select(Species,Type)   Synonymes,Type,Speciment_Abbreviations,Species_comment,Comments
## #
## 
## ########
## 
## # species_occurencies_unique_species2_type <- species_occurencies_unique_species2 %>% mutate(duplicates=duplicated(Species,fromLast = F)) %>% mutate(duplicatesLast=duplicated(Species,fromLast = T)) %>% mutate(duplicatesAll=if_else(duplicates=="TRUE","TRUE",if_else(duplicatesLast=="TRUE","TRUE","FALSE"))) %>% dplyr::select(-duplicatesLast,-duplicates) %>% filter(!(is.na(Type) & duplicatesAll=="TRUE")) %>% distinct() %>% mutate(duplicates=duplicated(Species,fromLast = F)) %>% mutate(duplicatesLast=duplicated(Species,fromLast = T)) %>% mutate(duplicatesAll_misspell=if_else(duplicates=="TRUE","TRUE",if_else(duplicatesLast=="TRUE","TRUE","FALSE")))
## #
## # species_occurencies_unique_species2_type_ok <- species_occurencies_unique_species2_type %>% mutate(., Type2 = if_else(duplicatesAll_misspell=="TRUE","NA",Type)) %>% filter(!(Type2=="NA" & duplicates=="TRUE")) %>% dplyr::select(Species, Type2)
## #
## # species_occurencies_unique_species2_type_ok[species_occurencies_unique_species2_type_ok=="NA"] <- NA
## # colnames(species_occurencies_unique_species2_type_ok) <- c("Species", "Troglofauna_characterisation_AB")
## #
## #
## # species_occurencies_unique_species1 <- species_occurencies_unique_species %>% left_join(.,species_occurencies_unique_species2_type_ok, by=c("Species"="Species")) %>% left_join(.,Troglofauna_characterisation, by=c("Troglofauna_characterisation_AB"="Troglofauna_characterisation_AB"))
## 
## 
## 

#' 
#' Summary table with the species characterisations. 
#' 
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------
species_troglofauna_characterisation <- species_occurencies %>% distinct(Species,species_epithet,Troglofauna_characterisation)

species_occurencies_unique_species_troglofauna_summary <- species_troglofauna_characterisation %>% group_by(Troglofauna_characterisation) %>% summarise(number_of_species=n())

species_occurencies_unique_species_troglofauna_summary_captio <- "Number of species per troglofauna characterisation"
kable(species_occurencies_unique_species_troglofauna_summary,caption=species_occurencies_unique_species_troglofauna_summary_captio, align = 'l')

#' 
#' 
#' ## Species hyperlinks in databases
#' 
#' 
## ----warning=FALSE, message=FALSE, echo=FALSE, eval=FALSE----------------
## 
## species_Conservation_links_file <-  species_occurencies %>% dplyr::select(Species,CheckIUCNRedList,CheckPESI,CheckFaunaEuropaea,CheckGBIF,CheckNCBI) %>% distinct(.)
## 
## 
## species_Conservation_links <- species_occurencies_unique_species %>% dplyr::select(Species) %>% left_join(.,species_Conservation_links_file, by=c("Species"="Species"))
## 
## species_links_summary <- data_frame(Source=c("CheckFaunaEuropaea","CheckNCBI","CheckGBIF","CheckPESI","CheckIUCNRedList"), Number_of_links=c(sum(!is.na(species_Conservation_links$CheckFaunaEuropaea)),sum(!is.na(species_Conservation_links$CheckNCBI)),sum(!is.na(species_Conservation_links$CheckGBIF)),sum(!is.na(species_Conservation_links$CheckPESI)),sum(!is.na(species_Conservation_links$CheckIUCNRedList))), Missing_links=c(nrow(species_occurencies_unique_species)-sum(!is.na(species_Conservation_links$CheckFaunaEuropaea)),nrow(species_occurencies_unique_species)-sum(!is.na(species_Conservation_links$CheckNCBI)),nrow(species_occurencies_unique_species)-sum(!is.na(species_Conservation_links$CheckGBIF)),nrow(species_occurencies_unique_species)-sum(!is.na(species_Conservation_links$CheckPESI)),nrow(species_occurencies_unique_species)-sum(!is.na(species_Conservation_links$CheckIUCNRedList))))
## 
## species_links_summary_caption <- "Links to different databases for individual species"
## kable(species_links_summary,caption=species_links_summary_caption, align = 'l')
## 
## ####
## 
## # links_Giannis <- read_xlsx("species_for_links_OK_15_01_2018_GIANNIS.xlsx",col_names = T) %>% dplyr::select(Species, CheckGBIF)
## #
## # links_Kaloust <- read_xlsx("species_for_links_OK_15_01_2018_Kaloust.xlsx",col_names = T) %>% dplyr::select(Species,CheckFaunaEuropaea,CorrectSpeciesName)
## # links_Savvas <- read_xlsx("species_for_links_OK_15_01_2018_Savvas.xlsx",col_names = T) %>% dplyr::select(Species,CheckPESI)
## #
## # species_links <- links_Kaloust %>% left_join(links_Giannis, by=c("Species"="Species")) %>% left_join(links_Savvas, by=c("Species"="Species")) %>% mutate(CorrectSpeciesName=if_else(is.na(CorrectSpeciesName),Species,CorrectSpeciesName))
## #
## # species_links[species_links=="NA"] <- NA
## #
## # colnames(species_links)[1] <- "Species_2018_2"
## # colnames(species_links)[3] <- "Species"
## 
## #colnames(species_occurencies)[38] <- "Species_2018_2"
## 
## #species_occurencies_links <- species_occurencies %>% dplyr::select(-c(CheckPESI,CheckFaunaEuropaea,CheckGBIF)) %>% left_join(species_links,by=c("Species_2018_2"="Species_2018_2"))
## 
## #write_delim(species_occurencies_links,path = "Cave_Fauna_of_Greece_Census_v4_17_01_2018.txt",col_names = T,delim = "\t")
## 
## # NCBI LINKS
## # first we find the NCBI ids with the taxise package and afterwards we create the links. According to their site in this link https://www.ncbi.nlm.nih.gov/Taxonomy/taxonomyhome.html/index.cgi?chapter=howlink we use the ids and a url prefix to create the urls.
## 
## species_names <- as.vector(species_occurencies_unique_species$Species_bare_name)
## 
## species_uid <- sapply(species_names, function(x) get_uid(x,verbose = T,ask=F))
## 
## species_uid_df <- data.frame(Species_bare_name=names(species_uid), species_uid=as.numeric(species_uid),row.names = NULL)
## 
## species_uid_df_no_na <-  species_uid_df %>% na.omit() %>% mutate(NCBI_links=paste("https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?id=",species_uid,sep = "")) %>% left_join(species_occurencies_unique_species_all, by=c("Species_bare_name"="Species_bare_name"))
## 
## write_delim(species_uid_df_no_na,path = "species_uid_df_no_na.txt",delim = "\t")
## 
## # Open all tabs
## 
## #sapply(species_uid_df_no_na$NCBI_links,FUN = browseURL)
## 
## species_occurencies2 <- species_occurencies %>% left_join(species_uid_df_no_na, by=c("Species_bare_name"="Species_bare_name"))
## 
## species_Conservation_linksNCBI <- species_Conservation_links %>% dplyr::select(Species,CheckNCBI) %>% na.omit
## 
## 

#' 
#' 
#' In the case of IUCN database there are missing links because these species are not included in the databese. This has to be clarified for the rest databases. 
#' 
#' 
#' Get ready the species data.
## ------------------------------------------------------------------------

# species_Data_Cave_Fauna_Greece <- species_occurencies_unique_species %>% left_join(species_troglofauna_characterisation, by=c("Species"="Species")) %>% left_join(species_IUCN_enrichment, by=c("Species"="Species")) %>% left_join(species_Conservation_links, by=c("Species"="Species"))

#write_delim(x = species_Data_Cave_Fauna_Greece,path = "Cave_Fauna_of_Greece_Species_10_12_2017.txt",delim = "\t",col_names = T)


#' 
#' ## Last additions
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE, eval=FALSE---------------
## 
## species_occurencies_only_sp2 <- species_occurencies_only_sp %>% dplyr::select(Species) %>% mutate(Species_2018=Species)
## 
## species_troglofauna_distributions <- read_xlsx(path = "Species_Chorology_and Troglofauna characterisation_11.01.2018.xlsx",col_names = T) %>% mutate(Species_2018=ifelse(is.na(`Correct Species name`),paste0(Species), paste0(`Correct Species name`))) %>% dplyr::select(Species,Species_2018,COMMENTS,Troglofauna_characterisation,Distribution) %>% bind_rows(.,species_occurencies_only_sp2)
## 
## colnames(species_troglofauna_distributions)[3] <- "Species_Comments_2018"
## colnames(species_troglofauna_distributions)[5] <- "Distribution_INSPEE"
## 
## distribution_INSPEE <- species_troglofauna_distributions %>% group_by(Distribution_INSPEE) %>% summarise(Dist=n())
## 
## 

#' 
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE, eval=FALSE---------------
## ## New species occurences file with species!!
## species_greek_red_data <- read_xlsx("Species_FOR_GREEK_RED_DATA_BOOK_plus_Beron.xlsx",col_names = T) %>% dplyr::select(Species,Status_Greek_Red_Data_Book)
## 
## species_occurencies_dist_troglo <- species_occurencies %>% dplyr::select(-Troglofauna_characterisation) %>% left_join(species_troglofauna_distributions, by=c("Species_Old"="Species")) %>% dplyr::filter(Species_2018!="DELETE") %>% left_join(species_greek_red_data, by=c("Species_Old"="Species"))
## 
## species_occurencies_dist_troglo$Genus <- as.character(lapply(strsplit(as.character(species_occurencies_dist_troglo$Species_2018), split=" "), "[", n=1))
## 
## species_Data_Cave_Fauna_Greece <- species_occurencies_dist_troglo %>% dplyr::select(Phylum,Class,Order,Family,Genus,Species_2018,CheckIUCNRedList,CheckPESI,CheckNCBI,CheckFaunaEuropaea) %>% distinct(.) %>% mutate(duplicates=duplicated(Species_2018,fromLast = F)) %>% mutate(duplicatesLast=duplicated(Species_2018,fromLast = T)) %>% mutate(duplicatesAll_misspell=if_else(duplicates=="TRUE","TRUE",if_else(duplicatesLast=="TRUE","TRUE","FALSE"))) %>% dplyr::select(-c(duplicatesLast,duplicates)) %>% filter(!(duplicatesAll_misspell=="TRUE" & is.na(CheckPESI) & is.na(CheckNCBI) & is.na(CheckFaunaEuropaea))) %>% filter(!(Species_2018=="Eurygyrus oertzeni (Verhoeff, 1901)" & is.na(CheckNCBI) & is.na(CheckFaunaEuropaea))) %>% dplyr::select(-c(Phylum,Class,Order,Family,Genus,duplicatesAll_misspell))
## 
## species_higher_taxonomy <- read_xlsx("Cave_fauna_of_greece_species_for_DATABASE_15_01_2018_matched-FINAL.xlsx", col_names = T) %>% mutate(Species_ready=if_else(is.na(CorrectSpeciesName),ScientificName,CorrectSpeciesName )) %>% dplyr::select(-CorrectSpeciesName) %>% left_join(species_Data_Cave_Fauna_Greece, by=c("ScientificName"="Species_2018")) %>% dplyr::select(-ScientificName) %>% distinct(.) %>% mutate(duplicates=duplicated(Species_ready,fromLast = F)) %>% mutate(duplicatesLast=duplicated(Species_ready,fromLast = T)) %>% mutate(duplicatesAll_misspell=if_else(duplicates=="TRUE","TRUE",if_else(duplicatesLast=="TRUE","TRUE","FALSE"))) %>% filter(!(duplicatesAll_misspell=="TRUE" & is.na(CheckPESI) & is.na(CheckNCBI) & is.na(CheckFaunaEuropaea))) %>% filter(!(Species_ready=="Acanthopetalum cycladicum (Verhoeff, 1901)" & is.na(CheckFaunaEuropaea))) %>% dplyr::select(-c(duplicates,duplicatesAll_misspell,duplicatesLast))
## 
## species_higher_taxonomy_ready <- read_xlsx("Cave_fauna_of_greece_species_for_DATABASE_15_01_2018_matched-FINAL.xlsx", col_names = T) %>% mutate(Species_ready=if_else(is.na(CorrectSpeciesName),ScientificName,CorrectSpeciesName )) %>% dplyr::select(-c(CorrectSpeciesName,Phylum,Class,Order,Family,Genus)) %>% left_join(species_higher_taxonomy,by=c("Species_ready"="Species_ready"))
## 
## species_occurencies_species_data_all <- species_occurencies_dist_troglo %>% dplyr::select(-c(Phylum,Class,Order,Family,Genus,CheckIUCNRedList,CheckPESI,CheckNCBI,CheckFaunaEuropaea)) %>% left_join(species_higher_taxonomy_ready, by=c("Species_2018"="ScientificName"))
## 
## species_for_links <- species_occurencies %>% distinct(Species,CheckIUCNRedList,CheckGBIF,CheckPESI,CheckNCBI,CheckFaunaEuropaea) %>% mutate(duplicates=duplicated(Species,fromLast = F)) %>% mutate(duplicatesLast=duplicated(Species,fromLast = T)) %>% mutate(duplicatesAll_misspell=if_else(duplicates=="TRUE","TRUE",if_else(duplicatesLast=="TRUE","TRUE","FALSE"))) %>% filter(!(duplicatesAll_misspell=="TRUE" & is.na(CheckGBIF))) %>% dplyr::select(-c(duplicatesAll_misspell,duplicatesLast,duplicates)) %>% mutate(duplicates=duplicated(Species,fromLast = F)) %>% mutate(duplicatesLast=duplicated(Species,fromLast = T)) %>% mutate(duplicatesAll_misspell=if_else(duplicates=="TRUE","TRUE",if_else(duplicatesLast=="TRUE","TRUE","FALSE"))) %>% mutate(Species_for_Search=Species)
## 
## #write_delim(x = species_for_links,path = "species_for_links_15_01_2018.txt",delim = "\t",col_names = T)
## 

#' 
#' # Caves in database
#' 
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE, eval=FALSE---------------
## 
## cave_references <- read_xlsx(path = "Cave_references_FOR_DATABASE_09_01_2018.xlsx",col_names = T)
## 
## 
## caves_with_references <- read_xlsx(path = "Caves_for_Description_11_01_2018.xlsx", col_names = T)
## 
## caves_with_references$Cave_References <- gsub(pattern = ", ",replacement = ";",x = caves_with_references$Cave_References)
## 
## 
## cave_short_references <- strsplit(x = caves_with_references$Cave_References,split = ";")
## cave_short_references_df <- data_frame(Cave_References=unlist(cave_short_references), Cave_ID=rep.int(caves_with_references$Cave_ID,times = sapply(cave_short_references,length)),CaveName=rep.int(caves_with_references$CaveName,times = sapply(cave_short_references,length))) %>% na.omit()
## 
## cave_short_references_df_toString <- cave_short_references_df %>% group_by(Cave_ID) %>% summarise(Cave_References=paste(Cave_References,collapse = ";"))
## 
## # Which cave reference annotations doesn't appear in the cave references file.
## 
## mistakes_cave_short_references <- cave_short_references_df[which(!(cave_short_references_df$Cave_References %in% cave_references$Cave_Short_Reference)),]
## 
## cave_short_references_used <- cave_short_references_df[which((cave_short_references_df$Cave_References %in% cave_references$Cave_Short_Reference)),]
## 
## 
## 

#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE, eval=FALSE---------------
## ## Update with caves.
## 
## caves_last_mistakes <- read_xlsx("caves_last_mistakes_12_01_2017.xlsx",col_names = T) %>% mutate(Cave_ID_2018=ifelse(is.na(`Correct Cave`),paste0(Cave_ID), paste0(`Correct Cave`)))
## 
## species_occurencies_unique_caves_all_info_file <- read_xlsx("Cave_Fauna_of_Greece_Caves_10_12_2017.xlsx", col_names = T) %>% left_join(cave_short_references_df_toString, by=c("Cave_ID"="Cave_ID"))
## 
## species_occurencies_unique_caves_all_info_N <- species_occurencies_unique_caves_all_info_file %>% dplyr::select(-c(CaveName,NearestVillage,Municipality,Island,County,Region,Latitude,Longitude,Municipalities_ISO_843)) %>% filter(Cave_ID %in% caves_last_mistakes$Cave_ID) %>% left_join(caves_last_mistakes, by=c("Cave_ID"="Cave_ID"))
## 
## species_occurencies_unique_caves_all_info_2018 <- species_occurencies_unique_caves_all_info_file %>% filter(!(Cave_ID %in% caves_last_mistakes$Cave_ID)) %>% bind_rows(species_occurencies_unique_caves_all_info_N)
## 
## species_occurencies_unique_caves_all_info_2018_occ <- species_occurencies_unique_caves %>% mutate(Cave_ID_OLD=Cave_ID, Census_Merge=paste(Species,Cave_ID_OLD,CaveName,Species_references,sep = ";")) %>% dplyr::select(-which(colnames(species_occurencies_species_data_all) %in% colnames(species_occurencies_unique_caves_all_info_2018))) %>% left_join(species_occurencies_unique_caves_all_info_2018, by=c("Cave_ID_OLD"="Cave_ID")) %>% mutate(Delete=ifelse(is.na(Delete), "Keep",Delete)) %>% filter(Delete!="Delete") %>% mutate(Cave_ID_2018=ifelse(is.na(Cave_ID_2018), Cave_ID_OLD,Cave_ID_2018)) %>% dplyr::select(-c(Delete, `Correct Cave`))
## 
## #write_delim(species_occurencies_unique_caves_all_info_2018_occ,path = "Cave_Fauna_of_Greece_Census_v2_15_01_2018.txt",col_names = T,delim = "\t")
## 
## caves_Cave_Fauna_of_Greece <- species_occurencies_unique_caves_all_info_2018_occ %>% distinct(Cave_ID_2018, CaveName, Longitude, Latitude)
## 

#' 
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE, eval=FALSE---------------
## 
## caves_new_names <- read_xlsx(path = "DataBaseCavesWORKING_DOC_04_12_2017.xlsx",col_names = TRUE) %>% unite(col = Merged,OldCaveName, NearestVillage, Cave_Comment,sep = ";",remove = F) %>% mutate(New_names=if_else(is.na(CorrectCaveName),paste(OldCaveName),paste(CorrectCaveName)),Location_status2=if_else(is.na(Location_status),"Location known",paste(Location_status)),Correct_CaveID_2=if_else(is.na(Correct_CaveID),paste(CaveID),paste(Correct_CaveID)))
## 
## caves_new_names$Cave_Comment <- ifelse(caves_new_names$Cave_Comment=="NA",NA,paste(caves_new_names$Cave_Comment))
## 
## caves_new_names$Cave_Comment_2 <- ifelse(caves_new_names$Cave_Comment_2=="NA",NA,paste(caves_new_names$Cave_Comment_2))
## 
## 
## caves_Correct_CaveID <- caves_new_names %>% dplyr::select(Correct_CaveID_2) %>% left_join(caves_new_names,by=c("Correct_CaveID_2"="CaveID")) %>%  dplyr::select(-Correct_CaveID_2.y, -Correct_CaveID, -NearestVillage, -Merged,-OldCaveName,-CorrectCaveName,-Location_status) %>% distinct() %>% mutate(Cave_comments= if_else(!is.na(Cave_Comment_2) & is.na(Cave_Comment), paste(Cave_Comment_2),if_else(is.na(Cave_Comment_2) & !is.na(Cave_Comment), paste(Cave_Comment),if_else(is.na(Cave_Comment_2) & is.na(Cave_Comment),paste(NA),if_else(Cave_Comment_2==Cave_Comment,paste(Cave_Comment),paste(Cave_Comment_2, Cave_Comment, sep = ";")))))) %>% unite(Cave_comments, Cave_comments,COMMENT,sep = ";",remove = TRUE) %>% dplyr::select(-Cave_Comment_2,-Cave_Comment)
## 
## colnames(caves_Correct_CaveID) <- c("Cave_ID","Cave_comments", "Latitude","Longtitude","Altitude","Nearest_Village","Municipality","County","Region","Cave_Name", "Location_Status")
## 
## #caves_Correct_CaveID <- caves_Correct_CaveID[,c(1,10,2,3,4,5,6,7,8,9,11)]
## 
## caves_Correct_CaveID$Cave_comments <- ifelse(caves_Correct_CaveID$Cave_comments=="NA;NA" |caves_Correct_CaveID$Cave_comments=="NA",NA,paste(caves_new_names$Cave_Comment))
## 
## 
## #allagi onomaton spilaion
## 
## # species_occurencies_unique_caves_s <- species_occurencies_unique_caves %>% unite(col = Merged,CaveName, NearestVillage,Cave_Comment,sep = ";",remove = T) %>% dplyr::select(-Municipality,-County,-Region)
## #
## # caves_merged <- species_occurencies_unique_caves_s %>% left_join(caves_new_names,by=c("Merged"="Merged"))
## 
## cave_ids <- caves_new_names %>% dplyr::select(CaveID,Correct_CaveID_2,Merged)
## 
## species_occurencies_new_caves <- species_occurencies %>% unite(col = Merged,CaveName, NearestVillage,Cave_Comment,sep = ";",remove = T) %>% dplyr::select(-Municipality,-County,-Region) %>% left_join(cave_ids,by=c("Merged"="Merged")) %>% dplyr::select(-CaveID) %>% left_join(caves_Correct_CaveID,by=c("Correct_CaveID_2"="Cave_ID"))
## 
## 
## #write_delim(species_occurencies_new_caves,"species_occurencies_new_caves.txt",delim = "\t",col_names = T)
## 
## 

#' 
#' 
#' 
#' ## Cave references
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------
# Get ready the cave references file

# cave_references$Year <- as.numeric(gsub(pattern = "\\.",replacement = "",x = cave_references$Year))
# 
# cave_references$Journal <- gsub(pattern = "\\,",replacement = "",x = cave_references$Journal)
# 
# cave_references$Pages <- gsub(pattern = "\\.",replacement = "",x = cave_references$Pages)
# 
# cave_references$Title <- gsub(pattern = "\\.",replacement = "",x = cave_references$Title)
# cave_references$Title <- paste0(cave_references$Title,".")
# 
# write_delim(cave_references,path = "Cave references FOR DATABASE_09_01_2018.txt",delim = "\t",col_names = T)


#' 
#' 
#' We are using `r length(unique(species_occurencies$Cave_References))` cave references. There are `r length(which(is.na(unique(species_occurencies$Cave_References))))` without reference.
#' 
#' 
#' # References
#' 
#' ## References from Mendeley
#' Get ready the references for the database.
#' 
## #Transform the .ris file to txt

#' 
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------
# Instead of using JabRef!!! Do this:
# From txt long to dataframe wide.
references_from_ris <- read_delim(file = "Cave_Fauna_References_23_01_2018.txt",delim = "@",col_names = FALSE)

references_from_ris$X1 <- trimws(references_from_ris$X1,which = "right")

references_from_ris <- references_from_ris %>% mutate(id=cumsum(if_else(X1=="ER",1,0))) %>% group_by(id,X1) %>% summarise(Value=paste0(X2,collapse = ";")) %>% ungroup() 

references_from_ris <- references_from_ris[!references_from_ris$id==max(references_from_ris$id),]

references_from_ris_wide <- references_from_ris %>% spread(X1,Value)


## colnames

colnames(references_from_ris_wide) <- c("Id","Author","Place_Published","DOI","Editor","End_Page","ER","Edition","Issue","Journal","Keywords", "Link_to_PDF","Notes","Abstract","Publisher","ISBN","Start_Page","Title","Type","URL","Volume","Year")

#sss <- c("id" ,"A1", "CY", "DO", "ED", "EP", "ER", "ET", "IS", "JF", "KW", "L1", "N1", "N2", "PB", "SN", "SP", "T1","TY" ,"UR", "VL", "Y1")

references_from_ris_wide <- references_from_ris_wide[,c(1,2,18,22,10,21,9,17,6,5,8,15,3,16,4,19,20,12,13,14,11,7)]

references_from_ris_wide$Year <- trimws(gsub("///","",references_from_ris_wide$Year),which = "left")



#' 
#' ## Creation of short references
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------
## Create unique short references

references_from_ris_wide$n_authors <- str_count(references_from_ris_wide$Author, pattern = ";") +str_count(references_from_ris_wide$Author, pattern = "&") +1

references_from_ris_wide$first_author <- gsub("^(.*?),.*", "\\1", references_from_ris_wide$Author)

references_from_ris_wide$second_author <- gsub(pattern = ".*;(.+?),.*",replacement = "\\1",references_from_ris_wide$Author)  #"; *([^.,]*)"


references_from_ris_wide$Pages <- with(references_from_ris_wide, ifelse(Start_Page==End_Page,paste0(Start_Page),paste0(Start_Page,"-",End_Page)))

references_from_ris_wide$short_reference <- with(references_from_ris_wide,ifelse(n_authors==1,paste0(first_author," ",Year),ifelse(n_authors==2,paste0(first_author," & ",second_author," ",Year),paste0(first_author," et al. ",Year))))

species_references_ready_ris <- references_from_ris_wide %>% group_by(short_reference) %>% mutate(n_dupl_citation=letters[1:n()]) %>% mutate(n_dupl_citation_number=n()) %>% mutate(short_reference_ready=if_else(n_dupl_citation_number==1,paste0(short_reference),paste0(short_reference,n_dupl_citation))) %>% ungroup() %>% dplyr::select(-short_reference,-second_author,-Start_Page,-End_Page,-n_authors,-first_author,-second_author,-n_dupl_citation,-n_dupl_citation_number,-ER)

## Initials of Authors 

species_references_author <- strsplit(x = as.character(species_references_ready_ris$Author), "[;]")

species_references_author_spreaded <- data.frame(short_reference_ready = rep.int(species_references_ready_ris$short_reference_ready,sapply(species_references_author, length)),Author = rep.int(species_references_ready_ris$Author,sapply(species_references_author, length)), Author_spread = gsub("^\\s|\\s$", "",unlist(species_references_author))) 


Last_names <- gsub(",.*", "\\1", species_references_author_spreaded$Author_spread,perl = TRUE)

sort_names_initials <- iconv(gsub("[:a-z:]|\\s|\\.|[[:punct:]]", "", gsub(".*,", "\\1", species_references_author_spreaded$Author_spread,perl = TRUE)), "UTF-8", "ASCII", sub = "")
 #\\B\\w

sort_names_initials_ready <- trimws(gsub("([A-Z])", "\\1. ", sort_names_initials),which = "right")

species_references_author_spreaded$Author_spread_Initials <- paste0(Last_names,", ",sort_names_initials_ready)

species_references_author <- species_references_author_spreaded %>% group_by(short_reference_ready) %>% summarise(Author=paste0(Author_spread_Initials,collapse="; ")) %>% mutate(Author=gsub(";",",",gsub("(.*)\\;(.*)", "\\1 &\\2",Author)))


## Combine and Ready

species_references_ready_ris_final <- species_references_ready_ris %>% dplyr::select(-Author) %>% left_join(.,species_references_author,by=c("short_reference_ready"="short_reference_ready"))

species_references_ready_ris_final$Year <- as.numeric(species_references_ready_ris_final$Year)


species_references <- species_references_ready_ris_final

#write_delim(x = species_references,path = "Cave_Fauna_of_Greece_References_23_01_2018.txt",col_names = T,delim = "\t")


#' 
#' 
#' ## Samplings over time
#' 
#' 
## ----warning=FALSE, message=FALSE, echo=FALSE----------------------------


species_occurencies_2 <- strsplit(x = as.character(species_occurencies$Species_references), ";")

species_references_spreaded <- data.frame(Census_ID = rep.int(species_occurencies$Census_ID, sapply(species_occurencies_2, length)), Species_references_spread = unlist(species_occurencies_2)) %>% group_by(Census_ID) 

species_references_spreaded_unique <- species_references_spreaded %>% group_by(Species_references_spread) %>% summarise(ref_count=n()) %>% ungroup() %>% na.omit()

which(!(species_references_spreaded_unique$Species_references_spread %in% species_references$short_reference_ready),useNames = TRUE)

Cave_Fauna_of_Greece_Census_LONG <- species_references_spreaded %>% left_join(species_occurencies,by=c("Census_ID"="Census_ID")) %>% left_join(species_references,by=c("Species_references_spread"="short_reference_ready")) %>% ungroup()

Cave_Fauna_of_Greece_Census_LONG_small <- Cave_Fauna_of_Greece_Census_LONG %>% dplyr::select(Cave_ID,CaveName,Species_references_spread, Species)

Cave_Fauna_of_Greece_Census_LONG_small2 <- Cave_Fauna_of_Greece_Census_LONG_small %>% unite(col =merged, Cave_ID,CaveName,Species_references_spread, Species,sep = ";",remove = F) %>% mutate(duplicates=duplicated(merged)) %>% mutate(duplicatesLast=duplicated(merged,fromLast = T)) %>% mutate(duplicatesAll=if_else(duplicates=="TRUE","TRUE",if_else(duplicatesLast=="TRUE","TRUE","FALSE")))

which(duplicated(Cave_Fauna_of_Greece_Census_LONG_small2))

#write_delim(dd,"lindberg1955.txt", delim = "\t",col_names = T)

species_of_Beron <- Cave_Fauna_of_Greece_Census_LONG %>% filter(Species_references_spread=="Beron 2016" & species_epithet!="sp.") %>% distinct(Species,CaveName, Cave_ID) #, CaveName, Cave_ID,NearestVillage,Municipality


#' 
## ----warning=FALSE, message=FALSE, echo=FALSE,eval=FALSE-----------------
## ## Create the new file Census with new references
## ## old file
## 
## old_references_used <- read_csv(file = "Old_files_database/Cave_Fauna_References28_08_2017.csv",col_names = T) %>% inner_join(.,species_references_spreaded_unique ,by=c("Identifier"="Reference_spread")) %>% left_join(.,species_references, by=c("Title"="Title")) %>% mutate(Author.x2=gsub(replacement = ",",";",Author.x)) %>% dplyr::select(Identifier, Author.x,Author.x2,Author.y,Year.x,Year.y,Title,Journal.x,Journal.y,Pages.x,Pages.y,Id,short_reference_ready)
## 
## old_references_used_dictionary <- old_references_used %>% dplyr::select(Identifier,short_reference_ready)
## 
## species_references_spreaded_new <- species_references_spreaded %>% left_join(., old_references_used_dictionary, by=c("Reference_spread"="Identifier"))
## 
## species_references_new_reference <- species_references_spreaded_new %>% group_by(Species,CaveName,NearestVillage,Cave_Comment,Number_references) %>% summarise(Species_references=paste0(short_reference_ready,collapse = ";"))
## 
## ### New species occurencies file
## 
## 
## species_occurencies_new <- species_occurencies %>% left_join(., species_references_new_reference)
## 
## #write_excel_csv(species_occurencies_new,path = "Cencus_Cave_fauna_of_Greece_v31_13_11_2017.txt")

#' 
#' 
#' There are `r sum(is.na(species_references$Year))` with no year data. In species occurences`r length(unique(species_references_spreaded$Reference_spread))` references where used from a total of `r length(unique(species_references$Identifier))` references. 
#' 
#' * Publications over time.
#' 
#' In the database the references will have the following format: 
#' 
#' Journals
#' 
#' Author. Year. Title. Journal, Volume (Number): Pages.
#' 
#' Books
#' 
#' Author. Year. Booktitle. Publisher. Pages.
#' 
#' 
#' # Data analysis and visualisation
#' ## Caves and species bar plots
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------

ggplot()+
  geom_col(data = caves_Region, aes(x=Region, y= number_of_caves, fill=Region),show.legend = F)+
  geom_text(data = caves_Region,aes(x =Region,y= number_of_caves, label=number_of_caves), position=position_dodge(width=0.7), vjust=-0.25,size=2.8)+
  scale_y_continuous(breaks = seq(0,200,50),limits = c(0,200))+
  #ggtitle("Caves Greece")+
  labs(x="Administrative Region", y= "Number of caves")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1))


ggsave("caves_Region_barplot.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")



#' 
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------

ggplot()+
  geom_col(data = species_class, aes(x=Class, y= number_of_species, fill=Class),show.legend = F)+
  geom_text(data = species_class,aes(x =Class,y= number_of_species, label=number_of_species), position=position_dodge(width=0.7), vjust=-0.25,size=2.8)+
  scale_y_continuous(breaks = seq(0,300,50),limits = c(0,300))+
  #ggtitle("Inferring methods of the Sign Score of the PPI network of Drosophila gene")+
  labs(x="Class", y= "Number of species")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("species_class_barplot.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")



#' 
#' 
#' * bar plots with Arachnida, Insecta, Malacostrata orders
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE,fig.height=15,fig.width=10----

most_abudant_classes <- species_occurencies_unique_species %>% filter(Class=="Arachnida" | Class=="Insecta" | Class=="Malacostraca") %>% group_by(Class,Order) %>% summarise(number_of_species=n())

ggplot()+
  geom_col(data = most_abudant_classes, aes(x=Order, y= number_of_species, fill=Order),show.legend = F)+
  geom_text(data = most_abudant_classes,aes(x =Order,y= number_of_species, label=number_of_species), position=position_dodge(width=0.7), vjust=-0.25,size=2.8)+
  scale_y_continuous(breaks = seq(0,200,50),limits = c(0,200))+
  #ggtitle("Inferring methods of the Sign Score of the PPI network of Drosophila gene")+
  labs(x="Order", y= "Number of species")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~ Class,scales = "free", ncol=1)
  

ggsave("most_abudant_classes.jpeg", plot = last_plot(), width = 15,height = 20,units = "cm",device = "jpeg", dpi = 300,path = "Plots/")



#' 
#' ## Taxon Occurencies Distribution
#' 
#' * Species, Genera, Families abudance in caves of Greece
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------

# Species occurencies distributions
species_occurencies_caves <- species_occurencies %>% group_by(Species) %>% filter(species_epithet!="sp.")%>% summarise(taxon_occurences=n()) %>% group_by(taxon_occurences) %>% summarise(number_of_taxon=n()) %>% mutate(taxon="Species")

dist_species_occurencies_caves <- ggplot(data=species_occurencies_caves)+
  geom_line(aes(x=taxon_occurences, y= number_of_taxon),color="dodgerblue2",show.legend = F)+
  geom_point(aes(x=taxon_occurences, y= number_of_taxon),color="dodgerblue2",show.legend = F, size=1)+
  ggtitle("Species")+
  scale_y_continuous(breaks = seq(0,600,100),limits = c(0,600))+
  scale_x_continuous(breaks = seq(0,90,10), limits = c(0,90))+
  #labs(x="Number of species", y= "Number of taxa")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),plot.title = element_text(hjust = 0.5, size = 11))

# Genera occurencies distributions
genera_occurencies_caves <- species_occurencies %>% group_by(Genus) %>% summarise(taxon_occurences=n()) %>% group_by(taxon_occurences) %>% summarise(number_of_taxon=n()) %>% mutate(taxon="Genus")

dist_genera_occurencies_caves <-  ggplot(data=genera_occurencies_caves)+
  geom_line(aes(x=taxon_occurences, y= number_of_taxon),color="orange",show.legend = F)+
  geom_point(aes(x=taxon_occurences, y= number_of_taxon),color="orange",show.legend = F, size=1)+
  ggtitle("Genus")+
  scale_y_continuous(breaks = seq(0,210,50),limits = c(0,210))+
  scale_x_continuous(breaks = seq(0,210,50), limits = c(0,210))+
  #labs(x="Number of species", y= "Number of taxa")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),plot.title = element_text(hjust = 0.5, size = 11))

# Family occurencies distribution

family_occurencies_caves <- species_occurencies %>% group_by(Family) %>% summarise(taxon_occurences=n()) %>% group_by(taxon_occurences) %>% summarise(number_of_taxon=n()) %>% mutate(taxon="Family")

dist_family_occurencies_caves <- ggplot(data=family_occurencies_caves)+
  geom_line(aes(x=taxon_occurences, y= number_of_taxon),color="turquoise",show.legend = F)+
  geom_point(aes(x=taxon_occurences, y= number_of_taxon),color="turquoise",show.legend = F, size=1)+
  ggtitle("Family")+
  scale_x_continuous(breaks = seq(0,250,50),limits = c(0,250))+
  scale_y_continuous(breaks = seq(0,90,20), limits = c(0,90))+
  #labs(x="Number of species", y= "Number of taxa")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),plot.title = element_text(hjust = 0.5, size = 11))

# Order occurencies distributions

order_occurencies_caves <- species_occurencies %>% group_by(Order) %>% summarise(taxon_occurences=n()) %>% group_by(taxon_occurences) %>% summarise(number_of_taxon=n()) %>% mutate(taxon="Order")

dist_order_occurencies_caves <- ggplot(data=order_occurencies_caves)+
  geom_line(aes(x=taxon_occurences, y= number_of_taxon),color="purple",show.legend = F)+
  geom_point(aes(x=taxon_occurences, y= number_of_taxon),color="purple",show.legend = F, size=1)+
  ggtitle("Order")+
  scale_x_continuous(breaks = seq(0,600,100),limits = c(0,600))+
  scale_y_continuous(breaks = seq(0,20,5), limits = c(0,20))+
  #labs(x="Number of species", y= "Number of taxa")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),plot.title = element_text(hjust = 0.5, size = 11))


## Class occurencies
class_occurencies_caves <- species_occurencies %>% group_by(Class) %>% summarise(taxon_occurences=n()) %>% group_by(taxon_occurences) %>% summarise(number_of_taxon=n()) %>% mutate(taxon="Class")

dist_class_occurencies_caves <- ggplot(data=class_occurencies_caves)+
  geom_line(aes(x=taxon_occurences, y= number_of_taxon),color="red",show.legend = F)+
  geom_point(aes(x=taxon_occurences, y= number_of_taxon),color="red",show.legend = F, size=1)+
  ggtitle("Class")+
  scale_y_continuous(breaks = seq(0,5,1),limits = c(0,4))+
  scale_x_continuous(breaks = seq(0,900,150), limits = c(0,900))+
  labs(x="Number of species", y= "Number of taxa")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),plot.title = element_text(hjust = 0.5, size = 11))

## Merge distributions

occurences_taxa <- do.call("rbind",list(species_occurencies_caves,genera_occurencies_caves,family_occurencies_caves,order_occurencies_caves,class_occurencies_caves))


 ### Arrange plots
distributions_occurences_taxa <- grid.arrange(dist_species_occurencies_caves,arrangeGrob(dist_genera_occurencies_caves,dist_family_occurencies_caves,dist_order_occurencies_caves,dist_class_occurencies_caves,ncol=2),nrow = 2,bottom=textGrob("Number of occurencies in caves", gp=gpar(fontface="plain", col="black", fontsize=11)),left=textGrob("Number of taxa", gp=gpar(fontface="plain", col="black",fontsize=11), rot=90), heights=c(0.35,0.65))


ggsave("distributions_distributions_occurences_taxa.jpeg", plot = distributions_occurences_taxa, device = "jpeg",width = 13,height = 18,units = "cm", dpi = 300,path = "Plots/")


#' 
#' Without chiroptera
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------

species_occurencies_no_chiroptera <- species_occurencies %>% filter(Order!="Chiroptera",species_epithet!="sp.")

# Species occurencies distributions
species_occurencies_caves <- species_occurencies_no_chiroptera %>% group_by(Species) %>% summarise(taxon_occurences=n()) %>% group_by(taxon_occurences) %>% summarise(number_of_taxon=n()) %>% mutate(taxon="Species")

dist_species_occurencies_caves <- ggplot(data=species_occurencies_caves)+
  geom_line(aes(x=taxon_occurences, y= number_of_taxon),color="dodgerblue2",show.legend = F)+
  geom_point(aes(x=taxon_occurences, y= number_of_taxon),color="dodgerblue2",show.legend = F, size=1)+
  ggtitle("Species")+
  scale_y_continuous(breaks = seq(0,600,100),limits = c(0,600))+
  scale_x_continuous(breaks = seq(0,50,5), limits = c(0,50))+
  #labs(x="Number of species", y= "Number of taxa")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),plot.title = element_text(hjust = 0.5, size = 11))

# Genera occurencies distributions
genera_occurencies_caves <- species_occurencies_no_chiroptera %>% group_by(Genus) %>% summarise(taxon_occurences=n()) %>% group_by(taxon_occurences) %>% summarise(number_of_taxon=n()) %>% mutate(taxon="Genus")

dist_genera_occurencies_caves <-  ggplot(data=genera_occurencies_caves)+
  geom_line(aes(x=taxon_occurences, y= number_of_taxon),color="orange",show.legend = F)+
  geom_point(aes(x=taxon_occurences, y= number_of_taxon),color="orange",show.legend = F, size=1)+
  ggtitle("Genus")+
  scale_y_continuous(breaks = seq(0,210,50),limits = c(0,210))+
  scale_x_continuous(breaks = seq(0,210,25), limits = c(0,150))+
  #labs(x="Number of species", y= "Number of taxa")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),plot.title = element_text(hjust = 0.5, size = 11))

# Family occurencies distribution

family_occurencies_caves <- species_occurencies_no_chiroptera %>% group_by(Family) %>% summarise(taxon_occurences=n()) %>% group_by(taxon_occurences) %>% summarise(number_of_taxon=n()) %>% mutate(taxon="Family")

dist_family_occurencies_caves <- ggplot(data=family_occurencies_caves)+
  geom_line(aes(x=taxon_occurences, y= number_of_taxon),color="turquoise",show.legend = F)+
  geom_point(aes(x=taxon_occurences, y= number_of_taxon),color="turquoise",show.legend = F, size=1)+
  ggtitle("Family")+
  scale_x_continuous(breaks = seq(0,150,25),limits = c(0,150))+
  scale_y_continuous(breaks = seq(0,90,20), limits = c(0,90))+
  #labs(x="Number of species", y= "Number of taxa")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),plot.title = element_text(hjust = 0.5, size = 11))

# Order occurencies distributions

order_occurencies_caves <- species_occurencies_no_chiroptera %>% group_by(Order) %>% summarise(taxon_occurences=n()) %>% group_by(taxon_occurences) %>% summarise(number_of_taxon=n()) %>% mutate(taxon="Order")

dist_order_occurencies_caves <- ggplot(data=order_occurencies_caves)+
  geom_line(aes(x=taxon_occurences, y= number_of_taxon),color="purple",show.legend = F)+
  geom_point(aes(x=taxon_occurences, y= number_of_taxon),color="purple",show.legend = F, size=1)+
  ggtitle("Order")+
  scale_x_continuous(breaks = seq(0,600,100),limits = c(0,600))+
  scale_y_continuous(breaks = seq(0,20,5), limits = c(0,20))+
  #labs(x="Number of species", y= "Number of taxa")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),plot.title = element_text(hjust = 0.5, size = 11))


## Class occurencies
class_occurencies_caves <- species_occurencies_no_chiroptera %>% group_by(Class) %>% summarise(taxon_occurences=n()) %>% group_by(taxon_occurences) %>% summarise(number_of_taxon=n()) %>% mutate(taxon="Class")

dist_class_occurencies_caves <- ggplot(data=class_occurencies_caves)+
  geom_line(aes(x=taxon_occurences, y= number_of_taxon),color="red",show.legend = F)+
  geom_point(aes(x=taxon_occurences, y= number_of_taxon),color="red",show.legend = F, size=1)+
  ggtitle("Class")+
  scale_y_continuous(breaks = seq(0,5,1),limits = c(0,4))+
  scale_x_continuous(breaks = seq(0,900,150), limits = c(0,900))+
  labs(x="Number of species", y= "Number of taxa")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),plot.title = element_text(hjust = 0.5, size = 11))

## Merge distributions

occurences_taxa <- do.call("rbind",list(species_occurencies_caves,genera_occurencies_caves,family_occurencies_caves,order_occurencies_caves,class_occurencies_caves))


 ### Arrange plots
distributions_occurences_taxa <- grid.arrange(dist_species_occurencies_caves,arrangeGrob(dist_genera_occurencies_caves,dist_family_occurencies_caves,dist_order_occurencies_caves,dist_class_occurencies_caves,ncol=2),nrow = 2,bottom=textGrob("Number of occurencies in caves", gp=gpar(fontface="plain", col="black", fontsize=11)),left=textGrob("Number of taxa (no chiroptera)", gp=gpar(fontface="plain", col="black",fontsize=11), rot=90), heights=c(0.35,0.65))


ggsave("distributions_distributions_occurences_taxa_no_chiroptera.jpeg", plot = distributions_occurences_taxa, device = "jpeg",width = 13,height = 18,units = "cm", dpi = 300,path = "Plots/")


#' 
#' Only chiroptera
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------

species_occurencies_only_chiroptera <- species_occurencies %>% filter(Order=="Chiroptera", species_epithet!="sp.")
species_occurencies_only_chiroptera2 <- species_occurencies_only_chiroptera %>% dplyr::select(Order,Family,Genus,Species) %>% distinct()

# Species occurencies distributions
species_occurencies_caves <- species_occurencies_only_chiroptera %>% group_by(Species) %>% summarise(taxon_occurences=n()) %>% group_by(taxon_occurences) %>% summarise(number_of_taxon=n()) %>% mutate(taxon="Species")

dist_species_occurencies_caves <- ggplot(data=species_occurencies_caves)+
  geom_line(aes(x=taxon_occurences, y= number_of_taxon),color="dodgerblue2",show.legend = F)+
  geom_point(aes(x=taxon_occurences, y= number_of_taxon),color="dodgerblue2",show.legend = F, size=1)+
  ggtitle("Species")+
  #scale_y_continuous(breaks = seq(0,600,100),limits = c(0,600))+
  scale_x_continuous(breaks = seq(0,50,5), limits = c(0,50))+
  #labs(x="Number of species", y= "Number of taxa")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),plot.title = element_text(hjust = 0.5, size = 11))

# Genera occurencies distributions
genera_occurencies_caves <- species_occurencies_only_chiroptera %>% group_by(Genus) %>% summarise(taxon_occurences=n()) %>% group_by(taxon_occurences) %>% summarise(number_of_taxon=n()) %>% mutate(taxon="Genus")

dist_genera_occurencies_caves <-  ggplot(data=genera_occurencies_caves)+
  geom_line(aes(x=taxon_occurences, y= number_of_taxon),color="orange",show.legend = F)+
  geom_point(aes(x=taxon_occurences, y= number_of_taxon),color="orange",show.legend = F, size=1)+
  ggtitle("Genus")+
  scale_y_continuous(breaks = seq(0,3,1),limits = c(0,3))+
  scale_x_continuous(breaks = seq(0,210,25), limits = c(0,150))+
  #labs(x="Number of species", y= "Number of taxa")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),plot.title = element_text(hjust = 0.5, size = 11))

# Family occurencies distribution

family_occurencies_caves <- species_occurencies_only_chiroptera %>% group_by(Family) %>% summarise(taxon_occurences=n()) %>% group_by(taxon_occurences) %>% summarise(number_of_taxon=n()) %>% mutate(taxon="Family")

# dist_family_occurencies_caves <- ggplot(data=family_occurencies_caves)+
#   geom_line(aes(x=taxon_occurences, y= number_of_taxon),color="turquoise",show.legend = F)+
#   geom_point(aes(x=taxon_occurences, y= number_of_taxon),color="turquoise",show.legend = F, size=1)+
#   ggtitle("Family")+
#   scale_x_continuous(breaks = seq(0,150,25),limits = c(0,150))+
#   scale_y_continuous(breaks = seq(0,90,20), limits = c(0,90))+
#   #labs(x="Number of species", y= "Number of taxa")+
#   theme_bw()+
#   theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),plot.title = element_text(hjust = 0.5, size = 11))

# Order occurencies distributions

order_occurencies_caves <- species_occurencies_only_chiroptera %>% group_by(Order) %>% summarise(taxon_occurences=n()) %>% group_by(taxon_occurences) %>% summarise(number_of_taxon=n()) %>% mutate(taxon="Order")

# dist_order_occurencies_caves <- ggplot(data=order_occurencies_caves)+
#   geom_line(aes(x=taxon_occurences, y= number_of_taxon),color="purple",show.legend = F)+
#   geom_point(aes(x=taxon_occurences, y= number_of_taxon),color="purple",show.legend = F, size=1)+
#   ggtitle("Order")+
#   scale_x_continuous(breaks = seq(0,600,100),limits = c(0,600))+
#   scale_y_continuous(breaks = seq(0,20,5), limits = c(0,20))+
#   #labs(x="Number of species", y= "Number of taxa")+
#   theme_bw()+
#   theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),plot.title = element_text(hjust = 0.5, size = 11))


## Class occurencies
class_occurencies_caves <- species_occurencies_only_chiroptera %>% group_by(Class) %>% summarise(taxon_occurences=n()) %>% group_by(taxon_occurences) %>% summarise(number_of_taxon=n()) %>% mutate(taxon="Class")

# dist_class_occurencies_caves <- ggplot(data=class_occurencies_caves)+
#   geom_line(aes(x=taxon_occurences, y= number_of_taxon),color="red",show.legend = F)+
#   geom_point(aes(x=taxon_occurences, y= number_of_taxon),color="red",show.legend = F, size=1)+
#   ggtitle("Class")+
#   scale_y_continuous(breaks = seq(0,5,1),limits = c(0,4))+
#   scale_x_continuous(breaks = seq(0,900,150), limits = c(0,900))+
#   labs(x="Number of species", y= "Number of taxa")+
#   theme_bw()+
#   theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),plot.title = element_text(hjust = 0.5, size = 11))

## Merge distributions

occurences_taxa <- do.call("rbind",list(species_occurencies_caves,genera_occurencies_caves,family_occurencies_caves,order_occurencies_caves,class_occurencies_caves))


 ### Arrange plots
distributions_occurences_taxa <- grid.arrange(dist_species_occurencies_caves,dist_genera_occurencies_caves,nrow = 2,bottom=textGrob("Number of occurencies in caves", gp=gpar(fontface="plain", col="black", fontsize=11)),left=textGrob("Number of taxa (only chiroptera)", gp=gpar(fontface="plain", col="black",fontsize=11), rot=90), heights=c(0.35,0.65))


ggsave("distributions_distributions_occurences_taxa_only_chiroptera.jpeg", plot = distributions_occurences_taxa, device = "jpeg",width = 13,height = 18,units = "cm", dpi = 300,path = "Plots/")


#' 
#' ## Taxon - Subtaxon Distribution
#' 
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------
# Genus
species_occurencies_unique_species_genus_dist <- species_occurencies_unique_species %>% dplyr::select(Species,Genus) %>% distinct() %>% group_by(Genus) %>% summarise(number_of_species=n()) %>% group_by(number_of_species) %>% summarise(number_of_taxon=n()) %>% mutate(taxon="Genus")

# Family
species_occurencies_unique_species_Family_dist <- species_occurencies_unique_species %>% dplyr::select(Species,Family) %>% distinct() %>% group_by(Family) %>% summarise(number_of_species=n()) %>% group_by(number_of_species) %>% summarise(number_of_taxon=n()) %>% mutate(taxon="Family")

# Order
species_occurencies_unique_species_Order_dist <- species_occurencies_unique_species %>% dplyr::select(Species,Order) %>% distinct() %>% group_by(Order) %>% summarise(number_of_species=n()) %>% group_by(number_of_species) %>% summarise(number_of_taxon=n()) %>% mutate(taxon="Order")

# Class
species_occurencies_unique_species_Class_dist <- species_occurencies_unique_species %>% dplyr::select(Species,Class) %>% distinct() %>% group_by(Class) %>% summarise(number_of_species=n()) %>% group_by(number_of_species) %>% summarise(number_of_taxon=n()) %>% mutate(taxon="Class")

species_taxon_distributions <- do.call("rbind", list(species_occurencies_unique_species_genus_dist, species_occurencies_unique_species_Family_dist, species_occurencies_unique_species_Order_dist,species_occurencies_unique_species_Class_dist))


ggplot()+
  geom_point(data = species_taxon_distributions, aes(x=number_of_species, y= number_of_taxon, color=taxon),show.legend = T)+
  #ggtitle("Inferring methods of the Sign Score of the PPI network of Drosophila gene")+
  scale_y_continuous(breaks = seq(0,320,20),limits = c(0,320))+
  scale_x_continuous(breaks = seq(0,280,20),limits = c(0,280))+
  labs(x="Number of species", y= "Number of taxa")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())
ggsave("species_taxon_distributions_plot.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")


#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------

dist_class <- species_taxon_distributions %>% filter(taxon=="Class") %>% 
  ggplot(data=.)+
  geom_line(aes(x=number_of_species, y= number_of_taxon),color="red",show.legend = F)+
  geom_point(aes(x=number_of_species, y= number_of_taxon),color="red",show.legend = F, size=1)+
  ggtitle("Class")+
  scale_y_continuous(breaks = seq(0,5,1),limits = c(0,4))+
  scale_x_continuous(breaks = seq(0,300,50), limits = c(0,300))+
  labs(x="Number of species", y= "Number of taxa")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),plot.title = element_text(hjust = 0.5, size = 11))

dist_order <- species_taxon_distributions %>% filter(taxon=="Order") %>% 
  ggplot(data=.)+
  geom_line(aes(x=number_of_species, y= number_of_taxon),color="purple",show.legend = F)+
  geom_point(aes(x=number_of_species, y= number_of_taxon),color="purple",show.legend = F, size=1)+
  ggtitle("Order")+
  scale_y_continuous(breaks = seq(0,20,5),limits = c(0,20))+
  scale_x_continuous(breaks = seq(0,180,20), limits = c(0,180))+
  #labs(x="Number of species", y= "Number of taxa")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),plot.title = element_text(hjust = 0.5, size = 11))

dist_family <- species_taxon_distributions %>% filter(taxon=="Family") %>% 
  ggplot(data=.)+
  geom_line(aes(x=number_of_species, y= number_of_taxon),color="turquoise",show.legend = F)+
  geom_point(aes(x=number_of_species, y= number_of_taxon),color="turquoise",show.legend = F, size=1)+
  ggtitle("Family")+
  scale_y_continuous(breaks = seq(0,120,20),limits = c(0,120))+
  scale_x_continuous(breaks = seq(0,80,10), limits = c(0,70))+
  #labs(x="Number of species", y= "Number of taxa")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),plot.title = element_text(hjust = 0.5, size = 11))

dist_genus <- species_taxon_distributions %>% filter(taxon=="Genus") %>% 
  ggplot(data=.)+
  geom_line(aes(x=number_of_species, y= number_of_taxon),color="orange",show.legend = F)+
  geom_point(aes(x=number_of_species, y= number_of_taxon),color="orange",show.legend = F, size=1)+
  ggtitle("Genus")+
  scale_y_continuous(breaks = seq(0,300,50),limits = c(0,300))+
  scale_x_continuous(breaks = seq(0,30,5), limits = c(0,30))+
  #labs(x="Number of species", y= "Number of taxa")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),plot.title = element_text(hjust = 0.5, size = 11))

distributions_species_taxon <- grid.arrange(arrangeGrob(grobs = list(dist_class,dist_order,dist_family,dist_genus)), ncol=2,bottom=textGrob("Number of species", gp=gpar(fontface="plain", col="black", fontsize=11)), left=textGrob("Number of taxa", gp=gpar(fontface="plain", col="black",fontsize=11), rot=90),widths=c(9,0.5))


ggsave("distributions_species_taxon.jpeg", plot = distributions_species_taxon, device = "jpeg", dpi = 300,path = "Plots/")



#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE, eval=FALSE---------------
## #determine order of appearance in facet_wrap
## 
## species_taxon_distributions$taxon_order <- factor(species_taxon_distributions$taxon, levels = c("Class","Order","Family","Genus"))
## 
## ggplot()+
##   geom_line(data = species_taxon_distributions, aes(x=number_of_species, y= number_of_taxon,colour = factor(taxon)),show.legend = F)+
##   geom_point(data = species_taxon_distributions, aes(x=number_of_species, y= number_of_taxon,colour = factor(taxon)),show.legend = F, size=1)+
##   #ggtitle("Inferring methods of the Sign Score of the PPI network of Drosophila gene")+
##   #scale_y_continuous(breaks = )+
##   #scale_x_continuous(breaks = seq(0,280,20))+
##   labs(x="Number of species", y= "Number of taxa")+
##   theme_bw()+
##   theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
##   facet_wrap( ~ taxon_order,scales = "free")
## 
## ggsave("species_taxon_distributions_plot_facet.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")

#' 
#' ## Species References
## ----warning=FALSE, message=FALSE, echo=FALSE----------------------------

species_references_year_dist <- species_references %>% group_by(Year) %>% summarise(publications_per_year=n())


ggplot()+
  geom_line(data=species_references_year_dist,aes(x=Year, y= publications_per_year),color="red",show.legend = F)+
  scale_x_continuous(breaks = seq(1860,2020,10),limits = c(1860,2020))+
  scale_y_continuous(breaks = seq(0,20,2), limits = c(0,20))+
  labs(x="Years",y="Number of new publications")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())

ggsave("species_references_year_dist.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")


#' 
## ----warning=FALSE, message=FALSE, echo=FALSE----------------------------

species_references_year_dist$Cumulative_publications <- cumsum(species_references_year_dist$publications_per_year)

ggplot(data=species_references_year_dist)+
  geom_line(aes(x=Year, y= Cumulative_publications),color="mediumseagreen",show.legend = F)+
  #ggtitle("Class")+
  scale_x_continuous(breaks = seq(1860,2020,10),limits = c(1860,2020))+
  scale_y_continuous(breaks = seq(0,750,100), limits = c(0,750))+
  labs(x="Years",y="Number of publications")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())

ggsave("species_references_year_dist_cumulative.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")


#' 
#' 
#' * New species discovered in time. (references)
#' 
## ----warning=FALSE, message=FALSE, echo=FALSE----------------------------
####
# species_references_spreaded2 <- species_references_spreaded %>% left_join(.,species_occurencies_unique_species,by=c("Species"="Species")) %>% left_join(.,species_references, by=c("Species_references_spread"="short_reference_ready")) %>% left_join(.,species_occurencies_unique_caves, by=c("CaveName"="CaveName" , "NearestVillage"="NearestVillage","Cave_ID"="Cave_ID"))
# 


species_references_spreaded_arranged <- Cave_Fauna_of_Greece_Census_LONG %>% ungroup() %>% dplyr::select(Species, Species_references_spread, Year) %>% group_by(Species,Species_references_spread, Year) %>% distinct(.) %>% arrange(.,Year) %>% ungroup() %>% mutate(Duplicates=duplicated(Species)) %>% mutate(.,First_occurance=if_else(Duplicates=="FALSE",1,0)) %>% na.omit()

species_references_spreaded_arranged$Cumulative_occurance <- cumsum(species_references_spreaded_arranged$First_occurance)

ggplot(data=species_references_spreaded_arranged)+
  geom_line(aes(x=Year, y= Cumulative_occurance),color="violetred1",show.legend = F)+
  #ggtitle("Class")+
  scale_x_continuous(breaks = seq(1860,2020,10),limits = c(1860,2020))+
  scale_y_continuous(breaks = seq(0,950,100), limits = c(0,950))+
  labs(x="Years",y="Number of new species in Greek caves")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())

ggsave("species_occurence_accumulation.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")


#' 
#' 
## ----warning=FALSE, message=FALSE, echo=FALSE----------------------------

species_references_spreaded_arranged_article <- species_references_spreaded_arranged %>% filter(Duplicates=="FALSE") %>% group_by(Species_references_spread, Year) %>% summarise(n_new_species=n()) %>% group_by(n_new_species) %>% summarise(n_references=n())

ggplot()+
  geom_line(data=species_references_spreaded_arranged_article,aes(y=n_references, x= n_new_species),color="violetred1",show.legend = F)+
  #ggtitle("Class")+
  scale_x_continuous(breaks = seq(0,180,20),limits = c(0,180))+
  scale_y_continuous(breaks = seq(0,180,20),limits = c(0,180))+
  labs(x="Number of references",y="Number of new species in Greek caves")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())

ggsave("species_references_spreaded_arranged_article.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")


#' 
#' # Spatial analysis of Caves
#' 
#' For the creation of the caves geodatabase in r we used multiple packages. 
#' 
#' **For spatial analysis**
#' 
#' 1. sp package [@R-sp]
#' 
#' 2. rgeos package [@R-rgeos]
#' 
#' 3. rgdal package [@R-raster]
#' 
#' 4. raster package [@R-raster]
#' 
#' **For spatial visualisations**
#' 
#' 1. maptools package [@R-maptools]
#' 
#' 2. tmaps package [@R-tmap]
#' 
#' 3. ggmap package [@R-ggmap]
#' 
#' 
#' Each cave location wes manually imported to Google Earth. All locations were then exported to a single kml file. In order to handle the data the kml file should be converted to a different format like xlsx. The procedure suggested from google forum:
#' 
#' 1. Download the kml file
#' 2. rename the file to .xml 
#' 3. from excel 2007 go to Data > From Other Sources > From XML Data Source 
#' 4. Browse to where you saved the file to impoort into excel. 
#' 5. Excel will prompt that it can find the schema and will try to make it by it's own, accept it and you should see your data imported successfully.
#' 
#' This procedure resulted to a xlsx file with 67 columns and tha names and coordinates of caves were spread across them for some reason.
#' 
#' So we used the [online convertor](www.gpsvisualizer.com) which resulted to a txt file with a consistent format.
#' 
#' 
#' ## Caves geographical data
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------
# Caves shape file! Caves Coordinates

# Caves_Database_kml_to_txt <- read_delim(file = "Caves_Database_kml_to_txt_25_09_2017.csv",delim = ";",col_names = T) %>% dplyr::select("name","latitude","longitude")

Caves_Database_kml_to_txt <- species_occurencies_unique_caves %>% dplyr::select(CaveName,Cave_ID,Latitude, Longitude) %>% na.omit()

Caves_Database_kml_to_txt$Latitude <- as.numeric(Caves_Database_kml_to_txt$Latitude)
Caves_Database_kml_to_txt$Longitude <- as.numeric(Caves_Database_kml_to_txt$Longitude)

Caves_Database_kml_to_txt$ID <- as.character(seq(1:nrow(Caves_Database_kml_to_txt)))

Caves_Database_kml_to_txt_shapefile <- Caves_Database_kml_to_txt

coordinates(Caves_Database_kml_to_txt_shapefile)<-~Longitude+Latitude
proj4string(Caves_Database_kml_to_txt_shapefile) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")  # this is WGS84


species_occurencies_unique_caves_without <- species_occurencies_unique_caves[which((is.na(species_occurencies_unique_caves$Latitude))),]


#' 
#' Are the cave names in the species occurencies file the same with the Google Earth data?
#' 
#' There are `r nrow(species_occurencies_unique_caves)` caves that have been sampled.
#' 
#' ## Administrative data of Greece
#' 
#' We downloaded the greek municipality boundaries (Kallikratis plan) from [http://geodata.gov.gr/en/dataset/oria-demon-kallikrates](http://geodata.gov.gr) in the epsg 4326 format. In this format the axis order is Latitude followed by Longitude. 
#' The Greek names of municipalities were converted using the ISO 843 traslitaration system from this [online portal](http://www.passport.gov.gr/elot-743.html).
#' 
#' It is preferable to download shapefiles because they are immediatly imported into the Spatial objects. 
#' 
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------
# Hellenic settlements 

hellenic_settlements <- maptools::readShapePoints("hellenic_settlements/hellenic_settlements",verbose=TRUE)
proj4string(hellenic_settlements) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")  # this is WGS84

# Municipalities shape file


municipalities_shape_file <- maptools::readShapePoly("municipalities_shape_file/municipalities_Kallikratis_plan_Greece",verbose=TRUE)
proj4string(municipalities_shape_file) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")  # this is WGS84

municipalities_greece_long_names_eng <- read_xlsx("municipalities_shape_file/names_municipalities_gr_eng.xlsx",col_names = T)
municipalities_greece_long_names_eng$KWD_YPES <- as.character(municipalities_greece_long_names_eng$KWD_YPES)

municipalities_shape_file@data <- municipalities_shape_file@data %>% left_join(., municipalities_greece_long_names_eng, by=c("KWD_YPES"="KWD_YPES"))
rownames(municipalities_shape_file@data) <- as.character(seq(1:nrow(municipalities_shape_file@data)))


over_municipality <- sp::over( x = Caves_Database_kml_to_txt_shapefile , y = municipalities_shape_file , fn = NULL)

caves_in_municipa <- over_municipality

caves_in_municipa$ID <- as.character(seq(1:nrow(caves_in_municipa)))

caves_in_municipa2 <- caves_in_municipa %>% left_join(., Caves_Database_kml_to_txt, by=c("ID"="ID"))



#combine previous file with the caves from occurence data

# species_occurencies_unique_caves$joined <- do.call(paste, c(species_occurencies_unique_caves[c("CaveName", "Municipality")], sep = ";"))
# 
# species_occurencies_unique_caves <- species_occurencies_unique_caves %>% left_join(., caves_in_municipa2, by=c("joined"="joined")) %>% dplyr::select(-V2,-name,-V3,-joined)
# 
# species_occurencies_unique_caves_coords <- species_occurencies_unique_caves %>% filter(!is.na(Cave_ID))
# 
# species_occurencies_unique_caves_coords_dataframe <- species_occurencies_unique_caves_coords


#' 
#' 
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE,eval=TRUE-----------------

caves_municipality_join <- caves_municipality %>% left_join(.,municipalities_greece_long_names_eng, by=c("Municipality"="Municipalities_ISO_843"),copy=TRUE) %>% left_join(.,municipalities_greece_long_names_eng, by=c("KWD_YPES"="KWD_YPES"),copy=TRUE) %>% dplyr::select(Municipalities_ISO_843,KWD_YPES,number_of_caves) %>% ungroup() %>% filter(!(KWD_YPES=="9170" & !is.na(KWD_YPES)))

# Irakleio is 2 times but they are different municipalities. Irakleio Attikis has 9170 code so we removed it.

caves_species_municipality_join <-  caves_municipality_join %>% left_join(.,species_municipality, by=c("Municipalities_ISO_843"="Municipality"))



#' 
#' 
#' ## Protected areas of Greece
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------


natura2000_NEW_shapefile <- maptools::readShapePoly("Natura2000_2017_NEW_shp/Kaloust/Nees_Natura", verbose = TRUE)

natura2000_NEW_shapefile_INFO <- maptools::readShapePoly("Natura2000_2017_NEW_shp/NEES_FINAL_V10", verbose = TRUE)

natura2000_NEW_shapefile_INFO_df <- natura2000_NEW_shapefile_INFO@data %>% mutate(id=as.character(seq(from=0,to=(nrow(.)-1))))

natura2000_NEW_shapefile@data <-  natura2000_NEW_shapefile@data %>% mutate(id=as.character(seq(from=0,to=(nrow(.)-1)))) %>% left_join(natura2000_NEW_shapefile_INFO_df, by=c("id"="id")) %>% dplyr::select(-c(descriptio,timestamp,begin,end,altitudeMo,tessellate,extrude,visibility,drawOrder,icon))

proj4string(natura2000_NEW_shapefile) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") # this is WGS84


##### Natura 2000
natura2000shapefile <- maptools::readShapePoly("natura2000shapefile/natura2000shapefile",verbose=TRUE)

proj4string(natura2000shapefile) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")  # this is WGS84

natura2000shapefile_data <- natura2000shapefile@data %>% dplyr::select(CODE,NAME_LATIN)
### Katafygia agrias zois
#katafygia_agrias_zwhs <- maptools::readShapePoly("katafygia_agrias_zwhs/katafygia_agrias_zwhs",verbose=TRUE)

#proj4string(katafygia_agrias_zwhs) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")  # this is WGS84

####
katafygia_agrias_zwhs <- maptools::readShapePoly("KAZ_data/KAZ_data",verbose=TRUE)

proj4string(katafygia_agrias_zwhs) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")  # this is WGS84

#ssss <- katafygia_agrias_zwhs@data

KAZ_data_translated <- read_xlsx("KAZ_data/KAZ_table_translated.xlsx",col_names = T)

KAZ_data_KODE <- read_csv("KAZ_data/KAZ_data.csv",col_names = T) %>% dplyr::select(-the_geom) %>% left_join(KAZ_data_translated, by=c("KODE"="KODE"))

### New Natura Over
over_NEW_natura <- sp::over( x = Caves_Database_kml_to_txt_shapefile, y = natura2000_NEW_shapefile , fn = NULL)

caves_in_over_NEW_natura <- over_NEW_natura %>% mutate(ID=as.character(seq(1:nrow(over_NEW_natura)))) %>% filter(SITE_TYPE=="SCI") %>% mutate(TYPE=gsub(pattern = "TROP",replacement = "Modified:",x =TYPE),Name_New_NATURA2=gsub(pattern = "TROP ",replacement = "",x = Name)) %>% left_join(Caves_Database_kml_to_txt,by=c("ID"="ID")) %>% dplyr::select(Cave_ID,TYPE,SITE_TYPE,Name_New_NATURA2) %>% left_join(natura2000shapefile_data, by=c("Name_New_NATURA2"="CODE")) %>% dplyr::select(-c(Name_New_NATURA2))

colnames(caves_in_over_NEW_natura) <- c("Cave_ID","CODE_NEW_NATURA","SITETYPE_NEW_NATURA", "NAME_LATIN_NEW_NATURA")

### over NATURA

over_natura <- sp::over( x = Caves_Database_kml_to_txt_shapefile , y = natura2000shapefile , fn = NULL)

# create file with the old and new natura2000 areas combined

caves_in_over_natura <- over_natura %>% mutate(ID=as.character(seq(1:nrow(over_natura)))) %>% left_join(Caves_Database_kml_to_txt,by=c("ID"="ID")) %>% dplyr::select(Cave_ID,CODE,SITETYPE,NAME_LATIN)

colnames(caves_in_over_natura) <- c("Cave_ID","CODE_NATURA","SITETYPE_NATURA", "NAME_LATIN_NATURA")

### over Katafygia agrias zwis

over_katafygia_agrias_zwhs <- sp::over( x = Caves_Database_kml_to_txt_shapefile , y = katafygia_agrias_zwhs , fn = NULL)

over_katafygia_agrias_zwhs$ID_cave <- as.character(seq(1:nrow(over_katafygia_agrias_zwhs)))

over_katafygia_agrias_zwhs_info <- over_katafygia_agrias_zwhs %>% dplyr::select(KODE,ID_cave) %>% left_join(KAZ_data_KODE, by=c("KODE"="KODE"))

caves_in_over_katafygia_agrias_zwhs <- Caves_Database_kml_to_txt %>% left_join(over_katafygia_agrias_zwhs_info,by=c("ID"="ID_cave")) %>% dplyr::select(Cave_ID,KODE,KAZ_NAME_GR,KAZ_Official_Government_Gazette)

### caves with all protected areas
capwords <- function(s, strict = FALSE) {
    cap <- function(s) paste(toupper(substring(s, 1, 1)),
                  {s <- substring(s, 2); if(strict) tolower(s) else s},
                             sep = "", collapse = " " )
    sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

caves_protection <- species_occurencies_unique_caves %>% dplyr::select(Cave_ID) %>% left_join(.,caves_in_over_natura, by=c("Cave_ID"="Cave_ID")) %>% left_join(.,caves_in_over_NEW_natura, by=c("Cave_ID"="Cave_ID")) %>% left_join(., caves_in_over_katafygia_agrias_zwhs, by=c("Cave_ID"="Cave_ID")) %>% mutate(NAME_LATIN_NATURA=gsub("NANA",NA_character_,gsub(" Kai "," and ",capwords(tolower(NAME_LATIN_NATURA))))) %>% mutate(NAME_LATIN_NEW_NATURA=gsub("NANA",NA_character_,gsub(" Kai "," and ",capwords(tolower(NAME_LATIN_NEW_NATURA))))) %>% mutate(SITETYPE_NATURA_ALL=ifelse(is.na(SITETYPE_NATURA), NA_character_, ifelse(SITETYPE_NATURA=="SPA","Special Protection Area", ifelse(SITETYPE_NATURA=="SCI","Special Area of Conservation ", "Special Protection Area - Special Area of Conservation"))),SITETYPE_NEW_NATURA_ALL=ifelse(is.na(SITETYPE_NEW_NATURA), NA_character_, ifelse(SITETYPE_NEW_NATURA=="SPA","Special Protection Area", ifelse(SITETYPE_NEW_NATURA=="SCI","Special Area of Conservation ", "Special Protection Area - Special Area of Conservation"))))

## some names in () had lower case first letters so i changed them manually.
#write_delim(caves_protection,path = "caves_protection.txt",delim = "\t",col_names = T)


caves_protectionxlsx <- read_xlsx("caves_protection.xlsx",col_names = T) 

caves_protectionxlsx[caves_protectionxlsx=="NA"] <- NA

caves_protectionxlsx2 <- caves_protectionxlsx %>% mutate(LEGISLATION_NATURA=if_else(is.na(NAME_LATIN_NATURA) & is.na(NAME_LATIN_NEW_NATURA),NA_character_,"Law 3937/2011 (OGG/Α 60/31.03.2011), Joint Ministerial Decision 50743/2017 (OGG 4432/B/15.12.17"))


species_occurencies2 <- species_occurencies %>% mutate(qqq=paste(Species,Cave_ID,CaveName,Species_references,sep = ";")) %>% mutate(duplicates=duplicated(qqq)) %>% mutate(duplicatesLast=duplicated(qqq,fromLast = T)) %>% mutate(duplicatesAll=if_else(duplicates=="TRUE","TRUE",if_else(duplicatesLast=="TRUE","TRUE","FALSE"))) %>% filter(duplicatesLast!="TRUE") %>% dplyr::select(-c(qqq,duplicates,duplicatesAll,duplicatesLast,ID,     objectid_NATURA,CODE_NATURA,area_NATURA,perimeter_NATURA,hectares_NATURA,SITETYPE_NATURA,                PERIPHERY_NATURA,PREFECTURE_NATURA,NAME_LATIN_NATURA,area_katafygia_agrias_zwhs,        FID_katafygia_agrias_zwhs,THESH_katafygia_agrias_zwhs,PERIFEREIA_katafygia_agrias_zwhs,   EPOPTEYOYS_katafygia_agrias_zwhs,DHMOS_KOIN_katafygia_agrias_zwhs,NOMOS_katafygia_agrias_zwhs,           EKTASH_katafygia_agrias_zwhs,APOFASH_katafygia_agrias_zwhs,FEK_katafygia_agrias_zwhs,EIDH_CHLOR_katafygia_agrias_zwhs,PANIDA_PTH_katafygia_agrias_zwhs,PANIDA_THI_katafygia_agrias_zwhs,PANIDA_PSA_katafygia_agrias_zwhs,PANIDA_ERP_katafygia_agrias_zwhs,PANIDA_AMF_katafygia_agrias_zwhs,area_strem_katafygia_agrias_zwhs,perimeter_katafygia_agrias_zwhs,CODE_SYNTO_NEW_NATURA,area_NEW_NATURA,CODE_SYN_1_New_NATURA,TYPE_NEW_NATURA,SITE_TYPE_NEW_NATURA)) %>% left_join(caves_protectionxlsx2, by=c("Cave_ID"="Cave_ID"))

#write_delim(species_occurencies2,path = "Cave_Fauna_of_Greece_Census_v5_24_01_2018.txt",delim = "\t",col_names = T)

census_for_database <- species_occurencies2 %>% dplyr::select(-c(Species_before_GNR,Species_Old,Speciment_Abbreviations,Species_comment,Synonymes,Merged,Comments,`Beron 2016`,Old_References,Number_references,References_Kaloust_old,Reference_comment,`Reference 3`,Species_Comments_2018,Species_2018,Distribution_IUCN,Countries_occurrences_GBIF,Distribution_CoT,Species_2018_2,Census_Merge,Cave_ID_OLD,Cave_comment2,NAME,KWD_YPES,Municipalities_gr_original,Municipalities_ISO_843,Municipalities_gr,CaveReferences,Cave_Comment)) %>% mutate(Locus_Typicus=if_else(is.na(Locus_Typicus),NA_character_,"Locus Typicus"))

#write_delim(census_for_database,"census_for_database_24_01_2018.txt",delim = "\t",col_names = T)

# ## NATURA TO dataframes
# 
# to_df_natura <- function(x){
#   
# y <- data.frame(Cave_ID=as.character(),NAME_LATIN_NATURA=as.character(),CODE_NATURA=as.character(),SITETYPE_NATURA=as.character(),objectid_NATURA=as.character(), stringsAsFactors=FALSE) 
# 
# for(i in 1:length(x)) {
#   
#   y[i,1] <- as.character(names(x[i]))
#   y[i,2] <- paste(as.character(x[[i]]$NAME_LATIN),collapse = ";")
#   y[i,3] <- paste(as.character(x[[i]]$CODE),collapse = ";")
#   y[i,4] <- paste(as.character(x[[i]]$SITETYPE),collapse = ";")
#   y[i,5] <- paste(as.character(x[[i]]$objectid),collapse = ";")
# }
#   y
# }
# 
# caves_in_over_natura <- to_df_natura(over_natura)
# 
# is.na(caves_in_over_natura) <- caves_in_over_natura==''

# ## KAZ TO dataframes
# 
# to_df_kaz <- function(x){
#   
# y <- data.frame(Cave_ID=as.character(),area=as.character(),area_strem=as.character(), stringsAsFactors=FALSE) 
# 
# for(i in 1:length(x)) {
#   
#   y[i,1] <- as.character(names(x[i]))
#   y[i,2] <- paste(as.character(x[[i]]$area),collapse = ";")
#   y[i,3] <- paste(as.character(x[[i]]$area_strem),collapse = ";")
#   
# }
#   y
# }
# 
# caves_in_over_katafygia_agrias_zwhs <- to_df_kaz(over_katafygia_agrias_zwhs)
# 
# is.na(caves_in_over_katafygia_agrias_zwhs) <- caves_in_over_katafygia_agrias_zwhs==''

#katafygia_agrias_zwhs_names <- read_delim(file = "katafygia_agrias_zwhs/katafygia_agrias_zwhs_names.csv", col_names=T,delim=";")

#katafygia_agrias_zwhs_names$area <- as.character(katafygia_agrias_zwhs_names$area)
# 
#caves_in_over_katafygia_agrias_zwhs$area <- as.character(caves_in_over_katafygia_agrias_zwhs$area)

#caves_in_over_katafygia_agrias_zwhs2 <- caves_in_over_katafygia_agrias_zwhs %>% left_join(.,katafygia_agrias_zwhs_names, by=c("area"="area"))

#caves_in_over_katafygia_agrias_zwhs2$ID <- as.character(seq(1:nrow(caves_in_over_katafygia_agrias_zwhs2)))

#colnames(caves_in_over_katafygia_agrias_zwhs2) <- c("area_katafygia_agrias_zwhs" ,"FID_katafygia_agrias_zwhs" ,"THESH_katafygia_agrias_zwhs", "PERIFEREIA_katafygia_agrias_zwhs"  ,"EPOPTEYOYS_katafygia_agrias_zwhs" ,"DHMOS_KOIN_katafygia_agrias_zwhs" ,"NOMOS_katafygia_agrias_zwhs","EKTASH_katafygia_agrias_zwhs","APOFASH_katafygia_agrias_zwhs" ,"FEK_katafygia_agrias_zwhs", "EIDH_CHLOR_katafygia_agrias_zwhs","PANIDA_PTH_katafygia_agrias_zwhs" ,"PANIDA_THI_katafygia_agrias_zwhs" ,"PANIDA_PSA_katafygia_agrias_zwhs","PANIDA_ERP_katafygia_agrias_zwhs", "PANIDA_AMF_katafygia_agrias_zwhs","area_strem_katafygia_agrias_zwhs", "perimeter_katafygia_agrias_zwhs","ID")


#' 
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------
# shapefiles to dataframes for plotting
## New Natura
natura2000_NEW_shapefile_names <- natura2000_NEW_shapefile@data %>% mutate(id=as.character(seq(from=0,to=(nrow(.)-1))))

natura2000_NEW_shapefile_dataframe <- tidy(natura2000_NEW_shapefile) %>% left_join(., natura2000_NEW_shapefile_names, by=c("id"="id"))

# %>% left_join(natura2000_NEW_shapefile_INFO_df, by=c("id"="id"))
# 
# %>% dplyr::select(-c(timestamp,begin,end,altitudeMo,tessellate,extrude,visibility,drawOrder,icon))


### Natura 2000
names_natura2000shapefile <- natura2000shapefile@data %>% mutate(id=as.character(seq(from=0,to=(nrow(.)-1))))

natura2000shapefile_dataframe <- tidy(natura2000shapefile) %>% left_join(., names_natura2000shapefile, by=c("id"="id"))

### katafygia_agrias_zwhs
#katafygia_agrias_zwhs_names$id <- as.character(seq(from=0,to=(nrow(katafygia_agrias_zwhs_names)-1)))

#katafygia_agrias_zwhs_dataframe <- tidy(katafygia_agrias_zwhs) %>% left_join(., katafygia_agrias_zwhs_names, by=c("id"="id"))

### caves
#caves_protected_areas <- species_occurencies_unique_caves_all_info %>% filter(!(is.na(NAME_LATIN_NATURA)) & !(is.na(area_katafygia_agrias_zwhs)))




#' 
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------
#### JOIN FILES ##########

caves_all_shapefiles <- caves_in_municipa2 %>% dplyr::select(-CaveName,-Longitude,-Latitude)%>% left_join(.,caves_in_over_natura, by=c("Cave_ID"="Cave_ID")) %>% left_join(., caves_in_over_katafygia_agrias_zwhs, by=c("Cave_ID"="Cave_ID"))

#sss <- caves_all_shapefiles[which(is.na(caves_all_shapefiles$Municipalities_ISO_843)),] %>% dplyr::select(Cave_ID) %>% left_join(species_occurencies_unique_caves, by=c("Cave_ID"="Cave_ID"))


#caves_all_shapefiles$joined <- do.call(paste, c(caves_all_shapefiles[c("name", "Municipalities_ISO_843")], sep = ";"))

#species_occurencies_unique_caves2 <- species_occurencies_unique_caves

#species_occurencies_unique_caves2$joined <- do.call(paste, c(species_occurencies_unique_caves2[c("CaveName", "Municipality")], sep = ";"))

# Caves Ready!!!

species_occurencies_unique_caves_all_info <- species_occurencies_unique_caves %>% left_join(.,caves_all_shapefiles, by=c("Cave_ID"="Cave_ID"))

#write_delim(x = species_occurencies_unique_caves_all_info,path = "Cave_Fauna_of_Greece_Caves_10_12_2017.txt",delim = "\t",col_names = T)

#species_occurencies_unique_caves3$joined <- do.call(paste, c(species_occurencies_unique_caves3[c("CaveName", "NearestVillage","Municipality", "Cave_Comment")], sep = ";"))


species_occurencies_unique_caves3 <- Caves_Database_kml_to_txt  %>% left_join(caves_in_over_NEW_natura, by=c("Cave_ID"="Cave_ID")) %>% dplyr::select(-c(CaveName,Latitude,Longitude)) %>% distinct(.)

## Species ready !!
species_occurencies_unique_species_sel <- species_occurencies_unique_species %>% dplyr::select(-c(Phylum,Class,Order,Family,Genus,species_epithet,Species_bare_name))


INSPEE_DATABASE_01_beta <- species_occurencies %>% left_join(.,species_occurencies_unique_caves3,by=c("Cave_ID"="Cave_ID"))

#write_delim(x = INSPEE_DATABASE_01_beta,path = "INSPEE_DATABASE_01_beta2_27_09_2017.txt",delim = "\t",col_names = T)


#' 
#' 
#' # Geospatial data visualisation
#' 
#' ## Species and caves per region
#' 
## ----eval=TRUE, cache=TRUE,warning=FALSE, message=FALSE, echo=FALSE------
greece_level_2 <-getData('GADM', country='GRC', level=2)  ##Get the Province Shapefile for France

#' 
## ----warning=FALSE, message=FALSE, echo=FALSE----------------------------
greece_regions <- c("Athos","East Macedonia and Thrace","Attica ","West Greece","West Macedonia","Ionian Islands ","Epirus ","Central Macedonia","Crete","South Aegean","Peloponnese ","Central Greece ","Thessaly","North Aegean")

caves_Region$regions <- greece_regions
species_Region$regions <- greece_regions


#' 
## ----warning=FALSE, message=FALSE, echo=FALSE----------------------------
# Transform to dataframe
# https://www.r-bloggers.com/using-r-working-with-geospatial-data-and-ggplot2/

greece_level_2@data$id <- rownames(greece_level_2@data)

watershedPoints <- fortify(greece_level_2, region = "id")

watershedDF <- merge(watershedPoints, greece_level_2@data, by = "id")

watershedDF <- watershedDF %>% left_join(., caves_Region, by=c("NAME_2"="regions")) %>% left_join(., species_Region, by=c("NAME_2"="regions"))

cnames <- aggregate(cbind(long, lat) ~ NAME_2, data=watershedDF, FUN=function(x)mean(range(x))) %>% left_join(., caves_Region, by=c("NAME_2"="regions"))

cnames_species <- aggregate(cbind(long, lat) ~ NAME_2, data=watershedDF, FUN=function(x)mean(range(x))) %>% left_join(., species_Region, by=c("NAME_2"="regions"))


#' 
#' 
#' Caves distribution across all regions in Greece.
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE, eval=FALSE,cache=TRUE----
## library(wesanderson)
## 
## 
## ggWatershed <- ggplot(data = watershedDF,aes(x=long, y=lat)) +
##   geom_polygon(data = watershedDF,aes(x=long, y=lat,group = group,fill = number_of_caves),color="white",lwd=0.2) +
##   #geom_path(size= 0.2,color = "white") +
##   #coord_equal() +
##   #geom_text(data=cnames,aes(label = number_of_caves, x = long, y = lat)) +
##   scale_fill_gradient(low="blue", high="red",breaks=seq(0,200,50), limits=c(0,200),name="Caves")+
##   theme_bw()+
##   theme(panel.border = element_blank(),panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.title = element_blank(), axis.text = element_blank(),axis.ticks = element_blank())
## 
## ggsave("caves_spatial_dist_per_region_no_text.jpeg", plot = ggWatershed, device = "jpeg", dpi = 300,path = "Plots/")
## 
## 

#' 
#' ![Samplings from caves in Greece.](Plots/caves_spatial_dist_per_region_no_text.jpeg)
#' 
#' Species distribution across all regions in Greece.
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE, eval=FALSE,cache=TRUE----
## 
## ggWatershed <- ggplot(data = watershedDF,aes(x=long, y=lat)) +
##   geom_polygon(data = watershedDF,aes(x=long, y=lat,group = group,fill = number_of_species),color="white",lwd=0.2) +
##   #geom_path(size= 0.2,color = "white") +
##   #coord_equal() +
##   #geom_text(data=cnames_species,aes(label = number_of_species, x = long, y = lat)) +
##   scale_fill_gradient(low="grey", high="red",breaks=seq(0,300,50), limits=c(0,300),name="Species")+
##   theme_bw()+
##   theme(panel.border = element_blank(),panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.title = element_blank(), axis.text = element_blank(),axis.ticks = element_blank())
## 
## ggsave("species_spatial_dist_per_region_no_text.jpeg", plot = ggWatershed, device = "jpeg", dpi = 300,path = "Plots/")

#' 
#' ![Cave fauna distribution in Greece.](Plots/species_spatial_dist_per_region_no_text.jpeg)
#' 
#' 
#' ## Species and caves distribution across Greek municipalities
#' 
#' 
## ----warning=FALSE, message=FALSE, echo=FALSE,eval=TRUE------------------
# Transform to dataframe
# https://www.r-bloggers.com/using-r-working-with-geospatial-data-and-ggplot2/

municipalities_shape_file_dataframe <- tidy(municipalities_shape_file)

municipalities_greece_long_names_eng$id <- as.character(seq(from=0, to=(nrow(municipalities_greece_long_names_eng)-1)))

watershedDF <- municipalities_shape_file_dataframe %>% left_join(., municipalities_greece_long_names_eng, by=c("id"="id")) %>% left_join(., caves_species_municipality_join, by=("Municipalities_ISO_843"="Municipalities_ISO_843"))

cnames <- aggregate(cbind(long, lat) ~ Municipalities_ISO_843, data=watershedDF, FUN=function(x)mean(range(x))) %>% left_join(., caves_species_municipality_join, by=c("Municipalities_ISO_843"="Municipalities_ISO_843"))

cnames_species <- aggregate(cbind(long, lat) ~ Municipalities_ISO_843, data=watershedDF, FUN=function(x)mean(range(x))) %>% left_join(., caves_species_municipality_join, by=c("Municipalities_ISO_843"="Municipalities_ISO_843"))

watershedDF$number_of_caves[is.na(watershedDF$number_of_caves)] <- 0
watershedDF$number_of_species[is.na(watershedDF$number_of_species)] <- 0


#' 
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE, eval=FALSE,cache=TRUE----
## library(wesanderson)
## 
## 
## ggWatershed <- ggplot(data = watershedDF,aes(x=long, y=lat)) +
##   geom_polygon(data = watershedDF,aes(x=long, y=lat,group = group,fill = number_of_caves),color="white",lwd=0.08) +
##   #geom_path(size= 0.2,color = "white") +
##   #coord_equal() +
##   #geom_text(data=cnames,aes(label = number_of_caves, x = long, y = lat)) +
##   scale_fill_gradient(low="blue", high="red",breaks=seq(0,20,5), limits=c(0,20),name="Caves")+
##   theme_bw()+
##   theme(panel.border = element_blank(),panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.title = element_blank(), axis.text = element_blank(),axis.ticks = element_blank())
## 
## ggsave("caves_spatial_dist_per_municipality_no_text.jpeg", plot = ggWatershed, device = "jpeg", dpi = 300,path = "Plots/")
## 
## 

#' 
#' ![Caves that have been sampled in Greece](Plots/caves_spatial_dist_per_municipality_no_text.jpeg)
#' 
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE, eval=FALSE,cache=TRUE----
## 
## ggWatershed <- ggplot(data = watershedDF,aes(x=long, y=lat)) +
##   geom_polygon(data = watershedDF,aes(x=long, y=lat,group = group,fill = number_of_species),color="white",lwd=0.082) +
##   #geom_path(size= 0.2,color = "white") +
##   #coord_equal() +
##   #geom_text(data=cnames,aes(label = number_of_caves, x = long, y = lat)) +
##   scale_fill_gradient(low="grey", high="red",breaks=seq(0,80,20), limits=c(0,80),name="Species")+
##   theme_bw()+
##   theme(panel.border = element_blank(),panel.grid.minor = element_blank(), panel.grid.major = element_blank(),axis.title = element_blank(), axis.text = element_blank(),axis.ticks = element_blank())
## 
## ggsave("species_spatial_dist_per_municipality_no_text.jpeg", plot = ggWatershed, device = "jpeg", dpi = 300,path = "Plots/")
## 
## 

#' 
#' ![Species sampled per municipality in Greece](Plots/species_spatial_dist_per_municipality_no_text.jpeg)
#' 
#' ## Greece caves and protected areas
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------
caves_in_SCI <- species_occurencies_unique_caves_all_info %>% filter(SITETYPE_NATURA=="SCI") %>% nrow()
caves_in_SPA <- species_occurencies_unique_caves_all_info %>% filter(SITETYPE_NATURA=="SPA") %>% nrow()
caves_in_SCISPA <- species_occurencies_unique_caves_all_info %>% filter(SITETYPE_NATURA=="SPASCI") %>% nrow()
caves_in_KAZ <- species_occurencies_unique_caves_all_info %>% filter(!is.na(KODE))%>% nrow()


caves_in_areas <- data.frame(Areas=c("NATURA2000 SCI","NATURA2000 SPA", "NATURA2000 SPASCI", "Wildlife Refugee"), Caves=c(caves_in_SCI,caves_in_SPA,caves_in_SCISPA,caves_in_KAZ))

caves_in_areas_caption <- "Number of caves per protected area type"
kable(caves_in_areas,caption=caves_in_areas_caption, align = 'l')


#' 
#' 
#' With google earth background.
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE, eval=FALSE,cache=TRUE----
## ## load greek borders
## 
## hellenic_borders_shapefile <- maptools::readShapeLines("hellenic_borders/hellenic_borders",verbose=TRUE)
## 
## proj4string(hellenic_borders_shapefile) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")  # this is WGS84
## 
## hellenic_borders_shapefile_dataframe <- tidy(hellenic_borders_shapefile)
## bbox_hellenic_borders <- hellenic_borders_shapefile@bbox
## bbox_hellenic_borders_lat <- bbox_hellenic_borders
## 
## ## get map
## map_greece <- get_map(location = c(lon=mean(bbox_hellenic_borders[1,]), lat=mean(bbox_hellenic_borders[2,])),zoom = 6,maptype = "terrain")
## 
## # plot map
## 
## map_greece_plot <- ggmap(map_greece)+
##   geom_polygon(data = natura2000shapefile_dataframe,aes(x=long, y=lat,group = group,fill="Natura2000"),lwd=0.082, alpha=0.6)+
##   geom_polygon(data = katafygia_agrias_zwhs_dataframe,aes(x=long, y=lat,group = group,fill="Wildlife Refuge"),lwd=0.082,alpha=0.8)+
##   geom_point(data = species_occurencies_unique_caves_all_info,aes(x=Longitude, y=Latitude,color="Caves"),size = 0.2)+
##   labs(x="Longitude",y="Latitude")+
##   scale_fill_manual(values = c("Natura2000"="chartreuse3","Wildlife Refuge"="chocolate2"),name="Protected areas")+
##   scale_color_manual(name="", values = c("Caves"="red"))+
##   scale_x_continuous(breaks = seq(20,30,2),limits = c(20,30))+
##   scale_y_continuous(breaks = seq(35,42,2),limits = c(34.5,42))+
##   coord_map(xlim = c(19,30.1), ylim = c(34.5,42))+
##   theme_bw()
## #geom_text(data = sisquoc, aes(label = paste("  ", as.character(name), sep="")), angle = 60, hjust = 0, color = "yellow")
## 
## ggsave("map_greece_plot_protected_areas.jpeg", plot = map_greece_plot, device = "jpeg", dpi = 300,path = "Plots/")
## 

#' ![Caves and protected areas in Greece](Plots/map_greece_plot_protected_areas.jpeg)
#' 
#' Only with borders.
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE, eval=FALSE,cache=TRUE----
## 
## 
## 
## map_greece_plot_lines <- ggplot()+
##   geom_polygon(data = natura2000_NEW_shapefile_dataframe,aes(x=long, y=lat,group = group,fill=SITE_TYPE),lwd=0.082, alpha=0.6)+
##   geom_polygon(data = hellenic_borders_shapefile_dataframe,aes(x=long, y=lat,group = group),lwd=0.12,color="black")+
##   geom_polygon(data = natura2000shapefile_dataframe,aes(x=long, y=lat,group = group,fill=SITETYPE),lwd=0.082, alpha=0.6)+
##   geom_polygon(data = katafygia_agrias_zwhs_dataframe,aes(x=long, y=lat,group = group,fill="Wildlife Refuge"),lwd=0.082,alpha=0.8)+
##   geom_point(data = species_occurencies_unique_caves,aes(x=Longitude, y=Latitude,color="Caves"),size = 0.2)+
##   labs(x="Longitude",y="Latitude")+
##   #scale_fill_manual(values = c("chartreuse3","purple","cyan3","chocolate2"),labels = c("Natura2000 SCI", "Natura2000 SPA", "Natura2000 SPASCI","Wildlife Refuge"),name="Protected areas")+
##   #scale_fill_manual(values = c("SCI"="chartreuse3","SPA"="purple","SPASCI"="cyan3", "Wildlife Refuge"="chocolate2"),labels = c("Natura2000 SCI", "Natura2000 SPA", "Natura2000 SPASCI","Wildlife Refuge")name="Protected areas")+
##   scale_color_manual(name="", values = c("Caves"="red"))+
##   scale_x_continuous(breaks = seq(20,30,1),limits = c(20,30))+
##   scale_y_continuous(breaks = seq(35,42,1),limits = c(34.5,42))+
##   coord_map(xlim = c(19,30.1), ylim = c(34.5,42))+
##   theme_bw()+
##   theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),legend.position = c(0.87, 0.75),legend.text = element_text(size=9),legend.title = element_text(size=10))
## #geom_text(data = sisquoc, aes(label = paste("  ", as.character(name), sep="")), angle = 60, hjust = 0, color = "yellow")
## 
## ggsave("map_greece_plot_lines.jpeg", plot = map_greece_plot_lines, device = "jpeg", dpi = 300,path = "Plots/")
## 
## 

#' 
#' ![Caves and protected areas in Greece (only borders)](Plots/map_greece_plot_lines.jpeg)
#' 
#' ## Macedonia caves and protected areas
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE, eval=FALSE,cache=TRUE----
## # plot map
## 
## location_alistrati <- c(lon=23.9663,lat=41.0508)
## 
## map_makedonia <- get_map(location = location_alistrati,zoom = 10,maptype="terrain")
## 
## map_makedonia_NATURA_dataframe <- natura2000shapefile_dataframe %>% filter(., CODE=="GR1260009")
## 
## map_makedonia_katafygia_agrias_zwhs_dataframe <- katafygia_agrias_zwhs_dataframe %>% filter(.,area=="5688567")
## 
## map_makedonia_plot <- ggmap(map_makedonia)+
##   geom_polygon(data = map_makedonia_NATURA_dataframe,aes(x=long, y=lat,group = group,fill=CODE),lwd=0.082, alpha=0.8)+
##   geom_polygon(data = map_makedonia_katafygia_agrias_zwhs_dataframe,aes(x=long, y=lat,group = group,fill=area),lwd=0.082, alpha=0.8)
## 
## ggsave("caves_protected_areas_Macedonia.jpeg", plot = map_makedonia_plot, device = "jpeg", dpi = 300,path = "Plots/")
## 

#' ![Caves and protected areas in Mecedonia region](Plots/caves_protected_areas_Macedonia.jpeg)
#' 
#' 
#' # What's next
#' 
#' 1. Assign to each cave the respective description and exploration references from literature
#' 
#' Done: `r sum(!is.na(unique(species_occurencies$CaveReferences)))`
#' 
#' Missing: `r nrow(species_occurencies_unique_caves)-sum(!is.na(unique(species_occurencies$CaveReferences)))`
#' 
#' 2. Locate all caves
#' 
#' Done: `r species_occurencies_unique_caves %>% dplyr::select(Latitude) %>% filter(!(is.na(Latitude))) %>% nrow()`
#' 
#' Missing: `r species_occurencies_unique_caves %>% dplyr::select(Latitude) %>% filter(is.na(Latitude)) %>% nrow()`
#' 
#' 3. Find the distributions of species
#' 
#' Done: `r species_occurencies %>% distinct(Species,Distribution_IUCN) %>% filter(!is.na(Distribution_IUCN)) %>% nrow()`
#' 
#' Missing: `r species_occurencies %>% distinct(Species,Distribution_IUCN) %>% filter(is.na(Distribution_IUCN)) %>% nrow()`
#' 
#' 4. Species type regarding troglofauna characterisation
#' 
#' Done:`r species_occurencies %>% distinct(Species,Troglofauna_characterisation) %>% filter(!is.na(Troglofauna_characterisation)) %>% nrow()`
#' 
#' Missing: `r species_occurencies %>% distinct(Species,Troglofauna_characterisation) %>% filter(is.na(Troglofauna_characterisation)) %>% nrow()`
#' 
#' 5. Complete the list of species hyperinks with other databases
#' 
#' `r species_links_summary_caption <- "Links to different databases for individual species"
#' kable(species_links_summary,caption=species_links_summary_caption, align = 'l')`
#' 
#' 
#' In the case of IUCN database there are missing links because these species are not included in the databese. This has to be clarified for the rest databases.
#' 
#' 
## ----setup, include=FALSE,warning=FALSE, message=FALSE,eval=FALSE--------
## 
## 
## jabref_caves <- read_csv(file = "Cave_Fauna_References28_08_2017.csv",col_names = T)
## 
## jabref_caves <- jabref_caves %>% select(Identifier,Author,Title,Journal,Volume,Number,Pages,Year,Booktitle,Chapter,Edition,Editor,Publisher,Custom3)
## 
## colnames(jabref_caves) <- c("Identifier","Author","Title","Journal","Volume","Number","Pages","Year","Booktitle","Chapter","Edition","Editor","Publisher","Keywords")
## 
## #write_delim(jabref_caves,path = "/Users/savasp/Dropbox/Ινστιτούτο Σπηλαιολογικών Ερευνών Ελλάδας/Conservation of the cave fauna of Greece - MAVA/Cave_Fauna_database/Cave_Fauna_References_28_08_2017_fromR.tsv",delim = "\t")
## 
## id <- jabref_caves %>% group_by(Identifier) %>% summarise(n=n())
## 
## 

#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE, eval=FALSE---------------
## 
## #### Not RUN
## 
## #from csv that RafJeb exported! Problems with the year..
## 
## species_references <- read_csv(file = "Cave_Fauna_References28_08_2017.csv",col_names = TRUE)
## 
## #species_references1 <- species_references %>% mutate(n_authors=)
## species_references$n_authors <- str_count(species_references$Author, pattern = ";") +str_count(species_references$Author, pattern = "&") +1
## 
## species_references$first_author <- gsub("^(.*?),.*", "\\1", species_references$Author)
## 
## species_references$second_author <- gsub(pattern = ".*& (.+?),.*",replacement = "\\1",species_references$Author)  #"; *([^.,]*)"
## 
## species_references$Pages_first <- gsub("^(.*?)--.*", "\\1", species_references$Pages)
## 
## species_references$Pages_last <- sub('.*\\--', '', species_references$Pages)
## 
## species_references$Pages <- with(species_references, ifelse(Pages_first==Pages_last,paste0(Pages_first),paste0(Pages_first,"-",Pages_last)))
## 
## species_references$short_reference <- with(species_references,ifelse(n_authors==1,paste0(first_author," ",Year),ifelse(n_authors==2,paste0(first_author," & ",second_author," ",Year),paste0(first_author," et al. ",Year))))
## 
## species_references_ready <- species_references %>% group_by(short_reference) %>% mutate(n_dupl_citation=n()) %>% mutate(short_reference_ready=if_else(n_dupl_citation==1,paste0(short_reference),paste0(short_reference,letters[1:n_dupl_citation]))) %>% ungroup(short_reference) %>% dplyr::select(-Author, -short_reference,-second_author,-Pages_last,-Pages_first,-n_authors,-first_author,-second_author,-n_dupl_citation,-BibliographyType,-ISBN,-Custom5,-Custom4)
## 
## 
## ## Initials of Authors
## 
## species_references_author <- strsplit(x = as.character(species_references$Author), "[;&]")
## 
## species_references_author_spreaded <- data.frame(Identifier = rep.int(species_references$Identifier,sapply(species_references_author, length)),Author = rep.int(species_references$Author,sapply(species_references_author, length)), Author_spread = gsub("^\\s|\\s$", "",unlist(species_references_author)))
## 
## 
## Last_names <- gsub(",.*", "\\1", species_references_author_spreaded$Author_spread,perl = TRUE)
## 
## sort_names_initials <- iconv(gsub("[:a-z:]|\\s|\\.|[[:punct:]]", "", gsub(".*,", "\\1", species_references_author_spreaded$Author_spread,perl = TRUE)), "UTF-8", "ASCII", sub = "")
##  #\\B\\w
## 
## sort_names_initials_ready <- trimws(gsub("([A-Z])", "\\1. ", sort_names_initials),which = "right")
## 
## species_references_author_spreaded$Author_spread_Initials <- paste0(Last_names,", ",sort_names_initials_ready)
## species_references_author <- species_references_author_spreaded %>% group_by(Identifier) %>% summarise(Author=paste0(Author_spread_Initials,collapse="; ")) %>% mutate(Author=gsub(";",",",gsub("(.*)\\;(.*)", "\\1 &\\2",Author)))
## 
## 
## ##References ready for import to database
## 
## species_references_ready <- species_references_ready %>% left_join(., species_references_author, by=c("Identifier"="Identifier"))
## 

#' 
#' 
## ----warning=FALSE, message=FALSE, echo=FALSE,eval=FALSE-----------------
## #Read XML.
## 
## 
## library(XML)
## 
## 
## 
## xmlfile <- xmlParse("My CollectionXML.xml")
## # the xml file is now saved as an object you can easily work with in R:
## class(xmlfile)
## 
## 
## xmltop <- xmlRoot(xmlfile) #gives content of root
## class(xmltop)#"XMLInternalElementNode" "XMLInternalNode" "XMLAbstractNode"
## xmlName(xmltop) #give name of node, PubmedArticleSet
## xmlSize(xmltop) #how many children in node, 19
## xmlName(xmltop[[1]]) #name of root's children
## 
## 
## data <- xmlSApply(xmltop,function(x) xmlSApply(x, xmlValue))
## 
## cd.catalog <- data.frame(t(data),row.names=NULL)
## 

#' 
## ----  warning=FALSE, message=FALSE, echo=FALSE, eval=FALSE--------------
## 
## ## RUN because here there are the names of municipalities
## 
## ########### Municipalities data from csv. It was unsuccessfull. ############## Some polygons were missing area. Maybe some of the transformations broke them.
## 
## municipalities_greece <- read_csv(file = "municipalities_shape_file/municipalities_Kallikratis_plan_Greece.csv",col_names = T) %>% dplyr::select(NAME,the_geom,KWD_YPES)
## 
## #municipalities_greece$the_geom0 <- gsub(" ",";",municipalities_greece$the_geom)
## 
## municipalities_greece$the_geom2 <- gsub("[MULTIPOLYGON()]","",municipalities_greece$the_geom)
## 
## #municipalities_greece$the_geom3 <- gsub(")))","",municipalities_greece$the_geom2)
## 
## 
## municipalities_greece_str <- strsplit(x = as.character(municipalities_greece$the_geom2), ",")
## 
## municipalities_greece_long <- data.frame(Municipality = rep.int(municipalities_greece$NAME, sapply(municipalities_greece_str, length)),KWD_YPES = rep.int(municipalities_greece$KWD_YPES, sapply(municipalities_greece_str, length)), Reference_spread = unlist(municipalities_greece_str))
## 
## municipalities_greece_long$Latitude <- as.character(lapply(strsplit(as.character(municipalities_greece_long$Reference_spread), split=" "), "[", n=2))
## 
## municipalities_greece_long$Longtitude <- as.character(lapply(strsplit(as.character(municipalities_greece_long$Reference_spread), split=" "), "[", n=3))
## 
## municipalities_greece_long_names <- municipalities_greece_long %>% dplyr::select(Municipality,KWD_YPES) %>% distinct()
## 
## municipalities_greece_long_names_eng <- read_xlsx("municipalities_shape_file/names_municipalities_gr_eng.xlsx",col_names = T)
## 
## municipalities_greece_long_names_combined <- cbind(municipalities_greece_long_names,municipalities_greece_long_names_eng)
## 
## colnames(municipalities_greece_long_names_combined) <- c("Municipality","KWD_YPES","Municipalities_ISO_843")
## # fileConn<-file("municipalities_greece_long_names.txt")
## # writeLines(as.character(municipalities_greece_long_names$Municipality), fileConn)
## # close(fileConn)
## 
## municipalities_greece_long_names_combined$KWD_YPES <- as.factor(municipalities_greece_long_names_combined$KWD_YPES)
## 
## # Get data ready to import to Spatial Polygon
## 
## municipalities_greece_long$Latitude <- as.numeric(municipalities_greece_long$Latitude)
## municipalities_greece_long$Longtitude <- as.numeric(municipalities_greece_long$Longtitude)
## 
## municipalities_greece_long_2 <- municipalities_greece_long %>% dplyr::select(Latitude,Longtitude)
## 
## #Transform dataframe to list
## 
## municipalities_greece_long_list <- split(municipalities_greece_long_2, municipalities_greece_long$Municipality)
## # only want lon-lats in the list, not the names
## municipalities_greece_long_list <- lapply(municipalities_greece_long_list, function(x) { x["Municipality"] <- NULL; x })
## 
## ps <- lapply(municipalities_greece_long_list, Polygon)
## 
## 
## # add id variable
## p1 <- lapply(seq_along(ps), function(i) Polygons(list(ps[[i]]),
##                                             ID = names(municipalities_greece_long_list)[i]))
## 
## 
## # create SpatialPolygons object
## EPSG4326<-CRS("+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
## 
## my_spatial_polys <- SpatialPolygons(p1, proj4string = EPSG4326 )
## 
## 

#' 
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE, eval=FALSE---------------
## #example
## #http://data-analytics.net/cep/Schedule_files/geospatial.html
## 
## 
## data(crime)
## plot(crime$lon,crime$lat,xlab="lon",ylab="lat")
## 
## HoustonMap<-qmap("houston", zoom = 14)  #First, get a map of Houston
## 
## HoustonMap+                                        ##This plots the Houston Map
## geom_point(aes(x = lon, y = lat), data = crime)
## 
## 
## houston <- get_map(location = "houston", zoom = 13) ##Get the houston map
## houstonMap<-ggmap(houston, extent = "device")       ##Prepare Map
## 
## houstonMap +
##   stat_density2d(aes(x = lon, y = lat, fill = ..level..,alpha=..level..), bins = 10, geom = "polygon", data = crime) +
##   scale_fill_gradient(low = "black", high = "red")+
##   ggtitle("Map of Crime Density in Houston")

#' 
#' 
#' # References

#' 
#' # Numerical Analysis
#' 
## ---- child='CFG_Numerical_Analysis.Rmd'---------------------------------

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
#library(tmap)
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
#' ## Q Mode
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------
library(cluster)
library(vegan)
#library(FD)
library(gclus)
library(ggdendro)



#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------
caves_crete <- caves %>% filter(Region=="Kriti")

troglobiont <- species %>% filter(Classification=="Troglobiont")

caves_species_wide <- census_long_man %>% dplyr::select(Cave_ID,Species) %>% distinct() %>% filter(Species %in% troglobiont$Species_Full_Name) %>% mutate(presence=1) %>% spread(value = presence,key = Species,fill = 0)

rownames(caves_species_wide) <- caves_species_wide$Cave_ID

caves_species_wide <- caves_species_wide %>% dplyr::select(-Cave_ID)


#' 
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------
caves_species_wide_distance <- vegdist(caves_species_wide,binary = T,method = "jaccard")
caves_species_wide_distance_t <- vegdist(t(caves_species_wide),binary = T,method = "jaccard")


caves_species_wide_distance_ma <- as.matrix(caves_species_wide_distance)

row.order <- order.dendrogram(as.dendrogram(hclust(caves_species_wide_distance))) # clustering
#col.order <- order.dendrogram(as.dendrogram(hclust(caves_species_wide_distance_t)))

caves_species_wide_distance_ma_df <- as.data.frame(caves_species_wide_distance_ma[row.order,row.order])

caves_species_wide_distance_ma_df$cave1 <- rownames(caves_species_wide_distance_ma_df)

caves_species_wide_distance_df <- gather(caves_species_wide_distance_ma_df,cave2,value = distance,-cave1)

#caves_species_wide_distance_df$item1 <- factor(x=caves_species_wide_distance_df$item1,levels=row.order,ordered = TRUE)
#caves_species_wide_distance_df$item2 <- factor(x=caves_species_wide_distance_df$item2,levels=col.order,ordered = TRUE)

caves_species_wide_distance_tidy <- tidy(caves_species_wide_distance)

#' 
#' ## Dendrograms
#' 
## ------------------------------------------------------------------------
plot(hclust(caves_species_wide_distance,method = "ward.D"),cex=0.5)

#' 
## ------------------------------------------------------------------------

cluster_caves_species_wide_distance <- hclust(caves_species_wide_distance,method = "ward.D")

dendro <- as.dendrogram(cluster_caves_species_wide_distance)


ddata <- dendro_data(dendro, type="rectangle")
labels_dendro <- label(ddata) %>% mutate(label_numeric=as.numeric(label)) %>% left_join(caves, by=c("label_numeric"="Cave_ID"))
last_branches <- ddata$segments %>% filter(yend == 0) %>% left_join(labels_dendro, by = "x") # merge segmants with labels

ggplot() + 
  geom_segment(data = segment(ddata),aes(x=x, y=y, xend=xend, yend=yend)) + 
  geom_segment(data = last_branches, aes(x=x, y=y.x, xend=xend, yend=yend, color = Region)) +
  geom_text(data = ddata$labels, aes(x, y, label = label, colour=labels_dendro$Region), hjust = 1, angle = 90, size = 1)+
  scale_colour_manual(values = colorRampPalette(c("orangered2","palegreen3","skyblue1","slateblue1","pink2","goldenrod1","slategray2"),space="Lab")( 14 ))+
  ggtitle("Caves hierarchical clustering")+
  theme_dendro()

ggsave("caves_species_jaccard_distance_dendro.png", plot = last_plot(), device = "png",width = 20,height = 20,units = "in", dpi = 300,path = "Plots/")

#' 
#' 
#' ## Heatmaps
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------
ggplot() +
  geom_tile(data = caves_species_wide_distance_df, aes(x = cave1, y = cave2,fill = distance)) +
  scale_fill_gradient(low = "red", high = "white") +
  ylab("Caves") +
  xlab("Caves") +
  theme_bw() +
  labs(fill = "Jaccard distance")+
  theme(axis.ticks = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1,size = 2),axis.text.y = element_text(angle = 45, hjust = 1,size = 2))

ggsave("caves_species_jaccard_distance.png", plot = last_plot(), device = "png",width = 20,height = 20,units = "in", dpi = 300,path = "Plots/")


#' 
## ------------------------------------------------------------------------
heatmap <- heatmap(caves_species_wide_distance_ma,col = heat.colors(256))


#' 
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------
ggplot() +
  geom_tile(data = caves_species_wide_distance_tidy, aes(x = item1, y = item2,fill = distance)) +
  scale_fill_gradient(low = "red", high = "white") +
  ylab("Caves") +
  xlab("Caves") +
  theme_bw() +
  labs(fill = "Jaccard distance")


#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------
caves_species_wide_distance_df_hist <- caves_species_wide_distance_df %>% group_by(distance) %>% summarise(n=n())

ggplot(data=caves_species_wide_distance_df_hist)+
  geom_line(aes(x=distance, y= n),color="purple",show.legend = F)+
  geom_point(aes(x=distance, y= n),color="purple",show.legend = F, size=1)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())


#' 
#' ## Example
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------
library(vegan)
library(ggplot2)
library(reshape2)

# set binary: TRUE if presence/absence data
#             FALSE if abundance data
binary <- TRUE

m <- 500
n <- m
set.seed(12345)
if(binary == TRUE){
  # Create matrix with random 0/1 values
  x <- sample.int (2, m*n, TRUE)-1L
  dim(x) <- c(m,n)  
} else {
  # Or create matrix with values in the range 0-10
  x <- matrix(round(runif(n*m, 0, 10)), nrow = m, ncol = n)  
}

#### Test the standardization 
jaccard <- vegdist(x, method = "jaccard", binary = FALSE)
jaccard.stand <- vegdist(x, method = "jaccard", binary = TRUE)
### Are they equal?
all(jaccard == jaccard.stand)

jacc.dft <- tidy(jaccard)


### Plot for safety
jacc.df <- melt(cbind(jaccard, jaccard.stand))
ggplot(jacc.df, aes(x = value, fill = Var2, colour = Var2)) + 
  geom_histogram(alpha = 0.2, position = "identity", binwidth = 0.01)

#' 
#' ## Species - Area Relationship
#' 
#' ## Species accumulation curve
#' 
## ---- warning=FALSE, message=FALSE, echo=FALSE---------------------------

species_accumulation <- census %>% distinct(Species,Cave_ID) %>% mutate(Duplicates=duplicated(Species), species_epithet= as.character(lapply(strsplit(as.character(Species), split=" "), "[", n=2))) %>% filter(species_epithet!="sp.") %>% mutate(.,First_occurance=if_else(Duplicates=="FALSE",1,0)) %>% filter(First_occurance==1) %>% group_by(Cave_ID) %>% mutate(First_occurance_per_cave=sum(First_occurance))# %>% group_by(First_occurance_per_cave) %>% summarise(number_of_caves=n()) #mutate(Cumulative_occurance= cumsum(First_occurance))

species_accumulation_wide <- census %>% mutate(species_epithet= as.character(lapply(strsplit(as.character(Species), split=" "), "[", n=2))) %>% filter(species_epithet!="sp.") %>% dplyr::select(Cave_ID,Species) %>% distinct() %>% mutate(presence=1) %>% spread(value = presence,key = Species,fill = 0) %>% tibble::column_to_rownames(.,var="Cave_ID")  # %>% dplyr::select(-Cave_ID)

acc <- specaccum(species_accumulation_wide,"random")

specpool_m <- poolaccum(x = species_accumulation_wide)

specpool_m_summary <- summary(specpool_m)

specpool_m_summary_chao <- as.data.frame(specpool_m_summary$chao)

specpool_m_summary_bootstrap <- as.data.frame(specpool_m_summary$boot)

Vspecpool_m_data <- as.data.frame(specpool_m$means)
data_accumutation_c <- data.frame(Sites=acc$sites, Richness=acc$richness, SD=acc$sd)

ggplot() +
  #geom_point(data=data_accumutation_c, aes(x=Sites, y=Richness)) +
  geom_line(data=data_accumutation_c, aes(x=Sites, y=Richness, color="Accumulation curve")) +
  #geom_line(data=Vspecpool_m_data, aes(x=, y=increasing)) +
  geom_line(data=specpool_m_summary_bootstrap, aes(x=N, y=Bootstrap,color="Bootstrap extrapolation")) +
  #geom_line(data=specpool_m_summary_chao, aes(x=N, y=Chao, color="Chao extrapolation")) +
  geom_ribbon(data=data_accumutation_c ,aes(x=Sites, ymin=(Richness-2*SD),ymax=(Richness+2*SD)),alpha=0.2)+
  geom_ribbon(data=specpool_m_summary_bootstrap ,aes(x=N, ymin=(Bootstrap-2*Std.Dev),ymax=(Bootstrap+2*Std.Dev)),alpha=0.2)+
  #geom_ribbon(data=specpool_m_summary_chao ,aes(x=N, ymin=(Chao-2*Std.Dev),ymax=(Chao+2*Std.Dev)),alpha=0.2)+
  scale_y_continuous(breaks = seq(0,1100,100),limits = c(0,1100))+
  scale_x_continuous(breaks = seq(0,500,50),limits = c(0,500))+
  scale_color_manual(name="",values = c("Accumulation curve"="black","Bootstrap extrapolation"="pink","Chao extrapolation"="blue"))+
  ggtitle("Cave fauna of Greece accumulation curve")+
  #coord_equal()+
  labs(x="Caves", y= "Number of species")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),legend.position = c(0.18,0.86))

ggsave("species_accumulation_curve.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,path = "Plots/")



#### CRETE


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

#' 
#' 
#' # References
