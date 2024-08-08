# Data exploration

library(tidyverse)
library(janitor)


## data
chalk_river <- read.csv("data/rimnet_-_chalk_river_laboratories_environment_database.csv")
str(chalk_river)


chalk_river <- janitor::clean_names(chalk_river)
colnames(chalk_river)
str(chalk_river)

# [1] "year"                    "facility_name"           "sample_station_name"    
# [4] "latitude"                "longitude"               "sample_type_en"         
# [7] "sample_type_fr"          "substance_name_en"       "substance_name_fr"      
# [10] "statistical_quantity_en" "statistical_quantity_fr" "energy_below_1_mev"     
# [13] "result"                  "units"  

unique(chalk_river$sample_type_en)
# [1] "Atmospheric Releases"                 "Liquid Releases"                     
# [3] "Air Quality"                          "Surface Water Quality - Ottawa River"
# [5] "Absorbed Dose"                        "Surface Water Quality - General"     
# [7] "Fruit"                                "Beans"                               
# [9] "Root"                                 "Vegetable"                           
# [11] "Milk"                                 "Sediment"                            
# [13] "Fish"                                 "Clams"   

unique(chalk_river$substance_name_en)




nordion <- read.csv("data/rimnet_-_nordion_environment_database.csv")
nordion <- janitor::clean_names(nordion)
colnames(nordion)
str(nordion)

# [1] "year"                    "facility_name"           "sample_station_name"    
# [4] "latitude"                "longitude"               "sample_type_en"         
# [7] "sample_type_fr"          "substance_name_en"       "substance_name_fr"      
# [10] "statistical_quantity_en" "statistical_quantity_fr" "energy_below_1_mev"     
# [13] "result"                  "units"                 

demonstration <- read.csv("data/rimnet_-_nuclear_power_demonstration_environment_database.csv")
demonstration <- janitor::clean_names(demonstration)
colnames(demonstration)

# [1] "year"                    "facility_name"           "sample_station_name"    
# [4] "latitude"                "longitude"               "sample_type_en"         
# [7] "sample_type_fr"          "substance_name_en"       "substance_name_fr"      
# [10] "statistical_quantity_en" "statistical_quantity_fr" "energy_below_1_mev"     
# [13] "result"                  "units"   

srbt <- read.csv("data/rimnet_-_srbt_environment_database.csv")
srbt  <- janitor::clean_names(srbt)
colnames(srbt)
str(srbt)

# [1] "year"                    "facility_name"           "sample_station_name"    
# [4] "latitude"                "longitude"               "sample_type_en"         
# [7] "sample_type_fr"          "substance_name_en"       "substance_name_fr"      
# [10] "statistical_quantity_en" "statistical_quantity_fr" "energy_below_1_mev"     
# [13] "result"                  "units"  


srbt_demonstration <- full_join(srbt, demonstration)
str(srbt_demonstration)

nordion_chalk_river <- full_join(nordion, chalk_river)
str(nordion_chalk_river)

# remove character values
nordion_chalk_river <- nordion_chalk_river %>% filter(result != "ND")
nordion_chalk_river <- nordion_chalk_river %>% filter(result != "NC")
nordion_chalk_river <- nordion_chalk_river %>% filter(result != "MDA")
srbt_demonstration <- srbt_demonstration %>% filter(result != "*")
srbt_demonstration <- srbt_demonstration %>% filter(result != "ND")
srbt_demonstration <- srbt_demonstration %>% filter(result != "NC")
srbt_demonstration <- srbt_demonstration %>% filter(result != "MDA")
srbt_demonstration$result <- as.numeric(srbt_demonstration$result)

# join all
all_facilites <- full_join(srbt_demonstration, nordion_chalk_river)


all_facilites$sample_category <- recode_factor(all_facilites$sample_category,
                                               "Atmospheric Releases" = "Abiotic",
                                               "Liquid Releases" = "Abiotic",
                                               "Air Quality" = "Abiotic",
                                               "Surface Water Quality" = "Abiotic",
                                               "Vegetation" = "Biotic",
                                               "Soil" = "Abiotic",
                                               "Surface Water Quality - Ottawa River" = "Abiotic",
                                               "Absorbed Dose" = "Abiotic",
                                               "Surface Water Quality - General" = "Abiotic",
                                               "Fruit" = "Consumption",
                                               "Beans" = "Consumption",
                                               "Root" = "Consumption",
                                               "Vegetable" = "Consumption",
                                               "Milk" = "Consumption",
                                               "Sediment" = "Abiotic",
                                               "Fish" = "Biotic",
                                               "Clams" = "Biotic")


write.csv(all_facilites, 'data/all_facilities.csv', row.names = FALSE)

# Exploration figures -----------------------------------------------------

all_facilities <- read.csv('data/all_facilities.csv')

all_facilites$sample_category <- all_facilites$sample_type_en


#counts
sample_substance <- all_facilites %>% group_by(sample_category,
                                               sample_type_en,
                                               substance_name_en) %>% count()

abiotic <- sample_substance %>% 
  filter(sample_category == 'Abiotic') %>% 
  ggplot(aes(x = substance_name_en , y = n, 
             group = sample_type_en, fill = sample_type_en)) +
  geom_col(linewidth = 1) +
  xlab(" ") +
  ylab("Count") +
  theme_bw() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.position = c(0.6,0.5),
        legend.title = element_blank()) +
  ggtitle("Abiotic Samples") +
  coord_flip()

biotic <- sample_substance %>% 
  filter(sample_category == 'Biotic') %>% 
  ggplot(aes(x = substance_name_en , y = n, 
             group = sample_type_en, fill = sample_type_en)) +
  geom_col(linewidth = 1) +
  xlab(" ") +
  ylab("Count") +
  theme_bw() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        legend.text = element_text(size = 13),
        legend.title = element_blank()) +
  ggtitle("Biotic Samples") +
  coord_flip()

consumption <- sample_substance %>% 
  filter(sample_category == 'Consumption') %>% 
  ggplot(aes(x = substance_name_en , y = n, 
             group = sample_type_en, fill = sample_type_en)) +
  geom_col(linewidth = 1) +
  xlab(" ") +
  ylab("Count") +
  theme_bw() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        legend.text = element_text(size = 13),
        legend.title = element_blank()) +
  ggtitle("Food samples") +
  coord_flip()

library(patchwork)

layout <- "
AAABBB
AAABBB
"

panel2 <- biotic / consumption

samples_panel <- abiotic + panel2 + 
  plot_layout(design = layout)

ggsave('output/sample_panel.jpg',
       width = 16.4,
       height = 9.06)
