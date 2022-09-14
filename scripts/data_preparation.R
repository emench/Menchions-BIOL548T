############################################
############# Data Preparation ############# 
############################################
# Emma Menchions, Sept 10/22
# This script is to prepare the data from Thiffault et al. (2016) for another analysis

install.packages("groundhog")
library(groundhog)

# installing & loading packages
date <- "2022-09-02"
requiredPackages <-  c("here", "tidyverse","Rfast","dplyr")

for (pkg in requiredPackages) {
  if (pkg %in% rownames(installed.packages()) == FALSE)
  {install.packages(pkg)}
  if (pkg %in% rownames(.packages()) == FALSE)
  {groundhog.library(pkg, date)}
}

# Loading data tables ----
X_mat <- read.csv(here::here("doi_10.5061_dryad.4767v__v1/Data_ThiffaultEtAl_EcoEvo/Matrice_X.csv"),
                  sep = ";") # explanatory variables - see metadata
Y_mat <- read.csv(here::here("doi_10.5061_dryad.4767v__v1/Data_ThiffaultEtAl_EcoEvo/Matrice_Y.csv"),
                  sep = ";") # response variables (mean ericaceous cover, in %)
clim <- read.csv(here::here("doi_10.5061_dryad.4767v__v1/Data_ThiffaultEtAl_EcoEvo/Climate_value.csv"),
                 sep = ";") # numerical codes used for clim vars in X_mat

# Classifying Forest Type ----
# Identifying two most dominant tree species in each plot and assigning forest type as 
  # conifer (c)
  # deciduous (d)
  # mixed (m)
# looping through each row of X_mat dataframe

for (i in 1:dim(X_mat)[1]){
  most_abun <-  as.numeric(Rfast::nth(as.matrix(X_mat[i,2:14]), k=1, descending = T,index.return = T)) # returning the index of the column with the most abundant tree species for each row
  sec_abun <- as.numeric(Rfast::nth(as.matrix(X_mat[i,2:14]), k=2, descending = T,index.return = T)) # returning index of second most abundant species
  dominant_sp <- c(most_abun, sec_abun)+1 # adding 1 to account for the first column which was left out of vector for previous function
  dominant_sp <- colnames(X_mat)[dominant_sp] # creating character vector containing column names of most dominant species 
  
  if (dominant_sp[1] == "AB" || # if both species conifers
      dominant_sp[1] == "PB" ||
      dominant_sp[1] == "PM"||
      dominant_sp[1] == "PM_AB" ||
      dominant_sp[1] == "TO" &&
      dominant_sp[2] == "AB" ||
      dominant_sp[2] == "PB" ||
      dominant_sp[2] == "PM"||
      dominant_sp[2] == "PM_AM" ||
      dominant_sp[2] == "TO"){
      X_mat$Forest_Type[i] = "c" # forest type conifer
      
  }else if (dominant_sp[1] == "BA" || # if both species deciduous
            dominant_sp[1] == "BP" ||
            dominant_sp[1] == "PT" &&
            dominant_sp[2] == "BA" ||
            dominant_sp[2] == "BP" ||
            dominant_sp[2] == "PT"){
    X_mat$Forest_Type[i] = "d" # forest type deciduous 
    
  }else if (dominant_sp[1] == "BP_AB" || # if either species is mixed
            dominant_sp[1] == "BP_PM" ||
            dominant_sp[1] == "PT_AB"|| 
            dominant_sp[1] == "PT_PM" ||
            dominant_sp[1] == "BP_AB" ||
            dominant_sp[1] == "BP_PM" ||
            dominant_sp[1] == "PT_AB"|| 
            dominant_sp[1] == "PT_PM"){
    X_mat$Forest_Type[i] = "m"  # forest type mixed
    
  }else if(dominant_sp[1] == "AB" || # if first species is conifer, second is deciduous 
            dominant_sp[1] == "PB" ||
            dominant_sp[1] == "PM"||
            dominant_sp[1] == "PM_AB" ||
            dominant_sp[1] == "TO" &&
            dominant_sp[2] == "BA" ||
            dominant_sp[2] == "BP" ||
            dominant_sp[2] == "PT"){
    X_mat$Forest_Type[i] = "m" # forest type mixed
    
  }else if(dominant_sp[1] == "BA" || # if first species is deciduous, second is conifer
            dominant_sp[1] == "BP" ||
            dominant_sp[1] == "PT" &&
            dominant_sp[2] == "AB" ||
            dominant_sp[2] == "PB" ||
            dominant_sp[2] == "PM"||
            dominant_sp[2] == "PM_AB" ||
            dominant_sp[2] == "TO"){
            X_mat$Forest_Type[i] = "m" # forest type mixed
}else{
  X_mat$Forest_Type[i] = "n" # non-forested - other
}
}

# checking the dimensions of the dataframe when we select for specific cateogires
# tells us that the conifer type dominates
# this difference will be normalized later when we take the difference 

dim(dplyr::filter(X_mat,Forest_Type=="c")) 
dim(dplyr::filter(X_mat,Forest_Type=="d")) 
dim(dplyr::filter(X_mat,Forest_Type=="m"))
dim(dplyr::filter(X_mat,Forest_Type=="n")) 

# Classifying as high or low precipitation ----
# PRECU is the annual rainfall variable 

med <- median(X_mat$PRECU)
# will use the median as the threshold 

X_mat <- X_mat %>% 
  mutate(., RainFall = case_when(PRECU < med ~ "L", 
            PRECU >= med ~ "H")) 
# Combining and classifying as forest type - rainfall index
X_mat <- 
  X_mat %>% 
  unite(., rf_index, c("RainFall","Forest_Type"), sep = "_", remove =F, na.rm = FALSE)

# writing altered data as .csv ----
write.csv(X_mat, here::here("X_mat_processed.csv"))
  

    
      
      
  
                  