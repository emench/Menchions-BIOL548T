############################################
############# Data Analysis ################
############################################
# Emma Menchions, Sept 23/22
# This script is to prepare the data from Thiffault et al. (2016) for another analysis

# Loading packages ----
date <- "2022-09-02"
requiredPackages <-  c("ggplot2","here", "tidyverse","tidyr","Rfast","dplyr")

for (pkg in requiredPackages) {
  if (pkg %in% rownames(installed.packages()) == FALSE)
  {install.packages(pkg)}
  if (pkg %in% rownames(.packages()) == FALSE)
  {groundhog.library(pkg, date)}
}

# loading in prepared data ----
  X_mat <- read.csv(here::here("data","X_mat_processed.csv"))
  X_mat <- X_mat %>% 
    select(CLE, Forest_Type, PRECU, RainFall,rf_index) 
  
  Y_mat <- read.csv(here::here("data","Matrice_Y.csv"),
                    sep = ";") # response variables (mean ericaceous cover, in %)
  clim <- read.csv(here::here("data","Climate_value.csv"),
                   sep = ";") # numerical codes used for clim vars in X_mat
  
# PLOTTING WITH BAR CHARTS ----
  
  # For Chamaedaphne calyculata
  CAL <- data.frame(Y_mat$V_CAL,X_mat$PRECU, X_mat$rf_index) # creating dataframe with only this species
  colnames(CAL) <- c("V_CAL","PRECU","rf_index") # renaming columns 
  ggplot(data=CAL, aes(x=rf_index, y=V_CAL)) + # plotting only this species with the rainfall-forest type index in box plots
    geom_boxplot() 
  
  ggplot(data=CAL, aes(x=rf_index, y=log(V_CAL))) + # log transforming the y axis for easier pattern detection
    geom_boxplot()  # automatically removes observations with 0 % cover - which is ok 
  
  # For Rhododendron groenlandicum
  RHG <- data.frame(Y_mat$V_RHG,X_mat$PRECU,X_mat$rf_index) # creating dataframe with only this species
  colnames(RHG) <- c("V_RHG","PRECU","rf_index") # renaming columns 
  ggplot(data=RHG, aes(x=rf_index, y=V_RHG)) + # plotting only this species with the rainfall-forest type index in box plots
    geom_boxplot() 
  
  ggplot(data=RHG, aes(x=rf_index, y=log(V_RHG))) + # log transforming the y axis for easier pattern detection
    geom_boxplot()  # automatically removes observations with 0 % cover - which is ok 
  
## For Kalmia angustifolia
  # Plotting: 
  KAA <- data.frame(Y_mat$V_KAA,X_mat$PRECU,X_mat$rf_index) # creating dataframe with only this species
  colnames(KAA) <- c("V_KAA","PRECU","rf_index") # renaming columns
  
  ggplot(data=KAA, aes(x=rf_index, y=V_KAA)) + # plotting only this species with the rainfall-forest type index in box plots
    geom_boxplot() 
  
  ggplot(data=KAA, aes(x=rf_index, y=log(V_KAA))) + # log transforming the y axis for easier pattern detection
    geom_boxplot()  # automatically removes observations with 0 % cover - which is ok 
  
## For Vaccinium sp. 
    VAAM <- data.frame(Y_mat$V_VAAM,X_mat$PRECU,X_mat$rf_index) # creating dataframe with only this species
    colnames(VAAM) <- c("V_VAAM","PRECU","rf_index") # renaming columns
    
    ggplot(data=VAAM, aes(x=rf_index, y=V_VAAM)) + # plotting only this species with the rainfall-forest type index in box plots
      geom_boxplot() 
    
    ggplot(data=VAAM, aes(x=rf_index, y=log(V_VAAM))) + # log transforming the y axis for easier pattern detection
      geom_boxplot()  # automatically removes observations with 0 % cover - which is ok 
    
## Plotting altogether in facets 
    # creating new variable to denote species to make table with all species
    CAL <- CAL %>% 
      add_column("species" = "CAL") %>%  # adding column with species names
      rename(cover ="V_CAL") # renaming percent cover column to represent all species
    
    RHG <- RHG %>% 
      add_column("species" = "RHG") %>% 
      rename(cover ="V_RHG")
    
    KAA <- KAA %>% 
      add_column("species" = "KAA") %>% 
      rename(cover ="V_KAA")
    
    VAAM <- VAAM %>% 
      add_column("species" = "VAAM") %>% 
      rename(cover ="V_VAAM")
    
    plot_cover <- rbind(CAL,RHG, KAA, VAAM) # combining all species by rows into one table 
    
    ## creating ggplot object
    p <- ggplot(data= plot_cover, aes(x=rf_index, y=log(cover))) + # log transforming the y axis for easier pattern detection
      geom_boxplot()+
      xlab("Rainfall-Forest Type Index (RF)") + # changing x axis labe
      ylab("Log (% cover)")+ # changing y axis label
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5), # changing plot themes for angled text and no gridlines
            panel.grid = element_blank())
    
    ## function to changes names to each plot in facet 
    # from https://stackoverflow.com/questions/3472980/how-to-change-facet-labels
      species_names <- list(
        'CAL'="C. calyculata",
        'KAA'="K. angustifolia",
        'RHG'="R. groenlandicum",
        'VAAM'="Vaccinium sp."
      )
      
      species_labeller <- function(variable,value){
        return(species_names[value])
      }
    
    ## creating facet plot with all species plots in one graph 
    p + facet_wrap( ~ species, scales="free",
                    labeller=species_labeller)
    
    # exporting plot to jpeg
    jpeg(file=here::here("outputs","Cover-RF_Bar-Chart.jpeg"))
    p + facet_wrap( ~ species, scales="free",
                    labeller=species_labeller)
    dev.off()

## ANOVA Analysis ----    
    
    
    