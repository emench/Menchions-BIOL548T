############################################
############# Data Analysis ################
############################################
# Emma Menchions, Sept 23/22
# This script is to prepare the data from Thiffault et al. (2016) for another analysis

# Loading packages ----
library(groundhog)
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
  
# PLOTTING WITH BOX PLOTS----
  
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
    jpeg(file=here::here("outputs","Cover-RF_Box-Plot.jpeg"))
    p + facet_wrap( ~ species, scales="free",
                    labeller=species_labeller)
    dev.off()

## Stats analysis ----    
    # using dplyr to compare summary stats
    group_by(plot_cover, species) %>%
      summarise(
        count = n(),
        mean = mean(cover, na.rm = TRUE),
        sd = sd(cover, na.rm = TRUE),
        median = median(cover, na.rm = TRUE),
        IQR = IQR(cover, na.rm = TRUE)
      )
    
    # We want to know is there any significant difference between percent over of plants in different rainfall and forest type conditions 
    
    ## 1) C. calyculata
      # Kruksal-Wallis test to determine if significant difference between means of rf-index groups
      CAL_cover <- plot_cover %>% filter(species=="CAL") # creating daframe with only this species
      kruskal.test(cover ~ rf_index, data = CAL_cover) # performing test - P < 0.05 - yes, indicates significatn differences between groups 
      # use pairwise wilcox test to determine which groups different 
      CAL_wt <- pairwise.wilcox.test(CAL_cover$cover, CAL_cover$rf_index,
                           p.adjust.method = "BH")
      
      # significant differences (p< 0.05): H_d vs H_c ; H_m vs H_d ; L_c vs H_d ; L_m vs H_c ; L_m vs L_c 
      
      CAL_wt_pframe <- data.frame(CAL_wt[["p.value"]]) # taking p-value matrix and coercing to dataframe
      write.csv(CAL_wt_pframe, here::here("outputs","Ccalyculata_pframe.csv")) # saving dataframe as csv
      write.csv(CAL_wt_pframe>=0.05, here::here("outputs","Ccalyculata_pframe_binary.csv")) # saving data frame indicating p< 0.05 for each paru
      
      # Creating matrix of difference between medians to pair with the p-value table 
      CAL_meds <- CAL_cover %>% # summarizing by rf group
        group_by(rf_index) %>% 
        summarise(
          median = median(cover, na.rm = TRUE) # calculating median for each group
        )
      
      CAL_meds_diff <- as.data.frame(outer(CAL_meds$median, CAL_meds$median, FUN= "-")) # creating pairwise difference matrix between medians
      colnames(CAL_meds_diff) <- CAL_meds$rf_index # renaming to have rf index names
      rownames(CAL_meds_diff) <- CAL_meds$rf_index
      write.csv(CAL_meds_diff, here::here("outputs","Ccalyculata_med_differences.csv")) # saving this file as .csv
      
    ##2) Repeating for K. angustifolia
      # Kruksal-Wallis test to determine if significant difference between means of rf-index groups
      KAA_cover <- plot_cover %>% filter(species=="KAA") # creating daframe with only this species
      kruskal.test(cover ~ rf_index, data = KAA_cover) # performing test - P < 0.05 - yes, indicates significatn differences between groups 
      # use pairwise wilcox test to determine which groups different 
      KAA_wt <- pairwise.wilcox.test(KAA_cover$cover, KAA_cover$rf_index,
                           p.adjust.method = "BH")
      
      # significant differences (P<0.05): H_c vs H_d ; H_c vs H_m ; H_c vs L_c ; H_d vs H_n ; H_m vs H_n ; H_m vs L_m ; H_n vs L_c ; H_n vs L_m 
      KAA_wt_pframe <- data.frame(KAA_wt[["p.value"]])
      write.csv(KAA_wt_pframe, here::here("outputs","Kangust_pframe.csv"))
      write.csv(KAA_wt_pframe>=0.05, here::here("outputs","Kangust_pframe_binary.csv"))
      
      # Creating matrix of difference between medians to pair with the p-value table 
      KAA_meds <- KAA_cover %>% 
        group_by(rf_index) %>% 
        summarise(
          median = median(cover, na.rm = TRUE)
        )
      
      KAA_meds_diff <- as.data.frame(outer(KAA_meds$median, KAA_meds$median, FUN= "-"))
      colnames(KAA_meds_diff) <- KAA_meds$rf_index
      rownames(KAA_meds_diff) <- KAA_meds$rf_index
      write.csv(KAA_meds_diff, here::here("outputs","Kangust_med_differences.csv"))
      
    ##3) R. groenlandicum
      # Kruksal-Wallis test to determine if significant difference between means of rf-index groups
      RHG_cover <- plot_cover %>% filter(species=="RHG") # creating daframe with only this species
      kruskal.test(cover ~ rf_index, data = RHG_cover) # performing test - P < 0.05 - yes, indicates significatn differences between groups 
      # use pairwise wilcox test to determine which groups different 
      RHG_wt <- pairwise.wilcox.test(RHG_cover$cover, RHG_cover$rf_index,
                           p.adjust.method = "BH")
      
      # significant differences (P<0.05): all types with H_c; H_d vs H_m ; H_d vs H_n ; H_d vs L_c ; L_c vs L_d ; L_c vs L_m ; L_c vs L_n 
      RHG_wt_pframe <- data.frame(RHG_wt[["p.value"]])
      write.csv(RHG_wt_pframe, here::here("outputs","Rgroenlandicum_pframe.csv"))
      write.csv(RHG_wt_pframe>=0.05, here::here("outputs","Rgroenlandicum_pframe_binary.csv"))
      
      # Creating matrix of difference between medians to pair with the p-value table 
      RHG_meds <- RHG_cover %>% 
        group_by(rf_index) %>% 
        summarise(
          median = median(cover, na.rm = TRUE)
        )
      
      RHG_meds_diff <- as.data.frame(outer(RHG_meds$median, RHG_meds$median, FUN= "-"))
      colnames(RHG_meds_diff) <- RHG_meds$rf_index
      rownames(RHG_meds_diff) <- RHG_meds$rf_index
      write.csv(RHG_meds_diff, here::here("outputs","Rgroenlandicum_med_differences.csv"))
      
    ##4) Vaccinium sp.
      # Kruksal-Wallis test to determine if significant difference between means of rf-index groups
      VAAM_cover <- plot_cover %>% filter(species=="VAAM") # creating daframe with only this species
      kruskal.test(cover ~ rf_index, data = VAAM_cover) # performing test - P < 0.05 - yes, indicates significatn differences between groups 
      # use pairwise wilcox test to determine which groups different 
      VAAM_wt <- pairwise.wilcox.test(VAAM_cover$cover, VAAM_cover$rf_index,
                           p.adjust.method = "BH")
      
      VAAM_wt_pframe <- data.frame(VAAM_wt[["p.value"]])
      write.csv(VAAM_wt_pframe, here::here("outputs","Vaccinium_pframe.csv"))
      write.csv(VAAM_wt_pframe>=0.05, here::here("outputs","Vaccinium_pframe_binary.csv"))
      # significant differences (P<0.05): H_c vs H_d; H_c vs H_m; H_c vs L_c ; H_c vs L_m; H_d vs L_c; H_d vs L_n ; H_m vs L_c; H_m vs L_n
      
      # Creating matrix of difference between medians to pair with the p-value table 
      VAAM_meds <- VAAM_cover %>% 
        group_by(rf_index) %>% 
        summarise(
          median = median(cover, na.rm = TRUE)
        )
      
      VAAM_meds_diff <- as.data.frame(outer(VAAM_meds$median, VAAM_meds$median, FUN= "-"))
      colnames(VAAM_meds_diff) <- VAAM_meds$rf_index
      rownames(VAAM_meds_diff) <- VAAM_meds$rf_index
      
      
      