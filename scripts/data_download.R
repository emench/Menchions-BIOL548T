############################################
############# Data Download ################
############################################
# Emma Menchions, Sept 10/22
# This script downloads the data from Thiffault et al. (2016) 

## managing packages with groundhog

#install.packages("groundhog")
library(groundhog)

date <- "2022-09-02"
groundhog.library(here, date)

# creating directory to store data
dir.create("data")

## downloading from dryad url ----

data.url <- "https://datadryad.org/stash/downloads/file_stream/41575"
metadata.url <- "https://datadryad.org/stash/downloads/file_stream/41576"

data.dest.file <- paste0("data/","Data_ThriffaultEtAl_EcoEvo_",Sys.Date(),".zip")
metadata.dest.file <- paste0("data/","Metadata_ThriffaultEtAl_EcoEvo_",Sys.Date(),".txt")

unzip(download.file(url = data.url, destfile = data.dest.file),exdir = here::here("data"))
download.file(url= metadata.url, destfile = metadata.dest.file)

unzip(list.files(path = here::here("data"), pattern = "*.zip", full.names = TRUE),exdir = here::here("data"))
