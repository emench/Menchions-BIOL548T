install.packages("rdryad")
library(rdryad)

# Create directories
dir.create("data/")
dir.create("scripts/")

dir.create("data/url")
dir.create("data/dryad")
dir.create("data/github")

## download from a url
# downloaded on 2022-09-10
data.url <- 
metadata.url <- 

data.dest.file <- here::here("")
metadata.dest.file <- here::here("")

download.file(url = data.url, destfile = data.dest.file)
download.file(url= metadata.url, destfile = metadata.des.file)

write.csv(data.dest.file, file=paste0("name","Sys.Date()",".csv"))


# from dryad

# doi: https://doi.org/10.5061/dryad.4767v
rdryad::dryad_get_cache()

# setting custom cache location
# https://github.com/emench/Menchions-BIOL548T/pull/36
rdryad_cache <- dryad_get_chache()
rdryad_cache$cache_path_set(full_path=normalizePath("here::here(data/dryad"),mustWork = F )
# mustWork = F because path doesn't exist yet
rdryad_cache $cache_path_get
dryad_download(dois="doi.org/10.5061/dryad.4767v")

# github

dir.create(here::here())


r