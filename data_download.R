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

