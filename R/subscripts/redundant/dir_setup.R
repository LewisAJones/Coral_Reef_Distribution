#get directories

files <- list.files("./data/enm/layers/")
files <- files[!files == "Modern"]

directories <- paste("C:\\Users/Lewis Jones/Documents/GitHub/Reef_distribution/data/enm/layers/", files, "/", ",", sep = "")

write.table(directories, "./data/enm/proj_codes.txt", row.names = FALSE)

