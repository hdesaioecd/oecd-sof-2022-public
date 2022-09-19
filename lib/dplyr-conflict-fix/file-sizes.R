
# Decreasing size of data files

# 1.       Write a script that extracts the variables you need
# 2.       Write these to csv i.e. write.csv(data, file = ‘vdem.csv’, row.names = F)
# 3.       Save this file into the data folder
# 4.       Remove the complete VDEM file from the master folder.
# 5.       Redo steps 1-4 for any data file that exceed 100MB (I have attached an R script that will look at all 
#               file sizes and filters for those > 100MB, open the oecd project in RStudio, 
#               open this script and run to get the list of files you need to do this with)
# 6.       Once done, reshare on Dropbox
# 7.       Hopefully I can then get access to it on dropbox and fix up the github repository.

# library(dplyr)
# files = file.info(list.files(full.names = T, recursive = T), extra_cols = T)
# files = add_rownames(files, "file")
# files = files %>% dplyr::filter(size/100000 >= 100)
