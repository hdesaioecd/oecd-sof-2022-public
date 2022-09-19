
# NOTE TO CODE REVIEWERS -------------------------------------------------

#sourcing this file will run the framework all the way through
#files 1-4 pertain specifically to the framework
#files 5 and 6 focus on the analysis and exports for the SoF platform
#checking 5 and 6 is not required, but we might have a couple of questions about the outputs in #5 
#based on the checks made in the framework (particularly #1)

#if there are any code breaks, please check package versions and folders
#check the latest uploaded packages and versions used by Harsh in the following:
source("./lib/load-libraries.R")
package_info <- fread("./lib/package_info_02052022.csv")
source("./src/01-SFR Calculation.R")
source("./src/02-SFR Biplots.R")
source("./src/03-SFR Snail.R")
source("./src/04-SFR Cluster Analysis.R")

source("./src/05-Analysis.R")
source("./src/06-Platform.R")