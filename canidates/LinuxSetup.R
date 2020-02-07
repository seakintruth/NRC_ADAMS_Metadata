# Useing this walkthrough
# https://cran.r-project.org/web/packages/miniCRAN/vignettes/miniCRAN-introduction.html
pacman::p_load(miniCRAN)

# use Revolution Analytics CRAN mirror
revolution <- c(CRAN = "http://cran.microsoft.com")

# Specify list of packages to download
download.file("https://raw.githubusercontent.com/seakintruth/NRC_ADAMS_Metadata/master/canidates/package_list.txt","tmp.list.txt")
new.package.list <- read.csv("tmp.list.txt")
file.remove("tmp.list.txt")
#chooseCRANmirror()
corrected.list <- c("Package_Name",as.vector(new.package.list$Package_Name))
list.with.dependancies <- pkgDep(corrected.list)
# Create temporary folder for miniCRAN
tmp.path <- tempdir()

# Make repo for source and win.binary for each version of R we need to support
makeRepoVersions <- function(a.version){
  dir.create(pth <- file.path(tmp.path,a.version , "miniCRAN"),recursive = TRUE)
  makeRepo(list.with.dependancies , path = pth, repos = revolution, Rversion = a.version, type = c("source", "win.binary"))
} 
lapply(c("3.4","3.5","3.6"),FUN=makeRepoVersions)

# List all files in miniCRAN
package.file.list<- list.files(tmp.path, recursive = TRUE, full.names = FALSE)

# Check for available packages
pkgAvail(repos = pth, type = "win.binary")[, c(1:3, 5)]



# Install packages with:
#install.packages(pkgs, 
#                 repos = paste0("file:///", pth),
#                 type = "source")

# next is "Adding an older version of a package from CRAN"
