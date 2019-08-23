# Import only!!!
# rm(list=ls()) 
if ("rstudioapi" %in% rownames(installed.packages())){
  project.wd <-file.path(dirname(rstudioapi::getActiveDocumentContext()$path),"nrc.documents")
} else { # not running from Rstudio, use a common path
  if (.Platform$OS.type == "windows") { 
    project.wd <- file.path(Sys.getenv("LOCALAPPDATA"),"nrc.documents")
  } else {
    project.wd <- file.path(Sys.getenv("HOME"),"nrc.documents")
  }
}
if (!dir.exists(project.wd)){
  dir.create(project.wd,recursive = TRUE)
}
setwd(project.wd)
nrc.document.results.filename <- "nrc.document.data.frame.Rdata"
# Only import if the results file doesn't exist
if (file.exists(nrc.document.results.filename)){
  load(nrc.document.results.filename)  
} else if(file.exists("../Results/FinalDownload.Rdata")) {
  load("../Results/FinalDownload.Rdata") 
} else {
  # Downloading XML and ingesting the data into a dataframe for every day in the database at a rate of 5 years per 1 hour
  # Looping through Dates https://stackoverflow.com/a/32611574
  days <- seq(from=as.Date("10/09/1999",format="%m/%d/%Y"), to=as.Date(Sys.Date()+1),by='days' )
  source(file.path(dirname(getwd()),"get.nrc.metadata.R"))
  nrc.document.results <- download.nrc.document.meta(days,project.wd,FALSE)
}
