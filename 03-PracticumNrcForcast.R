#rm(list=ls())
if ("rstudioapi" %in% rownames(installed.packages())){
  project.wd <-file.path(dirname(rstudioapi::getActiveDocumentContext()$path),"Results")
} else { # not running from Rstudio, use a common path
  if (.Platform$OS.type == "windows") { 
    project.wd <- file.path(Sys.getenv("LOCALAPPDATA"),"nrc.documents/Results")
  } else {
    project.wd <- file.path(Sys.getenv("HOME"),"nrc.documents/Results")
  }
}
if (!dir.exists(project.wd)){
  dir.create(project.wd,recursive = TRUE)
}
setwd(project.wd)

nrc.document.issues.filename <- "nrc.document.issues.Rdata"
# Only import if the results file doesn't exist
if (file.exists(nrc.document.issues.filename)){
  load(nrc.document.issues.filename) 
} else {
  source(file.path(dir.name(getwd()),"02-PracticumNrcWrangle.R"))
  load(nrc.document.issues.filename)  
}

tmp.doc.issues <- nrc.document.issues
nrc.document.issues <- tmp.doc.issues 
#issue.count.per.day <- table(factor(as.Date(nrc.document.issues$DocumentDate)))
#plot(issue.count.per.day)
issue.count.per.month <-as.data.frame(table(factor(format(lubridate::as_date(nrc.document.issues$DocumentDate),"%Y-%m"))))
names(issue.count.per.month)
# [TODO] sould filter rows by value not magic numbers!
#issue.count.per.month.rng <- issue.count.per.month[
#  lubridate::as_date(paste0(issue.count.per.month$Var1,"/01"),format="%Y/%m/%d",tz = "UTC")>
#    lubridate::as_date("11/01/1999",format="%m/%d/%Y",tz = "UTC"),
#  ]

issue.count.per.month.rng <- issue.count.per.month[-(1:438),]
issue.count.ts <- ts(issue.count.per.month.rng$Freq,frequency = 12, start = c(1999, 12))
plot(issue.count.ts, xlab = "Year", ylab = "Count Issue/Deficiencies", main = "NRC ADAMS Issue/Deficiency Document Count 1999 - 2019")

issue.count.ts.Holt <- HoltWinters(issue.count.ts, beta = TRUE, gamma = FALSE)
issue.count.ts.Holt

plot(issue.count.ts.Holt, main = "Beer Sales 1975-1990 Holt Model")





plot(issue.count.per.month.rng)
plot(y=(issue.count.per.month$Var1),x=issue.count.per.month$Freq)
issue.count.per.year <- table(factor(format(nrc.document.issues$DocumentDate,"%Y")))
#plot(issue.count.per.year)

# Fix date columns PublishDatePARS, DateDocketed,  and DocumentDate
nrc.title21.results$DocumentDate <- getNrcDocumentDateAsPosix(nrc.title21.results$DocumentDate)
#nrc.title21.results$PublishDatePARS <- getNrcPublishDateAsPosix(nrc.title21.results$PublishDatePARS)
#plot(x=nrc.title21.results$DocumentDate,y=nrc.title21.results)
nrc.DocumentDate <- sort(nrc.title21.results$DocumentDate)
# Simple linear regression of document dates
plot(y=1:length(nrc.DocumentDate),x=nrc.DocumentDate)
doc.integer=0
for (doc.uri in nrc.title21.results$URI){
  doc.integer = doc.integer + 1 
  print(doc.uri)
  #curl::curl_download(doc.uri,destfile=paste0(doc.integer,"_",nrc.title21.results$DocumentDate[doc.integer],".pdf"))
}

head(nrc.title21.results)
