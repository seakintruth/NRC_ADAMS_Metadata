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

if (!exists("nrc.document.results")){
  nrc.document.results.filename <- "nrc.document.data.frame.Rdata"
  # Only import if the results file doesn't exist
  if (file.exists(nrc.document.results.filename)){
    load(nrc.document.results.filename) 
  } else if(file.exists("FinalDownload.Rdata")) {
    load("FinalDownload.Rdata") 
  } else {
    source(file.path(dir.name(getwd()),"01-PracticumNrcGetAllDocuments.R"))
    load(nrc.document.results.filename)  
  }
}

# Functions
getNrcDocumentDateAsPosix <- function(date.character,time.zone = "UTC"){
  "10/03/2006 08:57 AM EDT"
  as_datetime(
    timeDate::strptimeDate(
      as.character(date.character),
      format="%m/%d/%Y",
      tz=time.zone
    )
  )
}

getNrcDateAsPosix <- function(date.character,time.zone = "UTC"){
lubridate::as_datetime(
    timeDate::strptimeDate(
      as.character(date.character),
      format="%m/%d/%Y",
      tz=time.zone
    )
  )
}

getNrcLongDateAsPosix <- function(date.character,time.zone = "UTC"){  # could use Sys.timezone() instead of "UTC"
  # Highest granularity used for this script is daily, so we can ignore the time zone...'"10/10/1999 01:44 PM EDT"'
  lubridate::as_datetime(
    timeDate::strptimeDate(
      as.character(date.character),
      format="%m/%d/%Y %H:%M",
      tz=time.zone
    )
  )
}

nrc.document.results.test <- stringr::str_to_lower(stringr::str_squish(nrc.document.results$DocumentType))
nrc.document.results$DocumentType <- stringr::str_to_lower(stringr::str_squish(nrc.document.results$DocumentType))

# Fix all invalid and missing document dates, this makes the assumption that documents with missing data 
test.nrc.docs<-subset(nrc.document.results, nchar(as.character(nrc.document.results$DocumentDate)) <= 15)

# {TODO} incorperate the cleaned and updated bad.document.dates into the dataset, for now we ignore them
# bad.document.dates <-dplyr::anti_join(nrc.document.results, test.nrc.docs, by=c("AccessionNumber", "PublishDatePARS"))
# bad.document.dates$DocumentDate <- getNrcDateAsPosix(bad.document.dates$PublishDatePARS)  
# bad.document.dates <- bad.document.dates[]
# Warning this needs additional data cleaning!!!
# nrc.document.results <- rbind(test.nrc.docs,bad.document.dates)
#rm("bad.document.dates")

nrc.document.results <- test.nrc.docs
rm("test.nrc.docs")

# Document Types are concatinated with commas, but many document types also include commas,
# so we need to replace expected commas with another string for a list of types that we need 
# to take aciton (without accidentally identifying commas that should be seperators) on See:
#   appendix F of the API guide: https://www.nrc.gov/site-help/developers/wba-api-developer-guide.pdf
options(stringsAsFactors = FALSE)
doc.type.find.replace <-readr::read_csv("doc.type.find.replace.csv")
doc.type.find.replace <- as.data.frame(
  cbind(
    doc.type.find.replace,
    stringr::str_replace_all(doc.type.find.replace,",",";")
  )
)  
colnames(doc.type.find.replace)<-c("Find","Replace")

# perform our find replace (wasn't able to pass in the vectors)
# Currently takes 2-3 seconds per loop
for(intFindReplace in seq_along(doc.type.find.replace$Find)){
  print(paste0(intFindReplace,":",Sys.time(),":Find '", doc.type.find.replace$Find[intFindReplace], "'"))
  nrc.document.results$DocumentType <-stringr::str_replace_all(
    nrc.document.results$DocumentType,
    doc.type.find.replace$Find[intFindReplace],
    doc.type.find.replace$Replace[intFindReplace]
  )
}

nrc.document.results$DocumentType <- stringr::str_trim(
  stringr::str_squish(
    tidyr::replace_na(
      nrc.document.results$DocumentType,"- None Specified"
    )
  )
)

names(nrc.document.results)
# exploring nrc.document.results
nrc.document <- nrc.document.results[
  c(
    "AccessionNumber","DocumentType",
    "EstimatedPageCount","DocumentReportNumber",
    "Keyword","LicenseNumber","PublishDatePARS",
    "DocumentDate","ContentSize"
  )
]

# forcast document type occurances per time period (weekly, monthly, quarterly, annually)
nrc.document <- 
  cbind(nrc.document,
        DocumentTypeCount = (stringr::str_count(nrc.document$DocumentType, ",")+1))

max(nrc.document$DocumentTypeCount)


# found upto 8 types applied to any individual document
nrc.document <- tidyr::separate(
  nrc.document,
  DocumentType,
  c(
    "DocumentType1","DocumentType2","DocumentType3","DocumentType4",
    "DocumentType5","DocumentType6","DocumentType7","DocumentType8","DocumentType9"
  ),sep=","
)

#rm("nrc.document.type.ls")
nrc.document.type.ls <- list(
  unique(na.omit(as.data.frame(nrc.document$DocumentType1))),
  unique(na.omit(as.data.frame(nrc.document$DocumentType2))),
  unique(na.omit(as.data.frame(nrc.document$DocumentType3))),
  unique(na.omit(as.data.frame(nrc.document$DocumentType4))),
  unique(na.omit(as.data.frame(nrc.document$DocumentType5))),
  unique(na.omit(as.data.frame(nrc.document$DocumentType6))),
  unique(na.omit(as.data.frame(nrc.document$DocumentType7))),
  unique(na.omit(as.data.frame(nrc.document$DocumentType8))),
  unique(na.omit(as.data.frame(nrc.document$DocumentType9)))
)

rm("nrc.document")
# split data into document type categories
for (intDoc in 1:length(nrc.document.type.ls)){
  colnames(nrc.document.type.ls[[intDoc]]) <- "DocumentType"
  if (intDoc == 1){
    nrc.document.type <- nrc.document.type.ls[[intDoc]]
  } else {
    nrc.document.type <- dplyr::bind_rows(nrc.document.type,nrc.document.type.ls[[intDoc]])
  }
}

rm("nrc.document.type.ls")
nrc.document.type$DocumentType <- stringr::str_trim(
  stringr::str_squish(
    tidyr::replace_na(
      stringr::str_to_lower(
        nrc.document.type$DocumentType,"Not Specified"
      )
    )
  )
)

nrc.document.type <-as.data.frame(nrc.document.type )
nrc.document.type <-as.data.frame(base::sort(unique(nrc.document.type$DocumentType)))
colnames(nrc.document.type) <- "DocumentType"
readr::write_csv(nrc.document.type,"nrc.document.types.csv")

# Categories of document types are found in Appendix F of the API guide listed above
# 10 CRF § 2.206
# https://www.nrc.gov/reading-rm/doc-collections/cfr/part002/part002-0206.html
for (intDocType in seq_along(nrc.document.type$DocumentType)){ #} 1:length(nrc.document.type[,1])){
  print(nrc.document.type$DocumentType[intDocType]) 
}

# Format date columns
nrc.document.results$DocumentDate <- getNrcDateAsPosix(nrc.document.results$DocumentDate)
nrc.document.results$PublishDatePARS <-getNrcLongDateAsPosix(nrc.document.results$PublishDatePARS)
#rm("total.count.per.day")
total.count.per.day <- table(factor(format(nrc.document.results$PublishDatePARS,"%D")))
#{TODO} Un-pipe everything forward if this is made into a package.
library(tidyverse)
days.with.errors <-
  total.count.per.day %>% as.data.frame %>% dplyr::filter(Freq == 1000)
#now for document date instead of published date:

rm("total.count.per.day")
total.count.per.day <- table(factor(nrc.document.results$DocumentDate))
#plot(total.count.per.day)
total.count.per.day <- as.data.frame(total.count.per.day)
total.count.per.day.rng <- total.count.per.day[
  lubridate::as_date(lubridate::as_date(total.count.per.day$Var1),format="%Y/%m/%d",tz = "UTC")>
    lubridate::as_date("11/01/1999",format="%m/%d/%Y",tz = "UTC"),
]

nrc.document.1999.on <- nrc.document.results[
  lubridate::as_date(lubridate::as_date(nrc.document.results$DocumentDate),format="%Y/%m/%d",tz = "UTC")>
    lubridate::as_date("11/01/1999",format="%m/%d/%Y",tz = "UTC"),
  ]

#ggplot(total.count.per.day.rng,aes(x=total.count.per.day.rng$Var1,y=total.count.per.day.rng$Freq))
#total.count.per.day.ts <- ts(total.count.per.day)

# 10 CFR parts here: https://www.nrc.gov/reading-rm/doc-collections/cfr/
#Significant Enforcement Actions:  https://www.nrc.gov/reading-rm/doc-collections/datasets/enforcement-actions.xls
issue.doc.type <- read_csv("issue.document.category.csv")

# This takes roughly 6 seconds per issue using this for loop method, there are definatly efficiencies to be made!
for (intIssue in seq_along(issue.doc.type$IssueDocType)) {
  print(paste0(intIssue,":",Sys.time(),": finding issue ",issue.doc.type[intIssue,] ))
  if (intIssue == 1) {
    nrc.document.issues <-
      nrc.document.results[grepl(as.character(issue.doc.type[intIssue,]),nrc.document.1999.on$DocumentType),]
  } else {
    nrc.document.issues <-rbind(
      nrc.document.issues,
      nrc.document.results[grepl(as.character(issue.doc.type[intIssue,]),nrc.document.1999.on$DocumentType),]
    )
  }
}

save(nrc.document.issues,file="nrc.document.issues.Rdata")

