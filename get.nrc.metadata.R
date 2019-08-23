download.nrc.document.meta <- function(days,data.directory,fGetHourly=FALSE){
  # days <- seq(from=as.Date("12/23/2004",format="%m/%d/%Y"), to=as.Date(Sys.Date()+1),by='days' )
  for(intDay in seq_along(days)){
    # U.S. Nuclear Regulatory Commission public documents
    # See page 16 of https://www.nrc.gov/site-help/developers/wba-api-developer-guide.pdf
    # Find all reports by date range to the NRC’s public library 
    strMonth <- format(days[intDay],"%m")
    strYear <- format(days[intDay],"%y")
    nrc.var.name <- paste0("nrc.document.",format(days[intDay],"%Y_%m_%d"))
    nrc.xml.filename <- paste0(file.path(data.directory,nrc.var.name,".xml"))
    nrc.rdata.filename <-paste0(file.path(data.directory,nrc.var.name,".Rdata"))
    # Resume where we left off
    if (file.exists(nrc.rdata.filename)){
      print(paste0("Loading:",nrc.rdata.filename))
      load(nrc.rdata.filename)
      if (exists("nrc.rows")){
        if(is.null(nrc.rows)){
          rm("nrc.rows")
        }
      }
    } else {
      if (intDay == 1){
        query.dates <- paste0("(left:%2701/01/1954+12:00+AM%27,",
                              "right:%27",format(days[intDay],"%m/%d/%Y"),"+12:00+AM%27),")
      } else{
        query.dates <- paste0("(left:%27",format(days[intDay],"%m/%d/%Y"),"+12:00+AM%27,",
                              "right:%27",format(days[intDay+1],"%m/%d/%Y"),"+12:00+AM%27),")
      }
      xml2::download_xml(
        paste0(
          "https://adams.nrc.gov/wba/services/search/advanced/nrc?q=",
          "(mode:sections,",
          "sections:(filters:(public-library:!t),",
          "options:(within-folder:(enable:!f,insubfolder:!f,path:%27%27)),",
          "properties_search_all:!(",
          "!(PublishDatePARS,range,",
          query.dates,
          "%27%27))))&",
          "qn=New&tab=advanced-search-pars&s=%24title&so=ASC"
        ),
        file=nrc.xml.filename
      )
      nrc.document.raw <- xml2::read_xml(nrc.xml.filename)
      file.remove(nrc.xml.filename)
      nrc.document.list <- xml2::as_list(nrc.document.raw)
      nrc.document.result.list <- xml2::xml_find_all(nrc.document.raw,"//result")
      getNrcDateAsPosix <- function(date.character,time.zone = Sys.timezone()){
        lubridate::as_datetime(
          timeDate::strptimeDate(
            as.character(date.character),
            format="%Y%m%d%H%M%S"
            ,tz=time.zone
          )
        )
      }
      if (nrc.document.list$search$count==1000){
        if (exists("error.days")){
          error.days <- rbind(error.days,days[intDay])
        }else{
          error.days <- matrix(days[intDay])
        }
        warning(paste0(format(days[intDay],"%y_%m_%d"),"-Exceeds Max Query Return Value of 1000"),call. = TRUE,immediate. = TRUE,noBreaks. = TRUE)
        print(error.days[length(error.days)])
      }
      if(nrc.document.list$search$count == 0){
        warning(paste0(format(days[intDay],"%y_%m_%d"),"-",nrc.document.list$search$matches)
                ,call. = TRUE,immediate. = TRUE,noBreaks. = TRUE)
      } else {
        print(paste0("Attempting to import: ",nrc.document.list$search$count," rows for date: ", days[intDay] ," at:",Sys.time()))
        nrc.document <-nrc.document.list$search$resultset
        # the aow by agonizing row method (need to figure out the functional programming paradigm here)
        #print("Row by row")
        for (intRow in 1:length(nrc.document)){
          tmp.column.count <- length(nrc.document[[intRow]])
          tmp.row <- matrix(unlist(nrc.document[[intRow]][1:tmp.column.count]), ncol=tmp.column.count)
          colnames(tmp.row)<-names(nrc.document[[intRow]])
          #print("binding rows")
          if (exists("nrc.rows")){
            # nrc.rows <-nrc.rows %>% add_row(as.data.frame(nrc.columns))  
            nrc.rows <- gtools::smartbind(nrc.rows,tmp.row)
          } else {
            nrc.rows <- tmp.row
          }
        }
      }
      #print("Done building rows")
      if(exists("nrc.rows")){
        if(length(nrc.rows >0)){
          rownames(nrc.rows)<-1:length(nrc.rows[,1])
          save(nrc.rows,file=paste0(nrc.var.name,".Rdata"))
        }else{
          nrc.rows <- NULL
          save(nrc.rows,file=paste0(nrc.var.name,".Rdata"))
          rm("nrc.rows")
        }
      } else {
        nrc.rows <- NULL
        save(nrc.rows,file=paste0(nrc.var.name,".Rdata"))
        rm("nrc.rows")
      }
    }
    if(exists("nrc.rows")){
      if ((!exists("nrc.document.results"))|is.null(nrc.document.results)){
        nrc.document.results <-nrc.rows
      } else {
        nrc.document.results <- gtools::smartbind(nrc.document.results,nrc.rows)
      }
      rm("nrc.rows")
    }
  }
  if (exists("error.days")){
    save(error.days,file=paste0("errors.",nrc.document.results.filename))
  }
  save(nrc.document.results,file=nrc.document.results.filename)
}
