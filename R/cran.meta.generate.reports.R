#' Generates various reports consering the CRAN meta data
#' @param include.code.review boolean, include code review will install all packages to this machine this can take a long time of first run
#' @return a data.frame containing CRAN metadata, and code review information
#' @importFrom magrittr "%>%"
#' @export
cran.meta.generate.reports <- function(reports.directory=NULL, include.code.review = FALSE){
  if(include.code.review){
    cran.meta.install.all.packages()
  }  
  if(is.null(reports.directory)){
    reports.directory <- .choose_directory()
  } else  {
    if (!dir.exists(reports.directory)){
      reports.directory <- .choose_directory()
    } 
  }
  #once loaded we don't manipulate the cranData object directly, don't reload...
  #if(!exists("cranData")) cranData<-tools::CRAN_package_db()
  cranData<-tools::CRAN_package_db()
  
  #library(dplyr)
  
  # Fix broken column names (duplicates, and invalid strings)
  names(cranData) <- make.names(names(cranData), unique=TRUE)
  
  package.license.restricts.use <- dplyr::filter(cranData,cranData$License_restricts_use == "yes") 
  # ------------------------------------------------------------------------------
  # [TODO] start recursive function untill all reverse depends, and revers imports
  #  are found, from starting list
  # ------------------------------------------------------------------------------
  RDRI.cols <- c("Package","Reverse.depends","Reverse.imports")
  pack.restricts.use.RDRI <- dplyr::select(package.license.restricts.use,RDRI.cols)
  pack.restricts.use.RDRI <- dplyr::union(pack.restricts.use.RDRI[,'Reverse.depends'],pack.restricts.use.RDRI[,'Reverse.imports'])
  # remove NA
  pack.restricts.use.RDRI <- pack.restricts.use.RDRI[!is.na(pack.restricts.use.RDRI)]
  pack.restricts.use.RDRI <- unlist(strsplit(pack.restricts.use.RDRI, ","))
  # Remove white space
  whitespace <- " \t\n\r\v\f"
  pack.restricts.use.RDRI <- stringr::str_replace_all(pack.restricts.use.RDRI, whitespace, "")
  pack.restricts.use.RDRI <- as.data.frame(pack.restricts.use.RDRI)
  names(pack.restricts.use.RDRI) <- "Package"
  descriptive.cols = c("Package","License","Title",RDRI.cols)
  
  pack.restricts.use.RDRI <- dplyr::inner_join(pack.restricts.use.RDRI,dplyr::select(cranData,descriptive.cols),by="Package")
  pack.restricts.use.RDRI <- dplyr::union(pack.restricts.use.RDRI,dplyr::select(package.license.restricts.use,descriptive.cols))
  # ------------------------------------------------------------------------------
  # End function, currently we get a final answer without going deeper than one iteration.
  # ------------------------------------------------------------------------------
  # Here is a list of packages that the enterprise may not
  # want to use as a general rule.
  # Here is a list of packages that the enterprise may not
  # want to use as a general rule.
  # check if xlsx is installed prior to exporting report
  if(exists("xlsx::write.xlsx")){
    report.filepath <- file.path(reports.directory,"package.license.restricts.use.xlsx") 
    write.xlsx(
      pack.restricts.use.RDRI,
      file=report.filepath,
      sheetName="license_restricts_use",
      row.names=FALSE
    )  
  }else{
    report.filepath <- file.path(reports.directory,"package.license.restricts.use.csv") 
    write.csv(pack.restricts.use.RDRI,
      file=report.filepath,
      row.names=FALSE
    )
  }
  if(.Platform$OS.type == "unix") {
   # if libre office is installed open with that!
    system("which soffice>/tmp/libreoffice.txt")
    libre.office.install <- read.fwf("/tmp/libreoffice.txt",10000)
    libre.office.install<-as.character(libre.office.install[1,1])
    if(dir.exists(libre.office.install)){
      system(paste0("screen ",libre.office.install," -o ",report.filepath))
    }
  } else {
    shell(report.filepath)
  }
  
  #https://rviews.rstudio.com/2018/03/08/cran-package-metadata
  pdb <- cranData
  meta_data <- pdb[,c(1,4,5,17,60,61)]
  
  libraryNames <- pdb$Package
  #library(dplyr)
  #write.csv(pdb$Package,"packageNames.csv")
  
  names(meta_data) <- c("Package", "Dep", "Imp", "Aut", "RD", "RI")
  
  fcn<-function(x,y){
    x <- strsplit(unlist(x),",")
    y <- strsplit(unlist(y) ,",")
    z <- unlist(na.omit(union(x,y)))
  }
  
  #library(dplyr)
  meta_data<-dplyr::mutate(meta_data,
                    DepImp=mapply(fcn,Dep,Imp),
                    RDRI=mapply(fcn,RD,RI))
  clean<-function(x){
    gsub("\\[[^]]*]","",x)
  }
  clean2<-function(x){
    gsub("[\r\n]","",x)
  }
  #library(purrr) 
  meta_data$Aut <- purrr::map(purrr::map(meta_data$Aut,clean),clean2)
  rm_na<-function(x){
    list(na.omit(unlist(x)))
  }
  #library(dplyr) #%>% pipes aren't loading like they should, may need to re-write without them.
  c_dat1<-seq_len(nrow(meta_data)) %>%
    purrr::map_df(~{
      meta_data[.x, ] %>%
        select(-Package, -DepImp, -RDRI) %>%
        purrr::map_df(~ifelse(is.na(.x), 0, length(stringr::str_split(.x, ",")[[1]]))) %>%
        dplyr::mutate(Package=meta_data$Package[.x])
    }) %>%
    dplyr::select(Package, Aut, Dep, Imp, RD, RI)
  c_dat2<-seq_len(nrow(meta_data)) %>%
    purrr::map_df(~{
      meta_data[.x, ] %>%
        dplyr::select(-Package, -Aut, -Dep, -Imp, -RD, -RI) %>%
        purrr::map_df(~ifelse(is.na(.x), 0, length(rm_na(.x)[[1]])))
    }) %>%
    dplyr::select(DepImp, RDRI)
  
  c_dat<-cbind(c_dat1,c_dat2)
  
  View(c_dat)
  #c_filt<-filter(c_dat,)
  
  #------------------------------------------------------------------
  ss<-function(x){
    avg<-round(mean(x),digits=2)
    std<-round(sd(x),digits = 2)
    med<-median(x)
    res<-list(mean=avg, sd=std, median=med)
  }
  res<-cbind(names(c_dat[-1]),purrr::map_df(c_dat[-1],ss))
  
  names(res)<-c("Features", "mean","sd", "median")
  res
  quantile(c_dat$RDRI)
  
  top_RDRI<-c_dat %>% arrange(desc(RDRI)) %>% unique()
  head(top_RDRI[,c(1,2,7,8)],15)
  quantile(c_dat$Aut,probs=seq(0,1,.1))
  quantile(c_dat$DepImp,probs=seq(0,1,.1))
  #quantile(c_dat$RDRI,probs=seq(.73,.76,.01))
  cran.has.rdri <- c_dat %>%
    dplyr::filter(RDRI>=1) %>% dplyr::select(Package)
  
  
  #colaboration, Author count
  plot(c_dat$Aut)
  quantile(c_dat$Aut,probs=seq(0,1,.1))
  # long tail, do a log transpose...
  # [TODO] save plots
  plot(x=c_dat$Aut,y=(c_dat$RDRI))
  quantile(log(c_dat$Aut),probs=seq(0,1,.1))
  
}  

