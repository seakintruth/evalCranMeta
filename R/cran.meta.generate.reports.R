#' Generates various reports consering the CRAN meta data
#' @param include.code.review boolean, include code review will install all packages to this machine this can take a long time of first run
#' @return a data.frame containing CRAN metadata, and code review information
#' @export
cran.meta.generate.reports <- function(include.code.review = FALSE){
  if(include.code.review){
    cran.meta.install.all.packages()
  }  
  #once loaded we don't manipulate the cranData object directly, don't reload...
  if(!exists("cranData")) cranData<-tools::CRAN_package_db()
  
  #library(dplyr)
  
  # Fix broken column names (duplicates, and invalid strings)
  names(cranData) <- make.names(names(cranData), unique=TRUE)
  
  package.license.restricts.use <- cranData %>%
    filter(License_restricts_use == "yes") # %>%
  select(Package)
  
  # Here is a list of packages that the enterprise may not
  # want to use as a general rule.
  if(exists("xlsx::write.xlsx")){
    write.xlsx(
      package.license.restricts.use,
      file="package.license.restricts.use.xlsx",
      sheetName="license_restricts_use",
      row.names=FALSE
    )  
  }else{
    write.csv(package.license.restricts.use,
      sheetName="license_restricts_use",
      row.names=FALSE
    )
  }
  #https://rviews.rstudio.com/2018/03/08/cran-package-metadata
  pdb <- cranData
  meta_data <- pdb[,c(1,4,5,17,60,61)]
  
  libraryNames <- pdb$Package
  #library(dplyr)
  #write.csv(pdb$Package,"packageNames.csv")
  
  names(meta_data) <- c("Package", "Dep", "Imp", "Aut", "RD", "RI")
  
  fcn<-function(x,y){
    x<-unlist(x) %>% strsplit(",")
    y<-unlist(y) %>% strsplit(",")
    z<-unlist(na.omit(union(x,y)))
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
  meta_data$Aut<-meta_data$Aut %>% purrr::map(clean) %>% purrr::map(clean2)
  rm_na<-function(x){
    list(na.omit(unlist(x)))
  }
  
  c_dat1<-seq_len(nrow(meta_data)) %>%
    purrr::map_df(~{
      meta_data[.x, ] %>%
        select(-Package, -DepImp, -RDRI) %>%
        purrr::map_df(~ifelse(is.na(.x), 0, length(str_split(.x, ",")[[1]]))) %>%
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
  
  #View(c_dat)
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

