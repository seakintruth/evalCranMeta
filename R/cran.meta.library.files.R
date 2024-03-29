logMessage <- function(strMessage, strLogFileName, fCreateNewFile = FALSE){
  #log our actions
  if(fCreateNewFile){
    file.remove(strLogFileName);
  }
  fileConn<-file(strLogFileName);
  if(file.exists(strLogFileName) && (!fCreateNewFile)){
    strMessage<-c(readLines(fileConn),strMessage);
  }
  writeLines(
    strMessage,
    fileConn
  );
  close(fileConn);
};

#' @export
cran.meta.gather.file.data <- function(reports.directory=Sys.getenv("R_LIBS_USER")){
  #strBasePath <- (.choose_directory(caption="Select installed folder directory"))
  # [TODO] make this work for windows and linux
  if(stringr::str_length(reports.directory)==0){
    reports.directory <- .choose_directory()
  } else  {
    if (!dir.exists(reports.directory)){
      reports.directory <- .choose_directory()
    } 
  }
  strBasePath <- reports.directory
  dir.create(".Ruserdata",showWarnings = FALSE)
  log.file <- file.path(getwd(),".Ruserdata","fileSearch.log")
  
  timer.start <-Sys.time();
  search.Recursive <- TRUE
  search.Include.Dirs <- TRUE
  strMessage <- paste0("-----------------------------------\nBegin list.files on ", strBasePath, " at ", Sys.time());
  logMessage(strMessage, log.file);
  # Write a list of all files on the system to a compressed data object.
  thisMachineFileList <- list.files(
    path = strBasePath,
    all.files=TRUE,
    full.names=TRUE,
    recursive=search.Recursive,
    ignore.case=TRUE,
    include.dirs=search.Include.Dirs
  );
  strMessage <- paste0(
    "Completed list.files on ",
    strBasePath,
    " after ",
    round(
      (Sys.time()-timer.start),2
    ),
    " seconds"
  );
  logMessage(strMessage ,log.file);
  timer.start.next <- Sys.time()
  file.list <- file.path(getwd(),".Ruserdata","installed.packages.thisMachineFileList.Rdata")
  save(thisMachineFileList,file=file.list);
  strMessage <- paste0(
    "Completed save of file.list after ",
    round(
      (Sys.time()-timer.start.next),
      2
    ),
    " seconds"
  );
  logMessage(strMessage ,log.file);
  rm(thisMachineFileList);
  load(file.list);
  strMessage <- paste0(
    "Completed at ",
    Sys.time(),
    ": total run time is ",
    round(
      (Sys.time()-timer.start),
      2
    ),
    " seconds"
  );
  logMessage(strMessage, log.file)
}
