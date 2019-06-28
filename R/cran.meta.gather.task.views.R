#' Gathers the CRAN task views into a data.frame
#' @return 
#' A data.frame
#' @export
cran.meta.gather.task.views <- function(){
  cran.available.views <- ctv::available.views()
  # transpose cvt.list type data set to data.frames
  ctv.topics <- NULL
  ctv.topic.packages <- NULL
  for (ctv.item in cran.available.views){
    item.common <- unlist(ctv.item)
    item.common.2 <- item.common[c(1:5,length(item.common))]
    ctv.topics <- rbind(ctv.topics,item.common.2)
    item.packages <- item.common[c(1,6:(length(item.common)-1))]
    ctv.topic.packages <- cbind(ctv.topic.packages,item.packages)
  }
  # cleanup temp objects
  rm(list=c("item.common","item.common.2"))
  return(dplyr::as_data_frame(ctv.topics))
}
