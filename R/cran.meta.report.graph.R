#' Creates a .html package report for many packages at once.
#' @param cran.packs list containing packages to build dependancies
#' If a single package is passed then a HTML report is generated and displayed in the browser using pkgnet.
#' @param reports.directory if the reports directory is left NULL and running interactivly then user is prompted for a directory, if NULL and not runnig interactivly uses current working directory 
#' @section 
#' Comments
#' It's really a terrible idea to use miniCRAN's makeDepGraph to generate a visual graph a graph of all available packages, (not usefull):
#' but it works, see:
#' https://www.bsetmet.com/wp-content/bespoke/project_assets/practicum.jeremy.gerdes/all.dependancies.graph.png
#' @return an igraph of the dependancy graph for supplied package list.
#' Graph installed package, except some package
#' For report details See: https://cran.r-project.org/web/packages/pkgnet/vignettes/pkgnet-intro.html
#' @examples
#' packs <- installed.packages()[,1]
#' packs.filtered <- dplyr::setdiff(packs,packs['evalCranMeta'] )
#' cran.meta.dependancies.graph(packs.filtered)
#' @export 
cran.meta.dependancies.graph <- function (
  cran.packs = as.list(installed.packages()[,1]),
  reports.directory = NULL
){
    if(is.null(reports.directory)){
      reports.directory <- .choose_directory()
    } else  {
      if (!dir.exists(reports.directory)){
        reports.directory <- .choose_directory()
      } 
    }
  message(reports.directory)
  #cranpacks <- available.packages()
  pkgnet.my.report <- NULL
  for (pack.item in cran.packs){
    pkgnet.my.report <- pkgnet::CreatePackageReport(
      pkg_name = pack.item,
      report_path=file.path(reports.directory,paste0(pack.item,".html"))
    )
  }
#  tryCatch(all.graph <- miniCRAN::makeDepGraph(cran.packs))
  # if (exists("all.graph")){
  #   if(generate.visual.graph == TRUE){
  #     if(is.null(visual.graph.filepath))
  #       svglite::editSVG(
  #         ggplot2::ggsave(visual.graph.filepath,plot=all.graph)
  #       )
  #     #plot(all.graph)
  #     #savePlot()
  #   }    
  #   return(all.graph)
  # }else{
  #   return(NULL)
  # }
}
.choose_directory = function(caption = 'Select data directory: ') {
  if(interactive()){
    # will prompt for a 
    repeat{
      if (exists('utils::choose.dir')) {
        result.dir <- choose.dir(caption = caption) 
      } else {
        result.dir <- readline(prompt = caption)
      }
      if(dir.exists(result.dir)){
        break;
      } else {
        dir.create(result.dir)
        if(dir.exists(result.dir)){
          break;
        } else {
          message("Failed to find or create the directory '",result.dir, "' try again")
        }
      }
    }  
  } else {
    if(is.null(result.dir)){
      result.dir <- getwd()
    }   
  }
  return(result.dir)
}
