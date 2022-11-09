save_pheatmap <- function(x, filename,type = "pdf") {
  stopifnot(!missing(x))
  stopifnot(!missing(filename))
  if(nchar(filename)>=250){
    tmp_filename <- paste0(
      getwd(),
      "/www/",
      paste(paste("ShinyOmics",Sys.time(),".png",sep = ""))
    )
  }
  if(type == "pdf"){
    pdf(filename)
    grid::grid.newpage()
    grid::grid.draw(x$gtable)
    dev.off()
  }else if(type == "png"){
    png(filename, width=800, height=400)
    grid::grid.newpage()
    grid::grid.draw(x$gtable)
    dev.off()
  }else if(type == "svg"){
    svglite::svglite(filename)
    grid::grid.newpage()
    grid::grid.draw(x$gtable)
    dev.off()
  }else if(type == "tiff"){
    tiff(filename)
    grid::grid.newpage()
    grid::grid.draw(x$gtable)
    dev.off()
  }
}