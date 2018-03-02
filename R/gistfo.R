CARBON_URL <- "https://carbon.now.sh/?bg=rgba(0,0,0,0)&t=solarized%20dark&l=r&ds=true&wc=true&wa=true&pv=48px&ph=32px&ln=true"

#' Create a private Github gist containing the active RStudio tab and browse to it.
#' File is named: RStudio_<project>_<filename or file_id>. file_id is a unique id for
#' untitled files. It does not relate to the untitled number.
#' @return nothing.
#' @export
gistfo <- function() gistfo_base(mode = "gistfo")

#' Create a public Github gist containing the active RStudio tab and browse to it,
#' then open the gist in carbon.now.sh, for easily Tweeting a saucy code pic.
#' Gist file is named: RStudio_<project>_<filename or file_id>. file_id is a unique id for
#' untitled files. It does not relate to the untitled number.
#' @return nothing.
#' @export
gistfoc <- function() gistfo_base(mode = "carbon")

gistfo_base <- function(mode){
        if(mode == "gistfo"){
          browse <- TRUE
          public <- FALSE
        } else if(mode == "carbon"){
          browse <- TRUE
          public <- TRUE
        } else{
          stop("mode must be 'gistfo' or 'carbon'")
        }
        source_context <- rstudioapi::getSourceEditorContext()
        if(source_context$path == ''){
          name <- paste("unititled",source_context$id,sep = "_")
        }else{
          name <- tail(unlist(strsplit(x = source_context$path, split = "/")), 1)
        }
        project <- rstudioapi::getActiveProject()
        if(!is.null(project)){
          project <-  tail(unlist(strsplit(x = project, split = "/")), 1)
        }else{
          project <- ""
        }
        gist_name <- paste("RStudio",project,name, sep="_")

        the_gist <- gistr::gist_create(filename = gist_name,
                           public = public,
                           browse = browse,
                           code = paste0(source_context$contents, collapse = "\n")
                           )
        if(mode == "carbon"){
          browseURL(paste0(CARBON_URL, the_gist$url))
          clipr::write_clip(the_gist$url)
        }
        invisible(NULL)
}
