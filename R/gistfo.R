#' Create a private Github gist containing the active RStudio tab and browse to it.
#' File is named: RStudio_<project>_<filename or file_id>. file_id is a unique id for
#' untitled files. It does not relate to the untitled number.
#' @return nothing.
#' @export
gistfo <- function(){
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

        gistr::gist_create(filename = gist_name,
                           public = FALSE,
                           browse = TRUE,
                           code = source_context$contents
                           )
        invisible(NULL)
}
