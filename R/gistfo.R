CARBON_URL <- "https://carbon.now.sh/{gist_id}"

#' Create a private Github gist and browse to it.
#'
#' The gist will contain the active text selection or the active RStudio tab.
#' File is named: `RStudio_<project>_<?selection>_<filename or file_id>`.
#' `file_id` is a unique id for untitled files. It does not relate to the
#' untitled number.
#' @return nothing.
#' @export
gistfo <- function() gistfo_base(mode = "gistfo")

#' Create a public Github gist and send to carbon.now.sh, then browse to both.
#'
#' Create a public Github gist and browse to it, then open the gist in
#' [carbon.now.sh](https://carbon.now.sh), for easily Tweeting a saucy code pic.
#' The gist will contain the active text selection or the active RStudio tab.
#' Gist file is named: `RStudio_<project>_<?selection>_<filename or file_id>`.
#' `file_id` is a unique id for untitled files. It does not relate to the
#' untitled number.
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
          name <- paste("untitled",source_context$id,sep = "_")
        }else{
          name <- tail(unlist(strsplit(x = source_context$path, split = "/")), 1)
        }
        project <- rstudioapi::getActiveProject()
        if(!is.null(project)){
          project <-  tail(unlist(strsplit(x = project, split = "/")), 1)
        }else{
          project <- ""
        }
        gist_content <- source_context$selection[[1]]$text
        if (gist_content == "") {
          gist_name <- paste("RStudio",project,name, sep="_")
          gist_content <- paste0(source_context$contents, collapse = "\n")
        } else {
          gist_name <- paste("RStudio",project,"selection",name, sep="_")
        }

        gist_file <- file.path(tempdir(), gist_name)
        cat(gist_content, file = gist_file)
        the_gist <- gistr::gist_create(files = gist_file,
                           public = public,
                           browse = browse)
        if(mode == "carbon"){
          # Add URL to gist as comment at bottom of gist
          gist_url <- url_git_io(the_gist$html_url)
          cat("\n\n#", gist_url, file = gist_file, append = TRUE)
          the_gist <- gistr::update_files(the_gist, gist_file)
          gistr::update(the_gist)

          # Send to carbon
          gist_id <- the_gist$id
          utils::browseURL(glue::glue(CARBON_URL))
          clipr::write_clip(the_gist$url)
        }
        invisible(NULL)
}

# Create Shortlink for URL using git.io
url_git_io <- function(url) {
        if (!requireNamespace("curl", quietly = TRUE)) return(url)
        h <- curl::new_handle()
        curl::handle_setform(h, url = url)
        r <- curl::curl_fetch_memory("https://git.io", h)
        if (!r$status_code %in% 200:203) return(url)
        short_url <- curl::parse_headers_list(r$headers)$location
        if (!is.null(short_url) && grepl("git\\.io", short_url)) short_url else url
}
