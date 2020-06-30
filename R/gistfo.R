CARBON_URL <- "https://carbon.now.sh/{gist_id}"

#' Share your code as a gist or on carbon.now.sh
#'
#' Creates a private or public gist containing the active text selection or the
#' active RStudio source file. Creating a public gist also prompts you to ask if
#' you want to share the code to [carbon](https://carbon.now.sh), where it is
#' converted into a beautiful screen shot.
#'
#' The default file name  of the [GitHub gist](https://gist.github.com) is:
#' `RStudio_<project>_<?selection>_<filename or file_id>`,
#' where `file_id` is a unique id for untitled files. It does not relate to the
#' untitled number. You'll be asked to confirm the file name before uploading.
#'
#' @return The URL of the gist on GitHub. Also opens browser windows to the
#'   GitHub gist and the carbon page (if the gist is public)
#' @name gistfo
NULL

#' @describeIn gistfo Create a private gist and browse to it
#' @export
gistfo <- function() gistfo_base(mode = "gistfo")

#' @describeIn gistfo Create a public gist and optionally share with
#'   [carbon](https://carbon.now.sh)
#' @export
gistfoc <- function() gistfo_base(mode = "carbon")

gistfo_base <- function(mode = c("gistfo", "carbon")) {
  mode <- match.arg(mode)

  source_context <- rstudioapi::getSourceEditorContext()

  if (source_context$path == "") {
    name <- paste0("untitled_", source_context$id, ".R")
  } else {
    name <- last(strsplit(x = source_context$path, split = "/")[[1]])
  }
  project <- rstudioapi::getActiveProject()
  if (!is.null(project)) {
    project <- last(strsplit(x = project, split = "/")[[1]])
  } else {
    project <- ""
  }
  gist_content <- source_context$selection[[1]]$text
  if (gist_content == "") {
    gist_name <- paste("RStudio", project, name, sep = "_")
    gist_content <- paste0(source_context$contents, collapse = "\n")
  } else {
    gist_name <- paste("RStudio", project, "selection", name, sep = "_")
  }

  # User prompts
  gist_name <- ask_for_filename(gist_name)
  if (is.null(gist_name)) {
    message("Upload cancelled by user")
    return()
  }
  open_in_carbon <- if (identical(mode, "carbon")) ask_if_carbon() else FALSE

  gist_file <- file.path(tempdir(), gist_name)
  cat(gist_content, file = gist_file)
  the_gist <- gistr::gist_create(
    files = gist_file,
    public = identical(mode, "carbon"),
    browse = FALSE
  )
  if (!(identical(mode, "carbon") && open_in_carbon)) {
    utils::browseURL(the_gist$html_url)
    return(the_gist$html_url)
  }

  # send to carbon ----
  # Add URL to gist as comment at bottom of gist
  if (is_file_ext(gist_name, "r", "html", "r?md", "js", "cpp", "py")) {
    gist_url <- url_git_io(the_gist$html_url)
    comment <- comment_single_line(gist_name, gist_url)
    cat(comment, file = gist_file, append = TRUE)
    the_gist <- gistr::update_files(the_gist, gist_file)
    gistr::update(the_gist)
  }

  utils::browseURL(the_gist$html_url)
  utils::browseURL(glue::glue(CARBON_URL, gist_id = the_gist$id))
  gist_url
}

# Create Shortlink for URL using git.io
url_git_io <- function(url) {
  if (!requireNamespace("curl", quietly = TRUE)) {
    return(url)
  }
  h <- curl::new_handle()
  curl::handle_setform(h, url = url)
  r <- curl::curl_fetch_memory("https://git.io", h)
  if (!r$status_code %in% 200:203) {
    return(url)
  }
  short_url <- curl::parse_headers_list(r$headers)$location
  if (!is.null(short_url) && grepl("git\\.io", short_url)) short_url else url
}

is_file_ext <- function(path, ...) {
  exts <- paste(tolower(c(...)), collapse = "|")
  grepl(glue::glue("[.]({exts})$"), tolower(path))
}

comment_single_line <- function(path, comment) {
  comment <- trimws(comment)
  if (grepl("\n", comment)) {
    stop("`comment` must be single-line")
  }
  if (is_file_ext(path, "r", "py")) {
    glue::glue("\n\n# {comment}\n", .trim = FALSE)
  } else if (is_file_ext(path, "html", "r?md")) {
    glue::glue("\n\n<!-- {comment} -->\n", .trim = FALSE)
  } else if (is_file_ext(path, "js", "cpp")) {
    glue::glue("\n\n// {comment}\n", .trim = FALSE)
  } else ""
}

last <- function(x) {
  x[[length(x)]]
}

ask_for_filename <- function(name) {
  if (!rstudioapi::hasFun("showPrompt")) {
    return(name)
  }

  rstudioapi::showPrompt(
    title = "Gist Name",
    message = "Gist Filename (including extension)",
    default = name
  )
}

ask_if_carbon <- function() {
  if (!rstudioapi::hasFun("showQuestion")) {
    return(TRUE)
  }

  rstudioapi::showQuestion(
    title = "Open on carbon.now.sh?",
    message = paste(
      "Do you want to open the gist on Carbon for a beautiful,",
      "shareable source code image?"
    ),
    ok = "Yes",
    cancel = "No"
  )
}
