GithubGists <- R6::R6Class(
  "GithubGists",
  public = list(
    user = NULL,
    complete = FALSE,
    initialize = function(user = NULL) {
      requires_pkg("gh")
      self$user <- if (is.null(user)) {
        gh::gh_whoami()$login
      } else user

      first_page <- gh::gh("/users/:user/gists", user = self$user, per_page = 10)
      private$is_complete(first_page)
      private$pages <- list(first_page)
    },
    next_page = function() {
      next_page <- gh::gh_next(private$pages[[length(private$pages)]])
      private$is_complete(next_page)
      private$pages <- c(private$pages, list(next_page))
      self$view(length(private$pages))
    },
    n_pages = function() {
      length(private$pages)
    },
    view = function(page = NULL) {
      if (is.null(page)) {
        pages <- lapply(seq_along(private$pages), self$view)
        out <- list()
        for (item in pages) {
          out <- c(out, item)
        }
        return(out)
      }

      if (page < 1) return(NULL)

      if (page > length(private$pages)) {
        if (!self$complete) {
          return(self$next_page())
        } else {
          return(NULL)
        }
      }

      gists <- private$pages[[page]]

      lapply(gists, function(g) {
        c(
          g[c("id", "html_url", "public", "created_at", "updated_at", "description")],
          list(files = paste(names(g$files), collapse = ", "))
        )
      })
    }
  ),
  private = list(
    pages = list(),
    is_complete = function(res) {
      link <- attributes(res)$response$link
      has_next <- !is.null(link) && grepl('rel="next"', link, fixed = TRUE)
      self$complete <- !has_next
      !has_next
    }
  )
)


gistfo_app <- function(user = NULL) {
  requires_pkg("miniUI")
  requires_pkg("shiny")
  requires_pkg("DT")

  gists <- GithubGists$new(user = user)

  ui <- miniUI::miniPage(
    title = "GitHub Gists",
    miniUI::miniTitleBar(
      title = "GitHub Gists",
      left = shiny::div(
        shiny::uiOutput("left_buttons")
      ),
      right = shiny::div(
        miniUI::miniTitleBarButton("save_gist", "Save Gist"),
        miniUI::miniTitleBarButton("open_gist", "Open Gist", TRUE)
      )
    ),
    miniUI::miniContentPanel(
      DT::dataTableOutput("gists", height = "100%"),
      shiny::tags$style(shiny::HTML(
        ".datatables .active > td > a { color: white; }",
        ".table.dataTable td { vertical-align: middle; }"
      ))
    ),
    miniUI::miniButtonBlock(
      shiny::actionButton("page_prev", "Previous Page"),
      shiny::actionButton("page_next", "Next Page")
    ),
    shiny::tags$script(shiny::HTML(
      "Shiny.addCustomMessageHandler('disable_button', function({id, state}) {
        const el = document.getElementById(id)
        if (state) {
          el.setAttribute('disabled', true)
        } else {
          el.removeAttribute('disabled')
        }
        el.classList.toggle('disabled', state)
      })"
    ))
  )

  server <- function(input, output, session) {
    page <- shiny::reactiveVal(1L)

    shiny::observeEvent(input$page_next, page(page() + 1L))
    shiny::observeEvent(input$page_prev, page(page() - 1L))

    disable <- function(id, when) {
      session$sendCustomMessage("disable_button", list(id = id, state = when))
    }

    shiny::observe({
      disable("page_prev", when = page() == 1L)
      disable("page_next", when = gists$complete && page() == gists$n_pages())
    })

    gists_page <- shiny::reactive({
      x <- gists$view(page())
      x <- lapply(x, function(g) {
        g$public <- ifelse(g$public, "Public", "Private")
        g$html_url <- paste0('<a href="', g$html_url, '">Browse</a>')
        g$updated_at <- sub("([\\d-]{10})T([\\d:]{5}).+", "\\1 \\2", g$updated_at, perl = TRUE)
        g$created_at <- sub("([\\d-]{10})T([\\d:]{5}).+", "\\1 \\2", g$created_at, perl = TRUE)
        g
      })
      if (length(x) == 1) {
        as.data.frame(x)
      } else {
        do.call("rbind", x)
      }
    })

    rstudio_open_gist_file <- shiny::reactivePoll(
      1000, session,
      checkFunc = function() {
        rstudioapi::getSourceEditorContext()$path
      },
      valueFunc = function() {
        open_tab <- rstudioapi::getSourceEditorContext()$path
        id <- basename(dirname(open_tab))
        ids <- sapply(gists$view(), function(x) x$id)
        if (id %in% ids) basename(open_tab)
      }
    )

    output$left_buttons <- shiny::renderUI({
      if (shiny::isTruthy(rstudio_open_gist_file())) {
        filename <- rstudio_open_gist_file()
        if (nchar(filename) > 16) {
          filename <- paste0(substr(filename, 1, 16), "...")
        }
        shiny::tagList(
          miniUI::miniTitleBarCancelButton(label = "Quit"),
          miniUI::miniTitleBarButton(
            "update_gist",
            shiny::HTML(
              paste0("Update <code>", filename, "</code>")
            )
          ),
        )
      } else {
        miniUI::miniTitleBarCancelButton(label = "Quit")
      }
    })

    output$gists <- DT::renderDataTable({
      DT::datatable(
        gists_page()[, c("description", "files", "created_at", "updated_at", "public", "html_url")],
        selection = "single",
        style = "bootstrap",
        escape = FALSE,
        height = "100%",
        colnames = c("Description", "Files", "Created", "Updated", "Public", ""),
        options = list(dom = 'ft')
      )
    })

    gists_selected <- shiny::reactive({
      shiny::req(gists_page())
      shiny::req(input$gists_rows_selected)
      gists_page()[, "id"][[input$gists_rows_selected]]
    })

    shiny::observeEvent(input$cancel, shiny::stopApp())
    shiny::observeEvent(input$open_gist, gist_open_rstudio(gists_selected()))
    shiny::observeEvent(input$save_gist, gist_open_rstudio(gists_selected(), NULL, NULL))
    shiny::observeEvent(input$update_gist, update_gist())
  }

  shiny::runGadget(ui, server, viewer = shiny::paneViewer(400))
}

gist_open_rstudio <- function(id, dir = tempdir(), open = TRUE) {
  if (is.null(dir)) {
    dir <- rstudioapi::selectDirectory(label = "Choose a parent directory for the gist")
  }
  if (is.null(dir)) {
    message("Cancelled by user")
    return(NULL)
  }
  if (is.null(open)) {
    open <- rstudioapi::showQuestion("Open Gist Files", "Do you want to open the gist files?", "Yes", "No")
  }
  g <- gistr::gist_save(id, dir)
  files <- list.files(file.path(dir, id), full.names = TRUE)
  lapply(files, rstudioapi::navigateToFile)
  invisible(id)
}

update_gist <- function() {
  path <- rstudioapi::getSourceEditorContext()$path
  id <- basename(dirname(path))
  g <- gistr::gist(id)
  g$update_files <- as.list(list.files(dirname(path), full.names = TRUE))
  g <- gistr::update(g)
  message("Updated gist ", id)
  invisible(g)
}
