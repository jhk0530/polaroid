library(argonDash)
library(argonR)
library(colourpicker)
library(ggplot2)
library(grid)
library(hexSticker)
library(imager)
library(png)
library(shiny)
library(shinyWidgets)
library(showtext)

# action button with type : default, primary, info, success, danger, warning, secondary
myButton <- function(inputId, label, icon = NULL, width = NULL, type = "default", ...) {
  value <- restoreInput(id = inputId, default = NULL)
  tags$button(
    id = inputId, style = if (!is.null(width)) {
      paste0("width: ", validateCssUnit(width), ";")
    },
    type = "button", class = paste0("btn btn-", type, " action-button"),
    `data-val` = value, list(icon, label),
    ...
  )
}

myDnButton <- function(outputId, label = "Download", type = "default", ...) {
  aTag <- tags$a(
    id = outputId, href = "", target = "_blank",
    class = paste0("btn btn-", type, " shiny-download-link"),
    download = NA, icon("download"), label, ...
  )
}

ui <- argonDash::argonDashPage(
  title = "polaroid",
  description = "shiny application to make hex sticker",
  author = "jhk0530",
  sidebar = argonDashSidebar(
    vertical = TRUE,
    skin = "light",
    background = "white",
    size = "md",
    side = "left",
    id = "my_sidebar",
    brand_url = "https://github.com/jhk0530/polaroid",
    brand_logo = "polaroid_sticker.png",
    argonSidebarDivider(),
    myButton(
      inputId = "polaroid",
      label = "Example",
      icon = argonIcon("favourite-28"),
      type = "success"
      ),
    argonSidebarDivider(),
    fileInput("file", "Background Image (PNG)", accept = ".png"),
    # clear button
    myButton(
      inputId = "clear",
      label = "Clear",
      icon = argonIcon("button-pause"),
      type = "warning"
    ),
    argonSidebarDivider(),
    myDnButton("imgdn", type = "danger"),
    argonSidebarDivider(),
    textInputIcon(
      inputId = "font",
      label = HTML('Font. See: <a href = "https://fonts.google.com/"  target = "_blank">Link</a>'),
      value = NULL,
      placeholder = "Enter font family",
      icon = argonIcon("caps-small")
    ),
    myButton(inputId = "fontApply", label = "Apply", icon = argonIcon("button-play"), type = "info")
  ),
  header = argonDashHeader(
    gradient = TRUE,
    color = "primary",
    separator = TRUE,
    separator_color = "default",
    top_padding = 5
  ),
  body = argonDashBody(
    # use css file
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),


    fluidRow(
      column(
        argonCard(
          icon = argonIcon("check-bold"),
          plotOutput("img"),
          width = 12
        ),
        style = "text-align:center",
        width = 12
      ),
      column(
        argonCard( # img
          "Hexagon",
          fluidRow(
            column(colourInput(inputId = "background_color", label = "Background", value = "#172b4d"), width = 12) # background_color
          ),
          fluidRow(
            column(colourInput(inputId = "border_color", label = "Border", value = "#6772E5"), width = 12) # border_color
          ),
          fluidRow(
            column(sliderInput(inputId = "border_width", label = "Width", value = 1.5, step = 0.05, min = 0.1, max = 3), width = 12) # border_width
          ),
          width = 12
        ),
        width = 3
      ),
      column(
        argonCard( # img
          "Image",
          fluidRow(
            column(
              sliderInput(
                inputId = "img_x",
                label = "X",
                value = 0.5,
                step = 0.05,
                min = -2,
                max = 2
              ),
              width = 6 # img_x
            ),
            column(
              sliderInput(
                inputId = "img_y",
                label = "Y",
                value = 0.4,
                step = 0.05,
                min = -2,
                max = 2
              ),
              width = 6 # img_y
            )
          ),
          fluidRow(
            column(
              sliderInput(
                inputId = "img_width",
                label = "Width",
                value = 1,
                step = 0.1, min = 0, max = 10),
              width = 6 # img_width
            ),
            column(
              sliderInput(
                inputId = "img_height",
                label = "Height",
                value = 1,
                step = 0.1,
                min = 0,
                max = 10
              ),
              width = 6 # img_height
            )
          ),
          width = 12
        ),
        width = 3
      ),
      column(
        argonCard( # url
          "URL",
          fluidRow(
            column(
              textInput(
                inputId = "url",
                label = "Name",
                value = "jhk0530/polaroid",
                placeholder = "Enter your URL"
              ), # url
              width = 6
            ),
            column(
              colourInput(
                inputId = "url_color",
                label = "Color",
                value = "#FFFFFF"
              ), # url color
              width = 6
            )
          ),
          fluidRow(
            column(
              sliderInput(
                inputId = "url_x",
                label = "X",
                value = 1,
                step = 0.01,
                min = 0,
                max = 2
              ),
              width = 6 # urlx
            ),
            column(
              sliderInput(
                inputId = "url_y",
                label = "Y",
                value = 0.08,
                step = 0.01,
                min = 0,
                max = 2
              ),
              width = 6 # urly
            )
          ),
          fluidRow(
            column(
              sliderInput(inputId = "url_size", label = "Size", value = 6, min = 0, max = 20, step = 0.5), # url size
              width = 6
            ),
            column(
              radioGroupButtons(
                inputId = "url_angle",
                label = "Label",
                choices = c("left" = 330, "right" = 30),
                size = "sm",
                status = 'primary',
                width = '100%'
              ),
              width = 6
            )
          ),
          width = 12,
        ),
        width = 3
      ),
      column(
        argonCard(
          "Package",
          fluidRow(
            column(
              textInput(inputId = "pkg_name", label = "Name", value = "polaroid", placeholder = "polaroid"),
              width = 6 # pkg
            ),
            column(
              colourInput(inputId = "pkg_color", label = "Color", value = "#FFFFFF"), # pkg color
              width = 6
            )
          ),
          fluidRow(
            column(
              sliderInput(inputId = "pkg_x", label = "X", value = 1, step = 0.1, min = 0, max = 3),
              width = 6 # pkgx
            ),
            column(
              sliderInput(inputId = "pkg_y", label = "Y", value = 1.6, step = 0.1, min = 0, max = 3),
              width = 6 # pkgy
            )
          ),
          sliderInput(inputId = "pkg_size", label = "Size", value = 12, min = 1, max = 20, step = 0.5), # pkg size
          width = 12,
        ),
        width = 3
      )
    )
  ),
  footer = argonDashFooter(
    copyrights = "@jhk0530, Since 2020",
    src = "https://github.com/jhk0530",
    argonFooterMenu(
      argonFooterItem(argonIcon("app"), "hexSticker", src = "https://github.com/GuangchuangYu/hexSticker"),
      argonFooterItem(argonIcon("palette"), "Argon", src = "https://rinterface.github.io/argonDash/")
    )
  )
)

mysticker <- function(im, img_x, img_y, img_width, img_height,
                      pkg_name, pkg_x, pkg_y, pkg_color, pkg_size,
                      background_color, border_color, border_width,
                      url, urlx, urly, url_color, url_size, url_angle, font_family) {

  if(font_family==''){font_family <- "Aller_Rg"}
  if(is.null(font_family)){font_family <- "Aller_Rg"}

  g <- ggplot() +
    geom_hexagon(border_width, background_color, border_color) +
    geom_pkgname(
      package = pkg_name,
      x = pkg_x,
      y = pkg_y,
      color = pkg_color,
      family = font_family,
      size = pkg_size
    ) +
    geom_url(
      url = url,
      x = urlx,
      y = urly,
      family = font_family,
      size = url_size,
      color = url_color,
      angle = url_angle
    ) +
    annotation_custom(
      rasterGrob(im, interpolate = FALSE),
      img_x, img_x + img_width,
      img_y, img_y + img_height
    ) +
    theme_sticker(size = h_size)

  ggsave(plot = g,
     filename = "temp.png",
     width = 43.9,
     height = 50.8,
     dpi = 300,
     bg = "transparent",
     units = "mm",
     device = 'png'
   )
  return(g)
}

server <- function(input, output, session) {
  # ====== set img margin narrow

  # Image as Global variable
  # im <- NULL

  # clear logic
  observeEvent(input$clear, {

    if (exists("im")){
      im <<- NULL
    }
    output$img <- renderPlot({
      NULL
    })

  }, ignoreInit = TRUE)

  # font change
  observeEvent(input$fontApply, {
    if (is.null(input$file) && !exists("im")) {
      showNotification("Please upload image", type = "warning")
      return()
    }

    if(is.null(im)){
      return(NULL)
    }

    withProgress(
      message = "Loading Font...",
      {
        tryCatch({
          font_add_google(name = input$font, family = input$font)
          showtext_auto()
        }, error = function(e) {
          showNotification("Font not supports will use 'sans' instead", type = "warning")
          return()
        })

        incProgress(1)
      }
    )

    s <- mysticker(im, input$img_x, input$img_y, input$img_width, input$img_height,
      input$pkg_name, input$pkg_x, input$pkg_y, input$pkg_color, input$pkg_size,
      input$background_color, input$border_color, input$border_width,
      input$url, input$url_x,
      urly = input$url_y, input$url_color, input$url_size, input$url_angle,
      font_family = input$font
    )

    output$img <- renderPlot({
      par(mar = c(0.8, 0.8, 0.8, 0.8))
      plot(imager::load.image("temp.png"), axes = FALSE)
    })

    output$imgdn <- downloadHandler(
      filename = "my_polaroid_sticker.png",
      content = function(file) {
        save_sticker(filename = file, s)
      }
    )
  })

  # Example button
  observeEvent(input$polaroid, {
    im <<- readPNG("images/nut.png")

    s <- mysticker(im, input$img_x, input$img_y, input$img_width, input$img_height,
      input$pkg_name, input$pkg_x, input$pkg_y, input$pkg_color, input$pkg_size,
      input$background_color, input$border_color, input$border_width,
      input$url, input$url_x, input$url_y, input$url_color, input$url_size, input$url_angle,
      font_family = input$font
    )

    output$img <- renderPlot({
      par(mar = c(0.8, 0.8, 0.8, 0.8))
      plot(imager::load.image("temp.png"), axes = FALSE)
    })

    output$imgdn <- downloadHandler(
      filename = "my_polaroid_sticker.png",
      content = function(file) {
        save_sticker(filename = file, s)
      }
    )
  })

  # event listener to input variables
  observeEvent(
    {
      input$play
      input$file
      input$background_color
      input$border_color
      input$border_width
      input$img_x
      input$img_y
      input$img_width
      input$img_height
      input$url
      input$url_color
      input$url_x
      input$url_y
      input$url_size
      input$url_angle
      input$pkg_name
      input$pkg_color
      input$pkg_x
      input$pkg_y
      input$pkg_size
    },
    {
      infile <- input$file
      if (is.null(infile) && !exists("im")) {
        showNotification("Please upload image", type = "warning")
        return(NULL)
      }

      # Initial Read or Sample image used.
      if (!exists("im")){
        im <<- readPNG(infile$datapath)
      }

      # Previous image exists : re-read after clear or overwrite
      if (exists("im")){
        im <<- readPNG(infile$datapath)
      }

      s <- mysticker(im, input$img_x, input$img_y, input$img_width, input$img_height,
        input$pkg_name, input$pkg_x, input$pkg_y, input$pkg_color, input$pkg_size,
        input$background_color, input$border_color, input$border_width,
        input$url, input$url_x,
        urly = input$url_y, input$url_color, input$url_size, input$url_angle,
        font_family = input$font
      )

      output$img <- renderPlot({
        par(mar = c(0.8, 0.8, 0.8, 0.8))
        plot(imager::load.image("temp.png"), axes = FALSE)
      })

      output$imgdn <- downloadHandler(
        filename = "my_polaroid_sticker.png",
        content = function(file) {
          save_sticker(filename = file, s)
        }
      )
    }
  )
}

shinyApp(
  ui,
  server,
  onStart = function() {
    onStop(function() {
      if (file.exists("temp.png")) {
        file.remove("temp.png")
        rm(im, envir = .GlobalEnv)
      }
    })
  }
)
