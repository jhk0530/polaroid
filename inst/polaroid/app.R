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

hexagon <- function(size = 1.2, fill = "#1881C2", color = "#87B13F", direction = "up") {
  if (direction == "up") {
    hexd <- data.frame(
      x = 1 + c(rep(-sqrt(3) / 2, 2), 0, rep(sqrt(3) / 2, 2), 0),
      y = 1 + c(0.5, -0.5, -1, -0.5, 0.5, 1)
    )
  } else {
    hexd <- data.frame(
      x = 1 + c(sqrt(3) / 4, sqrt(3) / 2, sqrt(3) / 4, -sqrt(3) / 4, -sqrt(3) / 2, -sqrt(3) / 4),
      y = 1 + c(-3 / 4, 0, 3 / 4, 3 / 4, 0, -3 / 4)
    )
  }
  hexd <- rbind(hexd, hexd[1, ])

  geom_polygon(aes(x = x, y = y),
    data = hexd, linewidth = size,
    fill = fill, color = color
  )
}

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

myDnButton <- function(outputId, label = "Save", type = "default", ...) {
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
    div(
      fileInput("file",
        label = div(
          style = "display: inline-flex; align-items: center;",
          "Background Image"
        ),
        buttonLabel = "PNG",
        placeholder = "",
        accept = ".png"
      )
    ),
    fluidRow(
      column(
        width = 6,
        myButton(
          inputId = "clear",
          label = "Clear",
          icon = argonIcon("button-pause"),
          type = "warning"
        )
      ),
      column(
        width = 6,
        myDnButton("imgdn", type = "danger")
      )
    ),
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
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
      tags$head(
        tags$link(rel = "shortcut icon", href = "favicon.png")
      ),
      tags$style(HTML("
        .tooltip-container {
          position: relative;
          display: inline-block;
        }
        .info-tooltip {
          margin-left: 8px;
          cursor: pointer;
          color: #5e72e4;
          font-size: 0.85em;
        }
        .info-content {
          display: none;
          position: absolute;
          left: 100%;
          top: 0;
          background: #172b4d;
          color: white;
          padding: 10px;
          border-radius: 4px;
          z-index: 1000;
          font-size: 0.85em;
          box-shadow: 0 7px 14px rgba(50,50,93,.1), 0 3px 6px rgba(0,0,0,.08);
        }
        .tooltip-container:hover .info-content {
          display: block;
        }
      "))
    ),
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
            column(colourInput(inputId = "background_color", label = "Background", value = "#172b4d"), width = 6), # background_color
            column(sliderInput(inputId = "background_alpha", label = "Transparency", value = 1, min = 0, max = 1, step = 0.1, ticks = FALSE), width = 6) # new transparency slider
          ),
          fluidRow(
            column(colourInput(inputId = "border_color", label = "Border", value = "#6772E5"), width = 6), # border_color
            column(sliderInput(inputId = "border_width", label = "Width", value = 1.5, step = 0.05, min = 0.1, max = 3, ticks = FALSE), width = 6) # border_width
          ),
          fluidRow(
            column(
              width = 12,
              radioGroupButtons(
                inputId = "hex_direction",
                label = "Direction",
                choices = c("up" = "up", "down" = "down"),
                selected = "up",
                size = "sm",
                status = "primary",
                individual = TRUE,
                width = "100%"
              )
            )
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
                step = 0.1, min = 0, max = 10
              ),
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
                choices = c("left" = 330, "right" = 30, "down" = 0),
                selected = 30,
                individual = TRUE,
                size = "sm",
                status = "primary",
                width = "100%"
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
    class = "custom-footer",
    argonFooterMenu(
      class = "footer-menu",
      argonFooterItem(argonIcon("app"), "hexSticker", src = "https://github.com/GuangchuangYu/hexSticker"),
      argonFooterItem(argonIcon("palette"), "Argon", src = "https://rinterface.github.io/argonDash/"),
      argonFooterItem(argonIcon("world"), "GitHub", src = "https://github.com/jhk0530/polaroid")
    )
  )
)

server <- function(input, output, session) {
  # Define reactive values to store state
  values <- reactiveValues(
    im = NULL,
    font_loaded = FALSE,
    current_font = NULL
  )

  # clear logic
  observeEvent(input$clear,
    {
      values$im <- NULL
      output$img <- renderPlot({
        NULL
      })
      showNotification("Image cleared", type = "message")
    },
    ignoreInit = TRUE
  )

  # font change with improved error handling
  observeEvent(input$fontApply, {
    req(input$font)

    if (is.null(values$im)) {
      showNotification("Please upload an image first", type = "warning")
      return()
    }

    withProgress(
      message = "Loading Font...",
      {
        tryCatch(
          {
            # Check if font name is not empty
            if (nchar(trimws(input$font)) == 0) {
              showNotification("Please enter a valid font name", type = "warning")
              values$font_loaded <- FALSE
              values$current_font <- "Aller_Rg"
              return()
            }

            font_add_google(name = input$font, family = input$font)
            showtext_auto()
            values$font_loaded <- TRUE
            values$current_font <- input$font
            showNotification(paste("Font", input$font, "loaded successfully"), type = "message")
          },
          error = function(e) {
            values$font_loaded <- FALSE
            values$current_font <- "Aller_Rg"
            showNotification(paste("Font error:", e$message, "- Using default font instead"), type = "error")
          }
        )

        incProgress(1)
      }
    )

    # Use the actual font that was loaded, or fallback to default
    font_to_use <- ifelse(values$font_loaded, values$current_font, "Aller_Rg")

    tryCatch(
      {
        s <- mysticker(values$im, input$img_x, input$img_y, input$img_width, input$img_height,
          input$pkg_name, input$pkg_x, input$pkg_y, input$pkg_color, input$pkg_size,
          input$background_color, input$border_color, input$border_width,
          input$url, input$url_x, input$url_y, input$url_color, input$url_size, input$url_angle,
          font_family = font_to_use,
          background_alpha = input$background_alpha,
          hex_direction = input$hex_direction # 방향 매개변수 추가
        )

        output$img <- renderPlot({
          par(mar = c(0.8, 0.8, 0.8, 0.8))
          plot(imager::load.image("temp.png"), axes = FALSE)
        })

        output$imgdn <- downloadHandler(
          filename = paste0("polaroid_", gsub("[^a-zA-Z0-9]", "_", input$pkg_name), ".png"),
          content = function(file) {
            save_sticker(filename = file, s)
          }
        )
      },
      error = function(e) {
        showNotification(paste("Error generating sticker:", e$message), type = "error")
      }
    )
  })

  # Example button with error handling
  observeEvent(input$polaroid, {
    tryCatch(
      {
        example_path <- "images/nut.png"
        if (!file.exists(example_path)) {
          showNotification("Example image not found", type = "error")
          return()
        }

        values$im <- readPNG(example_path)

        s <- mysticker(values$im, input$img_x, input$img_y, input$img_width, input$img_height,
          input$pkg_name, input$pkg_x, input$pkg_y, input$pkg_color, input$pkg_size,
          input$background_color, input$border_color, input$border_width,
          input$url, input$url_x, input$url_y, input$url_color, input$url_size, input$url_angle,
          font_family = ifelse(values$font_loaded, values$current_font, "Aller_Rg"),
          background_alpha = input$background_alpha,
          hex_direction = input$hex_direction # 방향 매개변수 추가
        )

        output$img <- renderPlot({
          par(mar = c(0.8, 0.8, 0.8, 0.8))
          plot(imager::load.image("temp.png"), axes = FALSE)
        })

        output$imgdn <- downloadHandler(
          filename = "polaroid_example.png",
          content = function(file) {
            save_sticker(filename = file, s)
          }
        )
      },
      error = function(e) {
        showNotification(paste("Error loading example:", e$message), type = "error")
      }
    )
  })

  # File input handler with validation
  observeEvent(input$file, {
    req(input$file)

    # Check file size (limit to 5MB)
    if (input$file$size > 5 * 1024 * 1024) {
      showNotification("File too large (max 5MB)", type = "error")
      return()
    }

    tryCatch(
      {
        values$im <- readPNG(input$file$datapath)
        showNotification("Image loaded successfully", type = "message")
      },
      error = function(e) {
        showNotification(paste("Error loading image:", e$message), type = "error")
      }
    )
  })

  # Input parameter changes with validation
  observeEvent(
    {
      input$background_color
      input$background_alpha
      input$border_color
      input$border_width
      input$hex_direction
      input$img_x
      input$img_y
      input$img_width
      input$img_height
      input$file
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
      if (is.null(values$im)) {
        return()
      }

      # Validate inputs
      if (input$img_width <= 0 || input$img_height <= 0) {
        showNotification("Image dimensions must be positive", type = "warning")
        return()
      }

      if (nchar(trimws(input$pkg_name)) == 0) {
        showNotification("Package name cannot be empty", type = "warning")
        return()
      }

      tryCatch(
        {
          s <- mysticker(values$im, input$img_x, input$img_y, input$img_width, input$img_height,
            input$pkg_name, input$pkg_x, input$pkg_y, input$pkg_color, input$pkg_size,
            input$background_color, input$border_color, input$border_width,
            input$url, input$url_x, input$url_y, input$url_color, input$url_size, input$url_angle,
            font_family = ifelse(values$font_loaded, values$current_font, "Aller_Rg"),
            background_alpha = input$background_alpha,
            hex_direction = input$hex_direction
          )

          output$img <- renderPlot({
            par(mar = c(0.8, 0.8, 0.8, 0.8))
            plot(imager::load.image("temp.png"), axes = FALSE)
          })

          output$imgdn <- downloadHandler(
            filename = paste0("polaroid_", gsub("[^a-zA-Z0-9]", "_", input$pkg_name), ".png"),
            content = function(file) {
              save_sticker(filename = file, s)
            }
          )
        },
        error = function(e) {
          showNotification(paste("Error generating sticker:", e$message), type = "error")
        }
      )
    }
  )

  # This is the only mysticker function you should have
  mysticker <- function(im, img_x, img_y, img_width, img_height,
                        pkg_name, pkg_x, pkg_y, pkg_color, pkg_size,
                        background_color, border_color, border_width,
                        url, urlx, urly, url_color, url_size, url_angle, font_family,
                        background_alpha = 1, hex_direction = "up") {
    if (is.null(font_family) || nchar(trimws(font_family)) == 0) {
      font_family <- "Aller_Rg"
    }

    # Validate parameters more thoroughly
    if (is.null(im)) {
      stop("No image provided")
    }

    # Apply alpha to background color
    if (!is.null(background_alpha) && background_alpha < 1) {
      # Convert hex color to rgba
      bg_col <- grDevices::col2rgb(background_color, alpha = TRUE)
      bg_col[4] <- round(255 * background_alpha) # Set alpha value
      background_color <- grDevices::rgb(bg_col[1], bg_col[2], bg_col[3], bg_col[4], maxColorValue = 255)
    }

    g_rotated <- rasterGrob(im, interpolate = TRUE)

    g <- ggplot() +
      hexagon(border_width, background_color, border_color, direction = hex_direction)

    g <- g +
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
        g_rotated, # Use the potentially rotated grob here
        xmin = img_x,
        xmax = img_x + img_width,
        ymin = img_y,
        ymax = img_y + img_height
      ) +
      theme_sticker(size = h_size)

    # Improved saving with error handling
    tryCatch(
      {
        ggsave(
          plot = g,
          filename = "temp.png",
          width = 43.9,
          height = 50.8,
          dpi = 300,
          bg = "transparent",
          units = "mm",
          device = "png"
        )
      },
      error = function(e) {
        stop(paste("Failed to save image:", e$message))
      }
    )

    return(g)
  }
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
