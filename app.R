library(tidyverse)
library(shiny)
library(ggtext)
library(shinybrowser)
library(extrafont)
library(extrafontdb)
library(bslib)
library(grid)
library(glue)
library(ggforce)
library(Cairo)
library(shinyWidgets)
library(shinyjs)
library(magrittr)
library(extraDistr)
library(formattable)
library(sn)
library(shinyalert)
library(stevemisc)

bs_global_theme()

ui <- fluidPage(

  useShinyjs(),

  shinybrowser::detect(),

  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),

  tags$style(
    HTML(
      "
      #right-column {
        background-color: #ebf6fa;
        border-radius: 12px;
        padding: 20px;
      }
      "
    )
  ),

  tags$head(tags$style(HTML("
  .modal-header .modal-title {
    text-align: center;
    width: 100%;
  }"))),

  tags$head(
    tags$style(HTML(".multicol {-webkit-column-count: 2; /* Chrome, Safari, Opera */-moz-column-count: 2; /* Firefox */column-count: 2;}"))

  ),

  theme = bs_theme(version = 4,
                   fg = "#081825",
                   bg = "#ffffff",
                   base_font = font_google("Jost"),
                   primary = "#ededed"
  ),

  tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),

  tags$script(HTML("
    $(document).on('shiny:connected', function(event) {
      setTimeout(function(){
        $('#loading').fadeOut('slow');
      }, 11000);  // Delay of 11 seconds
    });
  ")),

  tags$div(id="loading",
           tags$span(id="loading-text", "Distributr is loading..."),
           tags$div(id="rect1", class="rectangle"),
           tags$div(id="rect2", class="rectangle"),
           tags$div(id="rect3", class="rectangle"),
           tags$div(id="rect4", class="rectangle"),
           tags$div(id="rect5", class="rectangle"),
           tags$div(id="rect6", class="rectangle"),
           tags$div(id="rect7", class="rectangle"),
           tags$div(id="rect8", class="rectangle"),
           tags$div(id="rect9", class="rectangle"),
           tags$div(id="rect10", class="rectangle"),
           tags$div(id="rect11", class="rectangle")
  ),

  br(),
  br(),


  #### Plot area of ui ####
  fluidRow(
    column(6,
           fluidRow(column(10,
                           offset = 1,
                           plotOutput("left_plot",
                                      click = "plot_click"),
                           br(),
                           align = "center",
                           sliderInput("plot_range",
                                       "Zoom to percentile range:",
                                       value = c(0, 100),
                                       min = 0,
                                       max = 100,
                                       step = 1,
                                       width = "100%",
                                       ticks = FALSE)
           ))
    ),
    column(6,
           fluidRow(
             column(10,
                    offset = 1,
                    align = "center",
                    formattableOutput('table'),
                    formattableOutput("hdi_table"),
                    formattableOutput("click_table"))
           ),

    )
  ),

  br(),

  fluidRow(
    column(10,
           offset = 1,
           align = "center",
           formattableOutput('quantile_table'))
  ),

  br(),

  hr(),

  #### UI selection options ####
  fluidRow(
    column(9,
           fluidRow(
             column(4,
                    fluidRow(column(10,
                                    offset = 1,
                                    align = "center",
                                    uiOutput("distribution_choice_ui"),
                                    hr(),
                                    uiOutput("parameter_ui") ,
                                    br()
                    ))
             ),
             column(4,
                    fluidRow(column(10,
                                    offset = 1,
                                    align = "center",
                                    selectInput("plot_choice",
                                                "Plot type:",
                                                choices = c("Histogram", "Percentogram", "Density", "Points"),
                                                selected = "Histogram"),
                                    selectInput("plot_format",
                                                "Plot format:",
                                                choices = c("Standard", "Cumulative"),
                                                selected = "Standard"),
                                    uiOutput("plot_options")
                    ))
             ),
             column(4,
                    fluidRow(column(10,
                                    offset = 1,
                                    align = "center",
                                    selectInput("summary_range_type",
                                                "Interval shows (dotted lines):",
                                                choices = c("Highest density interval",
                                                            "Quantile/equal-tailed interval"),
                                                selected = "Highest density interval"),
                                    sliderInput("summary_range_number",
                                                "Interval width:",
                                                min = 5,
                                                max = 99,
                                                value = 95,
                                                step = 1,
                                                ticks = FALSE),
                                    selectInput("summary_point_type",
                                                "Point estimate shows (solid line):",
                                                choices = c("Mean",
                                                            "Median",
                                                            "Mode"),
                                                selected = "Mean"),
                    ))
             ),
           ),
           hr(),
           fluidRow(
             column(4,
                    offset = 2,
                    align = "center",
                    br(),
                    actionButton("upload_custom",
                                 "Upload custom distribution",
                                 style = "color: #ffffff; background-color: #2f6e8d; border-color: #2f6e8d",
                                 width = "90%"),
                    br()),
             column(4,
                    align = "center",
                    br(),
                    uiOutput("remove_custom_ui"),
                    br())

           ),
           hr(),
           br(),
           fluidRow(
             column(10,
                    offset = 1,
                    align = "center",
                    uiOutput("code_text_ui")),
             br(),
           )
           ),
    column(3,
           fluidRow(column(10,
                           id = "right-column",
                           offset = 1,
                           align = "center",
                           p("You can download 10000 samples from your specified distributions as a csv file using the download button below."),
                           p("Create your data by adding (or clearing/overwriting) columns with the buttons below, and then click the download button."),
                           uiOutput("select_column_ui"),
                           uiOutput("name_sample_ui"),
                           uiOutput("store_sample_ui"),
                           br(),
                           uiOutput("clear_sample_ui"),
                           br(),
                           uiOutput("column_count_ui"),
                           br(),
                           hr(),
                           uiOutput("download_button_ui")
           )
           )

    )
  ),

  br(),
  hr(),
  br(),

  br()

)

server <- function(input, output, session) {

  #### hdp code ####
  hdp <- function(data) {

    hdp <- density(data)$x[which.max(density(data)$y)]

    return(hdp)

  }

  #source("theme_distributr.R", local = TRUE)

  #### theme function ####
  theme_distributr <- function (base_size = 18,
                                legend_vjust = .075,
                                legend_hjust = 0,
                                base_family = "Helvetica",
                                base_line_size = base_size/22,
                                base_rect_size = base_size/22,
                                grid_line_color = "white",
                                x_ticks = FALSE,
                                y_ticks = FALSE,
                                top_margin = 5,
                                right_margin = 15,
                                bottom_margin = 5,
                                left_margin = 5) {
    half_line <- base_size/2
    t <- ggplot2::theme(line = element_line(colour = "black",
                                            size = base_line_size,
                                            linetype = 1,
                                            lineend = "butt"),
                        rect = element_rect(fill = "white",
                                            colour = "black",
                                            size = base_rect_size,
                                            linetype = 1),
                        text = element_text(family = base_family,
                                            face = "plain",
                                            colour = "black",
                                            size = base_size,
                                            lineheight = 0.9,
                                            hjust = 0.5,
                                            vjust = 0.5,
                                            angle = 0,
                                            margin = margin(),
                                            debug = FALSE),
                        axis.line = element_blank(),
                        axis.line.x = NULL,
                        axis.line.y = NULL,
                        axis.text = ggtext::element_markdown(size = rel(0.8),
                                                             colour = "grey5"),
                        axis.text.x = ggtext::element_markdown(margin = margin(6, 0, 2, 0),
                                                               vjust = 1),
                        axis.text.y = ggtext::element_markdown(margin = margin(0, 4, 0, 0),
                                                               vjust = .5,
                                                               hjust = 1),
                        axis.text.x.top = ggtext::element_markdown(margin = margin(b = 0.8 * half_line/2),
                                                                   hjust = 1),
                        axis.text.y.right = ggtext::element_markdown(margin = margin(l = 0.8 * half_line/2),
                                                                     hjust = 0),
                        axis.ticks.x = if(x_ticks == FALSE) {element_blank()} else{element_line(colour = "grey5")},
                        axis.ticks.y = if(y_ticks == FALSE) {element_blank()} else{element_line(colour = "grey5")},
                        axis.ticks.length = unit(half_line/2, "pt"),
                        axis.ticks.length.x = NULL,
                        axis.ticks.length.x.top = NULL,
                        axis.ticks.length.x.bottom = NULL,
                        axis.ticks.length.y = NULL,
                        axis.ticks.length.y.left = NULL,
                        axis.ticks.length.y.right = NULL,
                        axis.title.x = ggtext::element_markdown(margin = margin(6, 0, 3, 0),
                                                                vjust = 1),
                        axis.title.x.top = ggtext::element_markdown(margin = margin(b = half_line/2),
                                                                    vjust = 0),
                        axis.title.y = ggtext::element_markdown(angle = 90,
                                                                margin = margin(0, 5, 0, 0),
                                                                vjust = .5),
                        axis.title.y.right = ggtext::element_markdown(angle = -90,
                                                                      margin = margin(l = half_line/2), vjust = 0),
                        legend.background = element_rect(colour = NA),
                        legend.spacing = unit(2 * half_line, "pt"),
                        legend.spacing.x = NULL,
                        legend.spacing.y = NULL,
                        legend.margin = margin(0, 0, 0, 0),
                        legend.key = element_rect(fill = NA,
                                                  colour = NA),
                        legend.key.size = unit(.9, "lines"),
                        legend.key.height = NULL,
                        legend.key.width = NULL,
                        legend.text = ggtext::element_markdown(size = rel(0.85),
                                                               color = "grey5",
                                                               vjust = .075),
                        legend.text.align = NULL,
                        legend.title = ggtext::element_markdown(hjust = legend_hjust,
                                                                vjust = legend_vjust,
                                                                size = rel(.925)),
                        legend.title.align = NULL,
                        legend.position = "bottom",
                        legend.direction = NULL,
                        legend.justification = "center",
                        legend.box = NULL,
                        legend.box.margin = margin(0, 0, 0, 0, "cm"),
                        legend.box.background = element_blank(),
                        legend.box.spacing = unit(2 * half_line, "pt"),
                        panel.background = element_rect(fill = "grey94",
                                                        colour = NA),
                        panel.border = element_blank(),
                        panel.grid = element_line(colour = grid_line_color),
                        panel.grid.minor = element_line(size = rel(0.5)),
                        panel.spacing = unit(half_line, "pt"),
                        panel.spacing.x = NULL,
                        panel.spacing.y = NULL,
                        panel.ontop = FALSE,
                        strip.background = element_rect(fill = NA,
                                                        colour = NA),
                        strip.text = ggtext::element_markdown(colour = "black",
                                                              size = rel(0.95),
                                                              margin = margin(0.8 * half_line, 0.8 * half_line, 0.8 * half_line, 0.8 * half_line)),
                        strip.text.x = NULL,
                        strip.text.y = ggtext::element_markdown(angle = -90),
                        strip.text.y.left = ggtext::element_markdown(angle = 90),
                        strip.placement = "inside",
                        strip.placement.x = NULL,
                        strip.placement.y = NULL,
                        strip.switch.pad.grid = unit(half_line/2,
                                                     "pt"),
                        strip.switch.pad.wrap = unit(half_line/2, "pt"),
                        plot.background = element_rect(colour = "white"),
                        plot.title = ggtext::element_markdown(size = rel(1.15),
                                                              hjust = 0,
                                                              vjust = 1,
                                                              margin = margin(b = half_line)),
                        plot.title.position = "plot",
                        plot.subtitle = ggtext::element_markdown(hjust = 0,
                                                                 vjust = 1,
                                                                 margin = margin(1, 1, 8, 1)),
                        plot.caption = ggtext::element_markdown(size = rel(0.8),
                                                                hjust = 1,
                                                                vjust = 1,
                                                                margin = margin(2, 0, 0, 0)),
                        plot.caption.position = "panel",
                        plot.tag = ggtext::element_markdown(size = rel(1.15),
                                                            hjust = 0.5,
                                                            vjust = 0.5,
                                                            margin = margin(0, 0, 5, 0)),
                        plot.tag.position = "top",
                        plot.margin = margin(top_margin,
                                             right_margin,
                                             bottom_margin,
                                             left_margin),
                        complete = TRUE)
    #ggplot_global$theme_all_null %+replace% t
  }

  #### count_data function ####
  count_data <- function(data, ..., na.rm = FALSE, pct = FALSE) {
    checkmate::assert_data_frame(data)

    output <- dplyr::count(data, ...)

    # Remove missing observations if na.rm is set to TRUE
    if (na.rm) {
      output <- dplyr::filter(
        output,
        dplyr::if_all(dplyr::everything(), ~ !is.na(.))
      )
    }

    # Calculate proportion or percentage of each group per var
    if (pct) {
      output <- dplyr::mutate(output, pct = n / sum(n) * 100)
    } else {
      output <- dplyr::mutate(output, prop = n / sum(n))
    }


    # Add a tidystats class so we can use the tidy_stats() function to parse the
    # the output
    class(output) <- c("tidystats_counts", class(output))

    return(output)
  }

  #### nice_num function ####
  nice_num <- function(number, decimals = 2, remove_lead = TRUE) {

    sprintf_string <- sprintf('%%.%df', decimals)

    get_to_dp <- sprintf(sprintf_string, number)

    if (remove_lead == TRUE) {
      output <- sub("^0+", "", get_to_dp)
      output <- stringr::str_replace(output, "-0\\.", "-\\.")
    } else {
      output <- get_to_dp
    }

    return(output)

  }

  #### make_colors code ####
  make_colors <- function(colors = NULL, type = "diverging", n = 5, alpha = 1, append_missing = "none", missing_colors = "#D9D9D9") {

    if(is.null(colors)) {
      if(type == "diverging") {
        colors <- c("#56a3cf", "#cecece", "#f2763c")
      }
      else if(type == "linear") {
        colors <- c("#cbe8e7", "#037876")
      }
    }

    gradient_function <- colorRampPalette(colors)

    color_output <- gradient_function(n)

    if(append_missing == "start") {
      hex_missing_colors <- colorRampPalette(missing_colors)
      missing_colors <- hex_missing_colors(length(missing_colors))

      color_output <- c(missing_colors, color_output)
    }
    else if(append_missing == "end") {
      hex_missing_colors <- colorRampPalette(missing_colors)
      missing_colors <- hex_missing_colors(length(missing_colors))

      color_output <- c(color_output, missing_colors)
    }

    if(alpha != 1) {
      alpha_codes <- tibble(alpha_string = c("FF",
                                             "FC",
                                             "FA",
                                             "F7",
                                             "F5",
                                             "F2",
                                             "F0",
                                             "ED",
                                             "EB",
                                             "E8",
                                             "E6",
                                             "E3",
                                             "E0",
                                             "DE",
                                             "DB",
                                             "D9",
                                             "D6",
                                             "D4",
                                             "D1",
                                             "CF",
                                             "CC",
                                             "C9",
                                             "C7",
                                             "C4",
                                             "C2",
                                             "BF",
                                             "BD",
                                             "BA",
                                             "B8",
                                             "B5",
                                             "B3",
                                             "B0",
                                             "AD",
                                             "AB",
                                             "A8",
                                             "A6",
                                             "A3",
                                             "A1",
                                             "9E",
                                             "9C",
                                             "99",
                                             "96",
                                             "94",
                                             "91",
                                             "8F",
                                             "8C",
                                             "8A",
                                             "87",
                                             "85",
                                             "82",
                                             "80",
                                             "7D",
                                             "7A",
                                             "78",
                                             "75",
                                             "73",
                                             "70",
                                             "6E",
                                             "6B",
                                             "69",
                                             "66",
                                             "63",
                                             "61",
                                             "5E",
                                             "5C",
                                             "59",
                                             "57",
                                             "54",
                                             "52",
                                             "4F",
                                             "4D",
                                             "4A",
                                             "47",
                                             "45",
                                             "42",
                                             "40",
                                             "3D",
                                             "3B",
                                             "38",
                                             "36",
                                             "33",
                                             "30",
                                             "2E",
                                             "2B",
                                             "29",
                                             "26",
                                             "24",
                                             "21",
                                             "1F",
                                             "1C",
                                             "1A",
                                             "17",
                                             "14",
                                             "12",
                                             "0F",
                                             "0D",
                                             "0A",
                                             "08",
                                             "05",
                                             "03",
                                             "00"),
                            alpha_value = seq(1, 0, -.01))

      selected_alpha <- filter(alpha_codes,
                               round(alpha_value, 2) == round(alpha, 2))

      append_alpha <- function(color, .alpha) {
        new_color <- glue::glue("{color}{.alpha}")
        return(new_color)
      }

      color_output <- unlist(map(.x = color_output,
                                 .f = append_alpha,
                                 .alpha = selected_alpha[1, "alpha_string"]))

    }

    return(color_output)

  }

  #### Percentogram code ####
  make_percentogram_x <- function(data, percent = 5, percentile_range = FALSE, variable_name = "x", .ymin = 0, .ymax = .75) {

    # check whether a vector or dataframe was given to the function
    # and convert to a vector
    if(is.data.frame(data) == TRUE) {

      # extract the column of interest
      data <-
        data[, variable_name] %>%
        dplyr::pull()
    }

    # get length of all the data points, including possible NAs
    full_length <- length(data)

    # remove NAs
    data <- na.omit(data)

    # get new length of data points without NAs
    nona_length <- length(data)

    # get difference in lengths:
    difference <- full_length - nona_length

    # if there were NAs, provide message so user knows
    if(difference > 0) {
      #print(glue::glue("Note: {difference} NA values were dropped from the data"))
    }

    # get quantiles that will be used based on the percentage provided
    pg_quantiles <-
      quantile(data, seq(0, 1, percent/100))

    if(100 %% percent != 0) {
      #print(glue::glue("Chosen percent of {percent} does not perfectly split 100. Your final percentile/quantile bar ends at {round(names(pg_quantiles) %>% readr::parse_number() %>% last(), 3)}"))
    }

    pg_data <-
      tibble::tibble(xmin = pg_quantiles[1:length(pg_quantiles)-1],
                     xmax = pg_quantiles[2:length(pg_quantiles)],
                     width = abs(xmin - xmax),
                     lq = names(pg_quantiles) %>% readr::parse_number() %>% .[1:length(pg_quantiles)-1],
                     uq = names(pg_quantiles) %>% readr::parse_number() %>% .[2:length(pg_quantiles)],
                     lq_50 = abs(lq - 50),
                     uq_50 = abs(uq - 50),
                     ymin = .ymin,
                     ymax = 1 / width)

    max_height <- max(pg_data$ymax)

    pg_data <-
      pg_data %>%
      dplyr::mutate(ymax = ((ymax / max_height) * .ymax) + .ymin) %>%
      dplyr::relocate(xmin, xmax, ymin, ymax)

    if("quartile" %in% percentile_range) {
      pg_data <-
        pg_data %>%
        dplyr::mutate(quartile = case_when(lq >= 0 & uq <= 25 ~ "1",
                                           lq >= 25 & uq <= 50 ~ "2",
                                           lq >= 50 & uq <= 75 ~ "3",
                                           lq >= 75 & uq <= 100 ~ "4",
                                           TRUE ~ "Crosses quartiles"),
                      quartile = factor(quartile,
                                        levels = c("1", "2", "3", "4", "Crosses quartiles")))

    }
    if("quintile" %in% percentile_range) {
      pg_data <-
        pg_data %>%
        dplyr::mutate(quintile = case_when(lq >= 0 & uq <= 20 ~ "1",
                                           lq >= 20 & uq <= 40 ~ "2",
                                           lq >= 40 & uq <= 60 ~ "3",
                                           lq >= 60 & uq <= 80 ~ "4",
                                           lq >= 80 & uq <= 100 ~ "5",
                                           TRUE ~ "Crosses quintiles"),
                      quintile = factor(quintile,
                                        levels = c("1", "2", "3", "4", "5", "Crosses quintile")))

    }
    if("octile" %in% percentile_range) {
      pg_data <-
        pg_data %>%
        dplyr::mutate(octile = case_when(lq >= 0 & uq <= 12.5 ~ "1",
                                         lq >= 12.5 & uq <= 25 ~ "2",
                                         lq >= 25 & uq <= 37.5 ~ "3",
                                         lq >= 37.5 & uq <= 50 ~ "4",
                                         lq >= 50 & uq <= 62.5 ~ "5",
                                         lq >= 62.5 & uq <= 75 ~ "6",
                                         lq >= 75 & uq <= 87.5 ~ "7",
                                         lq >= 87.5 & uq <= 100 ~ "8",
                                         TRUE ~ "Crosses octiles"),
                      octile = factor(octile,
                                      levels = c("1", "2", "3", "4", "5", "6", "7", "8", "Crosses octiles")))

    }
    if("decile" %in% percentile_range) {
      pg_data <-
        pg_data %>%
        dplyr::mutate(decile = case_when(lq >= 0 & uq <= 10 ~ "1",
                                         lq >= 10 & uq <= 20 ~ "2",
                                         lq >= 20 & uq <= 30 ~ "3",
                                         lq >= 30 & uq <= 40 ~ "4",
                                         lq >= 40 & uq <= 50 ~ "5",
                                         lq >= 50 & uq <= 60 ~ "6",
                                         lq >= 60 & uq <= 70 ~ "7",
                                         lq >= 70 & uq <= 80 ~ "8",
                                         lq >= 80 & uq <= 90 ~ "9",
                                         lq >= 90 & uq <= 100 ~ "10",
                                         TRUE ~ "Crosses decile"),
                      decile = factor(decile,
                                      levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "Crosses deciles")))

    }

    return(pg_data)

  }

  #### User information ####
  device_type <- reactiveVal(
    "Desktop"
  )

  observe({
    device_info <- shinybrowser::get_all_info()
    device_type(device_info$device)
  })

  #### Code text output ####
  code_text_value <-
    reactiveVal("extraDistr::rprop(n, 10, .5)")

  code_text_reactive_values <- reactiveValues(
    my_text = "extraDistr::rprop(n, 10, .5)"
  )

  update_text_function <- function(my_input) {
    if(!my_input %in% c("Beta", "Beta as %", "Normal", "Skew normal", "Exponential", "Log-normal", "Student's t", "Uniform")) {
      return()
    }
    else if(my_input == "Beta") {
      delay(2000, code_text_reactive_values$my_text <- as.character(glue::glue("extraDistr::rprop(n, {input$beta_precision}, {input$beta_mean})")))
    }
    else if(my_input == "Normal") {
      delay(2000, code_text_reactive_values$my_text <- glue::glue("rnorm(n, {input$normal_mean}, {input$normal_sd})"))
    }
    else if(my_input == "Skew normal") {
      delay(2000, code_text_reactive_values$my_text <- glue::glue("sn::rsn(n, {input$skewnormal_location}, {input$skewnormal_scale}, {input$skewnormal_slant})"))
    }
    else if(my_input == "Beta as %") {
      delay(2000, code_text_reactive_values$my_text <- glue::glue("extraDistr::rprop(n, {input$betapercent_precision}, {input$betapercent_mean}) * 100"))
    }
    else if(my_input == "Exponential") {
      delay(2000, code_text_reactive_values$my_text <- glue::glue("rexp(n, {input$exp_rate})"))
    }
    else if(my_input == "Log-normal") {
      delay(2000, code_text_reactive_values$my_text <- glue::glue("rlnorm(n, {input$lognormal_mean}, {input$lognormal_sd})"))
    }
    else if(my_input == "Student's t") {
      delay(2000, code_text_reactive_values$my_text <- glue::glue("stevemisc::rst(n, {input$t_df}, {input$t_location}, {input$t_scale})"))
    }
    else if(my_input == "Uniform") {
      delay(2000, code_text_reactive_values$my_text <- glue::glue("runif(n, {input$uniform_min}, {input$uniform_max})"))
    }
  }

  observeEvent(input$distribution_choice, {

    update_text_function(my_input = input$distribution_choice)

  })

  observeEvent(input$beta_update, {

    update_text_function(my_input = input$distribution_choice)

  })

  observeEvent(input$betapercent_update, {

    update_text_function(my_input = input$distribution_choice)

  })

  observeEvent(input$normal_update, {

    update_text_function(my_input = input$distribution_choice)

  })

  observeEvent(input$skewnormal_update, {

    update_text_function(my_input = input$distribution_choice)

  })

  observeEvent(input$exp_update, {

    update_text_function(my_input = input$distribution_choice)

  })

  observeEvent(input$lognormal_update, {

    update_text_function(my_input = input$distribution_choice)

  })

  observeEvent(input$t_update, {

    update_text_function(my_input = input$distribution_choice)

  })

  observeEvent(input$uniform_update, {

    update_text_function(my_input = input$distribution_choice)

  })


  output$code_text_ui <- renderUI({

    if(!should_render_ui()) {
      return()
    }

    if(!input$distribution_choice %in% c("Beta", "Beta as %", "Normal", "Skew normal", "Exponential", "Log-normal", "Student's t", "Uniform")) {
      p(HTML("This is a custom distribution that you've uploaded,<br>so there is no associated R code."))
    }
    else {
      p(HTML(glue::glue("You can generate this distribution in R with the following code,<br>where <code>n</code> should be replaced with your desired number of samples:<br><code>{code_text_reactive_values$my_text}</code>")))
    }

  })

  #### Distribution choice UI section ####
  should_render_ui <- reactiveVal(FALSE)

  ##### reactive values for custom distribution distribution_values #####
  distribution_values <- reactiveValues(
    distribution_list = list("Distributions" = c("Beta", "Beta as %", "Normal", "Skew normal", "Exponential", "Log-normal", "Student's t", "Uniform")),
    n_custom = 0,
    custom_data = as.list(rep(NA, 15)),
    custom_data_summaries = as.list(rep(NA, 15)),
    custom_data_quantiles = as.list(rep(NA, 15)),
    custom_data_hdis = as.list(rep(NA, 15)),
    data_5000 = as.list(rep(NA, 15)),
    data_density = as.list(rep(NA, 15))

  )

  ##### ui buttons for custom distribution #####
  # add custom distributions
  output$distribution_choice_ui <- renderUI({

    selectInput("distribution_choice",
                "Distribution:",
                choices = distribution_values$distribution_list,
                selected = "Beta")

  })

  # remove custom distributions
  output$remove_custom_ui <- renderUI({

    if(!should_render_ui()) {
      return()
    }

    if(length(distribution_values$distribution_list) == 1) {
      disabled(actionButton("remove_custom",
                            "Remove custom distribution",
                            style="color: #ffffff; background-color: #ff8f6f; border-color: #ff8f6f",
                            width = "90%")
               )
    }
    else {
      actionButton("remove_custom",
                   "Remove custom distribution",
                   style="color: #ffffff; background-color: #ff8f6f; border-color: #ff8f6f",
                   width = "90%")
    }

  })

  ##### remove_custom observe event and modal dialogue #####
  observeEvent(input$remove_custom, {

    showModal(modalDialog(
      easyClose = TRUE,
      title = span("Select and remove custom distributions..."),
      fluidRow(column(12,
                      align = "center",
                      p('Use the dropdown menu below to select the custom distributions you would like to remove.'),
                      p('A total of 15 custom distributions are allowed at a time.'),
                      selectInput("remove_custom_dropdown",
                                "Remove:",
                                multiple = TRUE,
                                selected = NULL,
                                choices = distribution_values$distribution_list$Custom),
                      actionButton("remove_custom_confirm",
                                   "Confirm removal",
                                   style="color: #ffffff; background-color: #ff8f6f; border-color: #ff8f6f")
      )),
      footer = tagList(
        modalButton("Cancel")
      )
    ))

  })

  # confirming the removal:
  observeEvent(input$remove_custom_confirm, {

    # if they haven't selected anything then just close the modal
    if(is.null(input$remove_custom_dropdown)) {
      removeModal()
      return()
    }
    # if they have selected something then...
    else {
      # find out which of the things they have selected and get their numeric positions
      targets_for_removal <- which(distribution_values$distribution_list$Custom %in% input$remove_custom_dropdown)

      # if the number of things selected for removal is equivalent to every custom distribution
      # then just reset the custom stuff back to scratch
      if(length(targets_for_removal) == length(distribution_values$distribution_list$Custom)) {
        distribution_values$distribution_list <- list("Distributions" = c("Beta", "Beta as %", "Normal", "Skew normal", "Exponential", "Log-normal", "Student's t", "Uniform"))
        distribution_values$custom_data <- as.list(rep(NA, 15))
        distribution_values$custom_data_summaries <- as.list(rep(NA, 15))
        distribution_values$custom_data_quantiles <- as.list(rep(NA, 15))
        distribution_values$custom_data_hdis <- as.list(rep(NA, 15))
        distribution_values$data_5000 <- as.list(rep(NA, 15))
        distribution_values$data_density <- as.list(rep(NA, 15))
        shinyjs::delay(250,
                       removeModal())
        return()
      }
      # otherwise we need to target and modify the specific things
      else {
        # remove the targeted things from the respective lists
        distribution_values$distribution_list$Custom <- distribution_values$distribution_list$Custom[-c(targets_for_removal)]
        distribution_values$custom_data <- distribution_values$custom_data[-c(targets_for_removal)]
        distribution_values$custom_data_summaries <- distribution_values$custom_data_summaries[-c(targets_for_removal)]
        distribution_values$custom_data_quantiles <- distribution_values$custom_data_quantiles[-c(targets_for_removal)]
        distribution_values$custom_data_hdis <- distribution_values$custom_data_hdis[-c(targets_for_removal)]
        distribution_values$data_5000 <- distribution_values$data_5000[-c(targets_for_removal)]
        distribution_values$data_density <- distribution_values$data_density[-c(targets_for_removal)]
        shinyjs::delay(250,
                       removeModal())
        return()
      }

    }

  })

  ##### observe event for custom distribution #####
  observeEvent(input$upload_custom, {

    showModal(modalDialog(
      easyClose = TRUE,
      title = span("Upload a distribution from a csv file..."),
      fluidRow(column(12,
                      align = "center",
                      p('You can upload a .csv file containing a custom distribution, or distributions, to plot.'),
                      p('The .csv file must contain only those columns that you wish to upload and view. Each column must have a unique name, and contain only numeric values (ensure that any formulas are converted to numeric values, for example).'),
                      p('The plotting and summary functions are designed for working with distributions of thousands of samples, so they may work poorly or crash the app if you upload data that doesn\'t have many samples.'),
                      p('Up to 15 such custom distributions are supported.'),
                      fileInput("upload_custom_proper",
                                "Upload CSV File",
                                accept = ".csv")
                      )),
      footer = tagList(
        modalButton("Cancel")
      )
    ))

  })

  ##### function to update custom data #####
  update_custom_summary <- function(custom_target = 1) {

    data <- distribution_values$custom_data[[custom_target]]

    #code_text_value(glue::glue("This is a custom distribution"))

    distribution_values$custom_data_summaries[[custom_target]] <-
      summarise(data,
                Mean = mean(x),
                Median = median(x),
                Mode = hdp(x),
                `Lower ETI` = quantile(x, (1 - (input$summary_range_number / 100)) / 2),
                `Upper ETI` = quantile(x, (input$summary_range_number / 100) + (1 - (input$summary_range_number / 100)) / 2))

    distribution_values$custom_data_quantiles[[custom_target]] <-
      tibble(
        Min = nice_num(min(data$x), 2),
        `2.5%` = nice_num(quantile(data$x, .025), 2),
        `5%` =  nice_num(quantile(data$x, .05), 2),
        `10%` = nice_num(quantile(data$x, .1), 2),
        `25%` = nice_num(quantile(data$x, .25), 2),
        `50%` = nice_num(quantile(data$x, .5), 2),
        `75%` = nice_num(quantile(data$x, .75), 2),
        `90%` = nice_num(quantile(data$x, .9), 2),
        `95%` = nice_num(quantile(data$x, .95), 2),
        `97.5%` = nice_num(quantile(data$x, .975), 2),
        Max = nice_num(max(data$x), 2)
      )

    hdi_length <- length(tidybayes::hdi(data$x, input$summary_range_number / 100))

    if(hdi_length == 2) {
      distribution_values$custom_data_hdis[[custom_target]] <- tibble(HDI = glue::glue("{input$summary_range_number}%"),
                                                                    `Bound 1` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[1], 2),
                                                                    `Bound 2` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[2], 2))
    }
    else if(hdi_length == 4) {
      distribution_values$custom_data_hdis[[custom_target]] <- tibble(`Bnd 1` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[1], 2),
                                                                    `Bnd 2` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[3], 2),
                                                                    `Bnd 3` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[2], 2),
                                                                    `Bnd 4` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[4], 2))
    }
    else if(hdi_length == 6) {
      distribution_values$custom_data_hdis[[custom_target]] <- tibble(`Bnd 1` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[1], 2),
                                                                    `Bnd 2` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[4], 2),
                                                                    `Bnd 3` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[2], 2),
                                                                    `Bnd 4` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[5], 2),
                                                                    `Bnd 5` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[3], 2),
                                                                    `Bnd 6` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[6], 2))
    }
    else if(hdi_length == 8) {
      distribution_values$custom_data_hdis[[custom_target]] <- tibble(`Bnd 1` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[1], 2),
                                                                    `Bnd 2` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[5], 2),
                                                                    `Bnd 3` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[2], 2),
                                                                    `Bnd 4` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[6], 2),
                                                                    `Bnd 5` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[3], 2),
                                                                    `Bnd 6` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[7], 2),
                                                                    `Bnd 7` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[4], 2),
                                                                    `Bnd 8` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[8], 2))
    }

    distribution_values$data_5000[[custom_target]] <-
      if(nrow(data) > 5000) {
        sample_n(data, 5000) %>% mutate(y = 0 + runif(5000, -.5, .5))
      }
    else if(nrow(data) < 5000) {
      data %>% mutate(y = 0 + runif(nrow(data), -.5, .5))
    }
    else {
      data %>% mutate(y = 0 + runif(5000, -.5, .5))
    }

    distribution_values$data_density[[custom_target]] <-
      if(nrow(data) > 100000) {
        sample_n(data, 100000)
      }
    else {
      data
    }

    #code_text_value(glue::glue("This is a custom distribution"))

  }

  ##### observe event for input$upload_custom_proper #####
  observeEvent(input$upload_custom_proper, {

    # check if it's a csv, if not, then show a modal message that it needs to be a csv
    if (tools::file_ext(input$upload_custom_proper$name) != "csv") {
      showModal(modalDialog(
        title = "Invalid file type",
        "That doesn't appear to be a .csv file. Please only upload .csv file types.",
        easyClose = TRUE
      ))
    }

    # if it is a csv then...
    else {

      # read in the data as a tibble
      my_custom_data <-
        as_tibble(read_csv(input$upload_custom_proper$datapath)) %>%
        select(where(is.numeric))

      if (ncol(my_custom_data) == 0) {
        showModal(modalDialog(
          title = "Invalid columns in your csv",
          "It looks like you've uploaded a csv, but we haven't detected purely numeric columns in it. Make sure that all the columns are numeric - this includes not having missing values written as NA, for example.",
          easyClose = TRUE
        ))
        return()
      }

      # find out how many custom distributions there are currently
      current_custom <-
        if(length(distribution_values$distribution_list) == 1) {
          0
        }
      # stop if you've gotten to this point and there are 15
      else if(length(distribution_values$distribution_list$Custom) == 15) {
        showModal(modalDialog(
          title = "Max distributions reached",
          "You already have 15 custom distributions - you'll have to remove some in order to add other ones.",
          easyClose = TRUE
        ))
        return()
      }
        else {
          length(distribution_values$distribution_list$Custom)
        }

      # the most columns we can take from the new data is 15 - how many we already have
        max_take <- 15 - current_custom

        # how many new columns are available to take in the uploaded data?
        new_max <- ncol(my_custom_data)

        # assess whether we need to reduce the amount of data we're going to bring in
        my_custom_data <-
          # if it's less columns than we can add, we just take the whole lot
          if(new_max <= max_take) {
            my_custom_data
          }
        # otherwise we have to reduce the number of columns we're going to take
        else {
          my_custom_data[ , 1:max_take]
        }

        # now we want to update the names of the distributions
        if(current_custom == 0) {
          # we just stick all the names in if there aren't any custom distributions yet
          distribution_values$distribution_list$Custom <- names(my_custom_data)
        }
        else {

          # otherwise, we check if any of the names are replicated and append '_uniq' to them if so,
          # to make them unique
          if(sum(names(my_custom_data) %in% distribution_values$distribution_list$Custom) >= 1) {
            target_mod <- which(names(my_custom_data) %in%  distribution_values$distribution_list$Custom)
            names(my_custom_data)[target_mod] <- paste0(names(my_custom_data)[target_mod], "_uniq")
          }

          # then we update the names
          distribution_values$distribution_list$Custom <- c(distribution_values$distribution_list$Custom, names(my_custom_data))

        }

        # now we want to store the actual data somewhere useful:
        starting_point <- current_custom + 1

        import_custom_data <- function(import_column, custom_target) {
          distribution_values$custom_data[[custom_target]] <- my_custom_data[, import_column] %>% rename(x = 1)
        }

        map2(.x = 1:ncol(my_custom_data),
             .y = starting_point:(starting_point + ncol(my_custom_data) - 1),
             .f = import_custom_data)

        map(.x = starting_point:(starting_point + ncol(my_custom_data) - 1),
            .f = update_custom_summary)

      removeModal()

    }

  })

  observeEvent(input$distribution_choice, {
    should_render_ui(TRUE)
  })

  #### Download section ####
  ##### download reactive values #####
  download_info <- reactiveValues(
    column_count = 0,
    download_button_status = TRUE,
    clear_sample_button_status = TRUE,
    store_sample_button_status = FALSE,
    my_stored_data = "Empty",
    column_select_choices = "New column",
    custom_column_name = "Default"
  )

  new_col_name <- function() {

    my_text <-
      if(input$distribution_choice == "Beta") {
        glue::glue("rprop(n, {input$beta_precision}, {input$beta_mean})")
      }
    else if(input$distribution_choice == "Normal") {
      glue::glue("rnorm(n, {input$normal_mean}, {input$normal_sd})")
    }
    else if(input$distribution_choice == "Skew normal") {
      glue::glue("rsn(n, {input$skewnormal_location}, {input$skewnormal_scale}, {input$skewnormal_slant})")
    }
    else if(input$distribution_choice == "Beta as %") {
      glue::glue("perc_rprop(n, {input$betapercent_precision}, {input$betapercent_mean})")
    }
    else if(input$distribution_choice == "Exponential") {
      glue::glue("rexp(n, {input$exp_rate})")
    }
    else if(input$distribution_choice == "Log-normal") {
      glue::glue("rlnorm(n, {input$lognormal_mean}, {input$lognormal_sd})")
    }
    else if(input$distribution_choice == "Student's t") {
      glue::glue("rst(n, {input$t_df}, {input$t_location}, {input$t_scale})")
    }
    else if(input$distribution_choice == "Uniform") {
      glue::glue("runif(n, {input$uniform_min}, {input$uniform_max})")
    }

    return(my_text)
  }

  ##### Event handling for download buttons #####
  clean_column_name <- function(name) {
    name %>%
      iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%  # Remove accents and diacritics
      gsub("[ -]", "_", .) %>%  # Replace spaces and hyphens with underscores
      gsub("[^a-zA-Z0-9_.]", "", .) %>%  # Remove characters that are not letters, numbers, dots or underscores
      gsub("__+", "_", .) %>%  # Replace consecutive underscores with a single underscore
      gsub("\\.\\.+",".", .) %>%  # Replace consecutive dots with a single dot
      gsub("^[^a-zA-Z]+", "", .)  %>% # Remove leading numbers and special characters
      gsub("^_+", "", .) %>% # Remove leading underscores
      tolower()  # Convert to lowercase
  }

  # make sure we don't store a custom distribution in the csv again
  observeEvent(input$distribution_choice, {

    if(!input$distribution_choice %in% c("Beta", "Beta as %", "Normal", "Skew normal", "Exponential", "Log-normal", "Student's t", "Uniform")) {
      download_info$store_sample_button_status = TRUE
    }
    else if(download_info$column_count < 10) {
      download_info$store_sample_button_status = FALSE
    }

  })

  observeEvent(input$store_sample, {

    text_version_data <-
      if(input$distribution_choice == "Beta") {
        "beta"
      }
    else if(input$distribution_choice == "Normal") {
      "normal"
    }
    else if(input$distribution_choice == "Skew normal") {
      "skewnormal"
    }
    else if(input$distribution_choice == "Beta as %") {
      "betapercent"
    }
    else if(input$distribution_choice == "Exponential") {
      "exp"
    }
    else if(input$distribution_choice == "Log-normal") {
      "lognormal"
    }
    else if(input$distribution_choice == "Student's t") {
      "t"
    }
    else if(input$distribution_choice == "Uniform") {
      "uniform"
    }

    if(input$name_sample %in% c("Default", "New column") | is.na(input$name_sample) | is.null(input$name_sample)) {

      new_column_name <- new_col_name()

      if(input$select_column == "New column") {

        if(new_column_name %in% names(download_info$my_stored_data)) {
          new_column_name <- glue::glue("{new_column_name}_{paste0(sample(letters, 4, replace = TRUE), collapse = '')}")
        }

        if(download_info$column_count == 0) {

          download_info$my_stored_data <-
            tibble(!!new_column_name := eval(parse(text = glue::glue("{text_version_data}_data$data$x[1:10000]"))))
          download_info$column_count <-
            ncol(download_info$my_stored_data)

          download_info$column_select_choices <-
            c(names(download_info$my_stored_data), "New column")
        }
        else if(download_info$column_count > 0 & download_info$column_count < 9) {

          download_info$my_stored_data <-
            download_info$my_stored_data %>%
            mutate(!!new_column_name := eval(parse(text = glue::glue("{text_version_data}_data$data$x[1:10000]"))))
          download_info$column_count <-
            ncol(download_info$my_stored_data)

          download_info$column_select_choices <-
            c(names(download_info$my_stored_data), "New column")
        }
        else {

          download_info$my_stored_data <-
            download_info$my_stored_data %>%
            mutate(!!new_column_name := eval(parse(text = glue::glue("{text_version_data}_data$data$x[1:10000]"))))
          download_info$column_count <-
            ncol(download_info$my_stored_data)

          download_info$column_select_choices <-
            names(download_info$my_stored_data)
        }
      }
      else {

        current_names <- names(download_info$my_stored_data)[-which(names(download_info$my_stored_data) == input$select_column)]

        if(new_column_name %in% current_names) {
          new_column_name <- glue::glue("{new_column_name}_{paste0(sample(letters, 4, replace = TRUE), collapse = '')}")
        }

        download_info$my_stored_data <-
          download_info$my_stored_data %>%
          mutate(!!input$select_column := eval(parse(text = glue::glue("{text_version_data}_data$data$x[1:10000]")))) %>%
          rename(!!new_column_name := !!input$select_column)

        if(download_info$column_count == 10) {
          download_info$column_select_choices <-
            names(download_info$my_stored_data)
        }
        else {
          download_info$column_select_choices <-
            c(names(download_info$my_stored_data), "New column")
        }

      }

      if(download_info$column_count > 0) {
        download_info$download_button_status <- FALSE
      }

      if(download_info$column_count == 10) {
        #download_info$store_sample_button_status <- TRUE
        download_info$clear_sample_button_status <- FALSE
      }

    }

    else {

      custom_new_name <- clean_column_name(input$name_sample)

      if(input$select_column == "New column") {

        if(custom_new_name %in% names(download_info$my_stored_data)) {
          custom_new_name <- glue::glue("{custom_new_name}_{paste0(sample(letters, 4, replace = TRUE), collapse = '')}")
        }

        if(download_info$column_count == 0) {

          download_info$my_stored_data <-
            tibble(!!custom_new_name := eval(parse(text = glue::glue("{text_version_data}_data$data$x[1:10000]"))))
          download_info$column_count <-
            ncol(download_info$my_stored_data)

          download_info$column_select_choices <-
            c(names(download_info$my_stored_data), "New column")
          download_info$custom_column_name <- "Default"
          updateTextInput(session, "name_sample", value = "Default")
        }
        else if(download_info$column_count > 0 & download_info$column_count < 9) {

          download_info$my_stored_data <-
            download_info$my_stored_data %>%
            mutate(!!custom_new_name := eval(parse(text = glue::glue("{text_version_data}_data$data$x[1:10000]"))))
          download_info$column_count <-
            ncol(download_info$my_stored_data)

          download_info$column_select_choices <-
            c(names(download_info$my_stored_data), "New column")
          download_info$custom_column_name <- "Default"
          updateTextInput(session, "name_sample", value = "Default")
        }
        else {

          download_info$my_stored_data <-
            download_info$my_stored_data %>%
            mutate(!!custom_new_name := eval(parse(text = glue::glue("{text_version_data}_data$data$x[1:10000]"))))
          download_info$column_count <-
            ncol(download_info$my_stored_data)

          download_info$column_select_choices <-
            names(download_info$my_stored_data)
          download_info$custom_column_name <- "Default"
          updateTextInput(session, "name_sample", value = "Default")
        }
      }
      else {

        current_names <- names(download_info$my_stored_data)[-which(names(download_info$my_stored_data) == input$select_column)]

        if(custom_new_name %in% current_names) {
          custom_new_name <- glue::glue("{custom_new_name}_{paste0(sample(letters, 4, replace = TRUE), collapse = '')}")
        }

        download_info$my_stored_data <-
          download_info$my_stored_data %>%
          mutate(!!input$select_column := eval(parse(text = glue::glue("{text_version_data}_data$data$x[1:10000]")))) %>%
          rename(!!custom_new_name := !!input$select_column)

        if(download_info$column_count == 10) {
          download_info$column_select_choices <-
            names(download_info$my_stored_data)
        }
        else {
          download_info$column_select_choices <-
            c(names(download_info$my_stored_data), "New column")
        }

        download_info$custom_column_name <- "Default"
        updateTextInput(session, "name_sample", value = "Default")
      }

      if(download_info$column_count > 0) {
        download_info$download_button_status <- FALSE
      }

      if(download_info$column_count == 10) {
        #download_info$store_sample_button_status <- TRUE
        download_info$clear_sample_button_status <- FALSE
      }

    }

  })

  observeEvent(input$select_column, {

    if(input$select_column == "New column") {
      download_info$clear_sample_button_status <- TRUE
    }
    else if(input$select_column != "New column" & download_info$column_count > 1) {
      download_info$clear_sample_button_status <- FALSE
    }

  })

  observeEvent(input$clear_sample, {

    if(input$select_column != "New column") {
      target_column <- which(names(download_info$my_stored_data) == input$select_column)
      download_info$my_stored_data <-
        download_info$my_stored_data[,-target_column]
      download_info$column_count <-
        ncol(download_info$my_stored_data)
      download_info$column_select_choices <-
        c(names(download_info$my_stored_data), "New column")
      download_info$store_sample_button_status <- FALSE
    }

    if(download_info$column_count <= 1) {
      download_info$clear_sample_button_status <- TRUE
    }

    if(download_info$column_count > 0) {
      download_info$download_button_status <- FALSE
    }
    else {
      download_info$download_button_status <- TRUE
    }

  })

  ##### select_column download #####
  output$select_column_ui <-
    renderUI({

      selectInput("select_column",
                  "Store in/clear column...:",
                  choices = download_info$column_select_choices,
                  selected = download_info$column_select_choices[length(download_info$column_select_choices)])

    })

  ##### name_sample download #####
  output$name_sample_ui <-
    renderUI({

      textInput("name_sample",
                "Name column:",
                value = download_info$custom_column_name)

    })

  ##### store_sample download #####
  output$store_sample_ui <-
    renderUI({

      if(download_info$store_sample_button_status == TRUE) {
        disabled(actionButton("store_sample",
                              "Store sample in selected column",
                              style="color: #2f6e8d; background-color: #ffffff; border-color: #2f6e8d"))
      }
      else if(download_info$store_sample_button_status == FALSE) {
        actionButton("store_sample",
                     "Store sample in selected column",
                     style="color: #2f6e8d; background-color: #ffffff; border-color: #2f6e8d")
      }

    })

  output$clear_sample_ui <-
    renderUI({

      if(download_info$clear_sample_button_status == TRUE) {
        disabled(actionButton("clear_sample",
                              "Clear selected column",
                              style="color: #ffffff; background-color: #ff8f6f; border-color: #ff8f6f"))
      }
      else if(download_info$clear_sample_button_status == FALSE) {
        actionButton("clear_sample",
                     "Clear selected column",
                     style="color: #ffffff; background-color: #ff8f6f; border-color: #ff8f6f")
      }

    })

  ##### column_count_ui download #####
  output$column_count_ui <-
    renderUI({

      p(glue::glue("Columns in current csv: {download_info$column_count}"))

    })

  ##### download_button_ui download #####
  output$download_button_ui <-
    renderUI({

      if(download_info$download_button_status == TRUE) {
        disabled(actionButton("download_button",
                              "Download csv",
                              style="color: #ffffff; background-color: #2f6e8d; border-color: #2f6e8d"))
      }
      else if(download_info$download_button_status == FALSE) {
        actionButton("download_button",
                     "Download csv",
                     style="color: #ffffff; background-color: #2f6e8d; border-color: #2f6e8d")
      }

    })

  observeEvent(input$download_button, {
    showModal(modalDialog(
      easyClose = TRUE,
      title = "Download your csv...",
      fluidRow(column(12,
                      align = "center",
                      textInput("download_name",
                                "Name your csv file:"),
                      downloadButton("final_download_button",
                                     "Download csv",
                                     style="color: #ffffff; background-color: #2f6e8d; border-color: #2f6e8d"))),
      footer = tagList(
        modalButton("Cancel")
      )
    ))
  })


  output$final_download_button <- downloadHandler(
    # Provide a filename for the download (can be a reactive expression)

    filename = function() {
      if(is.na(input$download_name) | is.null(input$download_name) | input$download_name == "") {
        paste("distributr_data_", Sys.Date(), ".csv", sep = "")
      }
      else {

        clean_file_name <- function(name) {
          name <- sub("\\.csv$", "", name, ignore.case = TRUE)
          name %>%
            gsub("[\\/\\:*?\"<>|]", "_", .) %>%
            gsub("^[[:space:]]+|[[:space:]]+$", "", .) %>%
            tolower()
        }

        paste(clean_file_name(input$download_name), ".csv", sep = "")
      }

    },

    # The content function writes the content of the file
    content = function(file) {
      write.csv(download_info$my_stored_data, file, row.names = FALSE)
      removeModal()
    },

    # The default content type is 'text/csv'
    contentType = "text/csv"
  )

  #### Starting parameter reactive values for distributions ####
  normal_parameters <- reactiveValues(
    mean = 20,
    sd = 4
  )

  skewnormal_parameters <- reactiveValues(
    location = 20,
    scale = 4,
    slant = 0
  )

  lognormal_parameters <- reactiveValues(
    logmean = 0,
    logsd = .25
  )

  beta_parameters <- reactiveValues(
    mean = .5,
    precision = 10
  )

  betapercent_parameters <- reactiveValues(
    mean = 50,
    precision = 10
  )

  exp_parameters <- reactiveValues(
    rate = 1
  )

  t_parameters <- reactiveValues(
    location = 0,
    scale = 1,
    df = 5
  )

  uniform_parameters <- reactiveValues(
    min = 0,
    max = 1
  )

  #### Starting reactive values for distribution data ####
  beta_data <- reactiveValues(
    data = tibble(x = rprop(500000, 10, .5)),
    data_density = tibble(x = rprop(100000, 10, .5)),
    summary = tibble(Mean = "...",
                     Median = "...",
                     Mode = "...",
                     `Lower ETI` = "...",
                     `Upper ETI` = "..."),
    quantiles = tibble(Min = "...",
                       `2.5%` = "...",
                       `5%` = "...",
                       `10%` = "...",
                       `25%` = "...",
                       `50%` = "...",
                       `75%` = "...",
                       `90%` = "...",
                       `95%` = "...",
                       `97.5%` = "...",
                       Max = "..."),
    hdi = tibble(HDI = "95%",
                 `Bound 1` = "...",
                 `Bound 2` = "..."),
    data_5000 = tibble(x = rprop(5000, 10, .5),
                       y = 0 + runif(5000, -.5, .5))
  )

  betapercent_data <- reactiveValues(
    data = tibble(x = rprop(500000, 10, .5) * 100),
    data_density = tibble(x = rprop(100000, 10, .5) * 100),
    summary = tibble(Mean = "...",
                     Median = "...",
                     Mode = "...",
                     `Lower ETI` = "...",
                     `Upper ETI` = "..."),
    quantiles = tibble(Min = "...",
                       `2.5%` = "...",
                       `5%` = "...",
                       `10%` = "...",
                       `25%` = "...",
                       `50%` = "...",
                       `75%` = "...",
                       `90%` = "...",
                       `95%` = "...",
                       `97.5%` = "...",
                       Max = "..."),
    hdi = tibble(HDI = "95%",
                 `Bound 1` = "...",
                 `Bound 2` = "..."),
    data_5000 = tibble(x = rprop(5000, 10, .5) * 100,
                       y = 0 + runif(5000, -.5, .5))
  )

  normal_data <- reactiveValues(
    data = tibble(x = rnorm(500000, 20, 4)),
    data_density = tibble(x = rnorm(100000, 20, 4)),
    summary = tibble(Mean = "...",
                     Median = "...",
                     Mode = "...",
                     `Lower ETI` = "...",
                     `Upper ETI` = "..."),
    quantiles = tibble(Min = "...",
                       `2.5%` = "...",
                       `5%` = "...",
                       `10%` = "...",
                       `25%` = "...",
                       `50%` = "...",
                       `75%` = "...",
                       `90%` = "...",
                       `95%` = "...",
                       `97.5%` = "...",
                       Max = "..."),
    hdi = tibble(HDI = "95%",
                 `Bound 1` = "...",
                 `Bound 2` = "..."),
    data_5000 = tibble(x = rnorm(5000, 20, 4),
                       y = 0 + runif(5000, -.5, .5))
  )

  skewnormal_data <- reactiveValues(
    data = tibble(x = rsn(500000, 20, 4, 0)),
    data_density = tibble(x = rsn(100000, 20, 4, 0)),
    summary = tibble(Mean = "...",
                     Median = "...",
                     Mode = "...",
                     `Lower ETI` = "...",
                     `Upper ETI` = "..."),
    quantiles = tibble(Min = "...",
                       `2.5%` = "...",
                       `5%` = "...",
                       `10%` = "...",
                       `25%` = "...",
                       `50%` = "...",
                       `75%` = "...",
                       `90%` = "...",
                       `95%` = "...",
                       `97.5%` = "...",
                       Max = "..."),
    hdi = tibble(HDI = "95%",
                 `Bound 1` = "...",
                 `Bound 2` = "..."),
    data_5000 = tibble(x = rsn(5000, 20, 4, 0),
                       y = 0 + runif(5000, -.5, .5))
  )

  exp_data <- reactiveValues(
    data = tibble(x = rexp(500000, 1)),
    data_density = tibble(x = rexp(100000, 1)),
    summary = tibble(Mean = "...",
                     Median = "...",
                     Mode = "...",
                     `Lower ETI` = "...",
                     `Upper ETI` = "..."),
    quantiles = tibble(Min = "...",
                       `2.5%` = "...",
                       `5%` = "...",
                       `10%` = "...",
                       `25%` = "...",
                       `50%` = "...",
                       `75%` = "...",
                       `90%` = "...",
                       `95%` = "...",
                       `97.5%` = "...",
                       Max = "..."),
    hdi = tibble(HDI = "95%",
                 `Bound 1` = "...",
                 `Bound 2` = "..."),
    data_5000 = tibble(x = rexp(5000, 1),
                       y = 0 + runif(5000, -.5, .5))
  )

  lognormal_data <- reactiveValues(
    data = tibble(x = rlnorm(500000, 0, .25)),
    data_density = tibble(x = rlnorm(100000, 0, .25)),
    summary = tibble(Mean = "...",
                     Median = "...",
                     Mode = "...",
                     `Lower ETI` = "...",
                     `Upper ETI` = "..."),
    quantiles = tibble(Min = "...",
                       `2.5%` = "...",
                       `5%` = "...",
                       `10%` = "...",
                       `25%` = "...",
                       `50%` = "...",
                       `75%` = "...",
                       `90%` = "...",
                       `95%` = "...",
                       `97.5%` = "...",
                       Max = "..."),
    hdi = tibble(HDI = "95%",
                 `Bound 1` = "...",
                 `Bound 2` = "..."),
    data_5000 = tibble(x = rlnorm(5000, 0, 1),
                       y = 0 + runif(5000, -.5, .5))
  )

  t_data <- reactiveValues(
    data = tibble(x = rst(500000, 5, 0, 1)),
    data_density = tibble(x = rst(100000, 5, 0, 1)),
    summary = tibble(Mean = "...",
                     Median = "...",
                     Mode = "...",
                     `Lower ETI` = "...",
                     `Upper ETI` = "..."),
    quantiles = tibble(Min = "...",
                       `2.5%` = "...",
                       `5%` = "...",
                       `10%` = "...",
                       `25%` = "...",
                       `50%` = "...",
                       `75%` = "...",
                       `90%` = "...",
                       `95%` = "...",
                       `97.5%` = "...",
                       Max = "..."),
    hdi = tibble(HDI = "95%",
                 `Bound 1` = "...",
                 `Bound 2` = "..."),
    data_5000 = tibble(x = rst(5000, 5, 0, 1),
                       y = 0 + runif(5000, -.5, .5))
  )

  uniform_data <- reactiveValues(
    data = tibble(x = runif(500000, 0, 1)),
    data_density = tibble(x = runif(100000, 0, 1)),
    summary = tibble(Mean = "...",
                     Median = "...",
                     Mode = "...",
                     `Lower ETI` = "...",
                     `Upper ETI` = "..."),
    quantiles = tibble(Min = "...",
                       `2.5%` = "...",
                       `5%` = "...",
                       `10%` = "...",
                       `25%` = "...",
                       `50%` = "...",
                       `75%` = "...",
                       `90%` = "...",
                       `95%` = "...",
                       `97.5%` = "...",
                       Max = "..."),
    hdi = tibble(HDI = "Uniform",
                 `Bound 1` = "Uniform",
                 `Bound 2` = "Uniform"),
    data_5000 = tibble(x = runif(5000, 0, 1),
                       y = 0 + runif(5000, -.5, .5))
  )

  #### Observe event for when update is clicked ####
  observeEvent(input$beta_update, {

    if(!input$beta_mean > 0 | !input$beta_mean < 1 | !input$beta_precision > 0 | !is.numeric(input$beta_precision) | !is.numeric(input$beta_mean)) {
      shinyalert(
        title = "Check your input values",
        text = "Your 'mean' must be between 0 and 1, exclusive of 0 or 1 exactly, and 'precision' must be a positive number.",
        size = "s",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = FALSE,
        type = "info",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#CCCCCC",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
    }

    else {
      beta_parameters$mean <- input$beta_mean
      beta_parameters$precision <- input$beta_precision

      updated_data <- tibble(x = rprop(500000, input$beta_precision, input$beta_mean))
      beta_data$data <- updated_data

      # code_text_value(glue::glue("extraDistr::rprop(n, {input$beta_precision}, {input$beta_mean})"))

      beta_data$quantiles <-
        tibble(
          Min = round(min(updated_data$x), 2),
          `2.5%` = round(quantile(updated_data$x, .025), 2),
          `5%` =  round(quantile(updated_data$x, .05), 2),
          `10%` = round(quantile(updated_data$x, .1), 2),
          `25%` = round(quantile(updated_data$x, .25), 2),
          `50%` = round(quantile(updated_data$x, .5), 2),
          `75%` = round(quantile(updated_data$x, .75), 2),
          `90%` = round(quantile(updated_data$x, .9), 2),
          `95%` = round(quantile(updated_data$x, .95), 2),
          `97.5%` = round(quantile(updated_data$x, .975), 2),
          Max = round(max(updated_data$x), 2)
        )

      hdi_length <- length(tidybayes::hdi(updated_data$x, input$summary_range_number / 100))

      if(hdi_length == 2) {
        beta_data$hdi <- tibble(HDI = glue::glue("{input$summary_range_number}%"),
                                `Bound 1` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[1], 2),
                                `Bound 2` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[2], 2))
      }
      else if(hdi_length == 4) {
        beta_data$hdi <- tibble(`Bnd 1` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[1], 2),
                                `Bnd 2` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[3], 2),
                                `Bnd 3` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[2], 2),
                                `Bnd 4` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[4], 2))
      }
      else if(hdi_length == 6) {
        beta_data$hdi <- tibble(`Bnd 1` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[1], 2),
                                `Bnd 2` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[4], 2),
                                `Bnd 3` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[2], 2),
                                `Bnd 4` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[5], 2),
                                `Bnd 5` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[3], 2),
                                `Bnd 6` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[6], 2))
      }
      else if(hdi_length == 8) {
        beta_data$hdi <- tibble(`Bnd 1` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[1], 2),
                                `Bnd 2` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[5], 2),
                                `Bnd 3` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[2], 2),
                                `Bnd 4` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[6], 2),
                                `Bnd 5` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[3], 2),
                                `Bnd 6` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[7], 2),
                                `Bnd 7` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[4], 2),
                                `Bnd 8` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[8], 2))
      }

      beta_data$data_5000 <- sample_n(updated_data, 5000) %>% mutate(y = 0 + runif(5000, -.5, .5))
      beta_data$data_density <- sample_n(updated_data, 100000)
      beta_data$summary <- summarise(updated_data,
                                     Mean = mean(x),
                                     Median = median(x),
                                     Mode = hdp(x),
                                     `Lower ETI` = quantile(x, (1 - (input$summary_range_number / 100)) / 2),
                                     `Upper ETI` = quantile(x, (input$summary_range_number / 100) + (1 - (input$summary_range_number / 100)) / 2))


    }

  })

  observeEvent(input$betapercent_update, {

    if(!input$betapercent_mean > 0 | !input$betapercent_mean < 100 | !input$betapercent_precision > 0 | !is.numeric(input$betapercent_precision) | !is.numeric(input$betapercent_mean)) {
      shinyalert(
        title = "Check your input values",
        text = "Your 'mean' must be between 0 and 100, exclusive of 0 or 100 exactly, and 'precision' must be a positive number.",
        size = "s",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = FALSE,
        type = "info",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#CCCCCC",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
    }

    else {
      betapercent_parameters$mean <- input$betapercent_mean
      betapercent_parameters$precision <- input$betapercent_precision

      updated_data <- tibble(x = rprop(500000, input$betapercent_precision, input$betapercent_mean / 100) * 100)
      betapercent_data$data <- updated_data

      # code_text_value(glue::glue("extraDistr::rprop(1000, {input$betapercent_precision}, {input$betapercent_mean} / 100) * 100"))

      betapercent_data$quantiles <-
        tibble(
          Min = round(min(updated_data$x), 2),
          `2.5%` = round(quantile(updated_data$x, .025), 2),
          `5%` =  round(quantile(updated_data$x, .05), 2),
          `10%` = round(quantile(updated_data$x, .1), 2),
          `25%` = round(quantile(updated_data$x, .25), 2),
          `50%` = round(quantile(updated_data$x, .5), 2),
          `75%` = round(quantile(updated_data$x, .75), 2),
          `90%` = round(quantile(updated_data$x, .9), 2),
          `95%` = round(quantile(updated_data$x, .95), 2),
          `97.5%` = round(quantile(updated_data$x, .975), 2),
          Max = round(max(updated_data$x), 2)
        )

      hdi_length <- length(tidybayes::hdi(updated_data$x, input$summary_range_number / 100))

      if(hdi_length == 2) {
        betapercent_data$hdi <- tibble(HDI = glue::glue("{input$summary_range_number}%"),
                                       `Bound 1` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[1], 2),
                                       `Bound 2` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[2], 2))
      }
      else if(hdi_length == 4) {
        betapercent_data$hdi <- tibble(`Bnd 1` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[1], 2),
                                       `Bnd 2` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[3], 2),
                                       `Bnd 3` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[2], 2),
                                       `Bnd 4` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[4], 2))
      }
      else if(hdi_length == 6) {
        betapercent_data$hdi <- tibble(`Bnd 1` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[1], 2),
                                       `Bnd 2` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[4], 2),
                                       `Bnd 3` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[2], 2),
                                       `Bnd 4` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[5], 2),
                                       `Bnd 5` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[3], 2),
                                       `Bnd 6` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[6], 2))
      }
      else if(hdi_length == 8) {
        betapercent_data$hdi <- tibble(`Bnd 1` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[1], 2),
                                       `Bnd 2` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[5], 2),
                                       `Bnd 3` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[2], 2),
                                       `Bnd 4` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[6], 2),
                                       `Bnd 5` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[3], 2),
                                       `Bnd 6` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[7], 2),
                                       `Bnd 7` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[4], 2),
                                       `Bnd 8` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[8], 2))
      }

      betapercent_data$data_5000 <- sample_n(updated_data, 5000) %>% mutate(y = 0 + runif(5000, -.5, .5))
      betapercent_data$data_density <- sample_n(updated_data, 100000)
      betapercent_data$summary <- summarise(updated_data,
                                            Mean = mean(x),
                                            Median = median(x),
                                            Mode = hdp(x),
                                            `Lower ETI` = quantile(x, (1 - (input$summary_range_number / 100)) / 2),
                                            `Upper ETI` = quantile(x, (input$summary_range_number / 100) + (1 - (input$summary_range_number / 100)) / 2))
    }

  })

  observeEvent(input$normal_update, {

    if(!input$normal_sd > 0 | !is.numeric(input$normal_sd) | !is.numeric(input$normal_mean)) {
      shinyalert(
        title = "Check your input values",
        text = "Your 'standard deviation' value must be a positive number.",
        size = "s",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = FALSE,
        type = "info",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#CCCCCC",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
    }

    else {
      normal_parameters$mean <- input$normal_mean
      normal_parameters$sd <- input$normal_sd

      updated_data <- tibble(x = rnorm(500000, input$normal_mean, input$normal_sd))
      normal_data$data <- updated_data

      # code_text_value(glue::glue("rnorm(1000, {input$normal_mean}, {input$normal_sd})"))

      normal_data$quantiles <-
        tibble(
          Min = round(min(updated_data$x), 2),
          `2.5%` = round(quantile(updated_data$x, .025), 2),
          `5%` =  round(quantile(updated_data$x, .05), 2),
          `10%` = round(quantile(updated_data$x, .1), 2),
          `25%` = round(quantile(updated_data$x, .25), 2),
          `50%` = round(quantile(updated_data$x, .5), 2),
          `75%` = round(quantile(updated_data$x, .75), 2),
          `90%` = round(quantile(updated_data$x, .9), 2),
          `95%` = round(quantile(updated_data$x, .95), 2),
          `97.5%` = round(quantile(updated_data$x, .975), 2),
          Max = round(max(updated_data$x), 2)
        )

      hdi_length <- length(tidybayes::hdi(updated_data$x, input$summary_range_number / 100))

      if(hdi_length == 2) {
        normal_data$hdi <- tibble(HDI = glue::glue("{input$summary_range_number}%"),
                                  `Bound 1` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[1], 2),
                                  `Bound 2` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[2], 2))
      }
      else if(hdi_length == 4) {
        normal_data$hdi <- tibble(`Bnd 1` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[1], 2),
                                  `Bnd 2` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[3], 2),
                                  `Bnd 3` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[2], 2),
                                  `Bnd 4` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[4], 2))
      }
      else if(hdi_length == 6) {
        normal_data$hdi <- tibble(`Bnd 1` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[1], 2),
                                  `Bnd 2` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[4], 2),
                                  `Bnd 3` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[2], 2),
                                  `Bnd 4` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[5], 2),
                                  `Bnd 5` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[3], 2),
                                  `Bnd 6` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[6], 2))
      }
      else if(hdi_length == 8) {
        normal_data$hdi <- tibble(`Bnd 1` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[1], 2),
                                  `Bnd 2` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[5], 2),
                                  `Bnd 3` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[2], 2),
                                  `Bnd 4` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[6], 2),
                                  `Bnd 5` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[3], 2),
                                  `Bnd 6` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[7], 2),
                                  `Bnd 7` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[4], 2),
                                  `Bnd 8` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[8], 2))
      }

      normal_data$data_5000 <- sample_n(updated_data, 5000) %>% mutate(y = 0 + runif(5000, -.5, .5))
      normal_data$data_density <- sample_n(updated_data, 100000)
      normal_data$summary <- summarise(updated_data,
                                       Mean = mean(x),
                                       Median = median(x),
                                       Mode = hdp(x),
                                       `Lower ETI` = quantile(x, (1 - (input$summary_range_number / 100)) / 2),
                                       `Upper ETI` = quantile(x, (input$summary_range_number / 100) + (1 - (input$summary_range_number / 100)) / 2))
    }

  })

  observeEvent(input$skewnormal_update, {

    if(!input$skewnormal_scale > 0 | !is.numeric(input$skewnormal_scale) | !is.numeric(input$skewnormal_location) | !is.numeric(input$skewnormal_slant)) {
      shinyalert(
        title = "Check your input values",
        text = "Your 'scale' value must be a positive number.",
        size = "s",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = FALSE,
        type = "info",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#CCCCCC",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
    }

    else {
      skewnormal_parameters$location <- input$skewnormal_location
      skewnormal_parameters$scale <- input$skewnormal_scale
      skewnormal_parameters$slant <- input$skewnormal_slant

      updated_data <- tibble(x = rsn(500000, input$skewnormal_location, input$skewnormal_scale, input$skewnormal_slant))
      skewnormal_data$data <- updated_data

      # code_text_value(glue::glue("sn::rsn(1000, {input$skewnormal_location}, {input$skewnormal_scale}, {input$skewnormal_slant})"))

      skewnormal_data$quantiles <-
        tibble(
          Min = round(min(updated_data$x), 2),
          `2.5%` = round(quantile(updated_data$x, .025), 2),
          `5%` =  round(quantile(updated_data$x, .05), 2),
          `10%` = round(quantile(updated_data$x, .1), 2),
          `25%` = round(quantile(updated_data$x, .25), 2),
          `50%` = round(quantile(updated_data$x, .5), 2),
          `75%` = round(quantile(updated_data$x, .75), 2),
          `90%` = round(quantile(updated_data$x, .9), 2),
          `95%` = round(quantile(updated_data$x, .95), 2),
          `97.5%` = round(quantile(updated_data$x, .975), 2),
          Max = round(max(updated_data$x), 2)
        )

      hdi_length <- length(tidybayes::hdi(updated_data$x, input$summary_range_number / 100))

      if(hdi_length == 2) {
        skewnormal_data$hdi <- tibble(HDI = glue::glue("{input$summary_range_number}%"),
                                      `Bound 1` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[1], 2),
                                      `Bound 2` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[2], 2))
      }
      else if(hdi_length == 4) {
        skewnormal_data$hdi <- tibble(`Bnd 1` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[1], 2),
                                      `Bnd 2` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[3], 2),
                                      `Bnd 3` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[2], 2),
                                      `Bnd 4` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[4], 2))
      }
      else if(hdi_length == 6) {
        skewnormal_data$hdi <- tibble(`Bnd 1` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[1], 2),
                                      `Bnd 2` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[4], 2),
                                      `Bnd 3` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[2], 2),
                                      `Bnd 4` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[5], 2),
                                      `Bnd 5` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[3], 2),
                                      `Bnd 6` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[6], 2))
      }
      else if(hdi_length == 8) {
        skewnormal_data$hdi <- tibble(`Bnd 1` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[1], 2),
                                      `Bnd 2` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[5], 2),
                                      `Bnd 3` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[2], 2),
                                      `Bnd 4` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[6], 2),
                                      `Bnd 5` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[3], 2),
                                      `Bnd 6` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[7], 2),
                                      `Bnd 7` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[4], 2),
                                      `Bnd 8` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[8], 2))
      }

      skewnormal_data$data_5000 <- sample_n(updated_data, 5000) %>% mutate(y = 0 + runif(5000, -.5, .5))
      skewnormal_data$data_density <- sample_n(updated_data, 100000)
      skewnormal_data$summary <- summarise(updated_data,
                                           Mean = mean(x),
                                           Median = median(x),
                                           Mode = hdp(x),
                                           `Lower ETI` = quantile(x, (1 - (input$summary_range_number / 100)) / 2),
                                           `Upper ETI` = quantile(x, (input$summary_range_number / 100) + (1 - (input$summary_range_number / 100)) / 2))
    }

  })

  observeEvent(input$exp_update, {

    if(!input$exp_rate > 0 | !is.numeric(input$exp_rate)) {
      shinyalert(
        title = "Check your input values",
        text = "Your 'rate' must be a positive number.",
        size = "s",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = FALSE,
        type = "info",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#CCCCCC",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
    }

    else {
      exp_parameters$rate <- input$exp_rate

      updated_data <- tibble(x = rexp(500000, input$exp_rate))
      exp_data$data <- updated_data

      # code_text_value(glue::glue("rexp(1000, {input$exp_rate})"))

      exp_data$quantiles <-
        tibble(
          Min = round(min(updated_data$x), 2),
          `2.5%` = round(quantile(updated_data$x, .025), 2),
          `5%` =  round(quantile(updated_data$x, .05), 2),
          `10%` = round(quantile(updated_data$x, .1), 2),
          `25%` = round(quantile(updated_data$x, .25), 2),
          `50%` = round(quantile(updated_data$x, .5), 2),
          `75%` = round(quantile(updated_data$x, .75), 2),
          `90%` = round(quantile(updated_data$x, .9), 2),
          `95%` = round(quantile(updated_data$x, .95), 2),
          `97.5%` = round(quantile(updated_data$x, .975), 2),
          Max = round(max(updated_data$x), 2)
        )

      hdi_length <- length(tidybayes::hdi(updated_data$x, input$summary_range_number / 100))

      if(hdi_length == 2) {
        exp_data$hdi <- tibble(HDI = glue::glue("{input$summary_range_number}%"),
                               `Bound 1` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[1], 2),
                               `Bound 2` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[2], 2))
      }
      else if(hdi_length == 4) {
        exp_data$hdi <- tibble(`Bnd 1` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[1], 2),
                               `Bnd 2` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[3], 2),
                               `Bnd 3` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[2], 2),
                               `Bnd 4` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[4], 2))
      }
      else if(hdi_length == 6) {
        exp_data$hdi <- tibble(`Bnd 1` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[1], 2),
                               `Bnd 2` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[4], 2),
                               `Bnd 3` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[2], 2),
                               `Bnd 4` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[5], 2),
                               `Bnd 5` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[3], 2),
                               `Bnd 6` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[6], 2))
      }
      else if(hdi_length == 8) {
        exp_data$hdi <- tibble(`Bnd 1` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[1], 2),
                               `Bnd 2` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[5], 2),
                               `Bnd 3` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[2], 2),
                               `Bnd 4` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[6], 2),
                               `Bnd 5` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[3], 2),
                               `Bnd 6` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[7], 2),
                               `Bnd 7` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[4], 2),
                               `Bnd 8` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[8], 2))
      }

      exp_data$data_5000 <- sample_n(updated_data, 5000) %>% mutate(y = 0 + runif(5000, -.5, .5))
      exp_data$data_density <- sample_n(updated_data, 100000)
      exp_data$summary <- summarise(updated_data,
                                    Mean = mean(x),
                                    Median = median(x),
                                    Mode = hdp(x),
                                    `Lower ETI` = quantile(x, (1 - (input$summary_range_number / 100)) / 2),
                                    `Upper ETI` = quantile(x, (input$summary_range_number / 100) + (1 - (input$summary_range_number / 100)) / 2))
    }


  })

  observeEvent(input$lognormal_update, {

    if(!input$lognormal_sd > 0 | !is.numeric(input$lognormal_sd) | !is.numeric(input$lognormal_mean)) {
      shinyalert(
        title = "Check your input values",
        text = "Your 'log SD' value must be a positive number.",
        size = "s",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = FALSE,
        type = "info",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#CCCCCC",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
    }

    else {
      lognormal_parameters$logmean <- input$lognormal_mean
      lognormal_parameters$logsd <- input$lognormal_sd

      updated_data <- tibble(x = rlnorm(500000, input$lognormal_mean, input$lognormal_sd))
      lognormal_data$data <- updated_data

      # code_text_value(glue::glue("rlnorm(1000, {input$lognormal_mean}, {input$lognormal_sd})"))

      lognormal_data$quantiles <-
        tibble(
          Min = round(min(updated_data$x), 2),
          `2.5%` = round(quantile(updated_data$x, .025), 2),
          `5%` =  round(quantile(updated_data$x, .05), 2),
          `10%` = round(quantile(updated_data$x, .1), 2),
          `25%` = round(quantile(updated_data$x, .25), 2),
          `50%` = round(quantile(updated_data$x, .5), 2),
          `75%` = round(quantile(updated_data$x, .75), 2),
          `90%` = round(quantile(updated_data$x, .9), 2),
          `95%` = round(quantile(updated_data$x, .95), 2),
          `97.5%` = round(quantile(updated_data$x, .975), 2),
          Max = round(max(updated_data$x), 2)
        )

      hdi_length <- length(tidybayes::hdi(updated_data$x, input$summary_range_number / 100))

      if(hdi_length == 2) {
        lognormal_data$hdi <- tibble(HDI = glue::glue("{input$summary_range_number}%"),
                                     `Bound 1` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[1], 2),
                                     `Bound 2` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[2], 2))
      }
      else if(hdi_length == 4) {
        lognormal_data$hdi <- tibble(`Bnd 1` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[1], 2),
                                     `Bnd 2` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[3], 2),
                                     `Bnd 3` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[2], 2),
                                     `Bnd 4` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[4], 2))
      }
      else if(hdi_length == 6) {
        lognormal_data$hdi <- tibble(`Bnd 1` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[1], 2),
                                     `Bnd 2` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[4], 2),
                                     `Bnd 3` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[2], 2),
                                     `Bnd 4` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[5], 2),
                                     `Bnd 5` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[3], 2),
                                     `Bnd 6` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[6], 2))
      }
      else if(hdi_length == 8) {
        lognormal_data$hdi <- tibble(`Bnd 1` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[1], 2),
                                     `Bnd 2` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[5], 2),
                                     `Bnd 3` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[2], 2),
                                     `Bnd 4` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[6], 2),
                                     `Bnd 5` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[3], 2),
                                     `Bnd 6` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[7], 2),
                                     `Bnd 7` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[4], 2),
                                     `Bnd 8` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[8], 2))
      }

      lognormal_data$data_5000 <- sample_n(updated_data, 5000) %>% mutate(y = 0 + runif(5000, -.5, .5))
      lognormal_data$data_density <- sample_n(updated_data, 100000)
      lognormal_data$summary <- summarise(updated_data,
                                          Mean = mean(x),
                                          Median = median(x),
                                          Mode = hdp(x),
                                          `Lower ETI` = quantile(x, (1 - (input$summary_range_number / 100)) / 2),
                                          `Upper ETI` = quantile(x, (input$summary_range_number / 100) + (1 - (input$summary_range_number / 100)) / 2))
    }

  })

  observeEvent(input$t_update, {

    if(!input$t_scale > 0 | !is.numeric(input$t_scale) | !is.numeric(input$t_location) | !is.numeric(input$t_df) | !input$t_df > 0) {
      shinyalert(
        title = "Check your input values",
        text = "Your 'scale' and 'degrees of freedom' must be positive numbers.",
        size = "s",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = FALSE,
        type = "info",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#CCCCCC",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
    }

    else {
      t_parameters$location <- input$t_location
      t_parameters$scale <- input$t_scale
      t_parameters$df <- input$t_df

      updated_data <- tibble(x = rst(500000, input$t_df, input$t_location, input$t_scale))
      t_data$data <- updated_data

      # code_text_value(glue::glue("stevemisc::rst(1000, {input$t_df}, {input$t_location}, {input$t_scale})"))

      t_data$quantiles <-
        tibble(
          Min = round(min(updated_data$x), 2),
          `2.5%` = round(quantile(updated_data$x, .025), 2),
          `5%` =  round(quantile(updated_data$x, .05), 2),
          `10%` = round(quantile(updated_data$x, .1), 2),
          `25%` = round(quantile(updated_data$x, .25), 2),
          `50%` = round(quantile(updated_data$x, .5), 2),
          `75%` = round(quantile(updated_data$x, .75), 2),
          `90%` = round(quantile(updated_data$x, .9), 2),
          `95%` = round(quantile(updated_data$x, .95), 2),
          `97.5%` = round(quantile(updated_data$x, .975), 2),
          Max = round(max(updated_data$x), 2)
        )

      hdi_length <- length(tidybayes::hdi(updated_data$x, input$summary_range_number / 100))

      if(hdi_length == 2) {
        t_data$hdi <- tibble(HDI = glue::glue("{input$summary_range_number}%"),
                             `Bound 1` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[1], 2),
                             `Bound 2` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[2], 2))
      }
      else if(hdi_length == 4) {
        t_data$hdi <- tibble(`Bnd 1` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[1], 2),
                             `Bnd 2` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[3], 2),
                             `Bnd 3` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[2], 2),
                             `Bnd 4` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[4], 2))
      }
      else if(hdi_length == 6) {
        t_data$hdi <- tibble(`Bnd 1` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[1], 2),
                             `Bnd 2` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[4], 2),
                             `Bnd 3` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[2], 2),
                             `Bnd 4` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[5], 2),
                             `Bnd 5` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[3], 2),
                             `Bnd 6` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[6], 2))
      }
      else if(hdi_length == 8) {
        t_data$hdi <- tibble(`Bnd 1` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[1], 2),
                             `Bnd 2` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[5], 2),
                             `Bnd 3` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[2], 2),
                             `Bnd 4` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[6], 2),
                             `Bnd 5` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[3], 2),
                             `Bnd 6` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[7], 2),
                             `Bnd 7` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[4], 2),
                             `Bnd 8` = round(tidybayes::hdi(updated_data$x, input$summary_range_number / 100)[8], 2))
      }

      t_data$data_5000 <- sample_n(updated_data, 5000) %>% mutate(y = 0 + runif(5000, -.5, .5))
      t_data$data_density <- sample_n(updated_data, 100000)
      t_data$summary <- summarise(updated_data,
                                  Mean = mean(x),
                                  Median = median(x),
                                  Mode = hdp(x),
                                  `Lower ETI` = quantile(x, (1 - (input$summary_range_number / 100)) / 2),
                                  `Upper ETI` = quantile(x, (input$summary_range_number / 100) + (1 - (input$summary_range_number / 100)) / 2))
    }

  })

  observeEvent(input$uniform_update, {

    if(!is.numeric(input$uniform_min) | !is.numeric(input$uniform_max) | !input$uniform_min < input$uniform_max) {
      shinyalert(
        title = "Check your input values",
        text = "Your minimum must be lower than your maximum.",
        size = "s",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = FALSE,
        type = "info",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#CCCCCC",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
    }

    else {
      uniform_parameters$min <- input$uniform_min
      uniform_parameters$max <- input$uniform_max

      updated_data <- tibble(x = runif(500000, input$uniform_min, input$uniform_max))
      uniform_data$data <- updated_data

      # code_text_value(glue::glue("runif(1000, {input$uniform_min}, {input$uniform_max})"))

      uniform_data$quantiles <-
        tibble(
          Min = round(min(updated_data$x), 2),
          `2.5%` = round(quantile(updated_data$x, .025), 2),
          `5%` =  round(quantile(updated_data$x, .05), 2),
          `10%` = round(quantile(updated_data$x, .1), 2),
          `25%` = round(quantile(updated_data$x, .25), 2),
          `50%` = round(quantile(updated_data$x, .5), 2),
          `75%` = round(quantile(updated_data$x, .75), 2),
          `90%` = round(quantile(updated_data$x, .9), 2),
          `95%` = round(quantile(updated_data$x, .95), 2),
          `97.5%` = round(quantile(updated_data$x, .975), 2),
          Max = round(max(updated_data$x), 2)
        )

      uniform_data$data_5000 <- sample_n(updated_data, 5000) %>% mutate(y = 0 + runif(5000, -.5, .5))
      uniform_data$data_density <- sample_n(updated_data, 100000)
      uniform_data$summary <- summarise(updated_data,
                                        Mean = mean(x),
                                        Median = median(x),
                                        Mode = hdp(x),
                                        `Lower ETI` = quantile(x, (1 - (input$summary_range_number / 100)) / 2),
                                        `Upper ETI` = quantile(x, (input$summary_range_number / 100) + (1 - (input$summary_range_number / 100)) / 2))
    }

  })

  #### Update update_xxxx_summary functions code ####
  update_beta_summary <- function(type = "partial") {
    data <- beta_data$data

    # code_text_value(glue::glue("extraDistr::rprop(1000, {input$beta_precision}, {input$beta_mean})"))

    beta_data$summary <- summarise(data,
                                   Mean = mean(x),
                                   Median = median(x),
                                   Mode = hdp(x),
                                   `Lower ETI` = quantile(x, (1 - (input$summary_range_number / 100)) / 2),
                                   `Upper ETI` = quantile(x, (input$summary_range_number / 100) + (1 - (input$summary_range_number / 100)) / 2))

    hdi_length <- length(tidybayes::hdi(data$x, input$summary_range_number / 100))

    if(hdi_length == 2) {
      beta_data$hdi <- tibble(HDI = glue::glue("{input$summary_range_number}%"),
                              `Bound 1` = nice_num(tidybayes::hdi(data$x, input$summary_range_number / 100)[1], 2),
                              `Bound 2` = nice_num(tidybayes::hdi(data$x, input$summary_range_number / 100)[2], 2))
    }
    else if(hdi_length == 4) {
      beta_data$hdi <- tibble(`Bnd 1` = nice_num(tidybayes::hdi(data$x, input$summary_range_number / 100)[1], 2),
                              `Bnd 2` = nice_num(tidybayes::hdi(data$x, input$summary_range_number / 100)[3], 2),
                              `Bnd 3` = nice_num(tidybayes::hdi(data$x, input$summary_range_number / 100)[2], 2),
                              `Bnd 4` = nice_num(tidybayes::hdi(data$x, input$summary_range_number / 100)[4], 2))
    }
    else if(hdi_length == 6) {
      beta_data$hdi <- tibble(`Bnd 1` = nice_num(tidybayes::hdi(data$x, input$summary_range_number / 100)[1], 2),
                              `Bnd 2` = nice_num(tidybayes::hdi(data$x, input$summary_range_number / 100)[4], 2),
                              `Bnd 3` = nice_num(tidybayes::hdi(data$x, input$summary_range_number / 100)[2], 2),
                              `Bnd 4` = nice_num(tidybayes::hdi(data$x, input$summary_range_number / 100)[5], 2),
                              `Bnd 5` = nice_num(tidybayes::hdi(data$x, input$summary_range_number / 100)[3], 2),
                              `Bnd 6` = nice_num(tidybayes::hdi(data$x, input$summary_range_number / 100)[6], 2))
    }

    if(type == "full") {
      beta_data$quantiles <-
        tibble(
          Min = nice_num(min(data$x), 2),
          `2.5%` = nice_num(quantile(data$x, .025), 2),
          `5%` =  nice_num(quantile(data$x, .05), 2),
          `10%` = nice_num(quantile(data$x, .1), 2),
          `25%` = nice_num(quantile(data$x, .25), 2),
          `50%` = nice_num(quantile(data$x, .5), 2),
          `75%` = nice_num(quantile(data$x, .75), 2),
          `90%` = nice_num(quantile(data$x, .9), 2),
          `95%` = nice_num(quantile(data$x, .95), 2),
          `97.5%` = nice_num(quantile(data$x, .975), 2),
          Max = nice_num(max(data$x), 2)
        )
    }

  }

  update_betapercent_summary <- function(type = "partial") {
    data <- betapercent_data$data

    # code_text_value(glue::glue("extraDistr::rprop(1000, {input$betapercent_precision}, {input$betapercent_mean} / 100) * 100"))

    betapercent_data$summary <- summarise(data,
                                          Mean = mean(x),
                                          Median = median(x),
                                          Mode = hdp(x),
                                          `Lower ETI` = quantile(x, (1 - (input$summary_range_number / 100)) / 2),
                                          `Upper ETI` = quantile(x, (input$summary_range_number / 100) + (1 - (input$summary_range_number / 100)) / 2))

    hdi_length <- length(tidybayes::hdi(data$x, input$summary_range_number / 100))

    if(hdi_length == 2) {
      betapercent_data$hdi <- tibble(HDI = glue::glue("{input$summary_range_number}%"),
                                     `Bound 1` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[1], 2),
                                     `Bound 2` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[2], 2))
    }
    else if(hdi_length == 4) {
      betapercent_data$hdi <- tibble(`Bnd 1` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[1], 2),
                                     `Bnd 2` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[3], 2),
                                     `Bnd 3` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[2], 2),
                                     `Bnd 4` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[4], 2))
    }
    else if(hdi_length == 6) {
      betapercent_data$hdi <- tibble(`Bnd 1` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[1], 2),
                                     `Bnd 2` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[4], 2),
                                     `Bnd 3` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[2], 2),
                                     `Bnd 4` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[5], 2),
                                     `Bnd 5` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[3], 2),
                                     `Bnd 6` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[6], 2))
    }

    if(type == "full") {
      betapercent_data$quantiles <-
        tibble(
          Min = nice_num(min(data$x), 2),
          `2.5%` = nice_num(quantile(data$x, .025), 2),
          `5%` =  nice_num(quantile(data$x, .05), 2),
          `10%` = nice_num(quantile(data$x, .1), 2),
          `25%` = nice_num(quantile(data$x, .25), 2),
          `50%` = nice_num(quantile(data$x, .5), 2),
          `75%` = nice_num(quantile(data$x, .75), 2),
          `90%` = nice_num(quantile(data$x, .9), 2),
          `95%` = nice_num(quantile(data$x, .95), 2),
          `97.5%` = nice_num(quantile(data$x, .975), 2),
          Max = nice_num(max(data$x), 2)
        )
    }
  }

  update_normal_summary <- function(type = "partial") {
    data <- normal_data$data

    # code_text_value(glue::glue("rnorm(1000, {input$normal_mean}, {input$normal_sd})"))

    normal_data$summary <- summarise(data,
                                     Mean = mean(x),
                                     Median = median(x),
                                     Mode = hdp(x),
                                     `Lower ETI` = quantile(x, (1 - (input$summary_range_number / 100)) / 2),
                                     `Upper ETI` = quantile(x, (input$summary_range_number / 100) + (1 - (input$summary_range_number / 100)) / 2))

    hdi_length <- length(tidybayes::hdi(data$x, input$summary_range_number / 100))

    if(hdi_length == 2) {
      normal_data$hdi <- tibble(HDI = glue::glue("{input$summary_range_number}%"),
                                `Bound 1` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[1], 2),
                                `Bound 2` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[2], 2))
    }
    else if(hdi_length == 4) {
      normal_data$hdi <- tibble(`Bnd 1` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[1], 2),
                                `Bnd 2` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[3], 2),
                                `Bnd 3` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[2], 2),
                                `Bnd 4` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[4], 2))
    }
    else if(hdi_length == 6) {
      normal_data$hdi <- tibble(`Bnd 1` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[1], 2),
                                `Bnd 2` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[4], 2),
                                `Bnd 3` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[2], 2),
                                `Bnd 4` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[5], 2),
                                `Bnd 5` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[3], 2),
                                `Bnd 6` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[6], 2))
    }

    if(type == "full") {
      normal_data$quantiles <-
        tibble(
          Min = nice_num(min(data$x), 2),
          `2.5%` = nice_num(quantile(data$x, .025), 2),
          `5%` =  nice_num(quantile(data$x, .05), 2),
          `10%` = nice_num(quantile(data$x, .1), 2),
          `25%` = nice_num(quantile(data$x, .25), 2),
          `50%` = nice_num(quantile(data$x, .5), 2),
          `75%` = nice_num(quantile(data$x, .75), 2),
          `90%` = nice_num(quantile(data$x, .9), 2),
          `95%` = nice_num(quantile(data$x, .95), 2),
          `97.5%` = nice_num(quantile(data$x, .975), 2),
          Max = nice_num(max(data$x), 2)
        )
    }
  }

  update_skewnormal_summary <- function(type = "partial") {
    data <- skewnormal_data$data

    # code_text_value(glue::glue("sn::rsn(1000, {input$skewnormal_location}, {input$skewnormal_scale}, {input$skewnormal_slant})"))

    skewnormal_data$summary <- summarise(data,
                                         Mean = mean(x),
                                         Median = median(x),
                                         Mode = hdp(x),
                                         `Lower ETI` = quantile(x, (1 - (input$summary_range_number / 100)) / 2),
                                         `Upper ETI` = quantile(x, (input$summary_range_number / 100) + (1 - (input$summary_range_number / 100)) / 2))

    hdi_length <- length(tidybayes::hdi(data$x, input$summary_range_number / 100))

    if(hdi_length == 2) {
      skewnormal_data$hdi <- tibble(HDI = glue::glue("{input$summary_range_number}%"),
                                    `Bound 1` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[1], 2),
                                    `Bound 2` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[2], 2))
    }
    else if(hdi_length == 4) {
      skewnormal_data$hdi <- tibble(`Bnd 1` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[1], 2),
                                    `Bnd 2` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[3], 2),
                                    `Bnd 3` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[2], 2),
                                    `Bnd 4` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[4], 2))
    }
    else if(hdi_length == 6) {
      skewnormal_data$hdi <- tibble(`Bnd 1` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[1], 2),
                                    `Bnd 2` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[4], 2),
                                    `Bnd 3` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[2], 2),
                                    `Bnd 4` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[5], 2),
                                    `Bnd 5` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[3], 2),
                                    `Bnd 6` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[6], 2))
    }

    if(type == "full") {
      skewnormal_data$quantiles <-
        tibble(
          Min = nice_num(min(data$x), 2),
          `2.5%` = nice_num(quantile(data$x, .025), 2),
          `5%` =  nice_num(quantile(data$x, .05), 2),
          `10%` = nice_num(quantile(data$x, .1), 2),
          `25%` = nice_num(quantile(data$x, .25), 2),
          `50%` = nice_num(quantile(data$x, .5), 2),
          `75%` = nice_num(quantile(data$x, .75), 2),
          `90%` = nice_num(quantile(data$x, .9), 2),
          `95%` = nice_num(quantile(data$x, .95), 2),
          `97.5%` = nice_num(quantile(data$x, .975), 2),
          Max = nice_num(max(data$x), 2)
        )
    }
  }

  update_exp_summary <- function(type = "partial") {
    data <- exp_data$data

    # code_text_value(glue::glue("rexp(1000, {input$exp_rate})"))

    exp_data$summary <- summarise(data,
                                  Mean = mean(x),
                                  Median = median(x),
                                  Mode = hdp(x),
                                  `Lower ETI` = quantile(x, (1 - (input$summary_range_number / 100)) / 2),
                                  `Upper ETI` = quantile(x, (input$summary_range_number / 100) + (1 - (input$summary_range_number / 100)) / 2))

    hdi_length <- length(tidybayes::hdi(data$x, input$summary_range_number / 100))

    if(hdi_length == 2) {
      exp_data$hdi <- tibble(HDI = glue::glue("{input$summary_range_number}%"),
                             `Bound 1` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[1], 2),
                             `Bound 2` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[2], 2))
    }
    else if(hdi_length == 4) {
      exp_data$hdi <- tibble(`Bnd 1` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[1], 2),
                             `Bnd 2` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[3], 2),
                             `Bnd 3` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[2], 2),
                             `Bnd 4` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[4], 2))
    }
    else if(hdi_length == 6) {
      exp_data$hdi <- tibble(`Bnd 1` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[1], 2),
                             `Bnd 2` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[4], 2),
                             `Bnd 3` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[2], 2),
                             `Bnd 4` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[5], 2),
                             `Bnd 5` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[3], 2),
                             `Bnd 6` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[6], 2))
    }

    if(type == "full") {
      exp_data$quantiles <-
        tibble(
          Min = nice_num(min(data$x), 2),
          `2.5%` = nice_num(quantile(data$x, .025), 2),
          `5%` =  nice_num(quantile(data$x, .05), 2),
          `10%` = nice_num(quantile(data$x, .1), 2),
          `25%` = nice_num(quantile(data$x, .25), 2),
          `50%` = nice_num(quantile(data$x, .5), 2),
          `75%` = nice_num(quantile(data$x, .75), 2),
          `90%` = nice_num(quantile(data$x, .9), 2),
          `95%` = nice_num(quantile(data$x, .95), 2),
          `97.5%` = nice_num(quantile(data$x, .975), 2),
          Max = nice_num(max(data$x), 2)
        )
    }

  }

  update_lognormal_summary <- function(type = "partial") {
    data <- lognormal_data$data

    # code_text_value(glue::glue("rlnorm(1000, {input$lognormal_mean}, {input$lognormal_sd})"))

    lognormal_data$summary <- summarise(data,
                                        Mean = mean(x),
                                        Median = median(x),
                                        Mode = hdp(x),
                                        `Lower ETI` = quantile(x, (1 - (input$summary_range_number / 100)) / 2),
                                        `Upper ETI` = quantile(x, (input$summary_range_number / 100) + (1 - (input$summary_range_number / 100)) / 2))

    hdi_length <- length(tidybayes::hdi(data$x, input$summary_range_number / 100))

    if(hdi_length == 2) {
      lognormal_data$hdi <- tibble(HDI = glue::glue("{input$summary_range_number}%"),
                                   `Bound 1` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[1], 2),
                                   `Bound 2` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[2], 2))
    }
    else if(hdi_length == 4) {
      lognormal_data$hdi <- tibble(`Bnd 1` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[1], 2),
                                   `Bnd 2` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[3], 2),
                                   `Bnd 3` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[2], 2),
                                   `Bnd 4` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[4], 2))
    }
    else if(hdi_length == 6) {
      lognormal_data$hdi <- tibble(`Bnd 1` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[1], 2),
                                   `Bnd 2` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[4], 2),
                                   `Bnd 3` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[2], 2),
                                   `Bnd 4` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[5], 2),
                                   `Bnd 5` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[3], 2),
                                   `Bnd 6` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[6], 2))
    }

    if(type == "full") {
      lognormal_data$quantiles <-
        tibble(
          Min = nice_num(min(data$x), 2),
          `2.5%` = nice_num(quantile(data$x, .025), 2),
          `5%` =  nice_num(quantile(data$x, .05), 2),
          `10%` = nice_num(quantile(data$x, .1), 2),
          `25%` = nice_num(quantile(data$x, .25), 2),
          `50%` = nice_num(quantile(data$x, .5), 2),
          `75%` = nice_num(quantile(data$x, .75), 2),
          `90%` = nice_num(quantile(data$x, .9), 2),
          `95%` = nice_num(quantile(data$x, .95), 2),
          `97.5%` = nice_num(quantile(data$x, .975), 2),
          Max = nice_num(max(data$x), 2)
        )
    }
  }

  update_t_summary <- function(type = "partial") {
    data <- t_data$data

    # code_text_value(glue::glue("stevemisc::rst(1000, {input$t_df}, {input$t_location}, {input$t_scale})"))

    t_data$summary <- summarise(data,
                                Mean = mean(x),
                                Median = median(x),
                                Mode = hdp(x),
                                `Lower ETI` = quantile(x, (1 - (input$summary_range_number / 100)) / 2),
                                `Upper ETI` = quantile(x, (input$summary_range_number / 100) + (1 - (input$summary_range_number / 100)) / 2))

    hdi_length <- length(tidybayes::hdi(data$x, input$summary_range_number / 100))

    if(hdi_length == 2) {
      t_data$hdi <- tibble(HDI = glue::glue("{input$summary_range_number}%"),
                           `Bound 1` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[1], 2),
                           `Bound 2` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[2], 2))
    }
    else if(hdi_length == 4) {
      t_data$hdi <- tibble(`Bnd 1` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[1], 2),
                           `Bnd 2` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[3], 2),
                           `Bnd 3` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[2], 2),
                           `Bnd 4` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[4], 2))
    }
    else if(hdi_length == 6) {
      t_data$hdi <- tibble(`Bnd 1` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[1], 2),
                           `Bnd 2` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[4], 2),
                           `Bnd 3` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[2], 2),
                           `Bnd 4` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[5], 2),
                           `Bnd 5` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[3], 2),
                           `Bnd 6` = round(tidybayes::hdi(data$x, input$summary_range_number / 100)[6], 2))
    }

    if(type == "full") {
      t_data$quantiles <-
        tibble(
          Min = nice_num(min(data$x), 2),
          `2.5%` = nice_num(quantile(data$x, .025), 2),
          `5%` =  nice_num(quantile(data$x, .05), 2),
          `10%` = nice_num(quantile(data$x, .1), 2),
          `25%` = nice_num(quantile(data$x, .25), 2),
          `50%` = nice_num(quantile(data$x, .5), 2),
          `75%` = nice_num(quantile(data$x, .75), 2),
          `90%` = nice_num(quantile(data$x, .9), 2),
          `95%` = nice_num(quantile(data$x, .95), 2),
          `97.5%` = nice_num(quantile(data$x, .975), 2),
          Max = nice_num(max(data$x), 2)
        )
    }
  }

  update_uniform_summary <- function(type = "partial") {
    data <- uniform_data$data

    # code_text_value(glue::glue("runif(1000, {input$uniform_min}, {input$uniform_max})"))

    uniform_data$summary <- summarise(data,
                                      Mean = mean(x),
                                      Median = median(x),
                                      Mode = hdp(x),
                                      `Lower ETI` = quantile(x, (1 - (input$summary_range_number / 100)) / 2),
                                      `Upper ETI` = quantile(x, (input$summary_range_number / 100) + (1 - (input$summary_range_number / 100)) / 2))

    if(type == "full") {
      uniform_data$quantiles <-
        tibble(
          Min = nice_num(min(data$x), 2),
          `2.5%` = nice_num(quantile(data$x, .025), 2),
          `5%` =  nice_num(quantile(data$x, .05), 2),
          `10%` = nice_num(quantile(data$x, .1), 2),
          `25%` = nice_num(quantile(data$x, .25), 2),
          `50%` = nice_num(quantile(data$x, .5), 2),
          `75%` = nice_num(quantile(data$x, .75), 2),
          `90%` = nice_num(quantile(data$x, .9), 2),
          `95%` = nice_num(quantile(data$x, .95), 2),
          `97.5%` = nice_num(quantile(data$x, .975), 2),
          Max = nice_num(max(data$x), 2)
        )
    }

  }

  ##### update_xxxx_summary on observeEvent input$distribution_choice #####
  observeEvent(input$distribution_choice, {

    # if(is.null(input$distribution_choice) | is.na(input$distribution_choice)) {
    #   print("hi there")
    #   print(input$distribution_choice)
    #   Sys.sleep(1.5)
    # }

    if(input$distribution_choice == "Beta") {
      update_beta_summary(type = "full")
    }
    else if(input$distribution_choice == "Normal") {
      update_normal_summary(type = "full")
    }
    else if(input$distribution_choice == "Skew normal") {
      update_skewnormal_summary(type = "full")
    }
    else if(input$distribution_choice == "Beta as %") {
      update_betapercent_summary(type = "full")
    }
    else if(input$distribution_choice == "Exponential") {
      update_exp_summary(type = "full")
    }
    else if(input$distribution_choice == "Log-normal") {
      update_lognormal_summary(type = "full")
    }
    else if(input$distribution_choice == "Student's t") {
      update_t_summary(type = "full")
    }
    else if(input$distribution_choice == "Uniform") {
      update_uniform_summary(type = "full")
    }
    # unsure here
    else {
      my_target <- which(distribution_values$distribution_list$Custom == input$distribution_choice)
      update_custom_summary(my_target)
    }
  })

  #### observeEvent for input$summary_range_type ####
  observeEvent(input$summary_range_type, {

    if(!should_render_ui()) {
      return()
    }

    if(input$distribution_choice == "Beta") {
      update_beta_summary()
    }
    else if(input$distribution_choice == "Normal") {
      update_normal_summary()
    }
    else if(input$distribution_choice == "Skew normal") {
      update_skewnormal_summary()
    }
    else if(input$distribution_choice == "Beta as %") {
      update_betapercent_summary()
    }
    else if(input$distribution_choice == "Exponential") {
      update_exp_summary()
    }
    else if(input$distribution_choice == "Log-normal") {
      update_lognormal_summary()
    }
    else if(input$distribution_choice == "Student's t") {
      update_t_summary()
    }
    else if(input$distribution_choice == "Uniform") {
      update_uniform_summary()
    }
    # unsure here
    else {
      my_target <- which(distribution_values$distribution_list$Custom == input$distribution_choice)
      update_custom_summary(my_target)
    }
  })

  #### observeEvent for input$summary_range_number ####
  observeEvent(input$summary_range_number, {

    if(!should_render_ui()) {
      return()
    }

    if(input$distribution_choice == "Beta") {
      update_beta_summary()
    }
    else if(input$distribution_choice == "Normal") {
      update_normal_summary()
    }
    else if(input$distribution_choice == "Skew normal") {
      update_skewnormal_summary()
    }
    else if(input$distribution_choice == "Beta as %") {
      update_betapercent_summary()
    }
    else if(input$distribution_choice == "Exponential") {
      update_exp_summary()
    }
    else if(input$distribution_choice == "Log-normal") {
      update_lognormal_summary()
    }
    else if(input$distribution_choice == "Student's t") {
      update_t_summary()
    }
    else if(input$distribution_choice == "Uniform") {
      update_uniform_summary()
    }
    # unsure here
    else {
      my_target <- which(distribution_values$distribution_list$Custom == input$distribution_choice)
      update_custom_summary(my_target)
    }
  })

  #### observeEvent for input$summary_point_type ####
  observeEvent(input$summary_point_type, {

    if(!should_render_ui()) {
      return()
    }

    if(input$distribution_choice == "Beta") {
      update_beta_summary()
    }
    else if(input$distribution_choice == "Normal") {
      update_normal_summary()
    }
    else if(input$distribution_choice == "Skew normal") {
      update_skewnormal_summary()
    }
    else if(input$distribution_choice == "Beta as %") {
      update_betapercent_summary()
    }
    else if(input$distribution_choice == "Exponential") {
      update_exp_summary()
    }
    else if(input$distribution_choice == "Log-normal") {
      update_lognormal_summary()
    }
    else if(input$distribution_choice == "Student's t") {
      update_t_summary()
    }
    else if(input$distribution_choice == "Uniform") {
      update_uniform_summary()
    }
    # unsure here
    else {
      my_target <- which(distribution_values$distribution_list$Custom == input$distribution_choice)
      update_custom_summary(my_target)
    }
  })

  #### parameter_ui renderUI ####
  output$parameter_ui <- renderUI({

    if(!should_render_ui()) {
      return()
    }

    if(input$distribution_choice == "Beta") {

      fluidRow(column(10,
                      offset = 1,
                      align = "center",
                      numericInput("beta_mean",
                                   label = "Beta mean (0-1, exclusive)",
                                   min = 0,
                                   max = 1,
                                   value = beta_parameters$mean,
                                   step = .01),
                      numericInput("beta_precision",
                                   label = "Beta precision (a positive number)",
                                   min = 0,
                                   max = NA,
                                   value = beta_parameters$precision,
                                   step = 1),
                      br(),
                      actionButton("beta_update",
                                   label = "Update",
                                   style="color: #ffffff; background-color: #2f6e8d; border-color: #2f6e8d"))
      )

    }

    else if(input$distribution_choice == "Beta as %") {

      fluidRow(column(10,
                      offset = 1,
                      align = "center",
                      numericInput("betapercent_mean",
                                   label = "Beta mean (0-100, exclusive)",
                                   min = 0,
                                   max = 100,
                                   value = betapercent_parameters$mean,
                                   step = 1),
                      numericInput("betapercent_precision",
                                   label = "Beta precision (a positive number)",
                                   min = 0,
                                   max = NA,
                                   value = betapercent_parameters$precision,
                                   step = 1),
                      br(),
                      actionButton("betapercent_update",
                                   label = "Update",
                                   style="color: #ffffff; background-color: #2f6e8d; border-color: #2f6e8d"))
      )

    }

    else if(input$distribution_choice == "Normal") {

      fluidRow(column(10,
                      offset = 1,
                      align = "center",
                      numericInput("normal_mean",
                                   label = "Mean",
                                   min = NA,
                                   max = NA,
                                   value = normal_parameters$mean,
                                   step = 2.5),
                      numericInput("normal_sd",
                                   label = "Standard deviation (a positive number)",
                                   min = 0,
                                   max = NA,
                                   value = normal_parameters$sd,
                                   step = 1),
                      br(),
                      actionButton("normal_update",
                                   label = "Update",
                                   style="color: #ffffff; background-color: #2f6e8d; border-color: #2f6e8d"))
      )
    }

    else if(input$distribution_choice == "Skew normal") {

      fluidRow(column(10,
                      offset = 1,
                      align = "center",
                      numericInput("skewnormal_location",
                                   label = "Location (like the mean)",
                                   min = NA,
                                   max = NA,
                                   value = skewnormal_parameters$location,
                                   step = 2.5),
                      numericInput("skewnormal_scale",
                                   label = "Scale (+ve, like the SD)",
                                   min = 0,
                                   max = NA,
                                   value = skewnormal_parameters$scale,
                                   step = 1),
                      numericInput("skewnormal_slant",
                                   label = "Slant (+/-)",
                                   min = 0,
                                   max = NA,
                                   value = skewnormal_parameters$slant,
                                   step = 1),
                      br(),
                      actionButton("skewnormal_update",
                                   label = "Update",
                                   style="color: #ffffff; background-color: #2f6e8d; border-color: #2f6e8d"))
      )
    }

    else if(input$distribution_choice == "Exponential") {

      fluidRow(column(10,
                      offset = 1,
                      align = "center",
                      numericInput("exp_rate",
                                   label = "Rate",
                                   min = 0,
                                   max = NA,
                                   value = exp_parameters$rate,
                                   step = 1),
                      br(),
                      actionButton("exp_update",
                                   label = "Update",
                                   style="color: #ffffff; background-color: #2f6e8d; border-color: #2f6e8d"))
      )
    }

    else if(input$distribution_choice == "Log-normal") {

      fluidRow(column(10,
                      offset = 1,
                      align = "center",
                      numericInput("lognormal_mean",
                                   label = "Log Mean",
                                   min = NA,
                                   max = NA,
                                   value = lognormal_parameters$logmean,
                                   step = 2.5),
                      numericInput("lognormal_sd",
                                   label = "Log SD",
                                   min = 0,
                                   max = NA,
                                   value = lognormal_parameters$logsd,
                                   step = 1),
                      br(),
                      actionButton("lognormal_update",
                                   label = "Update",
                                   style="color: #ffffff; background-color: #2f6e8d; border-color: #2f6e8d"))
      )
    }

    else if(input$distribution_choice == "Student's t") {

      fluidRow(column(10,
                      offset = 1,
                      align = "center",
                      numericInput("t_location",
                                   label = "Location (mean)",
                                   min = NA,
                                   max = NA,
                                   value = t_parameters$location,
                                   step = 2.5),
                      numericInput("t_scale",
                                   label = "Scale (like SD, a positive number)",
                                   min = 0,
                                   max = NA,
                                   value = t_parameters$scale,
                                   step = 1),
                      numericInput("t_df",
                                   label = "Degrees of freedom",
                                   min = 0,
                                   max = NA,
                                   value = t_parameters$df,
                                   step = 1),
                      br(),
                      actionButton("t_update",
                                   label = "Update",
                                   style="color: #ffffff; background-color: #2f6e8d; border-color: #2f6e8d"))
      )
    }

    else if(input$distribution_choice == "Uniform") {

      fluidRow(column(10,
                      offset = 1,
                      align = "center",
                      numericInput("uniform_min",
                                   label = "Minimum",
                                   min = NA,
                                   max = NA,
                                   value = uniform_parameters$min,
                                   step = 1),
                      numericInput("uniform_max",
                                   label = "Maximum",
                                   min = NA,
                                   max = NA,
                                   value = uniform_parameters$max,
                                   step = 1),
                      br(),
                      actionButton("uniform_update",
                                   label = "Update",
                                   style="color: #ffffff; background-color: #2f6e8d; border-color: #2f6e8d"))
      )
    }

  })

  #### plot option ui ####
  output$plot_options <- renderUI({

    if(input$plot_choice == "Histogram") {
      selectInput("n_bins",
                  "Number of bins:",
                  choices = seq(5, 50, 5),
                  selected = 40)
    }
    else if(input$plot_choice == "Percentogram") {
      selectInput("n_percentile",
                  "%age per bar:",
                  choices = c(2.5, 5, 10, 20),
                  selected = 5)
    }

  })

  #### table output ####
  output$table <- renderFormattable({

    if(!should_render_ui()) {
      return()
    }

    if(input$distribution_choice == "Beta") {
      if(beta_data$summary$Mean == "...") {
        display_data <-
          beta_data$summary %>%
          pivot_longer(cols = everything(),
                       names_to = "Statistic",
                       values_to = "Proportion") %>%
          mutate(Percentage = "...")

        formattable(display_data, align = "c")
      }

      else {
        display_data <- beta_data$summary %>%
          pivot_longer(cols = everything(),
                       names_to = "Statistic",
                       values_to = "Proportion") %>%
          mutate(Percentage = Proportion * 100,
                 Percentage = round(Percentage, 2),
                 Proportion = round(Proportion, 2))

        formattable(display_data, align = "c")
      }
    }

    else if(input$distribution_choice == "Beta as %") {
      if(betapercent_data$summary$Mean == "...") {
        display_data <-
          betapercent_data$summary %>%
          pivot_longer(cols = everything(),
                       names_to = "Statistic",
                       values_to = "Percentage")

        formattable(display_data, align = "c")
      }

      else {
        display_data <- betapercent_data$summary %>%
          pivot_longer(cols = everything(),
                       names_to = "Statistic",
                       values_to = "Percentage") %>%
          mutate(Percentage = round(Percentage, 2))

        formattable(display_data, align = "c")
      }
    }

    else if(input$distribution_choice == "Normal") {
      if(normal_data$summary$Mean == "...") {
        display_data <-
          normal_data$summary %>%
          pivot_longer(cols = everything(),
                       names_to = "Statistic",
                       values_to = "Value") %>%
          mutate(Percentage = "...")

        formattable(display_data, align = "c")
      }

      else {
        display_data <- normal_data$summary %>%
          pivot_longer(cols = everything(),
                       names_to = "Statistic",
                       values_to = "Value") %>%
          mutate(Value = round(Value, 2))

        formattable(display_data, align = "c")
      }
    }

    else if(input$distribution_choice == "Skew normal") {
      if(skewnormal_data$summary$Mean == "...") {
        display_data <-
          skewnormal_data$summary %>%
          pivot_longer(cols = everything(),
                       names_to = "Statistic",
                       values_to = "Value") %>%
          mutate(Percentage = "...")

        formattable(display_data, align = "c")
      }

      else {
        display_data <- skewnormal_data$summary %>%
          pivot_longer(cols = everything(),
                       names_to = "Statistic",
                       values_to = "Value") %>%
          mutate(Value = round(Value, 2))

        formattable(display_data, align = "c")
      }
    }

    else if(input$distribution_choice == "Exponential") {
      if(exp_data$summary$Mean == "...") {
        display_data <-
          exp_data$summary %>%
          pivot_longer(cols = everything(),
                       names_to = "Statistic",
                       values_to = "Percentage")

        formattable(display_data, align = "c")
      }

      else {
        display_data <- exp_data$summary %>%
          pivot_longer(cols = everything(),
                       names_to = "Statistic",
                       values_to = "Percentage") %>%
          mutate(Percentage = round(Percentage, 2))

        formattable(display_data, align = "c")
      }
    }

    else if(input$distribution_choice == "Log-normal") {
      if(lognormal_data$summary$Mean == "...") {
        display_data <-
          lognormal_data$summary %>%
          pivot_longer(cols = everything(),
                       names_to = "Statistic",
                       values_to = "Percentage")

        formattable(display_data, align = "c")
      }

      else {
        display_data <- lognormal_data$summary %>%
          pivot_longer(cols = everything(),
                       names_to = "Statistic",
                       values_to = "Percentage") %>%
          mutate(Percentage = round(Percentage, 2))

        formattable(display_data, align = "c")
      }
    }

    else if(input$distribution_choice == "Student's t") {
      if(t_data$summary$Mean == "...") {
        display_data <-
          t_data$summary %>%
          pivot_longer(cols = everything(),
                       names_to = "Statistic",
                       values_to = "Percentage")

        formattable(display_data, align = "c")
      }

      else {
        display_data <- t_data$summary %>%
          pivot_longer(cols = everything(),
                       names_to = "Statistic",
                       values_to = "Percentage") %>%
          mutate(Percentage = round(Percentage, 2))

        formattable(display_data, align = "c")
      }
    }

    else if(input$distribution_choice == "Uniform") {
      if(uniform_data$summary$Mean == "...") {
        display_data <-
          uniform_data$summary %>%
          pivot_longer(cols = everything(),
                       names_to = "Statistic",
                       values_to = "Value") %>%
          mutate(Percentage = "...")

        formattable(display_data, align = "c")
      }

      else {
        display_data <- uniform_data$summary %>%
          pivot_longer(cols = everything(),
                       names_to = "Statistic",
                       values_to = "Value") %>%
          mutate(Value = round(Value, 2))

        formattable(display_data, align = "c")
      }
    }

    else {

        display_data <- distribution_values$custom_data_summaries[[which(distribution_values$distribution_list$Custom == input$distribution_choice)]] %>%
          pivot_longer(cols = everything(),
                       names_to = "Statistic",
                       values_to = "Value") %>%
          mutate(Value = round(Value, 2))

        formattable(display_data, align = "c")
      }
  })

  #### quantile table output ####
  output$quantile_table <- renderFormattable({

    if(!should_render_ui()) {
      return()
    }

    if(input$distribution_choice == "Beta") {
      formattable(beta_data$quantiles, align = "c")
    }
    else if(input$distribution_choice == "Beta as %") {
      formattable(betapercent_data$quantiles, align = "c")
    }
    else if(input$distribution_choice == "Normal") {
      formattable(normal_data$quantiles, align = "c")
    }
    else if(input$distribution_choice == "Skew normal") {
      formattable(skewnormal_data$quantiles, align = "c")
    }
    else if(input$distribution_choice == "Exponential") {
      formattable(exp_data$quantiles, align = "c")
    }
    else if(input$distribution_choice == "Log-normal") {
      formattable(lognormal_data$quantiles, align = "c")
    }
    else if(input$distribution_choice == "Student's t") {
      formattable(t_data$quantiles, align = "c")
    }
    else if(input$distribution_choice == "Uniform") {
      formattable(uniform_data$quantiles, align = "c")
    }
    else{
      formattable(distribution_values$custom_data_quantiles[[which(distribution_values$distribution_list$Custom == input$distribution_choice)]], align = "c")
    }
  })

  #### click table output ####
  click_table_values <- reactiveValues(
    click_info = tibble(`Click plot` = "...",
                        `for` = "...",
                        info = "...")
  )

  output$click_table <- renderFormattable({

    formattable(click_table_values$click_info, align = "c")

  })

  #### hdi table output ####
  output$hdi_table <- renderFormattable({

    if(!should_render_ui()) {
      return()
    }

    if(input$distribution_choice == "Beta") {
      formattable(beta_data$hdi, align = "c")
    }
    else if(input$distribution_choice == "Beta as %") {
      formattable(betapercent_data$hdi, align = "c")
    }
    else if(input$distribution_choice == "Normal") {
      formattable(normal_data$hdi, align = "c")
    }
    else if(input$distribution_choice == "Skew normal") {
      formattable(skewnormal_data$hdi, align = "c")
    }
    else if(input$distribution_choice == "Exponential") {
      formattable(exp_data$hdi, align = "c")
    }
    else if(input$distribution_choice == "Log-normal") {
      formattable(lognormal_data$hdi, align = "c")
    }
    else if(input$distribution_choice == "Student's t") {
      formattable(t_data$hdi, align = "c")
    }
    else if(input$distribution_choice == "Uniform") {
      formattable(uniform_data$hdi, align = "c")
    }
    else{
      formattable(distribution_values$custom_data_hdis[[which(distribution_values$distribution_list$Custom == input$distribution_choice)]], align = "c")
    }
  })

  #### Click info text ####
  observeEvent(input$plot_click, {

    x_point <- input$plot_click$x

    text_version_data <-
      if(input$distribution_choice == "Beta") {
        "beta"
      }
    else if(input$distribution_choice == "Normal") {
      "normal"
    }
    else if(input$distribution_choice == "Skew normal") {
      "skewnormal"
    }
    else if(input$distribution_choice == "Beta as %") {
      "betapercent"
    }
    else if(input$distribution_choice == "Exponential") {
      "exp"
    }
    else if(input$distribution_choice == "Log-normal") {
      "lognormal"
    }
    else if(input$distribution_choice == "Student's t") {
      "t"
    }
    else if(input$distribution_choice == "Uniform") {
      "uniform"
    }
    else {
      "custom"
    }

    if(text_version_data == "custom") {
      my_target <- which(distribution_values$distribution_list$Custom == input$distribution_choice)
    }

    my_raw_data <-
      if(text_version_data != "custom") {
        eval(parse(text = glue::glue("{text_version_data}_data$data")))
      }
    else {
      distribution_values$custom_data[[my_target]]
    }

    if(input$plot_choice == "Percentogram") {

      my_data <-
        percentogram_data <-
        make_percentogram_x(data = my_raw_data,
                            percent = if(is.null(input$n_percentile)) {5} else {as.numeric(input$n_percentile)},
                            percentile_range = "quintile") %>%
        mutate(x_click = x_point,
               inside = between(x_click, xmin, xmax)) %>%
        filter(inside == TRUE)

      # my_label <- glue::glue("{my_data %>% pull(lq)}%-{my_data %>% pull(uq)}% = {my_data %>% pull(xmin) %>% nice_num(2, FALSE)}-{my_data %>% pull(xmax) %>% nice_num(2, FALSE)}")

      my_raw_data <- my_raw_data %>% pull(x)

      if(input$plot_format == "Standard") {
        # my_density <- density(my_raw_data)
        # my_density <- approx(my_density$x, my_density$y, xout = x_point)$y
        my_cumulative_density <- ecdf(my_raw_data)
        my_cumulative_density <- my_cumulative_density(x_point)
        click_table_values$click_info <- tibble(`X` = nice_num(x_point, remove_lead = FALSE),
                                                `%ile` = glue::glue("{my_data %>% pull(lq)}%-{my_data %>% pull(uq)}%"),
                                                Range = glue::glue("{my_data %>% pull(xmin) %>% nice_num(2, FALSE)} - {my_data %>% pull(xmax) %>% nice_num(2, FALSE)}"),
                                                `~CDF value` = nice_num(my_cumulative_density, remove_lead = FALSE))
      }
      else if(input$plot_format == "Cumulative") {
        my_cumulative_density <- ecdf(my_raw_data)
        my_cumulative_density <- my_cumulative_density(x_point)
        click_table_values$click_info <- tibble(`X` = nice_num(x_point, remove_lead = FALSE),
                                                `%ile` = glue::glue("{my_data %>% pull(lq)}%-{my_data %>% pull(uq)}%"),
                                                Range = glue::glue("{my_data %>% pull(xmin) %>% nice_num(2, FALSE)} - {my_data %>% pull(xmax) %>% nice_num(2, FALSE)}"),
                                                `~CDF value` = nice_num(my_cumulative_density, remove_lead = FALSE))
      }
    }
    else {
      my_raw_data <- my_raw_data %>% pull(x)

      my_density <- density(my_raw_data)
      my_density <- approx(my_density$x, my_density$y, xout = x_point)$y
      my_cumulative_density <- ecdf(my_raw_data)
      my_cumulative_density <- my_cumulative_density(x_point)

      click_table_values$click_info <- tibble(`X` = nice_num(x_point, remove_lead = FALSE),
                                              `~PDF value` = nice_num(as.numeric(my_density), remove_lead = FALSE),
                                              `~CDF value` = nice_num(as.numeric(my_cumulative_density), remove_lead = FALSE))

    }

  })

  #### left_plot output ####
  theme_set(theme_distributr())

  output$left_plot <- renderPlot({

    # distribution_values$custom_data_hdis[[which(distribution_values$distribution_list$Custom == input$distribution_choice)]]

    if(!should_render_ui()) {
      return()
    }

    text_version_data <-
      if(input$distribution_choice == "Beta") {
        "beta"
      }
    else if(input$distribution_choice == "Normal") {
      "normal"
    }
    else if(input$distribution_choice == "Skew normal") {
      "skewnormal"
    }
    else if(input$distribution_choice == "Beta as %") {
      "betapercent"
    }
    else if(input$distribution_choice == "Exponential") {
      "exp"
    }
    else if(input$distribution_choice == "Log-normal") {
      "lognormal"
    }
    else if(input$distribution_choice == "Student's t") {
      "t"
    }
    else if(input$distribution_choice == "Uniform") {
      "uniform"
    }
    else{
      "custom"
    }

    if(text_version_data == "custom") {
      my_target <- which(distribution_values$distribution_list$Custom == input$distribution_choice)
    }

    if(input$plot_range[1] == 0 & input$plot_range[2] == 100) {
      plot_limits <- coord_cartesian()
    }
    else {
      lower_limit <-
        if(input$plot_range[1] == 0) {
          NA
        }
      else {
        if(text_version_data == "custom") {
          distribution_values$custom_data[[my_target]] %>%
            pull(x) %>%
            quantile(input$plot_range[1] / 100)
        }
        else {
          eval(parse(text = glue::glue("{text_version_data}_data$data"))) %>% pull(x) %>% quantile(input$plot_range[1] / 100)
        }
      }

      upper_limit <-
        if(input$plot_range[2] == 0) {
          NA
        }
      else {
        if(text_version_data == "custom") {
          distribution_values$custom_data[[my_target]] %>%
            pull(x) %>%
            quantile(input$plot_range[2] / 100)
        }
        else {
          eval(parse(text = glue::glue("{text_version_data}_data$data"))) %>% pull(x) %>% quantile(input$plot_range[2] / 100)
        }
      }

      plot_limits <- coord_cartesian(xlim = c(lower_limit, upper_limit))

    }

    my_subtitle <-
      if(input$distribution_choice == "Beta") {
        if(input$plot_choice == "Percentogram") {
          "Beta distribution data"
        }
        else{
          "Beta distribution"
        }
      }
    else if(input$distribution_choice == "Normal") {
      if(input$plot_choice == "Percentogram") {
        "Normal distribution data"
      }
      else{
        "Normal distribution"
      }
    }
    else if(input$distribution_choice == "Skew normal") {
      if(input$plot_choice == "Percentogram") {
        "Skew-normal distribution data"
      }
      else{
        "Skew-normal distribution"
      }
    }
    else if(input$distribution_choice == "Beta as %") {
      if(input$plot_choice == "Percentogram") {
        "Beta distribution data (as %ages)"
      }
      else{
        "Beta distribution as a %age"
      }
    }
    else if(input$distribution_choice == "Exponential") {
      if(input$plot_choice == "Percentogram") {
        "Exponential distribution data"
      }
      else{
        "Exponential distribution"
      }
    }
    else if(input$distribution_choice == "Log-normal") {
      if(input$plot_choice == "Percentogram") {
        "Log-normal distribution data"
      }
      else{
        "Log-normal distribution"
      }
    }
    else if(input$distribution_choice == "Student's t") {
      if(input$plot_choice == "Percentogram") {
        "*t* distribution data"
      }
      else{
        "*t* distribution"
      }
    }
    else if(input$distribution_choice == "Uniform") {
      if(input$plot_choice == "Percentogram") {
        "Uniform distribution data"
      }
      else{
        "Uniform distribution"
      }
    }
    else {
      if(input$plot_choice == "Percentogram") {
        glue::glue("Custom distribution data - {input$distribution_choice}")
      }
      else{
        glue::glue("Custom distribution - {input$distribution_choice}")
      }
    }

    percentogram_data <-
      if(text_version_data == "custom") {
        make_percentogram_x(data = distribution_values$custom_data[[my_target]],
                            percent = if(is.null(input$n_percentile)) {5} else {as.numeric(input$n_percentile)},
                            percentile_range = "quintile")
      }
    else {
      make_percentogram_x(data = eval(parse(text = glue::glue("{text_version_data}_data$data"))),
                          percent = if(is.null(input$n_percentile)) {5} else {as.numeric(input$n_percentile)},
                          percentile_range = "quintile")
    }

    if(text_version_data != "custom") {
      if(eval(parse(text = glue::glue("{text_version_data}_data$summary$Mean"))) == "...") {
        point_estimate_line <-
          tibble(x = as.numeric(NA))

        interval_line <-
          tibble(x = as.numeric(NA))
      }
      else {
        point_estimate_line <-
          if(input$summary_point_type == "Mean") {
            tibble(x = eval(parse(text = glue::glue("{text_version_data}_data$summary$Mean"))))
          }
        else if(input$summary_point_type == "Median") {
          tibble(x = eval(parse(text = glue::glue("{text_version_data}_data$summary$Median"))))
        }
        else if(input$summary_point_type == "Mode") {
          tibble(x = eval(parse(text = glue::glue("{text_version_data}_data$summary$Mode"))))
        }

        interval_line <-
          if(input$summary_range_type == "Highest density interval") {
            if(input$distribution_choice == "Uniform") {
              tibble(x = as.numeric(NA))
            }
            else {
              eval(parse(text = glue::glue("{text_version_data}_data$hdi"))) %>% select(-contains("HDI")) %>% pivot_longer(cols = everything(),
                                                                                                                           names_to = "bound",
                                                                                                                           values_to = "x")
            }
          }
        else if(input$summary_range_type == "Quantile/equal-tailed interval") {
          tibble(x = c(eval(parse(text = glue::glue("{text_version_data}_data$summary$`Lower ETI`"))),
                       eval(parse(text = glue::glue("{text_version_data}_data$summary$`Upper ETI`")))))
        }
      }
    }
    else {
      point_estimate_line <-
        if(input$summary_point_type == "Mean") {
          tibble(x = distribution_values$custom_data_summaries[[my_target]]$Mean)
        }
      else if(input$summary_point_type == "Median") {
        tibble(x = distribution_values$custom_data_summaries[[my_target]]$Median)
      }
      else if(input$summary_point_type == "Mode") {
        tibble(x = distribution_values$custom_data_summaries[[my_target]]$Mode)
      }

      interval_line <-
        if(input$summary_range_type == "Highest density interval") {
          distribution_values$custom_data_hdis[[my_target]] %>%
            select(-contains("HDI")) %>%
            pivot_longer(cols = everything(),
                         names_to = "bound",
                         values_to = "x")
        }
      else if(input$summary_range_type == "Quantile/equal-tailed interval") {
        tibble(x = c(distribution_values$custom_data_summaries[[my_target]]$`Lower ETI`,
                     distribution_values$custom_data_summaries[[my_target]]$`Upper ETI`))
      }
    }

    if(input$plot_choice == "Percentogram") {
      if(input$plot_format == "Standard") {
        ggplot(percentogram_data) +
          plot_limits +
          geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = quintile), alpha = .8, color = "black", linewidth = .275) +
          geom_vline(data = point_estimate_line, aes(xintercept = as.numeric(x)), linewidth = .67) +
          geom_vline(data = interval_line, aes(xintercept = as.numeric(x)), linetype = "dashed", linewidth = .67) +
          scale_fill_manual(values = c(make_colors(c("#f2b231", "grey97", "#327291"), n = 5))) +
          labs(title = "Percentogram",
               subtitle = glue::glue("1 Bar = {input$n_percentile}% of your {my_subtitle}"),
               fill = "Quintile:") +
          theme(axis.title = element_blank(),
                axis.text.y = element_blank())
      }
      else if(input$plot_format == "Cumulative") {
        ggplot(percentogram_data) +
          plot_limits +
          scale_y_continuous(breaks = seq(0, 100, 10), labels = scales::percent_format(scale = 1)) +
          geom_rect(aes(xmin = xmin, xmax = xmax, ymin = lq, ymax = uq, fill = quintile), alpha = .8, color = "black", linewidth = .275) +
          geom_vline(data = point_estimate_line, aes(xintercept = as.numeric(x)), linewidth = .67) +
          geom_vline(data = interval_line, aes(xintercept = as.numeric(x)), linetype = "dashed", linewidth = .67) +
          scale_fill_manual(values = c(make_colors(c("#f2b231", "grey97", "#327291"), n = 5))) +
          labs(title = "Percentogram",
               subtitle = glue::glue("1 Bar = {input$n_percentile}% of your {my_subtitle}"),
               fill = "Quintile:") +
          theme(axis.title = element_blank())
      }
    }
    else if(input$plot_choice == "Histogram") {

      my_plot_data <-
        if(text_version_data != "custom") {
          eval(parse(text = glue::glue("{text_version_data}_data$data")))
        }
      else {
        distribution_values$custom_data[[my_target]]
      }

      if(input$plot_format == "Standard") {
        ggplot(my_plot_data) +
          plot_limits +
          geom_histogram(aes(x = x), alpha = .8, fill = "#327291", color = "black", breaks = seq(min(my_plot_data$x), max(my_plot_data$x), length.out = as.numeric(input$n_bins) + 1), linewidth = .275) +
          geom_vline(data = point_estimate_line, aes(xintercept = as.numeric(x)), linewidth = .67) +
          geom_vline(data = interval_line, aes(xintercept = as.numeric(x)), linetype = "dashed", linewidth = .67) +
          labs(title = "Histogram",
               subtitle = my_subtitle) +
          theme(axis.title = element_blank(),
                axis.text.y = element_blank())
      }
      else if(input$plot_format == "Cumulative") {
        ggplot(my_plot_data) +
          plot_limits +
          scale_y_continuous(labels = paste0(seq(0, 100, 10), "%"), breaks = seq(0, 500000, 50000)) +
          geom_histogram(aes(x = x, y = cumsum(..count..)), alpha = .8, fill = "#327291", color = "black", breaks = seq(min(my_plot_data$x), max(my_plot_data$x), length.out = as.numeric(input$n_bins) + 1), linewidth = .275) +
          geom_vline(data = point_estimate_line, aes(xintercept = as.numeric(x)), linewidth = .67) +
          geom_vline(data = interval_line, aes(xintercept = as.numeric(x)), linetype = "dashed", linewidth = .67) +
          labs(title = "Histogram",
               subtitle = my_subtitle) +
          theme(axis.title = element_blank())
      }
    }
    else if(input$plot_choice == "Density") {

      my_plot_data <-
        if(text_version_data != "custom") {
          eval(parse(text = glue::glue("{text_version_data}_data$data_density")))
        }
      else {
        distribution_values$data_density[[my_target]]
      }

      if(input$plot_format == "Standard") {
        ggplot(my_plot_data) +
          plot_limits +
          geom_density(aes(x = x), alpha = .8, fill = "#327291", color = "white") +
          geom_vline(data = point_estimate_line, aes(xintercept = as.numeric(x)), linewidth = .67) +
          geom_vline(data = interval_line, aes(xintercept = as.numeric(x)), linetype = "dashed", linewidth = .67) +
          labs(title = "Density plot",
               subtitle = my_subtitle) +
          theme(axis.title = element_blank(),
                axis.text.y = element_blank())
      }
      else if(input$plot_format == "Cumulative") {
        ggplot(my_plot_data) +
          plot_limits +
          scale_y_continuous(breaks = seq(0, 1, .1), labels = scales::percent_format(scale = 100)) +
          stat_ecdf(aes(x = x), alpha = .8, fill = "#327291", color = "#327291") +
          geom_vline(data = point_estimate_line, aes(xintercept = as.numeric(x)), linewidth = .67) +
          geom_vline(data = interval_line, aes(xintercept = as.numeric(x)), linetype = "dashed", linewidth = .67) +
          labs(title = "Density plot",
               subtitle = my_subtitle) +
          theme(axis.title = element_blank())
      }
    }
    else if(input$plot_choice == "Points") {

      my_plot_data <-
        if(text_version_data != "custom") {
          eval(parse(text = glue::glue("{text_version_data}_data$data_5000")))
        }
      else {
        distribution_values$data_5000[[my_target]]
      }

      ggplot(my_plot_data) +
        plot_limits +
        geom_point(aes(x = x, y = y), shape = 21, alpha = .25, fill = "#327291", color = "white") +
        geom_vline(data = point_estimate_line, aes(xintercept = as.numeric(x)), linewidth = .67) +
        geom_vline(data = interval_line, aes(xintercept = as.numeric(x)), linetype = "dashed", linewidth = .67) +
        labs(title = "5000 points",
             subtitle = my_subtitle) +
        theme(axis.title = element_blank(),
              axis.text.y = element_blank())
    }
  })

}

# Run the application
shinyApp(ui = ui, server = server)

# library(rsconnect)
# deployApp()
