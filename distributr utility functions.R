#### distributr ggplot theme ####
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

#### make_colors function ####
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

#### make_percentogram_x function ####
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

#### hdp function ####
hdp <- function(data) {
  
  hdp <- density(data)$x[which.max(density(data)$y)]
  
  return(hdp)
  
}