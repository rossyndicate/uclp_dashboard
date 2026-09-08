#' Plot Total Organic Carbon (TOC) Forecast
#' Generates an interactive Plotly visualization displaying predicted TOC concentrations
#' (Intake or Distributed) with percentile uncertainty ribbons, reference thresholds,
#' and outdated-forecast warnings.
#'
#' @param forecast_data Data frame or tibble containing forecast dates (`date_24h`)
#'   and predicted TOC quantile columns (`min`, `q25`, central, `q75`, `max`).
#' @param title_suffix Character string to append to the plot title. Defaults to `""`.
#'
#' @return A `plotly` graph object.
#' @export
plot_toc_forecast <- function(forecast_data, title_suffix = "") {
  # Define RGBA colors
  col_red    <- 'rgba(255, 0, 0, 0.2)'
  col_orange <- 'rgba(255, 165, 0, 0.2)'
  col_green  <- 'rgba(0, 255, 0, 0.2)'
  col_blue   <- 'rgba(0, 0, 255, 0.2)'

  # Reference lines
  ref_lines <- c(2, 4, 8)
  hline_shapes <- lapply(ref_lines, function(y_val) {
    list(
      type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = y_val, y1 = y_val, yref = "y",
      line = list(color = "rgba(0, 0, 0, 0.4)", width = 1.5, dash = "dash")
    )
  })

  # Extract creation date
  forecast_date <- if("date" %in% names(forecast_data)) unique(forecast_data$date)[1] + days(1) else "Unknown"
  #check if forecast_date is the sys.date
  if (forecast_date == Sys.Date()) {
    forecast_date <- T
  } else {
    forecast_current = F
  }
  forecast_data <- forecast_data %>% filter(date_24h >= forecast_date) %>% arrange(date_24h)

  # Identify column names (Intake vs Distributed)
  # Intake uses: intake_q_swe_pred_min, etc.
  # Distributed uses: dist_min_pred_toc, etc.

  is_intake <- "intake_q_swe_pred" %in% names(forecast_data)

  if (is_intake) {
    y_col <- "intake_q_swe_pred"
    y_min <- "intake_q_swe_pred_min"
    y_q25 <- "intake_q_swe_pred_q25"
    y_q75 <- "intake_q_swe_pred_q75"
    y_max <- "intake_q_swe_pred_max"
    hover_label <- "Median Prediction"
  } else {
    y_col <- "dist_mean_pred_toc"
    y_min <- "dist_min_pred_toc"
    y_q25 <- "dist_q25_pred_toc"
    y_q75 <- "dist_q75_pred_toc"
    y_max <- "dist_max_pred_toc"
    hover_label <- "Mean Prediction"
  }

  p <- plot_ly(forecast_data, x = ~date_24h) %>%
    add_ribbons(ymin = as.formula(paste0("~", y_q75)), ymax = as.formula(paste0("~", y_max)),
                fillcolor = col_red, line = list(color = 'transparent'),
                showlegend = FALSE, hoverinfo = "none") %>%
    add_ribbons(ymin = as.formula(paste0("~", y_col)), ymax = as.formula(paste0("~", y_q75)),
                fillcolor = col_orange, line = list(color = 'transparent'),
                showlegend = FALSE, hoverinfo = "none") %>%
    add_ribbons(ymin = as.formula(paste0("~", y_q25)), ymax = as.formula(paste0("~", y_col)),
                fillcolor = col_green, line = list(color = 'transparent'),
                showlegend = FALSE, hoverinfo = "none") %>%
    add_ribbons(ymin = as.formula(paste0("~", y_min)), ymax = as.formula(paste0("~", y_q25)),
                fillcolor = col_blue, line = list(color = 'transparent'),
                showlegend = FALSE, hoverinfo = "none") %>%
    add_lines(
      y = as.formula(paste0("~", y_col)),
      line = list(color = "black", width = 2.5),
      name = hover_label,
      text = ~paste0(
        "Date: ", .data[["date_24h"]], "<br>",
        "Max: ", .data[[y_max]], " mg/L<br>",
        "Q75: ", .data[[y_q75]], " mg/L<br>",
        "Median/Mean: ", .data[[y_col]], " mg/L<br>",
        "Q25: ", .data[[y_q25]], " mg/L<br>",
        "Min: ", .data[[y_min]], " mg/L"
      ),
      hovertemplate = "%{text}<extra></extra>"
    ) %>%
    #add a vertical line for today
    add_lines(
      x = c(Sys.Date(), Sys.Date()),
      y = c(min(forecast_data[[y_min]], na.rm = TRUE) - 0.2, max(forecast_data[[y_max]], na.rm = TRUE) + 0.2),
      line = list(color = "black", width = 1.5, dash = "dot"),
      name = "Today",
      hoverinfo = "none"
    ) %>%
    layout(
      title = list(
        text = paste0(title_suffix, "<br><sup>Forecast Created: ", forecast_date, " 3:00 AM MT</sup>"),
        x = 0.5,
        y = 0.95
      ),
      xaxis = list(title = "Date",
                   range = c(
                     as.Date(min(forecast_data$date_24h, na.rm = TRUE)) - hours(6),
                     as.Date(max(forecast_data$date_24h, na.rm = TRUE)) + hours(6)
                   )),
      yaxis = list(
        title = "Predicted TOC (mg/L)",
        range = c(min(forecast_data[[y_min]], na.rm = TRUE) - 0.2, max(forecast_data[[y_max]], na.rm = TRUE) + 0.2)
      ),
      shapes = hline_shapes,
      hovermode = "x unified",
      legend = list(orientation = 'h', y = -0.2),
      margin = list(t = 50, b = 30, l = 50, r = 20)
    ) %>%
    config(displayModeBar = FALSE)

  if(!forecast_current) {
    # add red text saying that the forecast was not generated today
    p <- p %>%
      add_markers(
        data = forecast_data[as.Date(forecast_data$date_24h) == as.Date(forecast_date), ],
        x = ~date_24h,
        y = as.formula(paste0("~", y_col)),
        marker = list(color = "red", size = 10, symbol = "circle"),
        name = "Forecast Start Point",
        showlegend = FALSE,
        hoverinfo = "none"
      ) %>%
      add_annotations(
        text = "NOTE: Forecast was not generated today and may be outdated, due to lack of HEFS Flow Forecast",
        xref = "paper", yref = "paper",
        x = 0.5, y = 1.05,
        showarrow = FALSE,
        font = list(color = "red", size = 14)
      )

  }

  return(p)
}
