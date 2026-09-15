library(arrow)
library(ggplot2)
library(dplyr)
library(lubridate)
library(httr2)
library(base64enc)
library(tidyverse)
library(xgboost)
library(glue)
library(padr)
library(cdssr)

`%nin%` <- Negate(`%in%`)

# Source custom functions
walk(list.files("R", full.names = TRUE), source)
#setup ross theme

require(tidyverse)
require(ggthemes)
# basic theme for all ggplots, if Roboto is not installed, just use default, but message
if ({
  require(systemfonts)
  ("Roboto" %in% system_fonts()$family)
}) {
  ROSS_theme <- theme_bw() + #or theme_few()
    theme(plot.title = element_text(hjust = 0.5, face = 'bold', family = "Roboto"),
          plot.subtitle = element_text(hjust = 0.5, family = "Roboto"))
} else {
  message("You do not have the Roboto font family installed on your computer, currenly using ggplot default text family.
          See ROSS_themes.R for directions to install the font family on your computer.")
  ROSS_theme <- theme_bw() + #or theme_few()
    theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
          plot.subtitle = element_text(hjust = 0.5))
}

# --- 0. Secrets / config (pulled from GitHub Actions env vars) ---
required_secrets <- c("CDWR_API_KEY", "RESEND_API_KEY", "EMAIL_FROM", "EMAIL_TO", "DASHBOARD_LINK")
missing <- required_secrets[Sys.getenv(required_secrets) == ""]
if (length(missing) > 0) {
  stop("Missing required secrets: ", paste(missing, collapse = ", "))
}

cdwr_api_key <- Sys.getenv("CDWR_API_KEY")
resend_api_key <- Sys.getenv("RESEND_API_KEY")
email_from <- Sys.getenv("EMAIL_FROM")
email_to <- Sys.getenv("EMAIL_TO")
dashboard_link <- Sys.getenv("DASHBOARD_LINK")

message("=== Starting weekly report generation ===")

# --- 0b. Timezone-safe "now" and "today" ---
# Sys.Date() reflects the RUNNER's system timezone (UTC on GitHub Actions),
# not America/Denver, so derive today's date from an explicit MT timestamp instead.
now_mt <- with_tz(Sys.time(), tzone = "America/Denver")
today_mt <- as.Date(now_mt)

# Units and Colors
site_table <- tibble(
  site_code = c("sfm", "chd", "pfal", "pbr_fc", "pman_fc", "pbd"),
  site_name = c("South Fork CLP", "Chambers Lake Outflow", "CLP at Poudre Falls",
                "CLP at Indian Meadows", "CLP at Manners Bridge", "Canyon Mouth"),
  color = c("#002EA3", "#E70870", "#256BF5", "#56104E", "#FFCA3A", "#1E4D2B")
)

plot_param_table <- tibble(
  parameter = c("Temperature", "Turbidity", "pH", "DO",
                "Specific Conductivity", "Chl-a Fluorescence", "FDOM Fluorescence", "Depth",
                "TOC"),
  units = c("°C", "NTU", "", "mg/L", "µS/cm", "RFU", "RFU", "ft", "mg/L")
)

#---- Flow Plot ----
message("Step: Fetching CDWR flow data for streamflow plot...")

flow_sites <- c("CLASRKCO", "CLAFTCCO", "JWCCHACO", "CLANSECO", "MUNCANCO")

end_date <- as.character(Sys.Date() + days(1))
start_date <- as.character(Sys.Date() - days(7))

flow_data <- map_dfr(flow_sites, function(site_row) {
  site_id <- site_row
  param_code <- "DISCHRG"

  message(glue("  Fetching flow data for site: {site_id}"))

  tryCatch({
    cdssr::get_telemetry_ts(
      abbrev = site_id,
      start_date = start_date,
      end_date = end_date,
      api_key = cdwr_api_key,
      timescale = "hour"
    ) %>%
      select(DT_round = datetime, flow = meas_value, abbrev)
  }, error = function(e) {
    message(glue("  WARNING: Failed to fetch flow data for {site_id}: {conditionMessage(e)}"))
    tibble(DT_round = as.POSIXct(character()), flow = numeric(), abbrev = character())
  })

})%>%
  mutate(site_name = case_when(
    abbrev == "CLASRKCO" ~ "South Fork CLP",
    abbrev == "CLAFTCCO" ~ "CLP @ Canyon Mouth",
    abbrev == "JWCCHACO" ~ "Chambers Lake Outflow",
    abbrev == "CLANSECO" ~ "North Fork below Seaman Res",
    abbrev == "MUNCANCO" ~ "Munroe Canal"
  ))%>%
  filter(DT_round >= Sys.Date() - days(7))

if (nrow(flow_data) == 0) {
  message("WARNING: No flow data was retrieved for any site — flow plot will be empty.")
} else {
  message(glue("  Retrieved {nrow(flow_data)} flow records across {n_distinct(flow_data$abbrev)} site(s)."))
}

message("Step: Building streamflow plot...")

p_flow <- ggplot(flow_data, aes(x = DT_round, y = flow, color = site_name)) +
  geom_line()+
  scale_color_manual(values = c("South Fork CLP" = "#256BF5",
                                "CLP @ Canyon Mouth" = "#002EA3",
                                "Chambers Lake Outflow" = "#1E4D2B",
                                "North Fork below Seaman Res" = "#E70870",
                                "Munroe Canal" = "#56104E")) +
  labs(x = "Date",
       y = "Discharge (cfs)",
       color = "Site") +
  ROSS_theme+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_flow

png_flow_path <- tempfile(fileext = ".png")
tryCatch({
  ggsave(png_flow_path, p_flow, width = 8, height = 5, dpi = 300)
}, error = function(e) {
  stop(glue("Failed to save streamflow plot: {conditionMessage(e)}"))
})

flow_img_b64 <- base64enc::base64encode(png_flow_path)
flow_img_tag <- sprintf('<img src="data:image/png;base64,%s" style="max-width:100%%;" />', flow_img_b64)
message("Step: Streamflow plot complete.")


# --- Pull any data straight from the raw GitHub URL ---
message("Step: Downloading data files from GitHub...")

data_repo_url <- "https://github.com/rossyndicate/uclp_dashboard/raw/main/data/"
snapshot_url <- paste0(data_repo_url, "data_backup.parquet")
intake_forecast_url <- paste0(data_repo_url, "toc_forecast_intake_backup.parquet")

message("  Downloading TOC xgboost model folds (1-4)...")
toc_realtime_model <- map(1:4, function(fold) {
  url <- paste0(
    "https://raw.githubusercontent.com/rossyndicate/uclp_dashboard/main/data/models/",
    "ross_only_toc_xgboost_model_fold", fold, "_20260715.ubj"
  )
  local_path <- tempfile(fileext = ".ubj")

  tryCatch({
    download.file(url, local_path, mode = "wb", quiet = TRUE)
    xgb.load(modelfile = local_path)
  }, error = function(e) {
    stop(glue("Failed to download/load TOC model fold {fold} from {url}: {conditionMessage(e)}"))
  })
})
message("  All 4 TOC model folds loaded successfully.")

scaling_params_file_path <- paste0(data_repo_url, "models/scaling_params_toc_20260715.parquet")

message("  Downloading sensor data snapshot...")
sensor_df <- tryCatch({
  read_parquet(snapshot_url)
}, error = function(e) {
  stop(glue("Failed to download/read sensor data snapshot from {snapshot_url}: {conditionMessage(e)}"))
})
message(glue("  Sensor snapshot loaded: {nrow(sensor_df)} rows."))

#Clean up sensor data for plotting and TOC estimation
message("Step: Cleaning and QAQC'ing sensor data...")
cleaned_sensor_df <- sensor_df %>%
  mutate(DT_round_MT = with_tz(DT_round, tzone = "America/Denver")) %>%
  # trim to the last week (timezone-safe cutoff)
  filter(DT_round_MT >= today_mt - days(7)) %>%
  mutate(
    mean = ifelse(!is.na(mal_flag), NA, mean),
    mean = if_else(units == "m", mean * 3.28084, mean), # convert depth to feet
    units = if_else(units == "m", "ft", units)
  ) %>%
  select(-units)%>%
  # Apply QAQC/Summarization
  apply_cleaning_filters(df = ., new_value_col = "mean_cleaned") %>%
  apply_interpolation_missing_data(df = ., value_col = "mean_cleaned", dt_col = "DT_round_MT",
                                   method = "linear", max_gap = 4) %>%
  apply_low_pass_binomial_filter(df = ., value_col = "mean_filled", new_value_col = "mean_smoothed",
                                 dt_col = "DT_round_MT") %>%
  mutate(DT_round = as.POSIXct(DT_round_MT)) %>%
  apply_timestep_median(df = ., value_col = "mean_smoothed", new_value_col = "timestep_median",
                        timestep = "1 hour", dt_col = "DT_round") %>%
  select(DT_round = DT_group, site, parameter, mean = timestep_median) %>%
  distinct(site, parameter, mean, DT_round, .keep_all = TRUE)

message(glue("  Sensor data cleaned: {nrow(cleaned_sensor_df)} rows remain."))

#Pull in Canyon Mouth Flows
message("Step: Fetching Canyon Mouth flow data for TOC model...")
min_date <- as.Date(min(cleaned_sensor_df$DT_round, na.rm = TRUE)) - days(1)
max_date <- as.Date(max(cleaned_sensor_df$DT_round, na.rm = TRUE)) + days(1)

canyon_q_res <- tryCatch({
  cdssr::get_telemetry_ts(
    abbrev = "CLAFTCCO",
    start_date = min_date,
    end_date = max_date,
    api_key = cdwr_api_key,
    timescale = "hour"
  ) %>%
    mutate(date = as.Date(force_tz(datetime, tz = "America/Denver"))) %>%
    summarize(canyon_mouth_daily_flow_cfs = mean(meas_value, na.rm = TRUE), .by = date)
}, error = function(e) {
  stop(glue("Failed to fetch Canyon Mouth flow data (CLAFTCCO): {conditionMessage(e)}"))
})
message(glue("  Canyon Mouth flow data retrieved: {nrow(canyon_q_res)} daily records."))

# Generate TOC estimates
message("Step: Generating real-time TOC estimates from xgboost models...")
realtime_toc <- tryCatch({
  apply_toc_model(
    sensor_data = cleaned_sensor_df %>% filter(site %in% c("sfm", "chd", "pfal", "pbd")),
    toc_models = toc_realtime_model,
    scaling_params_file_path = scaling_params_file_path,
    summarize_interval = "1 hour",
    time_col = "DT_round",
    value_col = "mean",
    canyon_q_data = canyon_q_res,
    timeseries = TRUE
  ) %>%
    mutate(TOC_guess_ensemble = round(TOC_guess_ensemble, 2))
}, error = function(e) {
  stop(glue("Failed to generate TOC estimates: {conditionMessage(e)}"))
})
message(glue("  TOC estimates generated: {nrow(realtime_toc)} rows."))

final_sensor_df <- cleaned_sensor_df %>%
  bind_rows(
    realtime_toc %>%
      select(DT_round, site, mean = TOC_guess_ensemble) %>%
      mutate(parameter = "TOC")
  ) %>%
  left_join(site_table, by = c("site" = "site_code")) %>%
  left_join(plot_param_table, by = c("parameter" = "parameter")) %>%
  mutate(param_units = paste0(parameter, " (", units, ")")) %>%
  mutate(
    parameter = factor(parameter, levels = c(
      "Turbidity", "TOC", "Specific Conductivity", "pH", "DO",
      "Temperature", "Depth", "Chl-a Fluorescence", "FDOM Fluorescence"
    )),
    param_units = factor(param_units, levels = unique(param_units[order(parameter)])),
    site_name = factor(site_name, levels = site_table$site_name),
    date = as.Date(DT_round)
  ) %>%
  filter(parameter %nin% c("Depth", "Chl-a Fluorescence", "FDOM Fluorescence"))

message("Step: Building sensor data plot...")
p_sensor_data <- ggplot(final_sensor_df, aes(x = site_name, y = mean)) +
  geom_boxplot(aes(fill = site_name), alpha = 0.7) +
  scale_fill_manual(values = site_table$color) +
  facet_wrap(~param_units, scales = "free_y",
             labeller = label_wrap_gen(width = 10), ncol = 2) +
  labs(
    title = paste0("Weekly Sensor Data Update: ", min(final_sensor_df$date), " to ", max(final_sensor_df$date)),
    x = "Site",
    y = "Sensor Value"
  ) +
  ROSS_theme +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold")
  )

png_sensor_path <- tempfile(fileext = ".png")
tryCatch({
  ggsave(png_sensor_path, p_sensor_data, width = 7, height = 7, dpi = 300)
}, error = function(e) {
  stop(glue("Failed to save sensor data plot: {conditionMessage(e)}"))
})

sensor_img_b64 <- base64enc::base64encode(png_sensor_path)
sensor_img_tag <- sprintf('<img src="data:image/png;base64,%s" style="max-width:100%%;" />', sensor_img_b64)
message("Step: Sensor data plot complete.")

# --- Forecast plot ---
col_red    <- "red"
col_orange <- "orange"
col_green  <- "green"
col_blue   <- "blue"

message("Step: Downloading TOC forecast data...")
forecast_data <- tryCatch({
  read_parquet(intake_forecast_url) %>%
    mutate(date_24h = with_tz(date_24h, tzone = "America/Denver")) %>%
    mutate(date_24h = as.Date(date_24h)) %>%
    filter(date == max(date, na.rm = TRUE) & date_24h <= today_mt + days(7)) %>%
    arrange(date_24h)
}, error = function(e) {
  stop(glue("Failed to download/read TOC forecast data from {intake_forecast_url}: {conditionMessage(e)}"))
})
message(glue("  Forecast data loaded: {nrow(forecast_data)} rows."))

forecast_date <- force_tz(unique(forecast_data$date)[1], tz = "America/Denver")
forecast_current <- forecast_date == today_mt

forecast_gen_time <- ymd_hms(paste0(forecast_date, " 3:00:00"), tz = "America/Denver")

y_min_val <- min(forecast_data$intake_q_swe_pred_min, na.rm = TRUE)
y_max_val <- max(forecast_data$intake_q_swe_pred_max, na.rm = TRUE)

message("Step: Building TOC forecast plot...")
title_suffix <- "Poudre River Intake TOC Forecast"
p_forecast <- ggplot(forecast_data, aes(x = date_24h)) +
  geom_ribbon(aes(ymin = intake_q_swe_pred_q75, ymax = intake_q_swe_pred_max), fill = col_red, alpha = 0.2) +
  geom_ribbon(aes(ymin = intake_q_swe_pred, ymax = intake_q_swe_pred_q75), fill = col_orange, alpha = 0.2) +
  geom_ribbon(aes(ymin = intake_q_swe_pred_q25, ymax = intake_q_swe_pred), fill = col_green, alpha = 0.2) +
  geom_ribbon(aes(ymin = intake_q_swe_pred_min, ymax = intake_q_swe_pred_q25), fill = col_blue, alpha = 0.2) +
  geom_line(aes(y = intake_q_swe_pred), color = "black", linewidth = 1) +
  geom_vline(xintercept = with_tz(today_mt, tz = "America/Denver"), linetype = "dotted",
             color = "black", linewidth = 0.7) +
  annotate("text", x = with_tz(today_mt + hours(6), tz = "America/Denver"), y = y_min_val,
           label = "Today", color = "black", size = 4, hjust = 0) +
  geom_hline(yintercept = c(2, 4, 8), linetype = "dashed", color = alpha("black", 0.4), linewidth = 0.5) +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 day", name = "Date") +
  scale_y_continuous(limits = c(y_min_val - 0.2, y_max_val + 0.2), name = "Predicted TOC (mg/L)") +
  labs(title = title_suffix, subtitle = paste0("Forecast Created: ", forecast_date, " 3:00 AM MT")) +
  ROSS_theme+
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none"
  )

if (!forecast_current) {
  start_pt <- forecast_data[as.Date(forecast_data$date_24h) == as.Date(forecast_date), ]

  p_forecast <- p_forecast +
    geom_point(data = start_pt, aes(y = intake_q_swe_pred), color = "red", size = 3) +
    annotate(
      "text",
      x = with_tz(today_mt + hours(12), tz = "America/Denver"),
      y = y_max_val + 0.15,
      label = "NOTE: Forecast was not generated today\nRed point indicates forecast starting point.",
      color = "red", size = 4, hjust = 0
    )
}

png_forecast_path <- tempfile(fileext = ".png")
tryCatch({
  ggsave(png_forecast_path, p_forecast, width = 8, height = 6, dpi = 300)
}, error = function(e) {
  stop(glue("Failed to save TOC forecast plot: {conditionMessage(e)}"))
})

forecast_img_b64 <- base64enc::base64encode(png_forecast_path)
forecast_img_tag <- sprintf('<img src="data:image/png;base64,%s" style="max-width:100%%;" />', forecast_img_b64)
message("Step: TOC forecast plot complete.")


# --- Compose the HTML email ---
message("Step: Composing HTML email body...")
html_body <- sprintf('
  <html>
    <body style="font-family: sans-serif;">
      <h1 style="font-size:28px; margin-bottom:4px;">Automated Upper Poudre Decision Support System Weekly Report</h1>
      <h2 style="font-size:20px; margin-top:24px; margin-bottom:8px; color:#333;">Summary of Weekly Streamflow</h2>
      <p>%s</p>
      <h2 style="font-size:20px; margin-top:24px; margin-bottom:8px; color:#333;">Summary of Weekly Sensor Data</h2>
      <p>%s</p>
      <h2 style="font-size:20px; margin-top:24px; margin-bottom:8px; color:#333;">Summary of TOC Forecast</h2>
      <p>%s</p>
      <p style="font-size:14px; color:#333; margin-top:24px; line-height:1.5;">
        If you have any questions, please contact the ROSS team (Sam Struthers: <a href="mailto:samuel.struthers@colostate.edu">samuel.struthers@colostate.edu</a> or Daniel Duncan: <a href="mailto:d.duncan@colostate.edu">d.duncan@colostate.edu</a>).<br>
        This data is also available on the <a href="%s">ROSS PDSS Dashboard</a>.
      </p>
      <p style="font-size:14px; color:#333; margin-top:24px; line-height:1.5;">
        Best Regards,<br>
        ROSSyndicate PDSS Team
      </p>
      <p style="color:#888;font-size:12px; margin-top:32px;">Generated automatically via GitHub Actions.</p>
    </body>
  </html>', flow_img_tag, sensor_img_tag, forecast_img_tag, dashboard_link)



# --- Send via Resend API ---
message("Step: Sending email via Resend API...")

req <- request("https://api.resend.com/emails") |>
  req_auth_bearer_token(resend_api_key) |>
  req_body_json(list(
    from = email_from,
    to = email_to,
    subject = paste("Weekly Report -", today_mt),
    html = html_body
  ))

resp <- tryCatch({
  req_perform(req)
}, error = function(e) {
  stop(glue("Failed to send email via Resend API: {conditionMessage(e)}"))
})

status <- resp_status(resp)
if (status >= 200 && status < 300) {
  message(glue("Success: Email sent via Resend (status {status})."))
} else {
  stop(glue("Resend API returned non-success status {status}: {resp_body_string(resp)}"))
}
