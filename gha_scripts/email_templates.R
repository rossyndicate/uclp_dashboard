# ============================================================================
# email_templates.R
# Three email-safe HTML templates for the PDSS weekly report.
# Place in the R/ folder so send_report.R sources it automatically.
#
# All layout is table-based with inline styles (what email clients need).
# Each template takes the same inputs, so switching is a one-word change.
# Image `src` values are either "cid:..." (sending) or "data:image/png;..."
# (browser preview) - see png_data_uri() / resend_inline_attachment() below.
# ============================================================================

# Brand palette (ROSS_lt_pal) - named by role for readability
ross_email_pal <- list(
  navy   = "#002EA3",
  pink   = "#E70870",
  blue   = "#256BF5",
  purple = "#745CFB",
  green  = "#1E4D2B",
  plum   = "#56104E"
)

.font <- "font-family:Arial,Helvetica,sans-serif;"
.ink  <- "#1f2937"   # body text
.mute <- "#6b7280"   # secondary text

# ---- shared pieces ---------------------------------------------------------

.tpl_img <- function(src, alt) {
  glue::glue('<img src="{src}" alt="{alt}" width="640" style="display:block;width:100%;max-width:640px;height:auto;border:1px solid #e5e7eb;border-radius:4px;" />')
}

.tpl_date_range <- function(report_start, report_end) {
  paste0(format(report_start, "%b %d"), " &ndash; ", format(report_end, "%b %d, %Y"))
}

# Section titles / captions / alt text shared by every template
# Order: Weekly Sensor Data, TOC Forecast, Weekly Streamflow, HEFS Streamflow Forecast
.tpl_sections <- function(flow_src, sensor_src, forecast_src, flow_forecast_src) {
  list(
    list(num = 1, title = "Weekly Sensor Data",
         caption = "Distribution of sensor readings by site over the past 7 days. Data passed through auto QAQC filters, so some values may be missing. See the ROSS PDSS Dashboard for the full datasets.",
         img = .tpl_img(sensor_src, "Weekly sensor data plot"), accent = ross_email_pal$purple),
    list(num = 2, title = "TOC Forecast",
         caption = "Forecasted total organic carbon (TOC) at the Poudre River intake.",
         img = .tpl_img(forecast_src, "TOC forecast plot"), accent = ross_email_pal$pink),
    list(num = 3, title = "Weekly Streamflow",
         caption = "Discharge (cfs) at key monitoring sites over the past 7 days.",
         img = .tpl_img(flow_src, "Weekly streamflow plot"), accent = ross_email_pal$blue),
    list(num = 4, title = "HEFS Streamflow Forecast",
         caption = "Forecast of Canyon Mouth streamflow (cfs) over the next 7 days.",
         img = .tpl_img(flow_forecast_src, "Seven-day HEFS streamflow forecast plot"), accent = ross_email_pal$green)
  )
}

.tpl_contact <- function(dashboard_link, link_color, staff_email, text_color = .ink) {
  glue::glue('
<p style="margin:0 0 12px 0;{.font}font-size:14px;line-height:22px;color:{text_color};">
  For further investigation, this data is available on the <a href="{dashboard_link}" style="color:{link_color};font-weight:bold;">ROSS PDSS Dashboard</a>.
</p>
<p style="margin:0;{.font}font-size:14px;line-height:22px;color:{text_color};">
  For questions or issues, please contact the ROSS Project Lead at <a href="mailto:{staff_email}" style="color:{link_color};"> {staff_email}</a>.
</p>
<p style="margin:0;{.font}font-size:14px;line-height:22px;color:{text_color};">
  Best Regards,<br><strong>CSU ROSSyndicate PDSS Team</strong>
</p>')
}

.tpl_page_open <- function(title, preheader, bg) {
  glue::glue('<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>{title}</title>
</head>
<body style="margin:0;padding:0;background-color:{bg};">
<div style="display:none;max-height:0;overflow:hidden;opacity:0;color:{bg};">{preheader}</div>')
}

.tpl_page_close <- "</body>\n</html>"

# ============================================================================
# TEMPLATE 1: "classic" - white card on light gray, navy header, accent bars
# ============================================================================
email_template_classic <- function(flow_src, sensor_src, forecast_src, flow_forecast_src,
                                   dashboard_link, report_start, report_end, staff_email) {
  p <- ross_email_pal
  dates <- .tpl_date_range(report_start, report_end)
  secs <- .tpl_sections(flow_src, sensor_src, forecast_src, flow_forecast_src)

  sections_html <- paste(vapply(secs, function(s) {
    glue::glue('
<tr><td style="padding:30px 32px 0 32px;">
  <table role="presentation" width="100%" cellpadding="0" cellspacing="0" border="0"><tr>
    <td style="border-left:5px solid {s$accent};padding-left:12px;">
      <h2 style="margin:0;{.font}font-size:22px;line-height:28px;color:{p$navy};">{s$title}</h2>
      <p style="margin:4px 0 0 0;{.font}font-size:14px;line-height:20px;color:{.mute};">{s$caption}</p>
    </td>
  </tr></table>
</td></tr>
<tr><td style="padding:14px 32px 0 32px;">{s$img}</td></tr>')
  }, character(1)), collapse = "\n")

  paste0(
    .tpl_page_open("PDSS Weekly Report", paste("Weekly streamflow, sensor, and TOC forecast summary:", dates), "#f2f4f8"),
    glue::glue('
<table role="presentation" width="100%" cellpadding="0" cellspacing="0" border="0" style="background-color:#f2f4f8;">
<tr><td align="center" style="padding:24px 12px;">
<table role="presentation" width="680" cellpadding="0" cellspacing="0" border="0" style="width:100%;max-width:680px;background-color:#ffffff;border-radius:8px;">

  <tr><td bgcolor="{p$navy}" style="background-color:{p$navy};padding:32px 32px 28px 32px;border-radius:8px 8px 0 0;">
    <p style="margin:0 0 8px 0;{.font}font-size:12px;letter-spacing:2px;text-transform:uppercase;color:#c7d4f7;">ROSSyndicate &middot; PDSS</p>
    <h1 style="margin:0;{.font}font-size:30px;line-height:38px;color:#ffffff;">Upper Poudre Decision Support System Weekly Report</h1>
    <p style="margin:10px 0 0 0;{.font}font-size:15px;color:#c7d4f7;">{dates}</p>
  </td></tr>
  <tr><td style="height:5px;line-height:5px;font-size:5px;background-color:{p$pink};">&nbsp;</td></tr>

  {sections_html}

  <tr><td style="padding:34px 32px 0 32px;"><div style="border-top:1px solid #e5e7eb;font-size:0;line-height:0;">&nbsp;</div></td></tr>
  <tr><td style="padding:22px 32px 8px 32px;">{.tpl_contact(dashboard_link, p$blue, staff_email)}</td></tr>
  <tr><td style="padding:16px 32px 28px 32px;">
    <p style="margin:0;{.font}font-size:12px;color:#9ca3af;">Data is preliminary and subject to change.</p>
  </td></tr>

</table>
</td></tr></table>'),
    .tpl_page_close)
}

# ============================================================================
# TEMPLATE 2: "banner" - bold gradient hero, numbered cards, plum footer
# ============================================================================
email_template_banner <- function(flow_src, sensor_src, forecast_src, flow_forecast_src,
                                  dashboard_link, report_start, report_end, staff_email) {
  p <- ross_email_pal
  dates <- .tpl_date_range(report_start, report_end)

  secs <- .tpl_sections(flow_src, sensor_src, forecast_src, flow_forecast_src)
  cards_html <- paste(vapply(secs, function(s) {
    glue::glue('
<tr><td style="padding:20px 24px 0 24px;">
  <table role="presentation" width="100%" cellpadding="0" cellspacing="0" border="0" style="background-color:#ffffff;border:1px solid #dfe4f1;border-top:4px solid {s$accent};border-radius:6px;">
    <tr><td style="padding:18px 20px 4px 20px;">
      <table role="presentation" cellpadding="0" cellspacing="0" border="0"><tr>
        <td width="34" valign="middle">
          <div style="width:30px;height:30px;line-height:30px;border-radius:15px;background-color:{s$accent};color:#ffffff;text-align:center;{.font}font-weight:bold;font-size:15px;">{s$num}</div>
        </td>
        <td valign="middle" style="padding-left:10px;">
          <h2 style="margin:0;{.font}font-size:22px;line-height:28px;color:{p$navy};">{s$title}</h2>
        </td>
      </tr></table>
      <p style="margin:8px 0 0 0;{.font}font-size:14px;line-height:20px;color:{.mute};">{s$caption}</p>
    </td></tr>
    <tr><td style="padding:12px 20px 20px 20px;">{s$img}</td></tr>
  </table>
</td></tr>')
  }, character(1)), collapse = "\n")

  paste0(
    .tpl_page_open("PDSS Weekly Report", paste("Weekly streamflow, sensor, and TOC forecast summary:", dates), "#eef1f8"),
    glue::glue('
<table role="presentation" width="100%" cellpadding="0" cellspacing="0" border="0" style="background-color:#eef1f8;">
<tr><td align="center" style="padding:0 0 24px 0;">
<table role="presentation" width="680" cellpadding="0" cellspacing="0" border="0" style="width:100%;max-width:680px;">
  <tr><td bgcolor="{p$navy}" style="background-color:{p$navy};background-image:linear-gradient(135deg,{p$navy} 0%,{p$purple} 100%);padding:44px 32px 40px 32px;">
    <table role="presentation" cellpadding="0" cellspacing="0" border="0"><tr>
      <td bgcolor="{p$pink}" style="background-color:{p$pink};border-radius:14px;padding:5px 14px;{.font}font-size:12px;font-weight:bold;letter-spacing:1px;text-transform:uppercase;color:#ffffff;">Weekly Report</td>
    </tr></table>
    <h1 style="margin:16px 0 0 0;{.font}font-size:32px;line-height:40px;color:#ffffff;">Upper Poudre Decision Support System</h1>
    <p style="margin:10px 0 0 0;{.font}font-size:16px;color:#dbe4ff;">{dates}</p>
  </td></tr>{cards_html}

  <tr><td style="padding:24px 0 0 0;">
    <table role="presentation" width="100%" cellpadding="0" cellspacing="0" border="0">
      <tr><td bgcolor="{p$plum}" style="background-color:{p$plum};padding:28px 32px 24px 32px;">
        {.tpl_contact(dashboard_link, "#ffffff", staff_email)}
        <p style="margin:20px 0 0 0;{.font}font-size:12px;color:#d6b4d1;">Data is preliminary and subject to change.</p>
      </td></tr>
    </table>
  </td></tr>
</table>
</td></tr></table>'),
    .tpl_page_close
  )
}

# ============================================================================
# TEMPLATE 3: "minimal" - white, six-color brand strip, editorial layout
# ============================================================================
email_template_minimal <- function(flow_src, sensor_src, forecast_src, flow_forecast_src,
                                   dashboard_link, report_start, report_end, staff_email) {
  p <- ross_email_pal
  dates <- .tpl_date_range(report_start, report_end)
  secs <- .tpl_sections(flow_src, sensor_src, forecast_src, flow_forecast_src)

  strip <- paste(vapply(unlist(p, use.names = FALSE), function(col) {
    glue::glue('<td width="16.66%" bgcolor="{col}" style="background-color:{col};height:8px;line-height:8px;font-size:8px;">&nbsp;</td>')
  }, character(1)), collapse = "")

  sections_html <- paste(vapply(secs, function(s) {
    glue::glue('
<tr><td style="padding:34px 0 0 0;">
  <p style="margin:0 0 4px 0;{.font}font-size:12px;font-weight:bold;letter-spacing:2px;text-transform:uppercase;color:{s$accent};">Section {s$num}</p>
  <h2 style="margin:0;{.font}font-size:22px;line-height:28px;color:{p$navy};">{s$title}</h2>
  <p style="margin:4px 0 14px 0;{.font}font-size:14px;line-height:20px;color:{.mute};">{s$caption}</p>
  {s$img}
</td></tr>')
  }, character(1)), collapse = "\n")

  paste0(
    .tpl_page_open("PDSS Weekly Report", paste("Weekly streamflow, sensor, and TOC forecast summary:", dates), "#ffffff"),
    glue::glue('
<table role="presentation" width="100%" cellpadding="0" cellspacing="0" border="0" style="background-color:#ffffff;">
<tr><td align="center" style="padding:0 12px 32px 12px;">

<table role="presentation" width="100%" cellpadding="0" cellspacing="0" border="0"><tr>{strip}</tr></table>

<table role="presentation" width="640" cellpadding="0" cellspacing="0" border="0" style="width:100%;max-width:640px;">
  <tr><td style="padding:40px 0 0 0;">
    <h1 style="margin:0;{.font}font-size:32px;line-height:40px;color:{p$navy};">Upper Poudre Decision Support System</h1>
    <p style="margin:6px 0 0 0;{.font}font-size:20px;line-height:28px;color:{p$pink};">Weekly Report</p>
    <p style="margin:10px 0 0 0;{.font}font-size:14px;color:{.mute};">{dates}</p>
  </td></tr>

  {sections_html}

  <tr><td style="padding:40px 0 0 0;">
    <div style="border-top:2px solid {p$navy};font-size:0;line-height:0;">&nbsp;</div>
  </td></tr>
  <tr><td style="padding:20px 0 0 0;">{.tpl_contact(dashboard_link, p$navy, staff_email)}</td></tr>
  <tr><td style="padding:20px 0 0 0;">
    <p style="margin:0;{.font}font-size:12px;color:#9ca3af;">Data is preliminary and subject to change.</p>
  </td></tr>
</table>

</td></tr></table>'),
    .tpl_page_close)
}

# ---- dispatcher -------------------------------------------------------------
build_email_html <- function(template = c("classic", "banner", "minimal"), ...) {
  template <- match.arg(template)
  as.character(switch(template,
                      classic = email_template_classic(...),
                      banner  = email_template_banner(...),
                      minimal = email_template_minimal(...)
  ))
}

# ---- image helpers ------------------------------------------------------------
# Browser preview: embed the PNG directly in the HTML
png_data_uri <- function(path) {
  paste0("data:image/png;base64,", base64enc::base64encode(path))
}

# Sending: Resend inline attachment, referenced in the HTML as src="cid:<content_id>"
# (Gmail and several other clients do NOT render base64 data: URIs in <img>)
resend_inline_attachment <- function(path, content_id) {
  list(
    filename = paste0(content_id, ".png"),
    content = base64enc::base64encode(path),
    content_id = content_id
  )
}

# Write HTML to disk and open it in the default browser
write_email_preview <- function(html, file = "email_preview.html", open = interactive()) {
  writeLines(html, file, useBytes = TRUE)
  message("Email preview written to: ", normalizePath(file))
  if (open) utils::browseURL(normalizePath(file))
  invisible(file)
}
