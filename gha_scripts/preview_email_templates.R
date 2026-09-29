# ============================================================================
# preview_email_templates.R
# Renders all three email templates with placeholder plots so you can compare
# styles in your browser. No API calls, no secrets, nothing is sent.
#
# Run from the project root:  source("preview_email_templates.R")
# ============================================================================
library(ggplot2)
library(glue)
library(base64enc)

source("gha_scripts/email_templates.R")

# Placeholder plots (swap for real ones any time by pointing at your PNGs)
set.seed(1)
site_cols <- c("#002EA3", "#E70870", "#256BF5", "#56104E", "#FFCA3A", "#1E4D2B")
dummy <- data.frame(
  x = rep(1:7, 6), site = rep(LETTERS[1:6], each = 7),
  y = as.vector(replicate(6, cumsum(rnorm(7)) + 10))
)
p_dummy <- ggplot(dummy, aes(x, y, color = site)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = site_cols) +
  labs(title = "Placeholder plot", x = "Day", y = "Value") +
  theme_minimal()

dummy_png <- tempfile(fileext = ".png")
ggsave(dummy_png, p_dummy, width = 8, height = 5, dpi = 150)
img <- png_data_uri(dummy_png)

dir.create("email_preview", showWarnings = FALSE)
for (tpl in c("classic", "banner", "minimal")) {
  html <- build_email_html(
    template = tpl,
    flow_src = img, sensor_src = img, forecast_src = img, flow_forecast_src = img,
    dashboard_link = "https://example.com/dashboard",
    report_start = Sys.Date() - 7, report_end = Sys.Date(),
    staff_email = "staff@example.com"

  )
  write_email_preview(html, file = file.path("email_preview", paste0(tpl, ".html")))
}
