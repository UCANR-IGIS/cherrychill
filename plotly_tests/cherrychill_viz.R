samp_chill_tbl <- readRDS(here::here("plotly_tests/samp_chill_tbl.rds"))
head(samp_chill_tbl)

library(plotly)

samp_chill_tbl |>
  plot_ly(x = ~date_time) |>
  add_lines(y = ~bark_chill_cum, name = "Cherry Bark Chill") |>
  add_lines(y = ~air_chill_cum, name = "Air Chill") |>
  plotly::layout(title = list(text = "Cherry Chill<br><sup>Location:</sup>", x = 0.5),
                 xaxis = list(title = ""),
                 yaxis = list(title = "chill portions"),
                 hovermode = 'x',
                 legend = list(orientation = "h",   # show entries horizontally
                               xanchor = "center",  # use center of legend as anchor
                               x = 0.5)             # put legend in center of x-axis
                 ) |>
  config(
    displayModeBar = TRUE,
    staticPlot = FALSE,       ## default
    showTips = FALSE,         ## ??
    displaylogo = FALSE       ## don't show the plotly logo
  )


