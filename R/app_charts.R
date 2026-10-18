# Colours and the comparison waterfall -----------------------------------------
#
# Three roles, validated for colour-vision deficiency in both modes:
#   neighbour = the n nearest (map dots, spokes, "typical neighbour" bar)
#   subject   = the selected property (map dot, "this property" bar)
#   context   = everything else (background dots, step bars)
# The same colour means the same thing on the map and in the chart.

palette_for <- function(mode = "light") {
  if (identical(mode, "dark")) {
    list(
      neighbour = "#3987e5", subject = "#d95926", context = "#5f6870",
      surface = "#161c21", ink = "#e9edf0", ink2 = "#b4bec7", muted = "#87909a", grid = "#2a3239"
    )
  } else {
    list(
      neighbour = "#2a78d6", subject = "#eb6834", context = "#8a939c",
      surface = "#ffffff", ink = "#17212b", ink2 = "#4a5763", muted = "#87909a", grid = "#e3e7eb"
    )
  }
}

waterfall_plot <- function(steps, mode = "light") {
  col <- palette_for(mode)
  ticks <- c("Typical<br>neighbour", "Floor<br>area", "Rate<br>per m\u00b2", "Other<br>items", "This<br>property")
  x <- factor(ticks, levels = ticks)
  fill <- ifelse(steps$measure == "relative", col$context,
    ifelse(steps$measure == "absolute", col$neighbour, col$subject)
  )
  label <- ifelse(steps$measure == "relative", fmt_eur(steps$value, signed = TRUE), fmt_eur(steps$value))
  typical <- attr(steps, "typical")
  hover <- c(
    sprintf(
      "Typical neighbour: %s<br>%s at %s, the median of %d neighbours",
      fmt_eur(steps$value[1]), fmt_area(typical$area_m2), fmt_rate(typical$rate), typical$n
    ),
    sprintf("Floor area: %s<br>Difference in size, valued at the typical rate", fmt_eur(steps$value[2], TRUE)),
    sprintf("Rate per m\u00b2: %s<br>Difference in rate, across this property's area", fmt_eur(steps$value[3], TRUE)),
    sprintf(
      "Other items and rounding: %s<br>Additional items, less rounding of the official figure",
      fmt_eur(steps$value[4], TRUE)
    ),
    sprintf("This property: %s<br>Official valuation (NAV)", fmt_eur(steps$value[5]))
  )

  # Dashed connectors from the top of each running total to the next bar
  connectors <- lapply(seq_len(nrow(steps) - 1), function(i) {
    list(
      type = "line", xref = "x", yref = "y",
      x0 = i - 1 + 0.32, x1 = i - 0.32, y0 = steps$end[i], y1 = steps$end[i],
      line = list(color = col$muted, width = 1, dash = "dot")
    )
  })

  plotly::plot_ly(
    x = x, y = steps$end - steps$start, base = steps$start, type = "bar",
    marker = list(color = fill, line = list(width = 0)),
    text = label, textposition = "outside", cliponaxis = FALSE,
    textfont = list(color = col$ink2, size = 12),
    hovertext = hover, hoverinfo = "text",
    hoverlabel = list(bgcolor = col$surface, bordercolor = col$grid, font = list(color = col$ink))
  ) |>
    plotly::layout(
      bargap = 0.36,
      uniformtext = list(minsize = 12, mode = "show"),
      shapes = connectors,
      showlegend = FALSE,
      font = list(family = "Instrument Sans, system-ui, sans-serif", color = col$ink2, size = 12),
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      margin = list(l = 8, r = 8, t = 28, b = 8),
      xaxis = list(title = "", tickfont = list(color = col$ink2), tickangle = 0, fixedrange = TRUE, showgrid = FALSE),
      yaxis = list(
        title = "", tickprefix = EURO, separatethousands = TRUE, gridcolor = col$grid,
        zeroline = TRUE, zerolinecolor = col$muted, rangemode = "tozero",
        tickfont = list(color = col$muted), fixedrange = TRUE, automargin = TRUE
      )
    ) |>
    plotly::config(displayModeBar = FALSE)
}
