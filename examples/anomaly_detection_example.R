library(tidyverse)
library(plotly)
library(readr)
library(waterQUAC)

#example Total Suspended Solids dataframe
df <- waterQUAC::TSS_data


#overwritable QC codes, all else are retained. In this case, all codes will be overwritten
manual_codes = c(1:4000)

#upper and lower limits for the sensor uses (Trios Opus)
tst <- ts_anom(df = df,
               overwrite = manual_codes,
               window = 12,
               sensorMin = 0,
               sensorMax = 650,
               diag = TRUE)

# Define the mapping of Quality codes to colors
quality_colors <- c(
  "OK" = "#33b2ff",
  "above_limits" = "#5f1937",
  "below_limits" = "#5f1937",
  "spike" = "orange",
  "repeating_value" = "purple",
  "sensor_drift" = "brown",
  "impossible" = "red",
  "Unknown" = "grey"
)
# plot
plotly::plot_ly(tst[1:30000,]) |>
  plotly::add_markers(
    x =  ~ts,
    y =  ~Value,
    type = "scatter",
    color = ~Quality,
    colors = quality_colors
    #name = "Sensor values"
  ) |>
  plotly::add_lines(
    x = ~ts,
    y = ~median,
    line = list(color = 'black', dash = "dot"),
    name = "Rolling Median"
  ) |>
  plotly::add_lines(
    x = ~ts,
    y = ~median + 4 * sd,
    line = list(color = 'red', dash = "dash", width = 1),
    name = "+4 SD"
  ) |>
  plotly::add_lines(
    x = ~ts,
    y = ~median - 4 * sd,
    line = list(color = 'red', dash = "dash", width = 1),
    name = "-4 SD"
  )


# Example usage of the detect_sensor_drift function
result <- detect_sensor_drift(tst[1:30000,],
                              value_col = "Value",
                              overwrite = c('OK', 'spike', 'impossible'))
head(result)



plotly::plot_ly(result) |>
  plotly::add_markers(
    x = ~ts,
    y = ~Value,
    type = "scatter",
    color = ~Quality,
    colors = quality_colors
  ) |>
  plotly::add_lines(
    x = ~ts,
    y = ~median,
    line = list(color = 'black', dash = "dot"),
    name = "Rolling Median"
  ) |>
  plotly::add_lines(
    x = ~ts,
    y = ~median + 4 * sd,
    line = list(color = 'red', dash = "dash", width = 1),
    name = "+4 SD"
  ) |>
  plotly::add_lines(
    x = ~ts,
    y = ~median - 4 * sd,
    line = list(color = 'red', dash = "dash", width = 1),
    name = "-4 SD"
  ) |>
  plotly::layout(
    title = "Water Quality Sensor; Auto-QC Example",
    xaxis = list(title = "Timestamp"),
    yaxis = list(title = "Total Suspended Solids (mg/L)")
  )

