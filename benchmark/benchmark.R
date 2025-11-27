# Load required libraries
library(plotly)
library(readr)
library(dplyr)
library(waterQUAC)
library(yardstick)

# Import the CSV data
cat("Loading CSV data...\n")
df <- read_csv("benchmark/data/1160116_tsseq.csv")

# Convert value column to numeric (it may have been read as character due to NULL values)
df$value <- as.numeric(df$value)

# Convert timestamp to POSIXct for proper plotting
df$ts <- as.POSIXct(df$ts)

print(unique(df$desc))

# Create a plotly scatter plot colored by quality_type
p <- plot_ly(data = df,
             x = ~ts,
             y = ~value,
             color = ~desc,
             type = 'scatter',
             mode = 'markers',
             text = ~paste("Quality:", desc,
                           "<br>Station:", station_id,
                           "<br>Parameter:", parameter,
                           "<br>Units:", units),
             hovertemplate = "%{text}<br>Time: %{x}<br>Value: %{y}<extra></extra>") %>%
  layout(title = "Nitrate Measurements Over Time (Colored by Quality Type)",
         xaxis = list(title = "Timestamp"),
         yaxis = list(title = "Nitrate as N (mg/L)"),
         showlegend = TRUE)
p



df <- df %>%
  mutate(
    q_truth = case_when(
      # Good / OK
      desc %in% c("WQ - Good - Verified",
                  "[LEGACY] WQ - Good - Checked",
                  "Q - Good - Checked")
      ~ "OK",
      # Sensor drift
      desc %in% c("WQ - Bad - Sensor drift")
      ~ "sensor_drift",
      # Spikes (manual)
      desc %in% c("Q - Bad - Cluster of spikes",
                  "WQ - Bad - Large spike",
                  "WQ - Bad - Small spike",
                  "WQ - Bad - Auto (Large spike)",
                  "WQ - Bad - Auto (Small spike)",
                  "WQ - Uncertain - Possible Spike")

      ~ "spike",
      # Other anomalies
      desc %in% c("WQ - Bad",
                  "[LEGACY] Q - Bad - Dry Read",
                  "WQ - Bad - Equipment out of water",
                  "WQ - Bad - High variability")
      ~ "anomaly",
      # Uncertain
      desc %in% c(#"WQ - Uncertain - Unchecked",
                  "Sys - Uncertain - Null Value",
                  "WQ - Uncertain - Outside probe limits",
                  "WQ - Uncertain - Possible Instrument Failure")
      ~ "Uncertain",
      # Missing
      desc %in% c(
                  "NULL",
                  "WQ - Uncertain - Unchecked",
                  "Q - Uncertain - Unchecked")
      ~ "OK",
      # Catch-all
      TRUE ~ "other"
    )
  )

# Remove rows with NA values for cleaner plotting
df_clean <- df %>%
  filter(!is.na(value),
         ts > '2020-12-30',
         ts < '2024-11-24',
         q_truth != 'Uncertain') %>%
  arrange(ts)

# Create a plotly scatter plot colored by quality_type
p <- plot_ly(data = df_clean,
             x = ~ts,
             y = ~value,
             color = ~q_truth,
             type = 'scatter',
             mode = 'markers',
             text = ~paste("Quality:", desc,
                          "<br>Station:", station_id,
                          "<br>Parameter:", parameter,
                          "<br>Units:", units),
             hovertemplate = "%{text}<br>Time: %{x}<br>Value: %{y}<extra></extra>") %>%
  layout(title = "Nitrate Measurements Over Time (Colored by Quality Type)",
         xaxis = list(title = "Timestamp"),
         yaxis = list(title = "Nitrate as N (mg/L)"),
         showlegend = TRUE)
p



#overwritable QC codes, all else are retained. In this case, all codes will be overwritten
manual_codes = c(1:4000)

#upper and lower limits for the sensor uses (Trios Opus)
tst <- ts_anom(df = df_clean,
               overwrite = manual_codes,
               window = 6,
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
  "sensor_drift" = "pink",
  "impossible" = "red",
  "Unknown" = "grey"
)
# plot
plotly::plot_ly(tst) |>
  plotly::add_markers(
    x =  ~ts,
    y =  ~value,
    type = "scatter",
    color = ~quality,
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
result <- detect_sensor_drift(tst,
                              value_col = "value",
                              threshold_multiplier = 2.5, time_threshold_days = 14,
                              overwrite = c('OK', 'spike', 'impossible', 'above_limits'))
head(result)



plotly::plot_ly(result) |>
  plotly::add_markers(
    x =  ~ts,
    y =  ~value,
    type = "scatter",
    color = ~quality,
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
  )





tst <- result %>%
  mutate(
    quality_bin = if_else(quality == "OK", "OK", "anomaly"),
    q_truth_bin = if_else(q_truth == "OK", "OK", "anomaly"),
    # make sure both are factors with the same levels
    quality_bin = factor(quality_bin, levels = c("OK", "anomaly")),
    q_truth_bin = factor(q_truth_bin, levels = c("OK", "anomaly"))
  )

# Calculate metrics manually
tst <- tst %>%
  mutate(
    TN = (quality_bin == "anomaly" & q_truth_bin == "anomaly"),
    TP = (quality_bin == "OK" & q_truth_bin == "OK"),
    FN = (quality_bin == "anomaly" & q_truth_bin == "OK"),
    FP = (quality_bin == "OK" & q_truth_bin == "anomaly")
  )

# Create a plotly scatter plot colored by quality_type
p <- plot_ly(data = tst,
             x = ~ts,
             y = ~value,
             color = ~FP,
             type = 'scatter',
             mode = 'markers',
             text = ~paste("OG Quality:", desc,
                           "<br>mod quality:", quality,
                           "<br>q_truth:", q_truth,
                           "<br>Station:", station_id,
                           "<br>Parameter:", parameter,
                           "<br>Units:", units),
             hovertemplate = "%{text}<br>Time: %{x}<br>Value: %{y}<extra></extra>") %>%
  layout(title = "Nitrate Measurements Over Time (Colored by Quality Type)",
         xaxis = list(title = "Timestamp"),
         yaxis = list(title = "Nitrate as N (mg/L)"),
         showlegend = TRUE)
p


# Summarize
metrics <- tst %>%
  summarise(
    TP = sum(TP),
    TN = sum(TN),
    FP = sum(FP),
    FN = sum(FN),
    accuracy = (TP + TN) / n(),
    precision = TP / (TP + FP),
    recall = TP / (TP + FN),
    specificity = TN / (TN + FP)
  )


# Confusion matrix + metrics
tst %>%
  conf_mat(truth = q_truth_bin, estimate = quality_bin) %>%
  summary()

tst %>%
  conf_mat(truth = q_truth_bin, estimate = quality_bin) %>%
  autoplot(type = "heatmap")

# calculate % of good and bad data in the dataset
tst %>%
  group_by(q_truth_bin) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)


