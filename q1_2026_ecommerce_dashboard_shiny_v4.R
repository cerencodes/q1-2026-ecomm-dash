library(shiny)
library(tidyverse)
library(lubridate)
library(scales)
library(forcats)
library(bslib)
library(gt)

raw_data <- readr::read_csv("mock_ecommerce_dashboard_q1_2026.csv", show_col_types = FALSE) |>
  mutate(
    date = as.Date(date),
    month = floor_date(date, unit = "month"),
    month_label = factor(format(month, "%b %Y"), levels = c("Jan 2026", "Feb 2026", "Mar 2026")),
    source_medium = paste(source, medium, sep = " / ")
  )

ui <- page_fillable(
  title = "Q1 2026 Ecommerce Dashboard",
  theme = bs_theme(bootswatch = "cosmo"),
  gap = "1rem",
  padding = "1rem",
  tags$div(
    class = "mb-3",
    tags$h1("Q1 2026 Ecommerce Dashboard", class = "mb-1"),
    tags$p("Dashboard for a fictional ecommerce website", class = "text-muted mb-0")
  ),
  layout_columns(
    col_widths = c(4, 4, 4),
    value_box(
      title = "Total Sessions",
      value = textOutput("total_sessions"),
      theme = "primary",
      min_height = "90px"
    ),
    value_box(
      title = "Total Purchases",
      value = textOutput("total_purchases"),
      theme = "success",
      min_height = "90px"
    ),
    value_box(
      title = "Conversion Rate",
      value = textOutput("conversion_rate"),
      theme = "warning",
      min_height = "90px"
    )
  ),
  layout_columns(
    col_widths = c(8, 4),
    card(
      full_screen = FALSE,
      card_header("Purchases vs Sessions by Month"),
      plotOutput("traffic_vs_purchases", height = "320px")
    ),
    card(
      full_screen = FALSE,
      card_header("Purchases by Device"),
      plotOutput("purchases_by_device", height = "320px")
    )
  ),
  layout_columns(
    col_widths = c(5, 7),
    card(
      full_screen = FALSE,
      card_header("Top 5 States by Purchases"),
      plotOutput("top_states", height = "320px")
    ),
    card(
      full_screen = FALSE,
      card_header("Sessions and Purchases by Source / Medium"),
      gt_output("source_medium_breakdown")
    )
  )
)

server <- function(input, output, session) {
  output$total_sessions <- renderText({
    comma(sum(raw_data$sessions, na.rm = TRUE))
  })

  output$total_purchases <- renderText({
    comma(sum(raw_data$purchases, na.rm = TRUE))
  })

  output$conversion_rate <- renderText({
    sessions_total <- sum(raw_data$sessions, na.rm = TRUE)
    purchases_total <- sum(raw_data$purchases, na.rm = TRUE)
    conv_rate <- ifelse(sessions_total == 0, 0, purchases_total / sessions_total)
    percent(conv_rate, accuracy = 0.1)
  })

  output$traffic_vs_purchases <- renderPlot({
    monthly_data <- raw_data |>
      group_by(month, month_label) |>
      summarise(
        sessions = sum(sessions, na.rm = TRUE),
        purchases = sum(purchases, na.rm = TRUE),
        .groups = "drop"
      ) |>
      pivot_longer(
        cols = c(sessions, purchases),
        names_to = "metric",
        values_to = "value"
      ) |>
      mutate(metric = recode(metric, sessions = "Sessions", purchases = "Purchases"))

    ggplot(monthly_data, aes(x = month, y = value, color = metric, group = metric)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 3) +
      geom_text(aes(label = comma(value)), vjust = -0.8, size = 3.5, show.legend = FALSE) +
      scale_x_date(
        breaks = unique(monthly_data$month),
        labels = label_date("%b %Y")
      ) +
      scale_y_continuous(labels = comma) +
      labs(x = NULL, y = NULL, color = NULL) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "top",
        panel.grid.minor = element_blank()
      )
  })

  output$purchases_by_device <- renderPlot({
    device_data <- raw_data |>
      group_by(device) |>
      summarise(purchases = sum(purchases, na.rm = TRUE), .groups = "drop") |>
      filter(purchases > 0) |>
      arrange(desc(purchases)) |>
      mutate(
        share = purchases / sum(purchases),
        label = percent(share, accuracy = 0.1)
      )

    if (nrow(device_data) == 0) {
      ggplot() +
        annotate("text", x = 1, y = 1, label = "No purchases available", size = 6) +
        theme_void()
    } else {
      ggplot(device_data, aes(x = "", y = purchases, fill = device)) +
        geom_col(width = 1, color = "white") +
        geom_text(
          aes(label = label),
          position = position_stack(vjust = 0.5),
          color = "white",
          size = 4,
          fontface = "bold"
        ) +
        coord_polar(theta = "y") +
        labs(x = NULL, y = NULL, fill = "Device") +
        theme_void(base_size = 12) +
        theme(legend.position = "right")
    }
  })

  output$top_states <- renderPlot({
    state_data <- raw_data |>
      group_by(state) |>
      summarise(purchases = sum(purchases, na.rm = TRUE), .groups = "drop") |>
      arrange(desc(purchases), state) |>
      slice_head(n = 5) |>
      mutate(state = fct_reorder(state, purchases))

    ggplot(state_data, aes(x = state, y = purchases)) +
      geom_col() +
      geom_text(aes(label = comma(purchases)), hjust = -0.1, size = 3.5) +
      coord_flip() +
      scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.12))) +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank())
  })

  output$source_medium_breakdown <- render_gt({
    raw_data |>
      group_by(source_medium) |>
      summarise(
        Sessions = sum(sessions, na.rm = TRUE),
        Purchases = sum(purchases, na.rm = TRUE),
        `Conversion Rate` = ifelse(Sessions == 0, 0, Purchases / Sessions),
        .groups = "drop"
      ) |>
      arrange(desc(Purchases), desc(Sessions)) |>
      rename(`Source / Medium` = source_medium) |>
      gt() |>
      fmt_number(columns = c(Sessions, Purchases), decimals = 0, use_seps = TRUE) |>
      fmt_percent(columns = `Conversion Rate`, decimals = 1)
  })
}

app <- shinyApp(ui, server)
