library(grid)
library(magrittr)
library(plotly)
library(RColorBrewer)
library(shiny)
library(shinythemes)
library(tidyverse)

# Set ggplot2 theme

theme_set(theme_bw(base_family = "Lato"))

# Define color

qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
qual_col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals))) %>% unique
line_color = qual_col_vector[c(5:6, 52)]

# Define functions

annual_return = function(monthly_investment, annual_increment, profit, duration, year) {
  sum(monthly_investment * annual_increment^(year - 1) * profit^(1:12/12)) * profit^(duration - year)
}

annual_return = Vectorize(annual_return, "year")

final_return = function(initial, monthly_investment, annual_increment, profit, duration, inflation) {
  total_input = initial + sum(monthly_investment * 12 * annual_increment^(0:(duration - 1)))
  unadjusted_return = (initial * profit^duration) + sum(annual_return(monthly_investment, annual_increment, profit, duration, 1:duration))
  adjusted_return = ((initial * profit^duration) + sum(annual_return(monthly_investment, annual_increment, profit, duration, 1:duration))) / inflation^duration
  return(c(total_input = total_input, unadjusted = unadjusted_return, adjusted = adjusted_return))
}

final_return = Vectorize(final_return, "duration")

# Build up user interface

ui = fluidPage(
  
  tags$head(
    tags$style(HTML("
            @import url('https://fonts.googleapis.com/css2?family=Lato:ital,wght@0,100;0,300;0,400;0,700;0,900;1,100;1,300;1,400;1,700;1,900&family=Open+Sans:ital,wght@0,300;0,400;0,600;0,700;0,800;1,300;1,400;1,600;1,700;1,800&display=swap');
            body {
                font-family: Lato, 'Open Sans', sans-serif;
            }"))
  ),
  
  titlePanel("투자 수익 시뮬레이션"),
  
  hr(),
  
  fluidRow(
    column(6,
           sliderInput("initial", "초기 투자금액 (백만원)", min = 1, max = 200, value = 20, width = "100%"),
           sliderInput("monthly", "월 투자금액 (십만원)", min = 1, max = 200, value = 50, width = "100%"),
           sliderInput("increment", "연 투자금액 증가율 (%)", min = 0, max = 100, value = 5, width = "100%")
    ),
    column(6, 
           sliderInput("profit", "목표수익률 (%)", min = 0, max = 100, value = 20, width = "100%"),
           sliderInput("inflation", "물가상승률 (%)", min = 0, max = 20, value = 2, width = "100%"),
           sliderInput("duration", "투자기간 (년)", min = 1, max = 50, value = 30, width = "100%")
    )
  ),
  
  hr(),
  
  plotlyOutput("total_area", width = "100%", height = "auto")
)

# Instruct the server

server = function(input, output, session) {
  
  # Simulate the scenario to get a reactive result table
  
  return_simulation = reactive({
    final_return(initial = input$initial/100,
                 monthly_investment = input$monthly/1000,
                 annual_increment = 1 + input$increment/100, 
                 profit = 1 + input$profit/100, 
                 duration = 1:input$duration,
                 inflation = 1 + input$inflation/100) %>% 
      t %>% 
      as_tibble %>%
      mutate(`기간 (년)` = row_number()) %>%
      bind_rows(tibble(total_input = input$initial/100, unadjusted = input$initial/100, adjusted = input$initial/100, `기간 (년)` = 0), .) %>%
      pivot_longer(-`기간 (년)`, names_to = "category", values_to = "금액 (억원)") %>%
      mutate(category = case_when(category == "total_input" ~ "총 투자금",
                                  category == "unadjusted" ~ "자산 총액",
                                  category == "adjusted" ~ "물가상승률 보정 자산 총액") %>%
               factor(levels = c("자산 총액", "물가상승률 보정 자산 총액", "총 투자금")))
  })
  
  # Draw the simulation result as a reactive ggplot
  
  base_plot = reactive({
    return_simulation() %>%
      ggplot(mapping = aes(x = `기간 (년)`, y = `금액 (억원)`)) +
      geom_line(mapping = aes(color = category)) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_color_manual(values = line_color, guide = guide_legend(title = NULL)) +
      theme(legend.key = element_rect(fill = "transparent"), 
            legend.background = element_rect(fill = "transparent"))
  })
  
  output$total_area = renderPlotly({
    ggplotly(base_plot(),
             height = session$clientData$output_total_area_width/1.4,
             tooltip = c("기간 (년)", "금액 (억원)")) %>%
      layout(legend = list(x = 0.03, y = 0.95, font = list(size = 14)))
  })
}

# Create Shiny app

shinyApp(ui = ui, server = server)