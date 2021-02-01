library(magrittr)
library(shiny)
library(tidyverse)
library(shinythemes)
library(grid)
library(plotly)

# Install font

# Set ggplot2 theme

theme_set(theme_bw(base_family = "Lato"))

# Define functions

annual_return = function(initial_investment, annual_increment, profit, duration, year) {
    sum(initial_investment * annual_increment^(year - 1) * profit^(1:12/12)) * profit^(duration - year)
}

annual_return = Vectorize(annual_return, "year")

final_return = function(principal, initial_investment, annual_increment, profit, duration, inflation) {
    unadjusted_return = (principal * profit^duration) + sum(annual_return(initial_investment, annual_increment, profit, duration, 1:duration))
    adjusted_return = ((principal * profit^duration) + sum(annual_return(initial_investment, annual_increment, profit, duration, 1:duration))) / inflation^duration
    return(c(unadjusted = unadjusted_return, adjusted = adjusted_return))
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
               sliderInput("initial", "초기 투자금액 (백만원)", min = 1, max = 500, value = 20, width = "100%"),
               sliderInput("monthly", "월 투자금액 (십만원)", min = 1, max = 500, value = 50, width = "100%"),
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
        final_return(principal = input$initial/100,
                     initial_investment = input$monthly/1000,
                     annual_increment = 1 + input$increment/100, 
                     profit = 1 + input$profit/100, 
                     duration = 1:input$duration,
                     inflation = 1 + input$inflation/100) %>% 
            t %>% 
            as_tibble %>%
            mutate(`기간` = row_number()) %>%
            bind_rows(tibble(unadjusted = input$initial/100, adjusted = input$initial/100, `기간` = 0), .) %>%
            pivot_longer(-`기간`, names_to = "adjustment", values_to = "자산 (억원)") %>%
            mutate(adjustment = case_when(adjustment == "unadjusted" ~ "총액",
                                          adjustment == "adjusted" ~ "물가상승률 보정 총액") %>%
                       factor(levels = c("총액", "물가상승률 보정 총액")))
    })
    
    # Draw the simulation result as a reactive ggplot
    
    base_plot = reactive({
        return_simulation() %>% 
            ggplot(mapping = aes(x = `기간`, y = `자산 (억원)`)) +
            geom_line(mapping = aes(color = adjustment), size = 1) +
            scale_x_continuous(expand = c(0, 0)) +
            scale_color_brewer(type = "qual", guide = guide_legend(title = NULL)) +
            theme(legend.key = element_rect(fill = "transparent"), 
                  legend.background = element_rect(fill = "transparent"))
    })
    
    output$total_area = renderPlotly({
        ggplotly(base_plot(),
                 height = session$clientData$output_total_area_width/2,
                 tooltip = c("기간", "자산 (억원)")) %>%
            layout(xaxis = list(title = list(font = list(family = "Lato")), tickfont = list(family = "Lato")),
                   yaxis = list(title = list(font = list(family = "Lato")), tickfont = list(family = "Lato")),
                   legend = list(x = 0.03, y = 0.95, font = list(family = "Lato", size = 14)))
    })

}

# Create Shiny app

shinyApp(ui = ui, server = server)