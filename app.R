library(rfm)
library(fable)
library(purrr)
library(furrr)
library(shiny)
library(feasts)
library(scales)
library(future)
library(tsibble)
library(stringr)
library(patchwork)
library(lubridate)
library(tidyverse)
library(shinyalert)
library(data.table)
library(strucchange)
library(future.apply)
library(shinydashboard)
library(shinycssloaders)
library(dashboardthemes)


Sys.setlocale("LC_ALL","C")

plot_font_size <- 20


p_value_threshold <- 0.05

source("functions.R")



models <- readRDS("/Users/abhinavgupta/Development/SalesIntelligence-master/Data/models.RDS")
segments <- readRDS("/Users/abhinavgupta/Development/SalesIntelligence-master/Data/segments.RDS")
rfm_result <- readRDS("/Users/abhinavgupta/Development/SalesIntelligence-master/Data/rfm_results.RDS")
data_to_arima <- readRDS("/Users/abhinavgupta/Development/SalesIntelligence-master/Data/data_to_arima.RDS")




rfm_plot <- rfm_heatmap(rfm_result, print_plot = FALSE) +
  
  geom_rect(aes(xmin = 0.5, xmax = 5.5, ymin = 0.5, ymax = 5.5),
            fill = "#F1EEF6") +
  geom_tile(aes(frequency_score, recency_score, fill = monetary)) +
  ggtitle("Product level RFM") +
  theme(text = element_text(colour = "#DAD4D4"),
        panel.grid = element_line(colour = "#2D3741"),
        panel.background = element_rect(fill = "#2D3741"),
        axis.text = element_text(colour = "#BCB1B1", size = plot_font_size),
        plot.background = element_rect(fill = "#2D3741", color = "transparent"),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.box.margin = margin(t = 13),
        legend.background = element_rect(fill = "#2D3741"),
        legend.text = element_text(size = plot_font_size),
        legend.title = element_text(size = plot_font_size),
        plot.title = element_text(size = plot_font_size),
        axis.title = element_text(size = plot_font_size))


rfm_monetary_segments <- segments %>%
  group_by(segment) %>%
  summarise(median = median(amount)) %>%
  arrange(desc(median)) %>%
  
  mutate(segment = fct_reorder(segment, median)) %>%
  ggplot(aes(x = median, y = segment, fill = segment)) +
  geom_col() +
  scale_fill_brewer(palette = "Blues") +
  ggtitle("Median monetary value by product segment") +
  xlab("Median monetary value") +
  ylab(NULL) +
  theme(legend.position = "none",
        text = element_text(colour = "#DAD4D4"),
        panel.grid = element_line(colour = "#2D3741"),
        panel.background = element_blank(),
        axis.text = element_text(colour = "#BCB1B1", size = plot_font_size),
        plot.background = element_rect(fill = "#2D3741", color = "transparent"),
        plot.title = element_text(size = plot_font_size),
        axis.title = element_text(size = plot_font_size))



ui <- dashboardPage(
  title = "Sales dashboard",
  dashboardHeader(title = "Price optimization"),
  dashboardSidebar(
    selectizeInput("segments", "Filter products by segment",
                   choices = unique(data_to_arima$segment),
                   multiple = TRUE),
    selectizeInput("product_name", "Select product to optimize",
                   choices = sort(unique(data_to_arima$product_name)),
                   multiple = FALSE) %>% 
      tagAppendAttributes(class = "larger"),
    actionButton("run_optimization", "Run price optimization"),
    hr(),
    sidebarMenu(id = "menu",
                sidebarMenuOutput("sidebar")
    ),
    tags$head(tags$style(HTML(
      # Css to adjust sidebar spacing and coloring
      paste('.row {width: 90%;}',
            '#product_name+ div>.selectize-input {height: 60px !important;',
            'padding-top: 0px !important}',
            '#segments+ div>.selectize-input',
            '{margin-bottom: 0px; padding-top: 2px !important}',
            '.larger {padding-top: 0px !important}',
            '#run_optimization {background-color: #00C0EF; color: #FFFFFF}',
            '.shiny-bound-input.action-button {margin: auto !important}'
      ))))
  ),
  dashboardBody(
    shinyDashboardThemes(
      theme = "grey_dark"
    ),
    
    useShinyalert(),
    

    tabItems(
      tabItem(tabName = "rfm",
              fluidRow(
                column(12,
                       plotOutput("rfm_plot"),
                       align = "center"),
                tags$head(tags$style(HTML('.row {width: 90%;}'))))
      ),
      tabItem(tabName = "segments",
              fluidRow(
                column(12,
                       plotOutput("rfm_monetary_segments"),
                       align = "center"),
                tags$head(tags$style(HTML('.row {width: 90%;}'))))
      ),
      tabItem(tabName = "results",
              fluidRow(
                column(12,
                       uiOutput("info_boxes"),
                       plotOutput(outputId = "combination_plot",
                                  width = "1200px",
                                  height = "700px") %>% 
                         withSpinner(type = 7),
                       align = "center"),
                tags$head(tags$style(HTML(
                  paste0('.row {width: 90%;}',
                         '.info-box-content {text-align: left;}')))))
      )
    )
  )
)

server <- function(input, output, session){
  
  output$sidebar <- renderMenu({
    sidebarMenu(id = "menu",
                menuItem("RFM", tabName = "rfm"),
                menuItem("Segments", tabName = "segments")
    )
  })
  
  
  output$rfm_plot <- renderPlot({
    rfm_plot
  }, height = 600, width = 750)
  
  output$rfm_monetary_segments <- renderPlot({
    rfm_monetary_segments
  }, height = 600, width = 750)
  
  
  observeEvent(input$segments, 
               updateSelectizeInput(session,
                                    "product_name",
                                    choices = data_to_arima %>% 
                                      filter(segment %in% input$segments) %>% 
                                      pull(product_name) %>% 
                                      unique())
  )
  
  
  observeEvent(input$run_optimization, {
    
    
    shinyalert("Optimizing...",
               type = "info",
               closeOnEsc = FALSE,
               showConfirmButton = FALSE)
    
    
    output$sidebar <- renderMenu({
      sidebarMenu(id = "menu",
                  menuItem("RFM", tabName = "rfm"),
                  menuItem("Segments", tabName = "segments"),
                  menuItem("Results", tabName = "results")
      )
    })
    
    
    forecasts <- get_forecasts(input$product_name, data_to_arima, models)
    optimal_forecast <- get_optimal_forecast(forecasts)
    
    
    output$info_boxes <- renderUI({
      fluidRow(
        infoBox("Optimized price",
                paste0(intToUtf8(163),
                       number_format(0.01)(
                         optimal_forecast$new_price)),
                "For the next month",
                icon("balance-scale")),
        infoBox("Optimized revenue",
                paste0(intToUtf8(163),
                       number_format(0.01)(
                         optimal_forecast$pred_revenue)),
                paste0("Up from ",
                       paste0(intToUtf8(163),
                              number_format(0.01)(
                                optimal_forecast$pred_revenue_normal))),
                icon("pound-sign")),
        infoBox("Revenue increasement", 
                percent_format(0.1)(optimal_forecast$pred_revenue / 
                                      optimal_forecast$pred_revenue_normal - 1)
                , "Compared to no optimization",
                icon("percent"))
      )
    })
    
    
    output$combination_plot <- renderPlot({
      p1 <- isolate(plot_revenue_forecasts(optimal_forecast,
                                           input$product_name,
                                           data_to_arima,
                                           plot_font_size))
      
      p2 <- isolate(plot_quantity_forecasts(optimal_forecast,
                                            input$product_name,
                                            data_to_arima,
                                            plot_font_size))
      
      p3 <- isolate(plot_revenue_price(forecasts,
                                       input$product_name,
                                       data_to_arima,
                                       plot_font_size))
      
      
      suppressMessages(
        print(
          ((p1 / p2) | p3) +
            plot_annotation(
              "Effect of price optimization",
              theme = theme(
                text = element_text(colour = "#DAD4D4"),
                plot.background = element_rect(fill = "#2D3741",
                                               color = "transparent"),
                plot.title = element_text(size = plot_font_size)))))
    })
    
    
    updateTabItems(session, "menu", "results")
    
    closeAlert()
    
    if(length(optimal_forecast$pred_revenue) == 0){
      shinyalert(
        "Forecasts or optimization could not be done for this product",
        type = "error")
    }
    
  })
}

shinyApp(ui, server)
  