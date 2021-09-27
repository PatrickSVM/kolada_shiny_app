library(shiny)
library(apikolada)
library(ggplot2)

all_kpis_df <- get_all_kpis()
all_kpis <- unique(stringr::str_sort(all_kpis_df[["member_title"]]))

all_cities <- get_all_cities()


ui <- fluidPage(

  titlePanel("KPI visualizations for API-requests to kolada.se"),

  sidebarLayout(

    sidebarPanel(
      selectInput("kpi", "Choose Key Performance Indicator (KPI):", all_kpis, selected = "Anställda 65+, kommun, andel (%)"),
      selectInput("city", "Choose city:", all_cities, selected = "Malmö"),
      sliderInput("years", "Choose years range for KPIs:", value = c(2012, 2021), min = 2000, max = 2021, sep = ""),
      tableOutput("data_tab")
    ),

    mainPanel(
      plotOutput("kpi_plot")
    )
  )
)



server <- function(input, output, session) {

  # reactive programming, gets refreshed when input changes, globlaly available
  kpi_code <- reactive(all_kpis_df[all_kpis_df$member_title == input$kpi, ][["member_id"]])

  data <- reactive({
    data_prep <- get_infos(kpis = c(kpi_code()), cities = c(input$city), years = seq(input$years[1], input$years[2], 1))
    data_prep[data_prep["gender"] == "T", ][c("kpi", "period", "value")]
    })

  output$data_tab <- renderTable({
    data()
  })

  output$kpi_plot <- renderPlot({

    ggplot(data(), aes(period, value)) +
      geom_line(color="#00A5FF") +
      geom_point() +
      scale_x_continuous(name = "Years", breaks = data()[["period"]]) +
      scale_y_continuous(name = paste("KPI-code:", kpi_code()))  +
      ggtitle(paste("KPI overview for: ", input$city)) +
      theme(plot.title = element_text(hjust = 0.5))


  })

}

shinyApp(ui, server)


