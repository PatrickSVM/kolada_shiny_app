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
      selectInput("kpi", "Choose Key Performance Indicator (KPI):", all_kpis),
      selectInput("city", "Choose city:", all_cities, selected = "Linköping"),
      sliderInput("years", "Choose years range for KPIs:", value = c(2017, 2021), min = 2000, max = 2021, sep = ""),
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
  data <- reactive(get_infos(kpis = c(kpi_code()), cities = c(input$city), years = seq(input$years[1], input$years[2], 1))[c("kpi", "period", "value")])



  output$data_tab <- renderTable({
    data()
  })

  output$kpi_plot <- renderPlot({
    ggplot(data(), aes(period, value)) + geom_point()
  }, res = 96)

}

shinyApp(ui, server)


#kpi_code <- all_kpis_df[all_kpis_df$member_title == all_kpis[1], ][["member_id"]]

#res <- get_infos(cities=c("Linköping"), kpis = c(kpi_code), years = c(2009, 2021))

#plot(x=res$period, y=res$value)

#kpi_code seq(2012, 2021, 1)
