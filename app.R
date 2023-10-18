# import required packages
library(shiny)
library(rgl)
library(shinyRGL)
library(plotly)
library("R.matlab")
library(markdown)
library(shinymanager)

credentials <- readRDS("tm_shiny/creds.rds")



data <- readRDS("tm_shiny/game_data.rds")


file <- readMat("tm_shiny/Zboxes.mat")

d1sz99 <- file$D1sz99
mlbsz <- file$MLBsz

ui <- fluidPage(

  # Application title
  titlePanel("Pitch Location"),
  navbarPage(
    "Navigation Bar", tabPanel(
      "2D Zone Plot",
      # Sidebar with a slider input for number of bins
      sidebarLayout(
        sidebarPanel(
          radioButtons("myRadio", "Choose an option:",
            choices = c("Hitter View" = 1, "Pitcher View" = 2),
            selected = 1
          ),
          selectInput("pitcher", "Pitcher", choices = c(c("All"), data %>%
          arrange(Pitcher) %>% select(Pitcher) %>% unique())),
          sliderInput("velo",
            "Pitch Velocity",
            min = 50,
            max = 110,
            value = c(50, 100)
          ),
          sliderInput("spin",
            "Spin Rate",
            min = 500,
            max = 3500,
            value = c(500, 3500)
          ),
          dateRangeInput(
            "date_vals",
            "Date Range",
            start = "2023-01-01",
            end = "2023-12-31",
            min = NULL,
            max = NULL,
            format = "yyyy-mm-dd",
            startview = "month",
            weekstart = 0,
            language = "en",
            separator = " to ",
            width = NULL,
            autoclose = TRUE
          ),
        ),

        # Show a plot of the generated distribution
        mainPanel(
          # rgl widget to display the plot
          plotOutput("distPlot")
        )
      )
    ),
    tabPanel(
      "Contact Plot",
      sidebarPanel(radioButtons("view", "Choose the POV:",
        choices = c("Bird's Eye View" = 1, "Catcher View" = 2,
         "Dugout View" = 3),
        selected = 1
      )), plotOutput("contPlot")
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  # call the server part
  # check_credentials returns a function to authenticate users
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )

  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })

  output$contPlot <- renderPlot({
    if (input$view == 1) {
      ggplot() +
        annotate("segment", x = -8.5 / 12, xend = 8.5 / 12, y = 17 / 12, yend = 17 / 12, color = "black", size = 2) +
        annotate("segment", x = -8.5 / 12, xend = -8.5 / 12, y = 17 / 12, yend = 8.5 / 12, color = "black", size = 2) +
        annotate("segment", x = -8.5 / 12, xend = 0, y = 8.5 / 12, yend = 0, color = "black", size = 2) +
        annotate("segment", x = 0, xend = 8.5 / 12, y = 0, yend = 8.5 / 12, color = "black", size = 2) +
        annotate("segment", x = 8.5 / 12, xend = 8.5 / 12, y = 8.5 / 12, yend = 17 / 12, color = "black", size = 2) +
        geom_point(data = data, aes(x = ContactPositionZ, y = ContactPositionX, color = TaggedPitchType), size = 2) +
        ggtitle("Contact Location (Birds Eye View)") +
        theme(plot.title = element_text(hjust = 0.5)) +
        xlab("Plate Loc Side (in)") +
        ylab("Plate Loc Height (in)") +
        xlim(-1.5, 1.5)
    } else if (input$view == 2) {
      ggplot() +
        geom_point(data = data, aes(x = ContactPositionZ, y = ContactPositionY, color = TaggedPitchType), size = 2) +
        ggtitle("Contact Location (Catcher View)") +
        theme(plot.title = element_text(hjust = 0.5)) +
        xlab("Plate Loc Side (in)") +
        ylab("Plate Loc Height (in)") +
        geom_polygon(aes(x = mlbsz[1, ], y = mlbsz[2, ]), fill = NA, color = "orange") +
        geom_polygon(aes(x = d1sz99[1, ], y = d1sz99[2, ]), fill = NA, color = "black", linetype = "dotted", size = 2)
    } else {
      ggplot() +
        geom_point(data = data, aes(x = ContactPositionX, y = ContactPositionY, color = TaggedPitchType), size = 2) +
        ggtitle("Contact Location (Dugout View)") +
        theme(plot.title = element_text(hjust = 0.5)) +
        annotate("segment", x = 17 / 12, xend = 17 / 12, y = 18.16 / 12, yend = 38.8 / 12, color = "black", size = 2) +
        xlab("Plate Loc Side (in)") +
        ylab("Plate Loc Height (in)")
    }
  })

  output$distPlot <- renderPlot({
    filtered_data <- data %>%
      filter(RelSpeed >= input$velo[1]) %>%
      filter(RelSpeed <= input$velo[2]) %>%
      filter(Date >= input$date_vals[1]) %>%
      filter(Date <= input$date_vals[2]) %>%
      filter(SpinRate >= input$spin[1]) %>%
      filter(SpinRate <= input$spin[2])
    if (input$pitcher != "All") {
      filtered_data <- filtered_data %>% filter(Pitcher == input$pitcher)
    }
    if (input$myRadio == 1) {
      # Scatter plot using ggplot2
      ggplot() +
        geom_point(data = filtered_data, aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType), size = 2) +
        ggtitle("Pitch Location (Hitter View)") +
        theme(plot.title = element_text(hjust = 0.5)) +
        geom_polygon(aes(x = mlbsz[1, ], y = mlbsz[2, ]), fill = NA, color = "orange") +
        geom_polygon(aes(x = d1sz99[1, ], y = d1sz99[2, ]), fill = NA, color = "black", linetype = "dotted", size = 2) +
        xlab("Plate Loc Side (in)") +
        ylab("Plate Loc Height (in)") +
        xlim(2, -2) +
        ylim(-.5, 5)
    } else {
      ggplot() +
        geom_point(data = filtered_data, aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType), size = 2) +
        ggtitle("Pitch Location (Pitcher View)") +
        theme(plot.title = element_text(hjust = 0.5)) +
        geom_polygon(aes(x = mlbsz[1, ], y = mlbsz[2, ]), fill = NA, color = "orange") +
        geom_polygon(aes(x = d1sz99[1, ], y = d1sz99[2, ]), fill = NA, color = "black", linetype = "dotted", size = 2) +
        xlab("Plate Loc Side (in)") +
        ylab("Plate Loc Height (in)") +
        xlim(-2, 2) +
        ylim(-.5, 5)
    }
  })

  # Reactive expression to render the rgl plot
}

ui <- secure_app(ui)
# Run the application
shinyApp(ui = ui, server = server)
