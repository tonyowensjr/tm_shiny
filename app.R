# import required packages
library(shiny)
library(rgl)
library(shinyRGL)
library(plotly)
library("R.matlab")
library(markdown)
library(shinymanager)

credentials <- readRDS("data/creds.rds")

data <- readRDS("data/game_data.rds")

file <- readMat("data/Zboxes.mat")

ump_data <- read.csv("data/ump_eval.csv")


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
                    selectInput("pitcher", "Pitcher", choices = c(c("All"), data %>% arrange(Pitcher) %>% select(Pitcher) %>% unique())),
                    selectInput("batter_in", "Batter", choices = c(c("All"), data %>% arrange(Batter) %>% select(Batter) %>% unique())),
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
                    plotlyOutput("distPlot")
                )
            )
        ),
        tabPanel(
            "Contact Plot",
            fluidRow(
                # Define the first column for the sidebarPanel
                column(
                    8, # 4 out of 12 grid columns wide
                    sidebarPanel(
                        radioButtons("view", "Choose the POV:",
                            choices = c("Bird's Eye View" = 1, "Catcher View" = 2, "Dugout View" = 3),
                            selected = 1
                        ),
                        selectInput("view_pitcher", "Pitcher",
                            choices = c("All", data %>% arrange(Pitcher) %>% select(Pitcher) %>% unique())
                        ),
                        selectInput("view_batter", "Batter",
                            choices = c("All", data %>% arrange(Batter) %>% select(Batter) %>% unique())
                        )
                    )
                ),
                # Define the second column for the plotlyOutput
                column(
                    8, # 8 out of 12 grid columns wide
                    plotlyOutput("contPlot")
                )
            )
        ), tabPanel(
            "Umpire Evaluation",
            fluidRow(
                # Define the first column for the sidebarPanel
                column(
                    8, # 4 out of 12 grid columns wide
                    sidebarPanel(
                        radioButtons("ump_pov", "Choose the POV:",
                            choices = c("Hitter View" = 1, "Pitcher View" = 2), selected = 1
                        ),
                        selectInput("ump", "Choose the Umpire:",
                            choices = c("All", ump_data %>% arrange(ump) %>% select(ump) %>% unique()),
                            selected = 1
                        ),
                        selectInput("batter_side", "Batter Side",
                            choices = c("All", ump_data %>% arrange(BatterSide) %>% select(BatterSide) %>% unique())
                        ),
                        selectInput("pitcher_side", "Pitcher Side",
                            choices = c("All", ump_data %>% arrange(PitcherThrows) %>% select(PitcherThrows) %>% unique())
                        )
                    )
                ),
                # Define the second column for the plotlyOutput
                column(
                    8, # 8 out of 12 grid columns wide
                    plotlyOutput("ump_plot")
                )
            )
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

    output$contPlot <- renderPlotly({
        if (input$view_pitcher != "All") {
            data <- data %>% filter(Pitcher == input$view_pitcher)
        }
        if (input$view_batter != "All") {
            data <- data %>% filter(Batter == input$view_batter)
        }
        p <- NULL
        if (input$view == 1) {
            p <- ggplot() +
                annotate("segment", x = -8.5 / 12, xend = 8.5 / 12, y = 17 / 12, yend = 17 / 12, color = "black", size = 2) +
                annotate("segment", x = -8.5 / 12, xend = -8.5 / 12, y = 17 / 12, yend = 8.5 / 12, color = "black", size = 2) +
                annotate("segment", x = -8.5 / 12, xend = 0, y = 8.5 / 12, yend = 0, color = "black", size = 2) +
                annotate("segment", x = 0, xend = 8.5 / 12, y = 0, yend = 8.5 / 12, color = "black", size = 2) +
                annotate("segment", x = 8.5 / 12, xend = 8.5 / 12, y = 8.5 / 12, yend = 17 / 12, color = "black", size = 2) +
                geom_point(data = data, aes(x = ContactPositionZ, y = ContactPositionX, color = TaggedPitchType, text = paste(Batter, "EV:", round(ExitSpeed, 2), "LA:", round(Angle, 2))), size = 2) +
                ggtitle("Contact Location (Birds Eye View)") +
                theme(plot.title = element_text(hjust = 0.5)) +
                coord_fixed(ratio = 3 / 4) +
                xlab("Plate Loc Side (in)") +
                ylab("Plate Loc Height (in)") +
                xlim(-1.5, 1.5)
        } else if (input$view == 2) {
            p <- ggplot() +
                geom_point(data = data, aes(x = ContactPositionZ, y = ContactPositionY, color = TaggedPitchType, text = paste(Batter, "EV:", round(ExitSpeed, 2), "LA:", round(Angle, 2))), size = 2) +
                ggtitle("Contact Location (Catcher View)") +
                theme(plot.title = element_text(hjust = 0.5)) +
                xlab("Plate Loc Side (in)") +
                coord_fixed(ratio = 3 / 4) +
                ylab("Plate Loc Height (in)") +
                geom_polygon(aes(x = mlbsz[1, ], y = mlbsz[2, ]), fill = NA, color = "orange") +
                geom_polygon(aes(x = d1sz99[1, ], y = d1sz99[2, ]), fill = NA, color = "black", linetype = "dotted", size = 2)
        } else {
            p <- ggplot() +
                geom_point(data = data, aes(x = ContactPositionX, y = ContactPositionY, color = TaggedPitchType, text = paste(Batter, "EV:", round(ExitSpeed, 2), "LA:", round(Angle, 2))), size = 2) +
                ggtitle("Contact Location (Dugout View)") +
                theme(plot.title = element_text(hjust = 0.5)) +
                annotate("segment", x = 17 / 12, xend = 17 / 12, y = 18.16 / 12, yend = 38.8 / 12, color = "black", size = 2) +
                xlab("Plate Loc Side (in)") +
                coord_fixed(ratio = 3 / 4) +
                ylab("Plate Loc Height (in)")
        }
        ggplotly(p, tooltip = "text")
        # print(p)      # print(p)
    })

    output$distPlot <- renderPlotly({
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
        if (input$batter_in != "All") {
            filtered_data <- filtered_data %>% filter(Batter == input$batter_in)
        }
        p <- NULL
        if (input$myRadio == 1) {
            p <- plot_ly(data = filtered_data) %>%
                # Add markers for the data points
                add_markers(
                    x = ~PlateLocSide,
                    y = ~PlateLocHeight,
                    color = ~TaggedPitchType,
                    text = ~ paste(paste0(Balls, "-", Strikes), TaggedPitchType, round(RelSpeed, 2), "mph"),
                    size = 0.2,
                    colors = "Set1",
                    marker = list(sizemode = "area"),
                    showlegend = TRUE
                ) %>%
                # Add the orange polygon (assuming mlbsz is a two-row matrix or data frame with x and y coordinates)
                add_paths(
                    x = mlbsz[1, ],
                    y = mlbsz[2, ],
                    line = list(color = "orange"),
                    fillcolor = "transparent",
                    showlegend = FALSE
                ) %>%
                # Add the dotted black polygon (assuming d1sz99 is similar to mlbsz)
                add_paths(
                    x = d1sz99[1, ],
                    y = d1sz99[2, ],
                    line = list(color = "black", dash = "dot", width = 2),
                    fillcolor = "transparent",
                    showlegend = FALSE
                ) %>%
                # Set plot layout
                layout(
                    title = "Pitch Location (Hitter View)",
                    xaxis = list(title = "Plate Loc Side (in)", range = c(2, -2)),
                    yaxis = list(title = "Plate Loc Height (in)", range = c(-0.5, 5), scaleanchor = "x", scaleratio = 3 / 4),
                    hovermode = "closest"
                )
        } else {
            # Create a scatter plot with plot_ly
            p <- plot_ly(data = filtered_data) %>%
                # Add markers for the data points
                add_markers(
                    x = ~PlateLocSide,
                    y = ~PlateLocHeight,
                    color = ~TaggedPitchType,
                    text = ~ paste(paste0(Balls, "-", Strikes), TaggedPitchType, round(RelSpeed, 2), "mph"),
                    size = 0.2,
                    colors = "Set1",
                    marker = list(sizemode = "area"),
                    showlegend = TRUE
                ) %>%
                # Add the orange polygon (assuming mlbsz is a two-row matrix or data frame with x and y coordinates)
                add_paths(
                    x = mlbsz[1, ],
                    y = mlbsz[2, ],
                    line = list(color = "orange"),
                    fillcolor = "transparent",
                    showlegend = FALSE
                ) %>%
                # Add the dotted black polygon (assuming d1sz99 is similar to mlbsz)
                add_paths(
                    x = d1sz99[1, ],
                    y = d1sz99[2, ],
                    line = list(color = "black", dash = "dot", width = 2),
                    fillcolor = "transparent",
                    showlegend = FALSE
                ) %>%
                # Set plot layout
                layout(
                    title = "Pitch Location (Pitcher View)",
                    xaxis = list(title = "Plate Loc Side (in)", range = c(-2, 2)),
                    yaxis = list(title = "Plate Loc Height (in)", range = c(-0.5, 5), scaleanchor = "x", scaleratio = 3 / 4),
                    hovermode = "closest"
                )
        }
    })

    output$ump_plot <- renderPlotly({
        if (input$ump != "All") {
            ump_data <- ump_data %>% filter(ump == input$ump)
        }
        if (input$batter_side != "All") {
            ump_data <- ump_data %>% filter(BatterSide == input$batter_side)
        }
        if (input$pitcher_side != "All") {
            ump_data <- ump_data %>% filter(PitcherThrows == input$pitcher_side)
        }

        if (input$ump_pov == 1) {
            p <- plot_ly(data = ump_data) %>%
                # Add markers for the data points
                add_markers(
                    x = ~PlateLocSide,
                    y = ~PlateLocHeight,
                    color = ~PitchCall,
                    text = ~ paste(paste0(Balls, "-", Strikes), TaggedPitchType, round(RelSpeed, 2), "mph"),
                    size = 0.2,
                    colors = "Set1",
                    marker = list(sizemode = "area"),
                    showlegend = TRUE
                ) %>%
                # Add the orange polygon (assuming mlbsz is a two-row matrix or data frame with x and y coordinates)
                add_paths(
                    x = mlbsz[1, ],
                    y = mlbsz[2, ],
                    line = list(color = "orange"),
                    fillcolor = "transparent",
                    showlegend = FALSE
                ) %>%
                # Add the dotted black polygon (assuming d1sz99 is similar to mlbsz)
                add_paths(
                    x = d1sz99[1, ],
                    y = d1sz99[2, ],
                    line = list(color = "black", dash = "dot", width = 2),
                    fillcolor = "transparent",
                    showlegend = FALSE
                ) %>%
                # Set plot layout
                layout(
                    title = "Pitch Location (Hitter View)",
                    xaxis = list(title = "Plate Loc Side (in)", range = c(2, -2)),
                    yaxis = list(title = "Plate Loc Height (in)", range = c(-0.5, 5), scaleanchor = "x", scaleratio = 3 / 4),
                    hovermode = "closest"
                )
        } else {
            # Create a scatter plot with plot_ly
            p <- plot_ly(data = ump_data) %>%
                # Add markers for the data points
                add_markers(
                    x = ~PlateLocSide,
                    y = ~PlateLocHeight,
                    color = ~PitchCall,
                    text = ~ paste(paste0(Balls, "-", Strikes), TaggedPitchType, round(RelSpeed, 2), "mph"),
                    size = 0.2,
                    colors = "Set1",
                    marker = list(sizemode = "area"),
                    showlegend = TRUE
                ) %>%
                # Add the orange polygon (assuming mlbsz is a two-row matrix or data frame with x and y coordinates)
                add_paths(
                    x = mlbsz[1, ],
                    y = mlbsz[2, ],
                    line = list(color = "orange"),
                    fillcolor = "transparent",
                    showlegend = FALSE
                ) %>%
                # Add the dotted black polygon (assuming d1sz99 is similar to mlbsz)
                add_paths(
                    x = d1sz99[1, ],
                    y = d1sz99[2, ],
                    line = list(color = "black", dash = "dot", width = 2),
                    fillcolor = "transparent",
                    showlegend = FALSE
                ) %>%
                # Set plot layout
                layout(
                    title = "Pitch Location (Pitcher View)",
                    xaxis = list(title = "Plate Loc Side (in)", range = c(-2, 2)),
                    yaxis = list(title = "Plate Loc Height (in)", range = c(-0.5, 5), scaleanchor = "x", scaleratio = 3 / 4),
                    hovermode = "closest"
                )
        }
    })

    # Reactive expression to render the rgl plot
}

ui <- secure_app(ui)
# Run the application
shinyApp(ui = ui, server = server)
