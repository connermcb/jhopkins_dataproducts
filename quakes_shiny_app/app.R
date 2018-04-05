

# get requisite packages
library(dplyr)
library(ggplot2)
library(mapproj)
library(maps)
library(lubridate)
library(readr)
library(shiny)
library(SDMTools)


get_quake_data <- function(output_format="csv",
                           starttime=today()-180,
                           endtime=today(),
                           minlatitude=27,
                           minlongitude=-103,
                           maxlatitude=33,
                           maxlongitude=-100,
                           minmagnitude=2.5,
                           maxmagnitude=10){

        # compose API request
        base_url <- "https://earthquake.usgs.gov/fdsnws/event/1/query?"
        
        # add format arg
        api_url <- paste0(base_url, "format=", output_format)
        
        # add start, end times
        api_url <- paste0(api_url, "&starttime=", starttime)
        api_url <- paste0(api_url, "&endtime=", endtime)
        
        # set longitude range
        api_url <- paste0(api_url, "&minlongitude=", minlongitude)
        api_url <- paste0(api_url, "&maxlongitude=", maxlongitude)
        
        # set latitude range
        api_url <- paste0(api_url, "&minlatitude=", minlatitude)
        api_url <- paste0(api_url, "&maxlatitude=", maxlatitude)
        
        # set magnitude range
        api_url <- paste0(api_url, "&minmagnitude=", minmagnitude)
        api_url <- paste0(api_url, "&maxmagnitude=", maxmagnitude)
        
        return(api_url)
}

test_pnt_in_ploy <- function(map_poly, pnt_df){

        test_poly <- as.matrix(map_poly[ , 1:2])
        test_coords <- as.matrix(pnt_df[, c("longitude", "latitude")])
        
        idxs <- pnt.in.poly(test_coords, test_poly)[ , "pip"]
        idxs <- as.logical(idxs)
        
        new_df <- pnt_df[idxs, ]
        
        return(new_df)
}


# Define UI for application that draws a histogram
ui <- fluidPage(

        # Application title
        titlePanel("Earthquakes by State and Year"),
        
        # Sidebar with a slider input for number of bins 
        sidebarLayout(
                sidebarPanel(
                        selectInput("state_adjust",
                                    "Select State",
                                    choices = state.name[!(state.name %in%
                                                c("Alaska", "Hawaii"))],
                                    selected = "California",
                                    multiple = FALSE
                                    ),
        
        
                        selectInput("year_adjust",
                                    "Select Year",
                                    choices = 1978:2018, 
                                    selected = 2018, 
                                    multiple = FALSE,
                                    selectize = TRUE, 
                                    width = NULL, 
                                    size = NULL
                                    )
                        ),
                
                
        # Show a plot of the generated distribution
        mainPanel(
                plotOutput("quake_plot")
        )
))
        

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {


        output$quake_plot <- renderPlot({
        
                # get base-map with UI input
                base_map <- map_data("state", 
                                     regions = tolower(input$state_adjust))
                
                # get bounding box
                w_e <- range(base_map$long)
                s_n <- range(base_map$lat)
                
                # scrape USGS data
                strt_time <- as.Date(paste(input$year_adjust, 
                                           "01/01", sep = "/"))
                
                end_time <- as.Date(paste(input$year_adjust, 
                                          "12/31", sep = "/"))
                
                data_url <-   get_quake_data(starttime = strt_time, 
                                  endtime = min(today(), end_time),
                                  minlatitude = s_n[[1]], maxlatitude = s_n[[2]],
                                  minlongitude = w_e[[1]], maxlongitude = w_e[[2]],
                                  minmagnitude = 2.5, maxmagnitude = 10)
                
                quake_data <- read_csv(data_url)
                
                quake_data <- test_pnt_in_ploy(base_map, quake_data)
                
                quake_data <- quake_data %>%
                   mutate(short_date = as.Date(time)) %>%
                   arrange(mag)
                
                quake_smry <- quake_data %>%
                   group_by(short_date) %>%
                   summarise(energy=sum(mag))
                
                
                # create plot
                
                ggplot() +
                geom_polygon(data=base_map,
                             aes(x=long, y=lat, group=group),
                             color="black", fill = "grey90", size = 1.5) +
                
                geom_point(data=quake_data,
                           aes(x=longitude, y=latitude, color=mag),
                           alpha=1, size=4, shape=17) +
                
                scale_color_continuous(name="Earthquake \nMagnitude",
                                       high = "yellow", low = "blue",
                                       limits = c(2.5, 7.5),
                                       breaks = seq(from=2.5, to=7.5, by=1),
                                       guide = guide_colorbar(barwidth = 1,
                                                      barheight = 10,
                                                      title.position = "top",
                                                      label.vjust = 0.5)) +
                
                ggtitle(paste(input$state_adjust, "-", input$year_adjust)) +
                labs(caption = paste("Total Number of Earthquakes =",
                                     nrow(quake_data))) + 
                coord_map() +
                
                theme_void() +
                theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 26),
                      plot.caption = element_text(hjust = 0, size=14),
                      legend.position = "right",
                      legend.text = element_text(size=12),
                      legend.title = element_text(size=12))
        
        })
})

# Run the application 
shinyApp(ui = ui, server = server)

