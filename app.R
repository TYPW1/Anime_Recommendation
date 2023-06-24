# Load the necessary packages
library(shiny)
library(recommenderlab)
library(reshape2)

# Load and preprocess the anime data
anime_data <- read.csv("anime.csv")
anime_data_wide <- dcast(anime_data, user_id ~ anime_id, value.var = "rating")
rownames(anime_data_wide) <- anime_data_wide$user_id
anime_data_wide$user_id <- NULL
anime_matrix <- as(as(anime_data_wide, "matrix"), "realRatingMatrix")

# Define the Shiny app
ui <- fluidPage(
  titlePanel("Anime Recommendation System"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("userInput", "Enter your user ID:", value = 1),
      actionButton("goButton", "Get Recommendations")
    ),
    
    mainPanel(
      tableOutput("animeTable")
    )
  )
)

server <- function(input, output) {
  
  recommendations <- reactiveVal(NULL)
  
  observeEvent(input$goButton, {
    user_id <- as.numeric(input$userInput)
    
    # Create a recommender model
    recommender_model <- Recommender(anime_matrix, method = "UBCF")
    message("Recommender model created.")
    
    # Generate recommendations for the input user
    recs <- predict(recommender_model, anime_matrix[user_id, ], n = 5)
    recs_list <- as(recs, "list")
    
    if (length(recs_list[[1]]) == 0) {
      message("No recommendations generated.")
    } else {
      message("Recommendations generated: ", paste(names(recs_list[[1]]), collapse=", "))
    }
    
    message("Full recommendation object: ", paste(deparse(recs_list), collapse=", "))
    
    # Save the recommendations
    recommendations(recs_list)
  })
  
  output$animeTable <- renderTable({
    recs <- recommendations()
    if (!is.null(recs)) {
      data.frame(Anime = recs[["0"]], stringsAsFactors = FALSE)
    }
  })
}

shinyApp(ui = ui, server = server)

