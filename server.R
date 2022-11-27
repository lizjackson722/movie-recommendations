## server.R

# Load functions
source('functions/cf_algorithm.R') # collaborative filtering
source('functions/movies_by_genre.R') # get movies by genre
source('functions/similarity_measures.R') # similarity measures

# Define functions
get_user_ratings <- function(value_list) {
  dat <-
    data.table(
      MovieID = sapply(strsplit(names(value_list), "_"), function(x)
        ifelse(length(x) > 1, x[[2]], NA)),
      Rating = unlist(as.character(value_list))
    )
  dat <- dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat <- dat[Rating > 0]
  
  # get the indices of the ratings
  # add the user ratings to the existing rating matrix
  user_ratings <- sparseMatrix(
    i = dat$MovieID,
    j = rep(1, nrow(dat)),
    x = dat$Rating,
    dims = c(nrow(ratingmat), 1)
  )
}

#############
# Load data #
#############
movies = readLines('data/movies.dat')
movies = strsplit(movies,
                  split = "::",
                  fixed = TRUE,
                  useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)

# Convert accented characters
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

# Extract year
movies$Year = as.numeric(unlist(lapply(movies$Title, function(x)
  substr(x, nchar(x) - 4, nchar(x) - 1))))
movies$Title = unlist(lapply(movies$Title, function(x)
  substr(x, 0, nchar(x) - 7)))

# Add images
small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID,
                          function(x)
                            paste0(small_image_url, x, '.jpg?raw=true'))

# Ratings
ratings = read.csv(
  'data/ratings.dat',
  sep = ':',
  colClasses = c('integer', 'NULL'),
  header = FALSE
)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')

# reshape to movies x user matrix
ratingmat <-
  sparseMatrix(ratings$MovieID, ratings$UserID, x = ratings$Rating) # movie x user matrix
ratingmat <-
  ratingmat[unique(summary(ratingmat)$i), unique(summary(ratingmat)$j)] # remove movies and users with no ratings
dimnames(ratingmat) <-
  list(MovieID = as.character(unique(ratings$MovieID)), UserID = as.character(sort(unique(ratings$UserID))))


###################
# Server function #
###################

shinyServer(function(input, output, session) {
  
  ###################
  # Movies by genre #
  ###################
  
  genre_movies <- eventReactive(input$submit_genre, {
    withBusyIndicatorServer("submit_genre", {
      # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <-
        "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's rating data
      genre <- input$favorite_genre
      movie_list <- mean_rating_by_genre(genre, 20)
      
      # Return the results
      recom_results <- data.table(
        Rank = 1:20,
        MovieID = movie_list,
        Year = movies$Year[movie_list],
        Title = movies$Title[movie_list]
      )
      
    }) # still busy
    
  }) # clicked on button
  
  # Display genre recommendations
  output$genre_results <- renderUI({
    num_rows <- 4
    num_movies <- 5
    recom_result <- genre_movies()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(
          width = 2,
          status = "success",
          solidHeader = TRUE,
          title = paste0("Rank ", (i - 1) * num_movies + j),
          div(
            style = "text-align:center",
            a(
              href = "https://www.google.com",
              # TODO fix
              target = 'blank',
              img(src = movies$image_url[recom_result$MovieID[(i - 1) * num_movies + j]], height = 150)
            )
          ),
          div(style = "text-align:center; color: #999999; font-size: 80%",
              movies$Year[recom_result$MovieID[(i - 1) * num_movies + j]]),
          div(style = "text-align:center; font-size: 100%",
              strong(movies$Title[recom_result$MovieID[(i - 1) * num_movies + j]]))
        )
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
  
  ##################################
  # Movies based on user's ratings #
  ##################################
  
  # Show the movies to be rated
  output$ratings <- renderUI({
    num_rows <- 20
    num_movies <- 6 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(
          width = 2,
          div(
            style = "text-align:center",
            img(src = movies$image_url[(i - 1) * num_movies + j], style = "max-height:150;")
          ),
          div(style = "text-align:center; color: #999999; font-size: 80%", movies$Year[(i -
                                                                                          1) * num_movies + j]),
          div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
          div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(
            paste0("select_", movies$MovieID[(i - 1) * num_movies + j]),
            label = "",
            dataStop = 5
          ))
        )) #00c0ef
      })))
    })
  })
  
  # Calculate recommendations when user clicks the submit buttton
  df <- eventReactive(input$btn, {
    withBusyIndicatorServer("btn", {
      # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <-
        "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's rating data
      value_list <- reactiveValuesToList(input)
      user_ratings <- get_user_ratings(value_list)
      
      # add user's ratings as first column to rating matrix
      rmat <- cbind(user_ratings, ratingmat)
      
      # get the indices of which cells in the matrix should be predicted
      # predict all movies the current user has not yet rated
      items_to_predict <- which(rmat[, 1] == 0)
      prediction_indices <-
        as.matrix(expand.grid(items_to_predict, 1))
      
      # run the ubcf-alogrithm
      res <-
        predict_cf(rmat,
                   prediction_indices,
                   "ubcf",
                   TRUE,
                   cal_cos,
                   1000,
                   FALSE,
                   2000,
                   1000)
      
      # sort, organize, and return the results
      user_results <- sort(res[, 1], decreasing = TRUE)
      user_results <-
        user_results[which(as.numeric(names(user_results)) <= nrow(movies))]
      user_results <- user_results[1:20]
      user_predicted_ids <- as.numeric(names(user_results))
      print(user_results)
      print(user_predicted_ids)
      recom_results <- data.table(
        Rank = 1:20,
        MovieID = user_predicted_ids,
        Year = movies$Year[user_predicted_ids],
        Title = movies$Title[user_predicted_ids],
        Predicted_rating =  user_results
      )
      
    }) # still busy
    
  }) # clicked on button
  
  # Display UBCF recommendations
  output$ubcf_results <- renderUI({
    num_rows <- 4
    num_movies <- 5
    recom_result <- df()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(
          width = 2,
          status = "success",
          solidHeader = TRUE,
          title = paste0("Rank ", (i - 1) * num_movies + j),
          div(
            style = "text-align:center",
            a(
              href = "https://www.google.com",
              # TODO fix
              target = 'blank',
              img(src = movies$image_url[recom_result$MovieID[(i - 1) * num_movies + j]], height = 150)
            )
          ),
          div(style = "text-align:center; color: #999999; font-size: 80%",
              movies$Year[recom_result$MovieID[(i - 1) * num_movies + j]]),
          div(style = "text-align:center; font-size: 100%",
              strong(movies$Title[recom_result$MovieID[(i - 1) * num_movies + j]]))
        )
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
}) # server function