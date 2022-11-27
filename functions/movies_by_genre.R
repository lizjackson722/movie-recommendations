library(dplyr)

genre_list = c("Action", "Adventure", "Animation", 
               "Children's", "Comedy", "Crime",
               "Documentary", "Drama", "Fantasy",
               "Film-Noir", "Horror", "Musical", 
               "Mystery", "Romance", "Sci-Fi", 
               "Thriller", "War", "Western")

get_rating_count <- function(genre) {
  genre_index <- which(genre_matrix[,genre] == 1)
  movie_list <- movies[genre_index,]
  
  genre_ratings <- ratings[which(ratings$MovieID %in% movie_list$MovieID),]
  rating_count <- genre_ratings %>% group_by(MovieID) %>%
    summarise(TotalRatings=n(), .groups = 'drop')
  
  return(rating_count)
}

validate_N <- function(N, matrix) {
  if((class(N) != "numeric") || (N <= 0)) { # default to 0 if user gave bad N
    N <- 10
  } else if (N > nrow(matrix)) {
    N <- nrow(matrix)
  }
  
  return(N)
}

# Recommendation system 1: Return top N movies in the genre ordered by the
# mean rating per movie. If multiple movies have the same rating, the movie with
# more total ratings is returned first.
mean_rating_by_genre <- function(genre, N) {
  
  rating_count <- get_rating_count(genre)
  
  # Drop movies that have not been rated much so they don't skew the results
  rating_count <- rating_count[which(rating_count$TotalRatings >= 25),]
  
  rating_means <- ratings %>% group_by(MovieID) %>%
    summarise(MeanRating = mean(Rating, na.rm=TRUE))
  rating_means <- rating_means %>% inner_join(rating_count, by = "MovieID")
  rating_means <- rating_means[order(-rating_means$MeanRating, -rating_means$TotalRatings),]
  
  N <- validate_N(N, rating_means)
  
  movie_list <- rating_means[1:N, "MovieID"]
  movie_indices <- which(movies$MovieID %in% movie_list$MovieID)
  
  return(movie_indices)
}