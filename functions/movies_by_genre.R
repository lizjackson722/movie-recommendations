library(dplyr)

source('functions/load_data.R') # retrieve data from the web

genre_list = c("Action", "Adventure", "Animation", 
               "Children's", "Comedy", "Crime",
               "Documentary", "Drama", "Fantasy",
               "Film-Noir", "Horror", "Musical", 
               "Mystery", "Romance", "Sci-Fi", 
               "Thriller", "War", "Western")

movies <- get_movies()
ratings <- get_ratings()

make_genre_matrix <- function() {
  genres = as.data.frame(movies$Genres, stringsAsFactors=FALSE)
  tmp = as.data.frame(tstrsplit(genres[,1], '[|]',
                                type.convert=TRUE),
                      stringsAsFactors=FALSE)
  m = length(genre_list)
  genre_matrix = matrix(0, nrow(movies), length(genre_list))
  for(i in 1:nrow(tmp)){
    genre_matrix[i, genre_list %in% tmp[i,]] = 1
  }
  rownames(genre_matrix) = movies$MovieID
  colnames(genre_matrix) = genre_list
  remove("tmp", "genres")
  return(genre_matrix)
}

get_rating_count <- function(genre) {
  genre_matrix <- make_genre_matrix()
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
  
  N <- validate_N(N, rating_means) # fix N if client gave a bad value
  movie_list <- rating_means[1:N, "MovieID"]
  movie_indices <- which(movies$MovieID %in% movie_list$MovieID)
  
  return(movie_indices)
}


# Make a cached list of the top 20 films of each genre, to improve performance
# on the front end
make_top_movie_list <- function(N) {
  top_by_genre <<- matrix(0, nrow = length(genre_list), ncol = N)
  for (i in 1:length(genre_list)) {
    top_by_genre[i,] <<- mean_rating_by_genre(genre_list[i], N)
  }
}
