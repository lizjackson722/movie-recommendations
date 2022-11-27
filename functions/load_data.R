myurl = "https://liangfgithub.github.io/MovieData/"

get_movies <- function() {
  movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
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
  return(movies)
}

get_ratings <- function() {
  ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'), sep = ':',
                     colClasses = c('integer', 'NULL'), header = FALSE)
  colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
  
  return(ratings)
}