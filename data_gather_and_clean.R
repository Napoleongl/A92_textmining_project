library(readr)
library(stringr)
library(dplyr)
movies <- read_csv("IMDB_Horror_movies.csv")

#Clean out special chars from names And handle middle name initals and Jr.:s
plots <- str_replace_all(string = movies$Plot, pattern = "(\\b[A-Zr]{1,2})\\.", replacement = "\\1")
plots <- str_replace_all(string = plots, pattern = "[\\-\\']", "")

#Splitting into director, cast and actual plot
director_patt <- "Directed by ([[:alnum:]*[:space:]*[\\,]*]*)\\.*"
cast_patt     <- "With ([[:alnum:]*[:space:]*[\\,]*]*)\\.*"
plot_patt     <- "([:print:]*$)"
split_pattern <- paste0(director_patt,"[:space:]*", cast_patt, "[:space:]*", plot_patt)
split_plots   <- str_match(pattern = split_pattern, string = plots)

#Indexing movies without a plot
missing_plots <- split_plots[,4] == "" | is.na(split_plots[,4])

#Cleaning Title of movies
title_patt    <- "(^[[:alnum:]*[:space:]*[:punct:]]*)([:space:]\\([:digit:]{4}\\)?$)"
titles        <- str_replace(string = movies$Title, pattern = title_patt, replace = "\\1")

#Splitting genres into design matrix
genres        <- str_split(movies$Genres,"\\| ", simplify = TRUE)
genre_cats    <- as.vector(genres) %>% unique() %>% tibble() %>%
    filter(!is.na(.), . != "") %>% arrange(.data =., .) %>% pull()
genre_matrix  <- t(sapply(1:nrow(genres), function(x){
    as.integer(genre_cats[-12] %in% genres[x,])
    }))
colnames(genre_matrix) <- genre_cats[-12]
#genre_commons <- genre_matrix %*% t(genre_matrix)







u#genres <- str_replace( "[:space:]?Horror", "")
#genres <- lapply(genres, function(x){ str_replace(x, "\\|", "") } )


#Adapted from https://www.kaggle.com/bvarkoly/horror-movies-01-data-cleaning
genre_columns <- sapply(1:9, function(i) { paste0("genre_", i) })

library(caret)
separated_genres <- movies %>%
    select(Genres) %>%
    filter(!is.na(Genres)) %>%
    separate(col = Genres, into = genre_columns, sep = "\\|", remove = TRUE)
    gather(key = "genre_index", value = "genre", genre_1:genre_9) %>%
    mutate(genre = str_trim(genre)) %>%
    mutate(genre = str_replace(genre, "-", "_")) %>%
    filter(!is.na(genre)) %>%
    mutate(genre_index = NULL)


titles
split_plots[57,]

sum(!is.na(split_plots[,4]))

length(unique(split_plots[,2]))


