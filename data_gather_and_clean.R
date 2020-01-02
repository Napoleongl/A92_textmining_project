library(readr)
library(stringr)
library(dplyr)
library(magrittr)
movies <- read_csv("IMDB_Horror_movies.csv")

#Cleaning Title of movies
title_patt    <- "(^[[:alnum:]*[:space:]*[:punct:]]*)([:space:]\\([:digit:]{4}\\)?$)"
titles        <- str_replace(string = movies$Title, pattern = title_patt, replace = "\\1")

clean_movies  <- titles %>% tibble::enframe(name = NULL, value = "title")

#Clean out special chars from names And handle middle name initals and Jr.:s
plots <- str_replace_all(string = movies$Plot, pattern = "(\\b[A-Zr]{1,2})\\.", replacement = "\\1")
plots <- str_replace_all(string = plots, pattern = "[\\-\\']", "")

#Splitting into director, cast and actual plot
director_patt <- "Directed by ([[:alnum:]*[:space:]*[\\,]*]*)\\.*"
cast_patt     <- "With ([[:alnum:]*[:space:]*[\\,]*]*)\\.*"
plot_patt     <- "([:print:]*$)"
split_pattern <- paste0(director_patt,"[:space:]*", cast_patt, "[:space:]*", plot_patt)
split_plots   <- str_match(pattern = split_pattern, string = plots)

clean_movies  %<>% mutate(story = split_plots[,4],
                          director = split_plots[,2],
                          cast  = split_plots[,3])

#Indexing movies without a plot
missing_plots <- split_plots[,4] == "" | is.na(split_plots[,4])

#Splitting genres into design matrix
genre_list        <- str_split(movies$Genres,"\\| ", simplify = TRUE)
genre_cats    <- as.vector(genre_list) %>% unique() %>% tibble() %>%
    filter(!is.na(.), . != "") %>% arrange(.data =., .) %>% pull()
genres  <- as_tibble(t(sapply(1:nrow(clean_movies), function(x){
    as.integer(genre_cats[-12] %in% genre_list[x,])
    })))
colnames(genres) <- genre_cats[-12]

genre_counts <- tibble::enframe(colSums(genres), "genre", "movie_count")
min_count <-90
kept_genre_names <- genre_counts %>% filter(movie_count > min_count) %>% pull(genre)
kept_genres <- genres %>% select(kept_genre_names)

genre_counts %<>% mutate(included = ifelse(genre %in% kept_genre_names,
                                           "Included", "Not included"))
#genre_commons <- genre_matrix %*% t(genre_matrix)

# ============== Genre count plot ==============

library(ggplot2)
theme_set(theme_minimal(base_family = "serif"))

genre_counts %>% ggplot(aes(x = genre, y = movie_count, fill = included)) +
      geom_col(col = "grey20",  width = rel(0.35)) +
      theme(panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            axis.text.x = element_text(angle = 60, hjust = 1),
            axis.ticks.x = element_line()) +
      scale_y_log10(expand = c(0,0)) +
      scale_fill_manual("", values = c("grey30", "grey85")) +
      labs(title = "Count of movies by genre", x ="Genre", y = "No of movies")

ggsave("genre_dist.png", width = 8, height = 3.2)

# ==============================================

# Final binding and export

clean_movies %<>% bind_cols(., kept_genres) %>% filter(!missing_plots)

write_csv(clean_movies, "horror_movies_cleaned.csv")
