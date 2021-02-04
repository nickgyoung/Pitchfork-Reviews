#In this script we will take our best new album data frame with Spotify audio features as run a k-means cluster analysis to determine what meaningful types of albums Pitchfork awarded Best New Album from Jan 5, 1999 to Jan 8 , 2017

#1. Library load and package installation----
library(tidyverse)
library(feather)
library(cluster)


options(scipen = 99999)

#2. Data load
bna_spotify_cluster_df <- read_feather('clean data/bna_spotify_data_for_clustering.feather')

bna_spotify_cluster_df <- bna_spotify_cluster_df %>% arrange(artist_name , album_name)

#reading in the reviews and subsequent genres for all Best new Album reviews.
bna_pitchfork_reviews <- read_csv('clean data/bna_pitchfork_reviews.csv')
genres <- read_csv('clean data/genres.csv')

#joining genre to review
bna_genres <- left_join(bna_pitchfork_reviews , genres, by = 'reviewid') %>% select(reviewid , artist , title , genre)

#some albums have two genres. Here we combine two observations of genre into one string.
bna_genres <- bna_genres %>% arrange(reviewid)
bna_genres <- bna_genres %>% group_by(artist, title) %>% summarize(genre = paste(genre , collapse = ' / '))

rm(bna_pitchfork_reviews)
rm(genres)

#3. Pre-processing for cluster analysis

#making sure this boolean is a boolean
bna_spotify_cluster_df <- bna_spotify_cluster_df %>% mutate(explicit = as.logical(explicit))

#aggregating to the album level with mean value for all our features except duration is by sum of minutes.
bna_spotify_cluster_df <- bna_spotify_cluster_df %>% 
  group_by(artist_name , album_name) %>% 
  summarize(danceability = mean(danceability),
             energy = mean(energy),
             speechiness = mean(speechiness),
             acousticness = mean(acousticness),
             instrumentalness = mean(instrumentalness),
             liveness = mean(liveness),
             valence = mean(valence),
             loudness = mean(loudness),
             tempo = mean(tempo),
             mode = mean(mode),
             explicit = mean(as.integer(explicit)),
             duration_m = sum(duration_m))

#scaling our data so it may be comparable.
scaled_bna_spotify <- bna_spotify_cluster_df[, c(1,2)] %>% cbind(as.data.frame(scale(bna_spotify_cluster_df[, -c(1,2)]))) %>% mutate(artist_name = tolower(artist_name) , album_name = tolower(album_name))

#joining genre by artist name and album title.
scaled_bna_spotify <- left_join(scaled_bna_spotify , bna_genres , by = c('artist_name' = 'artist' , 'album_name' = 'title'))

scaled_bna_spotify <- scaled_bna_spotify %>% arrange(genre)

scaled_bna_spotify$x <- 1:nrow(scaled_bna_spotify)

#df of observations that couldn't pull genre due to mismatching artist/album name.
genre_na <- scaled_bna_spotify %>% filter(x >= 533) %>% select(-genre)

#removing these from our main data, will append afterwards.
scaled_bna_spotify <- scaled_bna_spotify %>% filter(x < 533)

#joining by album name alone.
genre_na <- left_join(genre_na , bna_genres, by = c('album_name' = 'title')) %>% select(-artist)

genre_na$x <- 1:nrow(genre_na)

#for every row of our missing genre df
for (i in 1:74){
  #if the genre is NA
  if(is.na(genre_na$genre[i])){
    #if the unique amount of genres to that corresponding index artist is length 1
    if(length(unique(bna_genres %>% filter(artist == genre_na$artist_name[i]) %>% pull(genre))) == 1){
      #set row index genre to that unique genre
      genre_na$genre[i] <- unique(bna_genres %>% filter(artist == genre_na$artist_name[i]) %>% pull(genre))
    }
    
  }
}

#block of hand defined genres for cases in which the artist has been given multiple genres across their reviews.
genre_na$genre[3] <- 'rock'
genre_na$genre[5] <- 'pop/r&b'
genre_na$genre[17] <- 'rock / electronic'
genre_na$genre[18]  <- 'rap'
genre_na$genre[35] <- 'rock / electronic'
genre_na$genre[43:44] <- 'experimental'
genre_na$genre[66] <- 'metal / experimental'

#joining old observations with new genre back into our df
scaled_bna_spotify <- scaled_bna_spotify %>% rbind(genre_na) %>% arrange(artist_name , album_name) %>% select(-x)
rm(genre_na)

#4. K-Means Cluster Analysis ----

#determing our ideal k with a scree plot
tot_within_ss <- map_dbl(1:10 , function(k){
  model <- kmeans(x = scaled_bna_spotify[, -c(1,2, 15)] , centers = k)
  model$tot.withinss
})

elbow_diagnostic_df <- data.frame(k = 1:10 , tot_withinss = tot_within_ss)

elbow_plot <- ggplot(data = elbow_diagnostic_df , aes(x = k , y = tot_withinss)) + geom_line() + geom_point() + labs(title = 'Scree plot for Total Within-Cluster Sum of Squares') + ylab('Tot. WSS') + scale_x_discrete(limits = c(1:10))

#saving for later use
ggsave(filename = 'C:/Users/Nick/Documents/personal-website/static/img/scree_plot.png' , device = 'png')

#Scree plot shows the eblow at k = 3. How about silhouette width analysis?

silhouette_width <- map_dbl(2:10, function(k){
  model <- pam(x = scaled_bna_spotify[, -c(1,2, 15)] , k = k)
  model$silinfo$avg.width
})

sil_df <- data.frame( k =2:10 , sil_width = silhouette_width)

print(sil_df)

silhouette_analysis <- ggplot(sil_df , aes(x = k , y = sil_width)) + geom_point() + geom_line() + labs(title = 'Avg. of Total Silhouette Widths for K') + ylab('Silhouette Width')

ggsave(filename = 'C:/Users/Nick/Documents/personal-website/static/img/sil_analysis.png' , device = 'png')

#this diagnostic method seems to suggest a k of 2. Let's try both and see what these clusters look like.

#saves the model with k of 2
km_model_2 <- kmeans(scaled_bna_spotify[, -c(1,2,15)] , centers = 2)

#appends the clusters from k = 2 model to unscaled data.
bna_spotify_cluster_df$cluster <- km_model_2$cluster

k2_cluster_analysis <- bna_spotify_cluster_df[, c(3:15)] %>%group_by(cluster) %>% summarize_all(mean)




#saves the model with k of 3
km_model_3 <- kmeans(scaled_bna_spotify[, -c(1,2,15)] , centers = 3)

#appends the clusters from k = 3 model to unscaled data.
bna_spotify_cluster_df$cluster <- km_model_3$cluster

k3_cluster_analysis <- bna_spotify_cluster_df[, c(3:15)] %>%group_by(cluster) %>% summarize_all(mean)


#saving these analysis
write.csv(k2_cluster_analysis , file = 'clean data/k2_analysis.csv')
write.csv(k3_cluster_analysis , file = 'clean data/k3_analysis.csv')



