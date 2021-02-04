#This script will take the bna_pitchfork_reviews table established in our last script, search for all artists and pull the relevant album information for all reviews and save that to a new dataframe. We will also determine which artists or albums are not offered by spotify and filter those out from our bna_pitchfork_reviews dataframe.

#1. Package installation / load & environment settings ----

#install.packages('genius')
#devtools::install_github('charlie86/spotifyr')

library(tidyverse)
library(genius)
library(spotifyr)
library(magrittr)
library(feather)
library(stringdist)

#2. Establish Spotifyr connection ----

#Client ID for the Spotify API
Sys.setenv(SPOTIFY_CLIENT_ID = 'XXXXXXXXXXXXXXX')
#Client secret for the Spotify API
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'XXXXXXXXXXXXXXX')
#takes Client ID and secret and assigns access token
access_token <- get_spotify_access_token()

#3. Load in tables ----
bna_pitchfork_reviews <- read_csv('clean data/bna_pitchfork_reviews.csv')

bna_pitchfork_reviews <- bna_pitchfork_reviews %>% select(-x)

#4. Defining a wrapper function around spotifyr

#this function will be used inside our wrapper function. It's purpose is to return the closest match by string distance from a vector of strings to a particular string
ClosestMatch2 <- function(string, stringVector){
  
  stringVector[amatch(string, stringVector, maxDist=Inf)]
  
}

#our wrapper function
load_selected_album_data <- function(band_name = "a place to bury strangers" , album = 'a place to bury strangers' ,  dedupe = FALSE, closest_artist = TRUE, level = c('album','single')){
  
try({
#removes x at every start, if x can't be defined afterwards, it will output an error message.  
rm(x)
    
x <- get_artist_audio_features(artist = band_name, include_groups = level, dedupe_albums = dedupe , return_closest_artist = closest_artist) %>%
  #filters to the closest match of the input album
  filter(album_name == ClosestMatch2(album, .$album_name)) %>%
  #mutate to change the list of all markets into a boolean for US market    
  mutate(US_market = str_detect(available_markets, 'US')) %>%
  #selecting our columns of interest, quantitative data about music determined by spotify
  select(artist_name, album_name, track_name, track_number, album_type, danceability, energy, loudness:tempo, duration_ms, US_market, explicit) 

return(x)

#a warning in case of errors    
if(count(x) == 0){ warning(paste('The query for' , band_name, 'produced nothing. Please recheck spelling/formatting of band name or album names'))}
  
  })
  
  if(exists('x') == FALSE){warning(paste('The query for' , band_name, 'threw an error. Please recheck the spelling/formatting of the band name'))
      }
  
}

#5. Our first pull ----

#This line initializes a diagnostic dataframe that our for loop will use in conjunction with our wrapper function
spotify_pull_diagnostic_df <- bna_pitchfork_reviews[,c(2,3)]
#intializes the column for closest matching artist output
spotify_pull_diagnostic_df$closest_spotify_artist <- NA
spotify_pull_diagnostic_df$closest_spotify_album <- NA

#vector of our unique artists
bna_artists <- unique(bna_pitchfork_reviews$artist)

#initializing empty data frame to store for loop results
bna_spotify_data <- data.frame()

#initialize a counter for our diagnostic data frame
num <- 1

#this for loop will iterate over how many unique artists there are. It'll take two hours or so
for (i in 1:length(bna_artists)){
#this nested for loop will iterate over albums belonging to a unique artist  
  for(a in bna_pitchfork_reviews %>% filter(artist == bna_artists[i]) %$% title){
    Sys.sleep(sample(10, 1) * 0.1)    
    try({
#always delete loop defined variable to avoid duplication in the event that y is not overrided
      rm(y)
#this line will use our function to query each album a of all artists i. 
      y <- load_selected_album_data(band_name = bna_artists[i], album = a)
#this line will append our query to the global data frame  
      bna_spotify_data <<- bna_spotify_data %>% rbind(y)    
#these lines updates the diagnostic data frame based on our num counter used as an index
      spotify_pull_diagnostic_df$closest_spotify_artist[num] <- unique(y$artist_name)
      spotify_pull_diagnostic_df$closest_spotify_album[num] <- unique(y$album_name)
  
     })
    #this sets the counter to the next value
    num <<- num + 1  
  }  
}

#saving locally so we don't have to run a 2+ hour loop again
write_feather(bna_spotify_data, 'clean data/bna_spotify_data.feather')

#6. Diagnostics of the first pull ----

spotify_pull_diagnostic_df <- spotify_pull_diagnostic_df %>%
  #mutating spotify names to all lower case to match pitchfork format
  mutate(closest_spotify_artist = tolower(closest_spotify_artist),
         closest_spotify_album = tolower(closest_spotify_album)) %>%
#string distance between pitchfork & spotify artist/album. 0 means a perfect match, count of 1 for every insertion, deletion, or substitution. The higher the string distance, the more unlikely of a match it is.
  mutate(artist_stringdist = stringdist(artist, closest_spotify_artist),
         album_stringdist = stringdist(title , closest_spotify_album))

#7. Adding missing data ----

#7.a Missing artists that are definitely/probably on Spotify ----

#if closest_spotify_artist is NA, then our wrapper couldn't find any matching artist for the query. What artists are these? Are they available to stream on spotify?

spotify_pull_diagnostic_df %>% filter(is.na(closest_spotify_artist)) %>% .[,c(1:4)]

#I find it very hard to believe Drake couldn't be found on spotify. Must've been some type of API limit error. It makes sense that 'various artist' didn't return anything. Typically one recognizes each artist on a compilation type album, rather than saying the album at large is by various artists. Let's programmatically check that each of these artists (aside from various artists) returns anything

#this defines a vector of missing artist names
missing_artists <- spotify_pull_diagnostic_df %>% 
  filter(is.na(closest_spotify_artist)) %>% 
  filter(artist != 'various artists') %$%
  artist %>%
  unique()


sapply(missing_artists, function(x){
  #wrapping function in a try to avoid breaking errors
  try({
    count(get_artist_audio_features(artist = x, dedupe_albums = F, return_closest_artist = F))})
      })

#only Drake returned any rows. So does this mean of these 5 artists only Drake streams through Spotify? No reason to double check programmatically or make guesswork, I have spotify. Drake and The Angels of Light are the only artists of these 5 to stream on spotify. Angels of Light doesn't pull because they dropped the 'the' from their name.

#removing artists we won't find from all dataframes
bna_pitchfork_reviews <- bna_pitchfork_reviews %>%
  filter(!artist %in% c('cyann & ben','diplo, m.i.a.' , 'joanna newsom')) %>%
  #updating one name
  mutate(artist = case_when(artist == 'the angels of light' ~ 'angels of light',
                            TRUE ~ artist)) %>%
  arrange(artist)

spotify_pull_diagnostic_df <- spotify_pull_diagnostic_df %>%
  filter(!artist %in% c('cyann & ben' , 'diplo, m.i.a.' , 'joanna newsom')) %>%
  mutate(artist = case_when(artist == 'the angels of light' ~ 'angels of light',
                            TRUE ~ artist))
#this for loop runs our wrapper function across the two missing artists with different arguments to prevent empty returns
for(i in which(spotify_pull_diagnostic_df$artist %in% c('drake' , 'angels of light') & is.na(spotify_pull_diagnostic_df$closest_spotify_artist))){
  num <<- i
  try({rm(y)
      y <- load_selected_album_data(band_name = spotify_pull_diagnostic_df$artist[i] , album = spotify_pull_diagnostic_df$title[i], closest_artist = FALSE)
      bna_spotify_data <<- bna_spotify_data %>% rbind(y)
      spotify_pull_diagnostic_df$closest_spotify_artist[num] <- unique(y$artist_name)
      spotify_pull_diagnostic_df$closest_spotify_album[num] <- unique(y$album_name)
  })
}

#7.b Adding in 'various artist' compilation albums ----

#Now we have to do something about those compilation albums with 'various artists.' Fortunately, spotify has a URI (unique resource indicator) for every type of object: artist, albums, songs. If each album is available on Spotify, I can copy the URI and find a way to load in the data from that.

#This saves a copy of our diagnostic df filtered to various artists
various_artists_df <- spotify_pull_diagnostic_df %>%
  filter(artist == 'various artists')

#This chunk of lines saves the URI of each album (pulled manually, if available) to the new df.

various_artists_df$closest_spotify_album[1] <- '55BEZB3lHsbsn07VPQO2ci'
various_artists_df$closest_spotify_album[2] <- '1ZgPLRwQF58fsgciZl1PO4'
various_artists_df$closest_spotify_album[3] <- '7bTL38LMOO2a4RaSLJuiyL'
various_artists_df$closest_spotify_album[4] <- NA #There's a good solve for this one
various_artists_df$closest_spotify_album[5] <- '4NpNNp4OpoIxXJSbseGQDn'
various_artists_df$closest_spotify_album[6] <- '5ihjmLIHckzZV07xzBaecz'
various_artists_df$closest_spotify_album[7] <- NA #Truly NA, nothing to do about this.

#initializing empty data frame
various_artist_data_df <- data.frame()
#This for loop iterates over our URIs
for (album in various_artists_df %>% filter(!is.na(closest_spotify_album)) %$% closest_spotify_album){
  try({rm(x)
    x <<- get_album_tracks(id = album, limit = 40) %>%
      select(artists, explicit, id, track_name = name, track_number)
    y <- data.frame()
    record_name <- get_album(album)[['name']]
    type_of_album <- get_album(album)[["album_type"]]
  })
  #this loop iterates over track ids for every album
  for(i in 1:as.integer(count(x))){
    try({
      y <- y %>% rbind(get_track_audio_features(x$id[i]) %>%
                       mutate(artist_name = paste(x[[1]][[i]]$name, collapse = ', '),
                              album_name = record_name,
                              track_name = x$track_name[i],
                              track_number = x$track_number[i],
                              album_type = type_of_album,
                              US_market = TRUE,
                              explicit = x$explicit[i]) %>%
                       select(artist_name, album_name, track_name, track_number, album_type, danceability, energy, loudness, mode, speechiness, acousticness, instrumentalness, liveness, valence, tempo, duration_ms, US_market, explicit))
    })
    # y$artist_name[i] <- x[[1]][[i]]$name
    # y$album_name[i] <- record_name
    # y$track_name[i] <- x$track_name[i]
    # y$track_number[i] <- x$track_number[i]
    # y$album_type[i] <- album_type
    # y$US_market[i] <- TRUE
    # y$explicit[i] <- x$explicit[i]
    # View(y)
    
}
   various_artist_data_df <<- various_artist_data_df %>% rbind(y)
}


bna_spotify_data <-  bna_spotify_data %>% rbind(various_artist_data_df) 

#Fortunately, for one of the albums which is not offered directly on Spotify, one user created a playlist containing most of the songs from the album. There's a total of 15 songs on this album (After Dark 2). This playlist has 10 of those. The other 5 songs are not available on spotify. I think still having these 66% songs will give us a good approximation of the aggregate statistics.

#this for loop iterates through our one URI
for (i in '1A91c8jMr9IIwgsyqqKdLp'){
  #This  defines a shape of a data frame to x
x <- get_playlist_audio_features(playlist_uris = c(i)) %>% select(track.artists, album_name = playlist_name, track_name = track.name, danceability, energy, loudness, mode , speechiness, acousticness, instrumentalness , liveness , valence , tempo , duration_ms = track.duration_ms  , explicit = track.explicit)
#adds columns to match the columns of bna_spotify_data, not all are important for later
x$arist_name <- NA
x$album_type <- 'compilation'
x$track_number <- NA
x$US_market <- TRUE
#this for loop assigns band name
  for(a in 1:10){
     x$artist_name[a] <- x[[1]][[a]]$name
  }
x <- x %>% select(artist_name , album_name, track_name , track_number, album_type , danceability , energy , loudness , mode , speechiness , acousticness , instrumentalness , liveness , valence , tempo , duration_ms , US_market , explicit)
}

#appending new rows 
bna_spotify_data <- bna_spotify_data %>% rbind(x)

#checking if we are missing still any albums, asides from those which are by 'various artists' as we've accounted for them already. Let's update the diagnostic df to remove them

spotify_pull_diagnostic_df <- spotify_pull_diagnostic_df %>%
  filter(artist != 'various artists')

#Are any of our values for closest matching album NA? If not, then there is a match (don't read as exact or good match) for all artists and all albums.
any(is.na(spotify_pull_diagnostic_df %$% closest_spotify_album))

#8. Correcting incorrect data ----

#Great, so let's start going through matches that have large string distances and double checking if they are truly matches. Setting up an index column now to help me later.
spotify_pull_diagnostic_df$x <- NA

for(i in 1:as.numeric(count(spotify_pull_diagnostic_df))){
  spotify_pull_diagnostic_df$x[i] <- i
}

#moving index to front
spotify_pull_diagnostic_df <- spotify_pull_diagnostic_df %>%
  select(x , artist , title , closest_spotify_artist , closest_spotify_album , artist_stringdist , album_stringdist)

#Are all album matches with string distance of 0 also string distance 0 for artist match? It's possible that two different artists can have the same album title. Let's make sure there's none of that instance.

spotify_pull_diagnostic_df %>% 
#filtering to only exact matches on album  
  filter(album_stringdist == 0) %>% 
#filtering out exact artist matches.  
  filter(artist_stringdist != 0)

#It seems all our albums with string dist of 0 also match for artist. Any string distance is caused by a difference of 1 or more for accent marks or extra/removed 'the' in the artist name. Let's continue checking this for our other string distance amounts. In fact, let's programmatically view these instances

for (i in sort(unique(spotify_pull_diagnostic_df$album_stringdist))){
  print(spotify_pull_diagnostic_df[,c(1:7)] %>%
    filter(album_stringdist == i))
}

#I should parse through these views by hand, for a few reasons. Firstly, because it's possible two artists can have the same name but not be the same artist. Therefore, although a search for the artist 'The Field' may have pulled a matching artist by name, it will not actually be a match. Judging by the string distances alone it would appear only the wrong album was pulled, when in fact the wrong album was pulled because the wrong artist was pulled. Second, because often times Pitchfork will review an album in the format it was released. Occasionally an album is released with another album simultaneously, Pitchfork will then review those two albums as one double LP, but Spotify and the artists themselves will still recognize these albums as separate. Sometimes that artist has changed names, so the query we've given our function to search for is now invalid. All these unique instances require domain knowledge. I know a lot about music, but not everything. So - the below chunk assigns true if the artist pulled was not the correct one. It may also update the name of the artist wherever applicable

spotify_pull_diagnostic_df$recheck_artist <- NA
spotify_pull_diagnostic_df$recheck_artist[c(28,29)] <- TRUE
spotify_pull_diagnostic_df$recheck_artist[248] <- TRUE
spotify_pull_diagnostic_df$recheck_artist[c(230:232)] <- TRUE
spotify_pull_diagnostic_df$recheck_artist[21] <- TRUE
spotify_pull_diagnostic_df$recheck_artist[111] <- TRUE
spotify_pull_diagnostic_df$recheck_artist[285] <- TRUE
spotify_pull_diagnostic_df$recheck_artist[207] <- TRUE
spotify_pull_diagnostic_df$recheck_artist[367] <- TRUE
spotify_pull_diagnostic_df$recheck_artist[8] <- TRUE
spotify_pull_diagnostic_df$recheck_artist[102] <- TRUE

spotify_pull_diagnostic_df$recheck_artist[180] <- TRUE
spotify_pull_diagnostic_df$recheck_artist[396] <- TRUE
spotify_pull_diagnostic_df$recheck_artist[526] <- TRUE
spotify_pull_diagnostic_df$recheck_artist[81] <- TRUE
spotify_pull_diagnostic_df$recheck_artist[411] <- TRUE
spotify_pull_diagnostic_df$recheck_artist[446] <- TRUE
spotify_pull_diagnostic_df$recheck_artist[c(513,514)] <- TRUE
spotify_pull_diagnostic_df$recheck_artist[539] <- TRUE

spotify_pull_diagnostic_df$recheck_artist[611] <- TRUE
spotify_pull_diagnostic_df$recheck_artist[180] <- TRUE
spotify_pull_diagnostic_df$recheck_artist[184] <- TRUE

spotify_pull_diagnostic_df$recheck_artist[502] <- TRUE
spotify_pull_diagnostic_df$recheck_artist[512] <- TRUE
spotify_pull_diagnostic_df$artist[132] <- 'danielson'
spotify_pull_diagnostic_df$recheck_artist[132] <- TRUE
spotify_pull_diagnostic_df$recheck_artist[606] <- TRUE
spotify_pull_diagnostic_df$recheck_artist[607] <- TRUE
spotify_pull_diagnostic_df$recheck_artist[618] <- TRUE
#This chunk adds the column to tell us if the artist was the correct one queried, but the album pulled was incorrect. This happens often due to deluxe versions of albums. Being that these albums we are looking for are often some of their 'better' ones, or more acclaimed ones, the artist will then usually have a deluxe version and that one is the one uploaded to streaming platforms, rather than the regular version. So, let's say we want to find the album 'Four' from a given artist, but they also have an album called 'Five.' These two albums are closer in string distance than 'Four (Deluxe Version)', so therefore it would give us the results for 'Five.' This is usually the case for the below chunk. this chunk will also update artist or album name if applicable.
spotify_pull_diagnostic_df$recheck_album <- NA
spotify_pull_diagnostic_df$recheck_album[425] <- TRUE
spotify_pull_diagnostic_df$recheck_album[338] <- TRUE
spotify_pull_diagnostic_df$recheck_album[60] <- TRUE
spotify_pull_diagnostic_df$recheck_album[106] <- TRUE
spotify_pull_diagnostic_df$recheck_album[164] <- TRUE
spotify_pull_diagnostic_df$recheck_album[143] <- TRUE
spotify_pull_diagnostic_df$recheck_album[267] <- TRUE
spotify_pull_diagnostic_df$recheck_album[320] <- TRUE
spotify_pull_diagnostic_df$recheck_album[569] <- TRUE

spotify_pull_diagnostic_df$recheck_album[303] <- TRUE
spotify_pull_diagnostic_df$title[303] <- 'FRKWYS Vol. 13: Sunergy'
spotify_pull_diagnostic_df$recheck_album[123] <- TRUE
spotify_pull_diagnostic_df$recheck_album[288] <- TRUE
spotify_pull_diagnostic_df$title[288] <- 'englaborn & variations'
spotify_pull_diagnostic_df$recheck_album[363] <- TRUE
spotify_pull_diagnostic_df$title[363] <- "blacksummers'night (2016)"
spotify_pull_diagnostic_df$recheck_album[373] <- TRUE 
spotify_pull_diagnostic_df$title[373] <- 'DJ-Kicks (Moodymann) [Mixed Tracks]'
spotify_pull_diagnostic_df$recheck_album[22] <- TRUE #special case, this was released as a playlist
spotify_pull_diagnostic_df$recheck_album[97] <- TRUE
spotify_pull_diagnostic_df$recheck_album[454] <- TRUE

spotify_pull_diagnostic_df$recheck_album[500] <- TRUE
spotify_pull_diagnostic_df$recheck_album[71] <- TRUE
spotify_pull_diagnostic_df$recheck_album[612] <- TRUE # special case for William Basinkski Disintegration loops, need to pull 4 separate albums
spotify_pull_diagnostic_df$recheck_album[147] <- TRUE #also special case for this deerhunter album, need to pull in one more album
spotify_pull_diagnostic_df$recheck_album[209] <- TRUE
spotify_pull_diagnostic_df$recheck_album[463] <- TRUE
spotify_pull_diagnostic_df$recheck_album[255] <- TRUE
spotify_pull_diagnostic_df$recheck_album[302] <- TRUE
spotify_pull_diagnostic_df$recheck_album[13] <- TRUE
spotify_pull_diagnostic_df$recheck_album[118] <- TRUE
spotify_pull_diagnostic_df$recheck_album[365] <- TRUE
spotify_pull_diagnostic_df$recheck_album[476] <- TRUE
spotify_pull_diagnostic_df$title[476] <- 'michigan'
spotify_pull_diagnostic_df$recheck_album[621] <- TRUE

#sometimes Spotify just doesn't offer that one album out of the artist's discography
spotify_pull_diagnostic_df <- spotify_pull_diagnostic_df %>% filter(!title %in% c('headdress' , 'ka' , 'm b v' , 'dj kicks' , 'the ecstatic' , 'simple songs' , 'return of 4eva' , 'uproot' , 'levon vincent' , 'wizard of ahhhs' , 'trans day of revenge' , "oh you're so silent jens" , 'esau mwamwaya and radioclit are the very best' ,'the unrelenting songs of the 1979 post disco crash' ))

#let's also take these out of our pitchfork data.
bna_pitchfork_reviews <- bna_pitchfork_reviews %>%
  filter(!title %in% c('headdress' , 'ka' , 'm b v' , 'dj kicks' , 'the ecstatic' , 'simple songs' , 'return of 4eva' , 'uproot' , 'levon vincent' , 'wizard of ahhhs' , 'trans day of revenge' , "oh you're so silent jens" , 'esau mwamwaya and radioclit are the very best' ,'the unrelenting songs of the 1979 post disco crash' ))


#It seems like a lot, but just how much of our data were we able to pull correctly from our wrapper function and subsequent light work-arounds? ~90%!!!
length(unique(spotify_pull_diagnostic_df %>% filter(is.na(recheck_artist) & is.na(recheck_album)) %$% title))/length(unique(spotify_pull_diagnostic_df$title))

#this initializes a new diagnostic df based on our repull for recheck artist/albums.
spotify_repull_diagnostic_df <- spotify_pull_diagnostic_df %>%
  filter(recheck_artist == TRUE | recheck_album == TRUE, 
         #special case for these two, will do separately
         artist != 'deerhunter' , artist != 'william basinski') %>%
  #index column now meaningless, removed
  select(-x)

#Since this is also the incorrect data, we should probably filter it out of our spotify df.
bna_spotify_data <- bna_spotify_data %>% 
  filter(!tolower(album_name) %in% c(spotify_repull_diagnostic_df %$% closest_spotify_album %>%
#this line prevents us from removing data that we need to 'recheck' because we need to add onto it.
                                      .[c(-7, -18, -58)]))

#reinitializing diagnostic columns of new df
spotify_repull_diagnostic_df$closest_spotify_album[1:58] <- NA
spotify_repull_diagnostic_df$closest_spotify_artist[1:58] <- NA
spotify_repull_diagnostic_df$album_stringdist[1:58] <- NA
spotify_repull_diagnostic_df$artist_stringdist[1:58] <- NA

#recoding recheck NAs as false
spotify_repull_diagnostic_df <- spotify_repull_diagnostic_df %>%
       replace_na(list(recheck_artist = FALSE, recheck_album = FALSE))

#initialize list
x <- list()
#initialize counter
num <- 1
#This iterates over every row in repull df
for (i in 1:as.numeric(count(spotify_repull_diagnostic_df))){
  #if that row is TRUE for recheck artist...
  if(spotify_repull_diagnostic_df$recheck_artist[i] == TRUE){
    try({rm(df)
    #... we define a dataframe using our wrapper function
    df <- load_selected_album_data(band_name = spotify_repull_diagnostic_df$artist[i] , album = spotify_repull_diagnostic_df$title[i] , closest_artist = FALSE)
    #then add the df to our list of objects
    x[[num]] <- df
    #and increase the counter for the next iteration
    num <<- num + 1
    #and define our diagnostic variables
    spotify_repull_diagnostic_df$closest_spotify_artist[i] <- unique(df$artist_name)
    spotify_repull_diagnostic_df$closest_spotify_album[i] <- unique(df$album_name) })
  } else {#if we're not rechecking because of the artist, it's because of the album. 
    try({
    rm(df)
    df <- get_artist_audio_features(artist = spotify_repull_diagnostic_df$artist[i] , return_closest_artist = F , dedupe_albums = F , include_groups = c('album', 'single')) %>% filter(str_detect(string = album_name , pattern = regex(spotify_repull_diagnostic_df$title[i] , ignore_case = T))) %>%
      mutate(US_market = str_detect(available_markets, 'US')) %>%
      #selecting our columns of interest, quantitative data about music determined by spotify
      select(artist_name, album_name, track_name, track_number, album_type, danceability, energy, loudness:tempo, duration_ms, US_market, explicit)
    
    x[[num]] <- df
    num <<- num + 1
    spotify_repull_diagnostic_df$closest_spotify_artist[i] <- unique(df$artist_name)
    spotify_repull_diagnostic_df$closest_spotify_album[i] <- unique(df$album_name)
    })
    
  }
  
}

#we then use string distance to give us a metric to guide us on how accurate our new pulls are.
spotify_repull_diagnostic_df <- spotify_repull_diagnostic_df %>% 
  mutate(artist_stringdist = stringdist(artist , tolower(closest_spotify_artist)),
         album_stringdist = stringdist(title , tolower(closest_spotify_album))) 

#index column matching to list index
spotify_repull_diagnostic_df$x <- 1:58

#presumably, we can add perfect matches in artist/album title to our spotify data.
for (i in spotify_repull_diagnostic_df %>%
  filter(album_stringdist == 0) %$% x){
  bna_spotify_data <<- bna_spotify_data %>% rbind(x[[i]])
  spotify_repull_diagnostic_df <<- spotify_repull_diagnostic_df %>%
    filter(x != i)
}

#despite the string distance by album, some of these are still matches (deluxe versions of albums for example). Here's a manually made vector of those matches.
deluxe_or_bonus_matches_vector <- c(2, 8, 10, 12, 14, 15, 17, 18, 22, 28, 32, 33, 36, 43, 44, 46)

#this iterates over those matches and adds to our data frame
for (i in deluxe_or_bonus_matches_vector){
  bna_spotify_data <<- bna_spotify_data %>% rbind(x[[i]])
  spotify_repull_diagnostic_df <<- spotify_repull_diagnostic_df %>%
    filter(x != i)
}

#after these two loops what we're left with is a repull data frame filtered only to those that were still not able to pull a good match. From here on, I check in Spotify individually if the wrong artist was pulled (due to having the same name), and change to URI code to avoid this issue or I correct the album name to something that works better with str_detect based on the matching spotify album name
spotify_repull_diagnostic_df$title[1] <- 'BEYONCÉ \\[Platinum Edition\\]'
spotify_repull_diagnostic_df$title[2] <- 'Pink'
spotify_repull_diagnostic_df$artist[3] <- '511KFfhbyj2pJ3QJdqE26S'
spotify_repull_diagnostic_df$artist[4] <- '0MT8Af4BlhE02l91O6cfyQ'
spotify_repull_diagnostic_df$artist[5] <- '07fCHWZAafI3Dd4T1svHhd'
spotify_repull_diagnostic_df$title[6] <- 'what is this heart'
spotify_repull_diagnostic_df$artist[7] <- '1bAdBYcsDdsbqmWbAE7qKR'
spotify_repull_diagnostic_df$title[7] <- 'jj n° 2'
spotify_repull_diagnostic_df$title[8] <- 'Englabörn \\& Variations'
spotify_repull_diagnostic_df$title[9]  <- 'The 20/20 Experience \\(Deluxe Version\\)'
spotify_repull_diagnostic_df$title[10] <- "blacksummers'night \\(2016\\)"
spotify_repull_diagnostic_df$title[11] <- 'METZ'
spotify_repull_diagnostic_df$title[12] <- 'DJ-Kicks'
spotify_repull_diagnostic_df$artist[13] <- '4fY9hRf8gHMOszNWFhR1wB'
spotify_repull_diagnostic_df$artist[14] <- '1WFsBUAgQmrGQQEUyFKS60'

#finishing up the repulls 
for (i in 1:as.numeric(count(spotify_repull_diagnostic_df))){
try({
  rm(df)
  df <- get_artist_audio_features(artist = spotify_repull_diagnostic_df$artist[i] , dedupe_albums = F , return_closest_artist = F) %>% 
    filter(str_detect(album_name, regex(spotify_repull_diagnostic_df$title[i] , ignore_case = T))) %>% 
    mutate(US_market = str_detect(available_markets, 'US')) %>%
    select(artist_name, album_name, track_name, track_number, album_type, danceability, energy, loudness:tempo, duration_ms, US_market, explicit)
#assigns df to list while simultaneously viewing  
  x[[spotify_repull_diagnostic_df[i,] %$% x]] <- df
  spotify_repull_diagnostic_df$closest_spotify_artist[i] <- unique(df$artist_name)
  spotify_repull_diagnostic_df$closest_spotify_album[i] <- # pasting unique with comma separator because str_detect may pull the original and deluxe version if both are available on spotify.
    paste(unique(df$album_name), collapse = ', ')
 
})
}

#correcting our list for any dfs comprised of two different albums (deluxe v. standard)
x[[39]] <- x[[39]] %>% filter(album_name == 'First Four EPs')
x[[9]] <- x[[9]] %>% filter(album_name == 'Pink (Deluxe Edition)')

#adding to our spotify data
for (i in spotify_repull_diagnostic_df %$% x){
  bna_spotify_data <<- bna_spotify_data %>% rbind(x[[i]])
}

#the last two missing artists: William Basinski & Deerhunter
df <- get_artist_audio_features(artist = 'deerhunter' , dedupe_albums = F) %>% filter(str_detect(album_name, pattern = regex('microcastle' , ignore_case = T))) %>%  mutate(US_market = str_detect(available_markets, 'US')) %>%
  select(artist_name, album_name, track_name, track_number, album_type, danceability, energy, loudness:tempo, duration_ms, US_market, explicit)

df <- df %>% rbind(get_artist_audio_features(artist = 'William Basinski' , dedupe_albums = F) %>% filter(str_detect(album_name, regex('the disintegration loops' , ignore_case = T))) %>% mutate(US_market = str_detect(available_markets, 'US')) %>%
                     select(artist_name, album_name, track_name, track_number, album_type, danceability, energy, loudness:tempo, duration_ms, US_market, explicit))

bna_spotify_data <- bna_spotify_data %>% rbind(df) %>% arrange(artist_name, album_name)

#9. Visually checking for missing albums / aggregating where necessary ----
#this view of a full join is used by scrolling through the data frame and noting where there are NAs for the duration. NAs are caused whenever there is no exact match between pitchfork and spotify album title. We arrange afterwards to visually see there is an acceptable match (deluxe version of album)
View(full_join(bna_pitchfork_reviews[, c(2,3)] , bna_spotify_data %>% mutate(artist_name = case_when(album_type == 'compilation' ~ 'Various Artists' , TRUE ~ artist_name)) %>% mutate(artist_name = tolower(artist_name) , album_name = tolower(album_name), duration_ms = as.numeric(duration_ms)) %>% select(artist_name , album_name, duration_ms) %>% arrange(artist_name , album_name) %>% group_by(artist_name , album_name) %>% summarize(duration_minutes = sum(duration_ms/60000)) , by = c('artist' = 'artist_name', 'title' = 'album_name') , suffix = c('pitchfork' , 'spotify')) %>% arrange(artist, title))

#removing one extra album from pitchfork, unavailable in spotify
bna_pitchfork_reviews <- bna_pitchfork_reviews %>% filter(!title %in% c('run the road' , 'live.love.a$ap'))

#saving locally
write.csv(bna_pitchfork_reviews , file = 'clean data/bna_pitchfork_spotify_reviews.csv')
write_feather(bna_spotify_data %>% mutate(artist_name = case_when(album_type == 'compilation' ~ 'Various Artists' , TRUE ~ artist_name), across(c('danceability':'duration_ms') , as.numeric)) %>% arrange(artist_name, album_name, track_name) %>% filter(!str_detect(artist_name, 'query')), 'clean data/bna_spotify_data.feather')

#This concludes this script, possibly the largest of the project. In this we have pulled all available audio features using the Spotifyr package of albums that have been given the 'Best New Album' distinction from Pitchfork within the time frame of our data. There is extra data due to different market versions, explicit and clean versions of albums, and deluxe versions of albums containing more music than the original release. Despite this, we are not missing any data or albums that are in the pitchfork data frame unless they are completely unoffered by Spotify.