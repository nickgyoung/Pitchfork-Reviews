#This script serves to remove duplicated records from our previously established bna_spotify_data df. We remove extra album information due to clean/explicit album version, US/international album versions, standard/deluxe album versions, and single/album versions. We should make attempts to aggregate data at the album level closest to what the Pitchfork reviewer heard. Being that there are audible differences in an album as a whole based on the mixing/mastering for a given market, or whether the expletives are censored, or whether there are 5 more bonus tracks means that we need to account for these when we aggregate to the album level. We do this by making sure an album is only recorded at the US market level (if more than one level), the uncensored and standard version - which are likely what the Pitchfork reviewer listened to - deluxe edition songs are removed as they are extraneous to the original review listening experience and all observations of tracks are the album version and not the single version.

#1. Library and package load ----

library(tidyverse)
library(feather)
library(stringdist)
library(spotifyr)

#Establish Spotifyr connection

#Client ID for the Spotify API
Sys.setenv(SPOTIFY_CLIENT_ID = 'XXXXXXXXXXXXX')
#Client secret for the Spotify API
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'XXXXXXXXXXXXX')
#takes Client ID and secret and assigns access token
access_token <- get_spotify_access_token()

options(scipen = 99999)

#2. Data load ----
bna_spotify_data <- read_feather('clean data/bna_spotify_data.feather')
#adding index
bna_spotify_data$x <- 1:13710
#updating track name to lowercase to account for capitalization discrepancies between different markets
bna_spotify_data <- bna_spotify_data %>% mutate(track_name = tolower(track_name))

#3. Defining and removing our market-based duplicates ----

#Establishing a new df that logs tracks that have more than one observation in our spotify df due to market
duplicated_tracks <- 
  #this indexes to rows that are duplicates based on the artist, album, track name groups
  bna_spotify_data[sort(bna_spotify_data[,c(1:3)] %>% duplicated() %>% which() %>% append(. - 1)),] %>% 
  #this groups our data frame by artist, album, and track
  group_by(artist_name , album_name, track_name) %>% 
  #we then aggregate and summarize the count of unique boolean values for whether it is offered in the US or not. A value of 2 would indicate it is offerered in the US and outside of the US. In the case of there being both instances, we only care to pull the US market data.
  summarize(n_markets = length(unique(US_market))) %>% filter(n_markets > 1)

#a vector for filtering based on mutiple markets
multiple_market_track_names <- duplicated_tracks %>% pull(track_name)

#removing our duplicates based on market
bna_spotify_data <- bna_spotify_data[-c(bna_spotify_data %>% 
                                          #filters to track_names in our vector and then only chooses those which are not the US market version, then pulls the index value
                                          filter(track_name %in% multiple_market_track_names, US_market == FALSE) %>% pull(x)),]

#4. Removing censorship duplicates ----

#re-indexing
bna_spotify_data$x <- 1:9162

#redefining duplicated tracks based on censorship
duplicated_tracks <- 
  bna_spotify_data[sort(bna_spotify_data[,c(1:3)] %>%
                          duplicated() %>% 
                          which() %>% 
                          append(. - 1)),] %>% 
                          group_by(artist_name , album_name, track_name) %>% 
                          summarize(n_censorship = length(unique(explicit))) %>%                           filter(n_censorship > 1)

#a vector for filtering based on censorship versions
censorship_duplicated_track_names <- duplicated_tracks %>% pull(track_name)


#removing censorship duplicates
bna_spotify_data <- bna_spotify_data[-c(bna_spotify_data %>% filter(track_name %in% censorship_duplicated_track_names, explicit == FALSE #if there's a censored and uncensored version, we would want to keep the uncensored as that is likely what a music reviewer would listen to
)%>% pull(x)) ,]

#5. Removing single/LP duplicates ----
bna_spotify_data$x <- 1:8743

#duplicated tracks based on album type
duplicated_tracks <- 
  bna_spotify_data[sort(bna_spotify_data[,c(1:3)] %>%
                          duplicated() %>% 
                          which() %>% 
                          append(. - 1)),] %>% 
  group_by(artist_name , album_name, track_name) %>% 
  summarize(n_type = length(unique(album_type))) %>%                           filter(n_type > 1)

#a vector for filtering based on single versions
single_duplicated_track_names <- duplicated_tracks %>% filter(n_type == 2) %>% pull(track_name)

#removing single from our data
bna_spotify_data <- bna_spotify_data[-c(bna_spotify_data %>% filter(track_name %in% single_duplicated_track_names, album_type == 'album') %>% pull(x)) ,]




#6. Removing extraneous bonus tracks ----

bna_spotify_data <- bna_spotify_data %>% mutate(track_number = as.integer(track_number)) %>% arrange(artist_name , album_name , track_number , track_name)

#index
bna_spotify_data$x <- 1:8718

#albums that have 'deluxe' in the name, are typically not the standard version that a reviewer would base their listening experience from, with one exception in our case.
deluxe_album_df <- bna_spotify_data %>% filter(str_detect(album_name , regex('deluxe' , ignore_case = T)) , album_name != 'Cupid Deluxe')

#from here I compare to wikipedia and selectively remove extra songs by index of the deluxe_album_df
extra_songs <- c(12:16, 25:32 , 34, 35, 37, 39, 42, 44, 45, 48, 50, 66:67, 75, 80, 83, 85:99, 113:114, 129:136, 149:154,165:166 , 182:183 , 185, 187, 189, 191, 192, 194, 196 , 199:202)

#then extract their index value of the larger df and remove those indices from that df.
bna_spotify_data <- bna_spotify_data[-c(deluxe_album_df[extra_songs,] %>% pull(x)), ]

#re-indexing
bna_spotify_data$x <- 1:8645

#not all extended versions will be called the 'deluxe' version. Let's filter for album names containing 'edition' but not containing 'deluxe.' We can overwrite our last deluxe df with this new information
deluxe_album_df <- bna_spotify_data %>% filter(str_detect(album_name , regex('edition' , ignore_case = T)) , !str_detect(album_name , regex('deluxe' , ignore_case = T))) %>% arrange(artist_name, album_name, track_number, track_name)

#parsing by hand again
extra_songs <- c(5,8,13,14,18,19, 33, 35, 38, 40,41,44, 49:82, 93, 95, 97, 98,100, 103, 104,106,111, 108)

bna_spotify_data <- bna_spotify_data[-c(deluxe_album_df[extra_songs, ] %>% pull(x)),] 

bna_spotify_data$x <- 1:8589

#there's a whole album worth of Disclosure's Settle remixes I've noticed, let's get rid of them here.
bna_spotify_data <- bna_spotify_data[-c(2264:2278),]

bna_spotify_data$x<- 1:8574

#Like 'edition', the word 'version' when included in an album name often implies there are multiple versions.
deluxe_album_df <- bna_spotify_data %>% filter(str_detect(album_name , regex('version' , ignore_case = T)))

extra_songs <- c(40:58)

bna_spotify_data <- bna_spotify_data[-c(deluxe_album_df[extra_songs,] %>% pull(x)),]

bna_spotify_data$x <- 1:8555

#now we can drill down to song title to attempt to discern if it's an additional song that was not in the standard release. For example, if the song title contained 'bonus' in the string.
deluxe_album_df <- bna_spotify_data %>% filter(str_detect(track_name, regex('bonus' , ignore_case = T)))

extra_songs <- c(1,2,5,6, 7,8,9,11,14, 15, 16 )
bna_spotify_data <- bna_spotify_data[-c(deluxe_album_df[extra_songs ,] %>% pull(x)),]

bna_spotify_data$x <- 1:8544
#typically, remixes are not part of the standard album release
deluxe_album_df <- bna_spotify_data %>% filter(str_detect(track_name , regex('remix' , ignore_case = T)))

extra_songs <- c(1:16, 17:21, 23:43 , 45:58, 61:65 , 67, 68, 71, 74, 87:92 )

bna_spotify_data <- bna_spotify_data[-c(deluxe_album_df[extra_songs ,] %>% pull(x)),]

bna_spotify_data$x <- 1:8473

#Likewise, having the word 'mix' in the title, implies that it is not the original album mix.

deluxe_album_df <- bna_spotify_data %>% filter(str_detect(track_name , regex('mix' , ignore_case = T)) , !str_detect(track_name , regex('remix' , ignore_case = T)))

extra_songs <- c(1, 10 ,12, 14, 15 , 17:18 , 23:24 , 25, 29, 31:37)

bna_spotify_data <-  bna_spotify_data[-c(deluxe_album_df[extra_songs ,] %>% pull(x)),]

bna_spotify_data$x <- 1:8455

#I have this theory that any song containing 'edited' in the title, is the clean version of a uncensored song. Since the df is arranged by artist/album/track number/song name alphabetically, the censored and uncensored versions should be right next to each other. The uncensored should be right before the censored to be specific. Let's test this theory.

bna_spotify_data[c(sort(bna_spotify_data %>% filter(str_detect(track_name , regex('edited' , ignore_case = T))) %>% pull(x) %>% append(. -1))) , ]

#there are some songs that don't link up with my pattern, but that's due to track rearrangement for clean/uncensored versions. All song titles with 'edited' have a corresponding duplicate unedited version. So, we can remove them.

bna_spotify_data <- bna_spotify_data %>% filter(!str_detect(track_name , regex('edited' , ignore_case = T)))

bna_spotify_data$x <- 1:8290

#these are all true duplicates, by every column. No need to keep them.
bna_spotify_data <- bna_spotify_data[-c(which(duplicated(bna_spotify_data[, -19]))) ,]

bna_spotify_data$x <- 1:8019

#these are duplicate in every single way except the index column and song title. While we may think title is important, there is actually no musical difference between the song 'We Are Young' by Fun. and 'We Are Young (Feat. Janelle Monae)' by Fun. It's just a matter of record keeping by Spotify.

bna_spotify_data <- bna_spotify_data[-c(which(duplicated(bna_spotify_data[, -c(3,19)]))) ,]

bna_spotify_data <- bna_spotify_data[-c(which(duplicated(bna_spotify_data[, -c(3, 17, 18 ,19 )]))) ,]

bna_spotify_data$x <- 1:7969

#7. Removing unexplained true duplicates ----

#being that our audio feature variables are recorded as a continuous number from 0 to 1, I'd like to round these variables to two decimal places and then recheck for duplicates. There's little need to record the difference between 0.788 and 0.789. Likewise, we can round tempo to a whole number, loudness to one decimal, and change the duration from a measurement of milliseconds to minutes and then round. Any observations that are duplicative by all numerical data are true duplicates and do not need to be recorded.

bna_spotify_data <- bna_spotify_data %>% 
  #rearranging for simplicity of the mutate
  select(x, artist_name, album_name, track_name, track_number, album_type, danceability, energy, speechiness:valence, loudness, tempo, mode, duration_ms, US_market, explicit) %>% 
  #changing milliseconds to minutes, will round next. Rounding tempo to a whole number and loudness to 1 decimal
  mutate(duration_m = duration_ms / 60000, tempo = round(tempo), loudness = round(loudness , 1)) %>% 
  #rounding percent audio features to two decimals, rounding minutes to whole number.
  mutate(across('danceability':'valence', round, digits = 2) , duration_m = round(duration_m, 2))

#how many true duplicates do we have now that can be removed. This call to check for duplicates does not consider the index value (unique to each observation) artist/album/track name (might be unimportant formatting inconsistencies) track number (different album arrangements across different version), album_type, duration in ms or m (differences in milliseconds don't matter, rounded minutes shouldn't matter too much), or US_market or explicit (these should already be accounted for). We can view the below output to make sure it would make sense to remove the duplicates. As long as there is two of each song, we can remove one.
bna_spotify_data[sort(which(duplicated(bna_spotify_data[, -c(1,2,3,4,5,6, 17:20)])) %>% append(.-1)),]

#it's a little unclear for some of the songs. We need to clean up our data based on some other factors. For example, a db level of -60 or less is inaudible silence. This is not a portion of an album that we should aggregate. Often, a band might have a number of silent tracks at the end of an album to conceal one final secret track. A reviewer may or may not be aware of a secret track, but they would not likely consider the portion of silence in their review as part of the musical album, neither should we.

bna_spotify_data <- bna_spotify_data %>% filter(loudness > -60.0)
bna_spotify_data$x <- 1:7952

#there's only one album that's a culprit of this musical silence in our data. A wiki search also shows me I missed removing some bonus songs from this album
bna_spotify_data <- bna_spotify_data[-c(7356:7362),]

#the before duplicate check makes a lot more sense now, so we can remove them from our data.

bna_spotify_data <- bna_spotify_data[-c(which(duplicated(bna_spotify_data[, -c(1,2,3,4,5,6, 17:20)]))) ,]

bna_spotify_data$x <- 1:7896

#we removed the true duplicates (duplicated observations by all measured numerical variables), now which observations are duplicate in name alone and how do we account for the discrepancies in their measurements?

#8. Differencing observations of unexplained duplicates ----

#for all numeric measurements (except mode, loudness, tempo, and duration_m) let's consider a change of 0.05 as significant. We need to arrange our df so any tracks (by name) belonging to the same artist and album are arranged alphabetically.
bna_spotify_data <- bna_spotify_data %>% arrange(artist_name , album_name, track_name)
bna_spotify_data$x <- 1:7896

#df of our name-only duplicated tracks
duplicated_tracks <- bna_spotify_data[sort((which(duplicated(bna_spotify_data[, c(2:4)])) %>% append(. -1))),]

#a counter tiggered by an in in the next loop
num <- 0
#for every unique song in duplicated_tracks
for (i in unique(duplicated_tracks %>% pull(track_name))){
  #for all our numerical variables where changes >= 0.05 are significant
  for(a in colnames(duplicated_tracks)[7:13]){
    #threshold - if the max absolute value of difference is >= 0.05
    if (max(abs(diff(duplicated_tracks %>% filter(track_name == i) %>% pull(a)))) >= 0.90){
      #print the artist name, track name, variable, and difference.
      print(paste(unique(duplicated_tracks %>% filter(track_name == i) %>% pull(artist_name)), i, a, max(abs(diff(duplicated_tracks %>% filter(track_name == i) %>% pull(a))))))
      num <<- num +1
    }
  }
}

#there's only 141 instances where the difference of an audio feature between two duplicates is significant. If we run the loop and increase the threshold we can take a look at our biggest offenders. With the threshold set at 0.60, we only have 3 songs that have that high a difference in their duplicates. I'm familiar with the Grizzly Bear songs. These two are from an EP where the last 3 songs were covers of one song by 3 different bands. This explains why the audio features are so different - in spotify the song and artist have the same name despite being a cover of one song by 3 different artists. This is further proven by the track number of these 'duplicates.' The Bloc Party song is a different story, I'm also familiar with this song. Of the three observations of this song, 2 clock in at 7 minutes with a 0.00 rating for energy. This is inaccurate, let's remove.

bna_spotify_data <- bna_spotify_data[-c(877,878) ,]
bna_spotify_data$x <- 1:7894

#I will change the threshold of the above loop and continue to assess whether it is actually a significant difference and decide an action accordingly: threshold - 0.50
NA

#threshold - 0.40

# I really can't explain the Jamie XX song. There's two version of this on Spotify from two versions of the same album. As far as I can hear, they are the same song, but Spotify has rated them very different as for how 'live' they are. Being that it is not a live song, I'll remove the observation with the higher live rating. These duplicates from Jay-Z are not actually duplicates, but looking into it has revealed other duplicates.

bna_spotify_data <- bna_spotify_data[-c(3394, 3535, 3544, 3545),]
bna_spotify_data$x <- 1:7890

#threshold - 0.30

#again, can't explain Jamie xx, but I'll remove the more 'live' duplicate. Jon Hopkins is an electronic instrumental artist - well keep the more instrumental of his duplicates. Smith Westerns' track is not recorded in front of a live audience. We can remove the more live duplicate.

bna_spotify_data <- bna_spotify_data[-c(3409,3725, 5814),]
bna_spotify_data$x <- 1:7887

#threshold - 0.20

#HTDW's Set it Right is not a live song. Also removing the acapella bonus track. Jamie xx's 'Loud Places' is very much not an instrumental song. But her voice has a certain instrument-like quality to it, I can see where Spotify's algorithm went wrong. Likewise, their song 'The rest is noise' is not acoustic. Danceability is a variable that's a little more subjective, despite the methodology Spotify may use, so we can't discern a duplicate of Jeremih's 'oui' to remove based on this. Fortunately, tempo is a bit more objective, and there's a substantial difference in tempo between the duplicates. The tempo is 120bpm, we will remove the duplicate that observes 79bpm. I really can't tell with the Tame Impala song. Nothing is object besides the duration, but there are actually two versions of the album, one with the song that is that much longer with a small snippet of music that doesn't really feel like it belongs to the song nor is a part of the next one. It's like a musical post-script. I think the best course here is to aggregate the duplicates by average. The Foreign Exchange tracks are not duplicates. They are an original and remix not marked as such. Three duplicates for the TV on the radio song, only one stands out from the other two, let's go with majority rule.

bna_spotify_data <- bna_spotify_data[-c(3216, 3218, 3403, 3412, 3575, 7273 ),]
bna_spotify_data$x <- 1:7881

#threshold - 10 

#Best Coast's 'Summer Mood' is not instrumental. Neither is 'The End.' For Bloc Party's 'So Here We Are,' again danceability is subjective. I don't think this song is very danceable, but I won't remove any duplicates based on this discrepancy. I will the one duplicate that rates it highly for instrumentalness. It's not instrumental. Two duplicates for Broken Social Scene. Will aggregate because both are more accurate for different features. Will aggregate for Cee-lo Green song: despite a large difference between two duplicates for liveness, the song is in two parts - each with different tempos recorded accurately by the duplicates. Will aggregate the spoken word + drum outro as well. Will aggregate CEO songs, no objective features with substantial difference. Remove Cults duplicate as one is inaccurate for major/minor mode and has an incorrect value for instrumentalness. Removing Deerhoof duplicate based on acousticness. Removing 1st Delorean duplicate based on liveness. Removing second duplicate based on incorrect tempo. Removing third song based on instrumentalness. Removing HTDW duplicates based on instrumentalness. Removing Jamie xx duplicates based on acousticness and instrumentalness. Removing miguel duplicate based on liveness. Patrick Wolf based on instrumentalness. Removing Tame Impala's 'why wont you make up your mind?' based on liveness. The Knife's song based on liveness. The Tough Alliance based on instrumentallness. Toro y Moi based on instrumentalness. Removing one TV on the Radio duplicate based on tempo. Removing Wavves bonus tracks.

bna_spotify_data <- bna_spotify_data[-c(718, 721:722, 894, 1513, 1818, 1936, 1949, 1950, 3206, 3217, 3222, 3402 , 3407, 4661, 5242, 6160, 6578, 6882, 7213, 7277, 7627:7628),]

#any differences less than 0.10 I've decided are no longer significant considered they will be aggregated into an average. By checking and correcting for differences larger than 0.10 we've guaranteed there will be no outliers among songs with more than two duplicates that will greatly affect the aggregated measure. Therefore, we no longer need to check for duplicates at this point.

#9. Last check for extra songs ----

#It turns out some albums have been the deluxe version or otherwise a version that contains extra songs from the standard without denoting it as such in the Spotify album name. I will double check by hand that there are the correct amount of songs per album by use of wikipedia.

#First I will check if the album name in Spotify matches to that in our Pitchfork review. If the album names do not match, and if there is a greater difference in string distance, then I will consider that to mean it is more likely not the same exact album.

pitchfork_reviews <- read_csv('clean data/bna_pitchfork_reviews.csv')

pitchfork_reviews <- pitchfork_reviews %>% select(-x) %>% filter(!title %in% c('live.love.a$ap' , 'return of 4eva', 'the unrelenting songs of the 1979 post disco crash' , "oh you're so silent jens" , 'run the road' , 'esau mwamwaya and radioclit are the very best')) %>% filter(!artist %in% c('cyann & ben', 'diplo, m.i.a.' , 'excepter', 'jim orourke', 'joanna newsom' , 'levon vincent' , 'mos def')) %>% mutate(artist = case_when(artist == 'the angels of light' ~ 'angels of light',
artist == 'majesticons' ~ 'the majesticons',
TRUE ~ artist), title = case_when(title == 'greetings from michigan: the great lakes state' ~ 'michigan', TRUE ~ title)) 

bna_spotify_data <- bna_spotify_data %>% filter(!artist_name %in% c('Big K.R.I.T.' , 'Excepter' , 'Jason Forrest' , "Jim O'Rourke" , 'Levon Vincent'), !album_name %in% c('Two Young Lovers' , 'Tha Carter III (MTV Bonus Version)' , 'The Very Best Remixes of the Very Best')) %>% mutate(album_name = case_when(album_name == 'Microcastle' ~ 'Microcastle / Weird Era Cont.',
album_name == 'Weird Era Cont.' ~ 'Microcastle / Weird Era Cont.',
artist_name == 'William Basinski' ~ 'The Disintegration Loops I-IV',
TRUE ~ album_name))

pitchfork_reviews %>% arrange(artist , title) %>% select(artist, title) %>% .[c(301:400),] %>% cbind(

bna_spotify_data %>% arrange(artist_name, album_name) %>% select(artist_name  , album_name, track_name) %>% group_by(artist_name, album_name) %>% summarize(n_tracks = n()) %>% .[c(301:400),]

)

bna_spotify_data <- bna_spotify_data %>% filter(!artist_name %in% c('Mos Def'))

#this has shown me I missed pulling one album into my data, let's correct this now.

missing_from_df <- get_artist_audio_features(artist = 'Menomena' , dedupe_albums = F) %>% filter(str_detect(album_name , regex('friend and foe' , ignore_case =  T))) %>% select(artist_name , album_name, track_name, track_number , album_type, danceability, energy, speechiness , acousticness , instrumentalness , liveness , valence, loudness , tempo, mode , duration_ms, available_markets, explicit) %>% mutate(US_market = str_detect(available_markets , 'US') , duration_m = round((duration_ms / 60000) , 2)) %>% select(-available_markets) %>% mutate(across('danceability':'valence' , round, digits =2)) %>% mutate(loudness = round(loudness , 1) , tempo = round(tempo)) %>% filter(US_market == TRUE) 

missing_from_df$x <- NA

missing_from_df <- missing_from_df %>% select(x, artist_name:duration_m) %>% arrange(track_number)

#removing duplicates based mostly on tempo. Differences in other numeric variables aren't significant or can be aggregated without outlier affect.
missing_from_df <- missing_from_df[-c(7, 21),]

#joining back in with our main data
bna_spotify_data <- bna_spotify_data %>% rbind(missing_from_df)

#arranging again
bna_spotify_data <- bna_spotify_data %>% arrange(artist_name, album_name)

#index 
bna_spotify_data$x <- 1:7807

album_name_comparison_df <- 
pitchfork_reviews %>% filter(title != 'live.love.a$ap') %>% arrange(artist , title) %>% select(artist, title) %>% .[c(1:613),] %>% cbind(
  
  bna_spotify_data %>% arrange(artist_name, album_name) %>% select(artist_name  , album_name, track_name) %>% group_by(artist_name, album_name) %>% summarize(n_tracks = n()) %>% .[c(1:613),]
  
)

album_name_comparison_df <- album_name_comparison_df %>% mutate(across('artist':'album_name' , tolower)) %>% mutate(album_dist = stringdist(title, album_name))

#the further the spotify album name is from the pitchfork name, the less likely they are the same album and the more likely it will have extra tracks.


album_name_comparison_df %>% arrange(desc(album_dist))

#Sheer Mag's album is a compilation of 3 different EPs, we only want the last EP.

bna_spotify_data <- bna_spotify_data[-c(#Sheer Mag's album is a compilation of 3 different EPs, we only want the last EP.
  5670, 5672, 5673, 5676:5680,
  #removing nine b-sides from Menomena's album
  4602, 4603, 4605, 4606, 4608, 4610, 4611, 4613, 4617,
  #extra beyonce songs
  726, 739, 740, 
  #gloss songs
  2809:2813,
  #Johann Johannsson variations/covers
  3599, 3617, 3607, 3611, 3609, 3608, 3614, 3621, 3615, 3602,3619, 
  #black kids song,
  831,
  #wrong DJ/ Rupture album
  2101:2124,
  #Removing extraneous compilation songs for Curren$y,
  1527, 1529:1532, 1535,1536, 1539:1545, 1547, 1548, 1550:1555, 1557, 1559, 1561,1562, 1564, 
  #removing erlend oye songs
  2400, 2401, 
  #removing incorrect MBV album
  4779:4782,
  #Wrong sunburned hand of the man album
  5984:5992,
  #removing extra The Weeknd songs
  6952:6958, 6962:6964, 6966:6978, 6983:6990, 6994, 
  #while listening to the album, I learned this one Chance the Rapper song couldn't be listed on Spotify due to sample clearances. Instead, the song is supplanted by a spoken word notice about sample clearance and a mention that listening to the small spoken word track still generates proceeds that are donated to charity.
  1255),]

#missing intro by Disclosure, removed by accident in previous lines
intro_by_disclosure <- read_feather('clean data/bna_spotify_data.feather')
intro_by_disclosure <- intro_by_disclosure %>% filter(artist_name == 'Disclosure' , album_name == 'Settle (Deluxe)', track_name == 'Intro') 
intro_by_disclosure$x <- 1
intro_by_disclosure <- intro_by_disclosure %>% select(x, artist_name, album_name , track_name, track_number , album_type, danceability , energy , speechiness , acousticness , instrumentalness , liveness , valence , loudness , tempo , mode, duration_ms , US_market , explicit) %>% mutate(duration_m = duration_ms / 60000)

bna_spotify_data <- bna_spotify_data %>% rbind(intro_by_disclosure)
rm(intro_by_disclosure)

bna_spotify_data$x <- 1:7672

#now we will check which albums have an inordinately large number of tracks. These are typically deluxe albums.

View(album_name_comparison_df %>% arrange(desc(n_tracks)))

bna_spotify_data <-  bna_spotify_data[-c(
  #all Crystal Castles' albums are titled 'Crystal Castles,' we just have to filter to the right one
  1460:1463, 1465, 1467:1469, 1473, 1475, 1477:1480, 1482, 1483, 1485:1487, 1489:1494, 1497:1499) ,]

#Adding accidentally deleted song
birds_by_crystal_castles <- read_feather('clean data/bna_spotify_data.feather')
birds_by_crystal_castles <- birds_by_crystal_castles %>% filter(artist_name == 'Crystal Castles' , track_name == 'Birds') 
birds_by_crystal_castles$x <- 1
birds_by_crystal_castles <- birds_by_crystal_castles %>% select(x, artist_name, album_name , track_name, track_number , album_type, danceability , energy , speechiness , acousticness , instrumentalness , liveness , valence , loudness , tempo , mode, duration_ms , US_market , explicit) %>% mutate(duration_m = duration_ms / 60000)

bna_spotify_data <- bna_spotify_data %>% rbind(birds_by_crystal_castles)
rm(birds_by_crystal_castles) %>% mutate(track_number = as.integer(track_number))

bna_spotify_data$x <- 1:7645

bna_spotify_data <- bna_spotify_data[-c(
  #extra bloc party song
  894,
  #extra TVOTR and Lil Wayne songs
  7030, 7031, 7036, 7037, 4146, 4169, 4154, 4152, 4155, 4159, 4167, 4147, 4148, 4170, 4163, 4164,
  #extra Patrick Wolf song
  5061, 
  #extra jay-z songs
  seq(from = 3398, to = 3422 , by = 2)[-7],
  #Extra Janelle Monae
  3332, 3344, 3331, 3326, 3339, 3333,
  #Ghostface Killah,
  2738),]
  
#index
bna_spotify_data <- bna_spotify_data %>% mutate(track_number = as.integer(track_number)) %>% arrange(artist_name , album_name , track_number)
bna_spotify_data$x <- 1:7608

#We're getting to the point of album length where i'm just guessing (the mid 20 range). I don't know all these albums to know the precise amount of tracks they should have. From here on I'll check if an album has two songs listed as the first track or the second and so on. this will tell us if there's duplicates we've accepted as insubstantial or if there is a second CD which would begin the track number from 1. If there are duplicates we've accepted, most likely every track on that album would have that amount of duplicates. If only the first 6 tracks have two duplicates out of 12 tracks, then there is likely a 2nd CD which has 6 songs. This could be a bonus CD or a legitimate second CD from the standard release.

bna_spotify_data %>% arrange(artist_name , album_name , track_number) %>% group_by(artist_name , album_name , track_number) %>% summarize(n_duplicate = n())

#removing duplicative songs, bonus songs, etc.
bna_spotify_data <- bna_spotify_data[-c(53, 61, 64, 250 , 383, 388, 392, 396 , 515, 537 , 604, 615 , 622, 646 , 759, 825, 857 , 963, 967, 1017, 1018, 1024, 1035, 1202, 1240, 1241, 1271, 1380, 1382, 1387, 1391, 1393, 1534, 1536, 1593, 1608, 1703, 1707 ,1709, 1753 , 1779 , 1785, 1824, 2072, 2074 , 2076, 2078 , 2081 , 2084 , 2086 , 2089, 2091 , 2093 ,2095, 2171, 2176 , 2279, 2353, 2376:2381 , 2458, 2461 , 2471 , 2477 , 2630 , 2631 , 2712, 2801 ,2802, 2812, 2828, 2876, 2913, 2917 , 2919, 2922, 2924, 2926, 2928 , 2940, 2941, seq(from = 2944, to = 2952 , by =2), 3045, 3047, 3067, 3106 , 3118 , 3149 , 3158 , 3250 , 3251 , 3254, 3255, 3257, 3260, 3261, 3268, 3269 ,3361 , 3450, 3645, 3646 , 3649, 3650 , 3735, 3745 , 3945, 3947, 3956, 4062, 4084, 4248, 4500, 4599, 4610, 4634, 4751, 5008, 5011, 5074, 5091, 5094, 5095, 5098, 5100, 5105, 5123, 5125:5127, 5148, 5168, 5226, 5230, 5359:5362, 5377, 5378, 5368, 5379, 5380, 5416, 5425, 5492, 5495, 5671, 5701, 5745, 5904, 5952, 6013, 6026, 6087, 6145, 6166, 6167, 6171 , 6247:6250, 6252, 6255, 6259, 6266, 6269, 6272, 6320, 6321, 6324:6333, 6374, 6376, 6378, 6386, 6388, 6406, 6482, 6484, 6486, 6488, 6501, 6508:6510, 6533, 6643, 6649, 6669, 6672, 6673, 6675, 6676, 6678, 6679, 6681,6682, 6684, 6686, 6688, 6689, 6729 , 6732, 6734, 6736, 6737, 6740 , 6741, 6742, 6748, 6749, 6754, 6823, 6874, 6907, 6914, 7016, 7338, 7529, 7531, 7533, 7535, 7538 , 7606, 7608 ),]

#10. Adding songs accidentally deleted ----

#This loop prints all unique track numbers for every artist/album combo. I've noticed some albums are missing the first or last song. They may have been deleted accidentally towards the beginning of the script. I scroll through the output and check if there's any missing tracks from 1 to the end. If the last track is missing, unless I know the album very well, I likely won't catch this. Missing songs are commented below. Missing songs are commented below for my convenience.

for (i in unique(bna_spotify_data$artist_name)){
  for(a in unique(bna_spotify_data %>% filter(artist_name == i) %>% pull(album_name))){
    
  }
}

#Annie's Anniemal is missing the intro, Bloc Party Silent Alarm Track 2, Broken Social Scene You Forgot It In People Track 4, Camera Obscura Track 8, CeeLo Green Track 1, Chance Acid Rap 5, Converge All We Love 8, Deerhunter Cryptograms 1, Drake Take Care 5, Dum Dum Girls End of Daze 4, Eno Hyde High Life 6,  Girls Album 8, Girls Broken Dreams Club 4, Harvey Milk Life 8 , Hot Chip One Life 4, Iceage New Brigade 1, Iceage Plowing 5, Jessie Ware Devotion 13, Johann Johannson Forsetar c(25,27) , Junior Boys Last Exit c(12, 13), Lower Dens Escape from Evil 9 , M83 Dead Cities 11, M83 Hurry Up 1, Nas Life is Good 17, Neon Indian Vega Intl 2, Non Prophets Hope 1, Off! First Four , OneohTrix Garden of c(1, 8, 12) , Parquet Courts Human PErformance 4, Phoenix Wolfgang 5, Portishead Third 7 , Sufjan Age of Adz 2, Flaming Lips Embryonic c(15, 16) , The xx xx - 1, Weyes Blood Front row - 2, Wild Nothing Nocturne 8 

#This function allows us to pull an artist/album from Spotify API and filter to a specific song then adds to our main df
add_missing_song <- function(band = 'Annie' , record = 'Anniemal' , song = 'Intro', multiple_songs = FALSE){
  df <- get_artist_audio_features(artist = band, return_closest_artist = F , dedupe_albums = F, include_groups = c('album','single','compilation')) %>% filter(album_name == record) %>% filter(str_detect(track_name, regex(song , ignore_case = T)))
  
  df$x <- 1:nrow(df)
  
  df <- df %>%
    mutate(US_market = str_detect(available_markets, 'US'), duration_m = duration_ms / 60000) %>%
    select(x, artist_name, album_name, track_name, track_number, album_type, danceability, energy, speechiness, acousticness, instrumentalness , liveness , valence, loudness, tempo, mode, duration_ms, US_market, explicit, duration_m) 
  
  if(multiple_songs == FALSE){
    if(length(unique(df$US_market)) > 1){df <- df %>% filter(US_market == TRUE)}
    
    if(length(unique(df$explicit)) > 1){df <- df %>% filter(explicit == TRUE)}
  }
  
  if(nrow(df) > 0){bna_spotify_data <<- bna_spotify_data %>% rbind(df)}
  else {warning(paste('The query for' , band , '-' , song, 'threw an error and pulled no results'))}
  
}

#Adding missing songs
add_missing_song() 
#replacing the whole Bloc Party album with the appropriate US Version
add_missing_song(band = 'Bloc Party' , record = 'Silent Alarm (U.S. Version)' , song = '.', multiple_songs = TRUE)
bna_spotify_data <- bna_spotify_data %>% filter(album_name != 'Silent Alarm')

#replacing whole A$AP ROcky album with uncensored version
add_missing_song(band = 'A$AP Rocky' , record = 'LONG.LIVE.A$AP (Deluxe Version)' , song = '.' , multiple_songs = TRUE)
bna_spotify_data <- bna_spotify_data %>% filter(album_name != 'LONG.LIVE.A$AP')

add_missing_song(band = 'Broken Social Scene' , record = 'You Forgot It In People' , song = 'Almost Crimes')
add_missing_song(band = 'CeeLo Green' , record = 'Cee-Lo Green... Is The Soul Machine' , song = 'Intro')
add_missing_song(band = 'deerhunter' , record = 'Cryptograms' , song = 'Intro')
add_missing_song(band = 'Drake' , record = 'Take Care' , song = 'take care')

add_missing_song(band = 'Dum Dum Girls' , record = 'End of Daze' , song = 'Lord Knows')

add_missing_song(band = 'Girls' , record = 'Album', song = 'Summertime')

add_missing_song(band = 'Girls', record = 'Broken Dreams Club' , song = 'Alright')

add_missing_song(band = 'Harvey Milk' , record = 'Life...The Best Game in Town' , song = 'Roses')

add_missing_song(band = 'Iceage', record = 'New Brigade' , song = 'Intro')

add_missing_song(band = 'Iceage' , record = 'Plowing into the Field of Love' , song = 'Stay')

add_missing_song(band = 'Lower Dens' , record = 'Escape From Evil' , song = 'Company')

add_missing_song(band = 'M83' , record = 'Dead Cities, Red Seas & Lost Ghosts' , song = 'Gone')

add_missing_song(band = 'M83', record = "Hurry up, We're Dreaming" , song = 'Intro')
add_missing_song(band = 'Neon Indian' , record = 'Vega Intl. Night School' , song = 'Annie')
add_missing_song(band = 'Non Prophets' , record = 'Hope' , song = 'Intro')
add_missing_song(band = '4fY9hRf8gHMOszNWFhR1wB' , record = 'First Four EPs' , song = 'Black Thoughts')
add_missing_song(band = 'Oneohtrix Point Never', record = 'Garden of Delete', song = 'Intro|Animals|No Good')
add_missing_song(band = 'Parquet Courts' , record = 'Human Performance' , song = 'Outside')
add_missing_song(band = 'Portishead' , record = 'Third' , song = 'deep water')
add_missing_song(band = 'Sufjan Stevens' , record = 'The Age of Adz' , song = 'Too Much')
add_missing_song(band = 'The xx', record = 'xx' , song = 'Intro')
add_missing_song(band = 'Weyes Blood' , record = 'Front Row Seat to Earth' , song = 'Used to Be')
add_missing_song(band = 'Wild Nothing' , record = 'Nocturne' , song = 'Paradise')


#Even when adding songs, we discover there are some albums that had extra bonus tracks
bna_spotify_data <-  bna_spotify_data %>% filter(!track_name %in% c('life in san francisco',  'solitude', 'imagine it was us', "what you won't do for love - live at the cherrytree house" , 'unbirthday' , 'a certain association', 'nasty', 'the black bond' , "where's the love" , 'doomage' , 'already dead' , 'magic doors - live' , 'Jodye' , 'Ghetto Symphony (feat. Gunplay & A$AP Ferg)' , 'Angels','I Come Apart (feat. Florence Welch)' , 'Purple Swag REMIX (feat. Bun B, Paul Wall & Killa Kyleon)'))

#reorganizing again to group by artist and album
bna_spotify_data <- bna_spotify_data %>% mutate(track_number = as.integer(track_number)) %>% arrange(artist_name , album_name , track_number)
bna_spotify_data$x <- 1:nrow(bna_spotify_data)

#I don't have the domain knowledge of every album in this data to know if it has the correct amount of tracks. I can check that every album has track number 1 through the last track, but I won't know what the appropriate last track is. Rather than check every album, I will check a significant sample size of the population. If our sample of albums looks good, then I will proceed in the next part of this project as though the rest of our data is clean.

#Seed for reproducibility
set.seed(30)
#filters our population of albums to 235 unique albums. This number was chosen from this website https://www.qualtrics.com/experience-management/research/determine-sample-size/
View(bna_spotify_data %>% filter(album_name %in% sample(unique(bna_spotify_data$album_name) , 235 , replace = F)))

#songs to remove from this comb-through
bna_spotify_data <- bna_spotify_data %>% filter(!track_name %in% c('counting feat. mykki blanco', 'heroes and villains - instrumental' , 'shining violence' , 'circled sun' , 'bell','the gemini','accelerator' , 'cold youth' , 'world of hair (the ram ones)' , 'the motto' , 'velvet cake' , 'life after death' , 'ambrosia' , 'good night' , 'where you go i go too, pt. 1 - prins thomas edit' , 'where you go i go too, pt. 2 - prins thomas edit' , 'grand ideas - prins thomas radio edit' , 'the long way home - prins thomas edit' , 'black and white (7-inch version)' , 'almost there',' american blood','constant conversations - stripped' , "take a walk - burns' sftcr version" , 'culling of the fold' ))

#songs to add from this comb-through
add_missing_song(band = 'Bat For Lashes' , record = 'The Haunted Man', song = 'Laura')
add_missing_song(band = 'Sepalcure' , record = 'Sepalcure' , song = 'outside')

#From our sample, we only had to add two songs. We had to remove 24 songs however. On average, an album has about 12 songs. Across a sample size of 235 albums, that's 2820 songs. We removed less than one percent of our sample data. Based on this, I think we can proceed to the next step of the project believing that our data is as clean as it needs to be.

#11. Saving cleaned data
write_feather(bna_spotify_data, 'clean data/bna_spotify_data_for_clustering.feather')
