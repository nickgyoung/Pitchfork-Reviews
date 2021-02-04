#Best New Music (bnm) is an accolade given to new music of distinction by Pitchfork within the review. To quote the website, "Best New Music is Pitchfork's way of of highlighting the finest music of the current moment."

#Best New Music is broken up into three categories: best new album, best new re-issue, and best new track. Best new album is given for genuinely new music that Pitchfork feels is of distinction, best new reissue are for older albums that have been given a re-release or a reissue in a new format than it was when originally released. Best new track is given to promotional singles or individual songs that are of distinction. Because out pitchfork_reviews table is of reviews of music at the album level, our best_new_music variable is really just a measure of whether that album was given best new album or best new reissue.

#The goal of this project is to take all observations of best new album and see if we can draw insights by observing commonalities between these albums or other patterns.

#This script will filter pitchfork_reviews to only those albums that have been given Best New Album.

#1. Package installation / load ----
library(rvest)
library(dplyr)
library(stringr)

#2. Load relevant data ----
pitchfork_reviews <- read.csv('clean data/pitchfork_reviews.csv')

#3. Filtering our table ----

pitchfork_reviews <- pitchfork_reviews %>%
#this line filters out albums that have not been given the best new music accolade
  filter(best_new_music == 1) %>%
#this line creates a new variable as the difference in years between album release and year the review was written. Differences in years greater than 1 are likely to be reissues.
  mutate(years_between_review_and_release = pub_year - year) %>%
#this line arranges our new variable descending
  arrange(desc(years_between_review_and_release))

#A useful intricacy of the Pitchfork review methodology is that they will often give a re-issue review two years of release. The year of it's original release, and the year of the re-issue release. Meaning, any albums that are re-issues should hopefully be picked up when we check for duplicates.

#Creating a vector of row indices for duplicates
duplicates <- pitchfork_reviews %>%
#this line arranges by URL. Every review on their website has one unique URL.  
  arrange(url) %>%
#this line selects only the URL column.
  select(url) %>%
#this line checks for duplicates. The value is true if that index is the same as the previous value
  duplicated() %>%
#this changes our vector of boolean values into the index values of those which are TRUE  
  which(TRUE) %>%
#this appends those values to all the same values subtracted by 1 (to get the value it duplicated)  
  append(. - 1) %>%
#this sorts in numerical order
  sort()

#Not all reviews that have two years are re-issues. An important distinction is the album with a multi-national release. An example may be release in the US near the end of 2005 and then in Asia at the start of 2006. In which case Pitchfork will use both years for their review. We can utilize web-scraping package rvest to check our duplicated urls for whichever contain the phrase 'best new reissue' in their html.
  
duplicated_urls <- pitchfork_reviews %>% 
  arrange(url) %>% 
#this sorts to the index vector 
  .[duplicates,] %>%
  pull(url) %>%
#eliminates duplicated urls, we don't need to check a web page twice if it contains a phrase we're matching to
  unique() 


#this will return a boolean vector which we will use as indices for our data frame
reissue_boolean_vector <- as.vector(duplicated_urls %>%
    #applies a user defined function across our vector of urls  
  sapply(function(url) {
  #user function begins by reading the html of the given url
    read_html(url) %>%
  #then we extract html within the node bnm-txt. The name of this node was found within Google Chrome inspector
    html_node('.bnm-txt') %>%
  #then we convert the html to plain text
    html_text() %>%
  #convert the text to all lowercase for simplicity
    str_to_lower() %>%
  #then search for 'best new reissue'
    str_detect('best new reissue')
  }) 
)

#filtering by the negation of our boolean vector, these are not actually reissues
sapply(duplicated_urls[!reissue_boolean_vector] , browseURL)

#so we can filter our data frame to urls only not within the TRUE duplicated urls, as in this should only be our best new albums (bna)
pitchfork_reviews <- pitchfork_reviews %>%
  filter(!url %in% duplicated_urls[reissue_boolean_vector]) %>%
  arrange(url)

#there probably are still some duplicate entries for that multiple release year type of instance I mentioned previously, let's filter them out.

duplicates <- which(duplicated(pitchfork_reviews$url) == TRUE)
pitchfork_reviews <- pitchfork_reviews[-duplicates,]

#Unfortunately, not all re-issue reviews notate two years. It was a nice detail to filter a portion of our data, but we'll have to do the rest in one go.

reissue_boolean_vector <- as.vector(pitchfork_reviews$url %>%
  sapply(function(url) {
#adding a system sleep with a degree of randomness to avoid the server shutting us out since this is a much larger data pull than before.
    Sys.sleep(sample(10,1) * 0.1)
    read_html(url) %>%
    html_node('.bnm-txt') %>%
    html_text() %>%
    str_to_lower() %>%
    str_detect('best new reissue')

    
  }) 
)

#hopefully this is only now only our actual best new albums
pitchfork_reviews <- pitchfork_reviews[-c(which(reissue_boolean_vector == TRUE)),]

#we can check one last time that all our urls in the data frame contain 'best new album'

as.vector(pitchfork_reviews$url[1:629] %>%
            sapply(function(url) {
              #adding a system sleep with a degree of randomness to avoid the server shutting us out since this is a much larger data pull than before.
              Sys.sleep(sample(10,1) * 0.1)
              read_html(url) %>%
                html_node('.bnm-txt') %>%
                html_text() %>%
                str_to_lower() %>%
                str_detect('best new album')
              
              
            }) 
)

#4. Cleaning our data frame

#removing the read.csv imported x index column. It's no longer meaningful being that we've filtered out so many rows.
bna_pitchfork_reviews <- pitchfork_reviews[,-1] %>%
  arrange(artist) %>%
#most EPs don't have the designation that it's an EP in the title
  mutate(title = str_remove(title, ' ep$'), 
#index column
          x= 1:629) %>%
#selecting by preferred order
  select(x, artist, title, reviewid, score, author, author_type, pub_date, pub_weekday, album_release_year = year)

#The next chunk of lines will be updated individual values based on domain knowledge I might have about an artist to improve our use of this table in subsequent scripts

#This chunk of lines serve to update artist name based on changes to their name occuring after the review was written.
bna_pitchfork_reviews$artist[363] <- 'caribou'
bna_pitchfork_reviews$artist[384] <- 'mutsumi'
bna_pitchfork_reviews$artist[597] <- 'preoccupations'

#this chunk of lines serve to change punctuation or accent letter formatting in the artist name
bna_pitchfork_reviews$artist[57] <- 'belle & sebastian'
bna_pitchfork_reviews$artist[60:61] <- 'beyonce'
bna_pitchfork_reviews$artist[67:68] <- 'bjork'
bna_pitchfork_reviews$artist[132] <-"d'angelo"
bna_pitchfork_reviews$artist[189] <- 'erlend oye'
bna_pitchfork_reviews$artist[277:278] <- 'jay-z'
bna_pitchfork_reviews$artist[285:286] <- 'johann johannsson'
bna_pitchfork_reviews$artist[343:344] <- 'lindstrom'

#this chunk corrects the artist name from the predominant artist and collaborators/backing band to the predominant artist, which is the more common practice for streaming services
bna_pitchfork_reviews$artist[79] <- 'bonnie prince billie'
bna_pitchfork_reviews$artist[279] <- 'kanye west'
bna_pitchfork_reviews$artist[308] <- 'kaitlyn aurelia smith'
bna_pitchfork_reviews$artist[598] <- 'vijay iyer'
bna_pitchfork_reviews$artist[37:38] <- 'ariel pink'
bna_pitchfork_reviews$artist[478] <- 'stephen malkmus'
bna_pitchfork_reviews$artist[383] <- 'mount eerie'

#this chunk of lines corrects the stylized punctuation or adds plain letters for removed accented letter.
bna_pitchfork_reviews$title[4] <- 'live.love.a$ap'
bna_pitchfork_reviews$title[5] <- 'long.live.a$ap'
bna_pitchfork_reviews$title[60] <- 'beyonce'

#this corrects the artist name to their last name(s), how they've chosen to be recognized as opposed to their full name(s)
bna_pitchfork_reviews$artist[82] <- 'eno & hyde'
bna_pitchfork_reviews$artist[367] <- 'herbert'

#this corrects the album title to the recognized album title on streaming platforms
bna_pitchfork_reviews$title[224] <- 'DS2 (Deluxe)'
bna_pitchfork_reviews$title[306] <- 'justice'


#this line changes the mixtape title to the encapsulating compilation album title
bna_pitchfork_reviews$title[563] <- 'trilogy'

#arranging again by artist
bna_pitchfork_reviews <- bna_pitchfork_reviews %>%
  arrange(artist)

#4. Saving out data ----
write.csv(bna_pitchfork_reviews, 'clean data/bna_pitchfork_reviews.csv')
