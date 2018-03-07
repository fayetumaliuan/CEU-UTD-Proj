library(RCurl)
library(XML)
library(stringr)
library(dplyr)
library(tidyr)
library(data.table)

# ##############CREATE SINGER LIST##############
# #These singer-songwriters are known to have committed suicide
# #These songwriters are part of a band, we get only the dates wherein they were alive and part of the bands
# #singer_list2 = c('Nirvana')
# #singer_list3 = c('The-Doors')
# #singer_list4 = c('Thin-Lizzy')
# #Nirvana - 1995 - 3 (Kurt Cobain)
# #The Doors - 1974 - 4 (Jim-Morrison)
# #Joy Division - Ian Curtis
# #Thin Lizzy -Phil Lynott- 1983


singer_list = c('Michael-Jackson',
                'Elliot-Smith',
                'Amy-Winehouse',
                'Prince',
                'Jimi-Hendrix',
                'Nick-Drake',
                'Joy-Division',
                'Nirvana','The-Doors','Thin-Lizzy')
num_pages = c(6,1,2,10,3,2,1,3,4,3)
year_active = c('2019','2019','2019','2019','2019','2019','2019','1995','1998','1984')


##############SCRAPE SONGS PART 2####################
completesongs <- data.frame() 

#get all songs from list of artists

for (i in 1:length(singer_list)) { 
  for (s in 1:num_pages[i]) {
  # create the URL for each artist
  URL <- paste("http://metrolyrics.com/",singer_list[i],"-alpage-",s,sep="")
  #print(URL)
  # parse the HTML
  results <- htmlTreeParse(getURL(URL, followlocation=TRUE), useInternal=TRUE)
  artist_songs <- readHTMLTable(results)[1]
  artist_songs <- data.frame(artist_songs)
  artist_songs$NULL.Â. <- NULL
  artist_songs$NULL.Popularity <- NULL
  artist_songs <- artist_songs %>% mutate(artist = singer_list[i])
  artist_songs <- artist_songs %>% mutate(title2 = str_replace_all(artist_songs$NULL.Name, ' ', "-"))
  artist_songs <- artist_songs %>% mutate(lyrics = "")
  completesongs <- bind_rows(completesongs, artist_songs)
  #print(year_active[i])
  #get only songs for which songwriter was present in the band
  completesongs$year <- as.numeric(as.character(completesongs$NULL.Year.))

  }
}

completesongs2 <- data.frame() 

for (i in 1:length(singer_list)){
  active_year <- as.numeric(year_active[i])
  completesongs1 <- completesongs %>%
    filter(artist == singer_list[i]) %>%
    filter(year < active_year)
  completesongs2 <- bind_rows(mutate_all(completesongs2, as.character),mutate_all(completesongs1, as.character))
}

completesongs2$year <- NULL
completesongs <- completesongs2
#clean titles used for html parsing
completesongs <- completesongs %>% mutate(title2 = str_replace_all(title2, '[&!?()*/\']', ''))

completesongs <- completesongs %>% group_by(title2) %>% filter(row_number() == 1)

#############SCRAPE THE LYRICS###################

for (s in 1:nrow(completesongs))  {
  
  artist <- completesongs[s,][3]
  songtitle <- completesongs[s,][4]
  # make URLs
  metroURL <- paste("http://metrolyrics.com/",songtitle,"-",artist,".html",sep="")
  print(metroURL)
  lyriclocs <- "//div[@id='lyrics-body-text']/p"
  
  tryCatch({ 
    results <- htmlTreeParse(metroURL, useInternal=TRUE, isURL=TRUE)
    lyrics <- xpathSApply(results, lyriclocs, xmlValue)
    lyrics <- toString(lyrics)
  },
  error = function(x) { 
    message(paste(s, "failed")) 
    lyrics <- ''
  },
  finally={ 
    if (!is.numeric(results)) { 
      if (length(lyrics)!=0) { 
        completesongs[s,][5] <- lyrics[[1]]
        message(paste(s, "success"))
        #break
      }
    } 
  }) 
  
} 


#delete duplicates and clean data
clean_completesongs <- completesongs %>% group_by(lyrics) %>% filter(row_number() == 1)
clean_completesongs <- clean_completesongs %>% filter(!lyrics == '{instrumental}')
clean_completesongs <- clean_completesongs %>% filter(!lyrics == '')

#write to csv
write.csv(clean_completesongs, "artist_songs_final.csv", row.names=FALSE)

