library(shinydashboard)
library(leaflet)
library(curl) 
library(tidyverse)
library(sf)
library(rtweet)
library(sentimentr)
library(magrittr)

 cities <- st_read("cities.shp") %>% 
  select(OBJECTID, NAME, geometry, POPULATION) %>% 
  rename(name = NAME, CID = OBJECTID, pop = POPULATION)

#map <- leaflet(cities) %>% 
# addTiles()
#map

function(input, output, session) {
  
  update_cities <- eventReactive(input$search, {
    search_term <- input$search_terms
    input$n_cities
    temp <- cities %>% 
      top_n(input$n_cities, pop) %>% 
      mutate(query = paste(geometry[]))
    
    
    # Create a character vector will house each lat long pair 
 
    # Initialize vector
    search_xy <- character(5)
    for (i in 1:nrow(temp)) {
      search_xy[i] <- paste0(temp$geometry[[i]][2],",", temp$geometry[[i]][1],",10mi")
      #print(paste(cities$NAME[i], cities$geometry[[i]][2], cities$geometry[[i]][1]))
    }
    # Bind the query vector to the cities internal dataframe
    temp$query <- search_xy
  
    city <- temp
    tweets <- map_df(city$query, function (x) {search_tweets(search_term, n = 150, geocode = x, lang = "en", include_rts = FALSE) %>% mutate(query = x)})
    
    city_tweets <- left_join(tweets, city, by = "query") %>% 
      select(screen_name, text, retweet_count, 
             favorite_count, CID, geometry, name) %>% 
      rename(city = name) %>% 
      mutate(id = row_number()) # Create unique identifier
    
    # Perform sentence level sentiment analyis
    tweet_sentiment <- city_tweets %>% 
      select(id, text) %>%
      mutate(sentence = get_sentences(text)) %$% # Note the idiosyncratic pipe
      sentiment_by(sentence, id)
    
    # Extract Id & sentiment
    id_sent <- tweet_sentiment %>% 
      select(id, ave_sentiment) %>% 
      as_tibble() # convert to a tibble for ease of use (though data.table is good)
    
    # Each tweet in each city joined to their respective sentiment 
    city_sentiment <- inner_join(city_tweets, id_sent, by = "id") 
    
    city_agg <- city_sentiment %>% 
      group_by(CID) %>% 
      summarise(avg_sent = mean(ave_sentiment),
                min_sent = min(ave_sentiment),
                max_sent = max(ave_sentiment),
                tweet_min_id = which.min(ave_sentiment),
                tweet_max_id = which.max(ave_sentiment))
    
      pos <- city_sentiment[city_agg$tweet_min_id,"text"] %>% 
          rename(pos = "text")

      neg <- city_sentiment[city_agg$tweet_max_id,"text"] %>% 
        rename(neg = "text")

      city_agg <- bind_cols(city_agg, pos, neg) %>% 
        inner_join(temp) %>% 
        st_as_sf()
    #z  <- abs((((city_aggregate$avg_sent) - mean(city_aggregate$avg_sent))/sd(city_aggregate$avg_sent)))*70000
    return(city_agg)
  })
  
  output$tweet_sentiment <- renderLeaflet({
    locations <- update_cities()
    
    from_min <- min(locations$avg_sent)
    to_min <- 0
    
    from_max <- max(locations$avg_sent)
    to_max <- 1
    
    z <- (locations$avg_sent - from_min) * (to_max - to_min) / (from_max - from_min) + to_min
    tbl <- bind_cols(locations %>% select(name, pos, neg, avg_sent))
    
    #z <- abs((((locations$avg_sent) - mean(locations$avg_sent))/sd(locations$avg_sent)))*70000
    # if (length(locations) == 0)
    #   return(NULL)
    # 
    # Show only selected directions
    # locations <- filter(locations, Direction %in% as.numeric(input$directions))
    
    # Four possible directions for bus routes
    #dirPal <- colorFactor(dirColors, names(dirColors))
    pal <- colorNumeric(
      palette = palette(c("#F54C54", "blue")),
      domain = z
    )
    map <- leaflet(locations) %>%
      addTiles() %>% 
      addCircles(radius = z*(10^5.2), stroke = FALSE, fillOpacity = .7,
                 popup = paste("City:", locations$name, "<br>", 
                               "Sentiment:", round(locations$avg_sent,4)),  color = ~pal(z)) 
    # %>% 
    #   addLegend(position = "bottomleft", pal = pal, values = ~z, opacity = 1, 
    #             title = "Average Sentiment")
    #   
    map
    
   
 
    
  })
  

}
 