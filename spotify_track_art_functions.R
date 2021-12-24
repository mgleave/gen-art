### Functionalize

search_track <- function(track_name) {
  require(spotifyr)
  
  search_result <- search_spotify(track_name, type="track")
  
  search_top <- search_result[1,]
  print(paste("The top result is", search_top[1,"name"], "by", search_top[1,"album.artists"]))
  
  search_top_id <- search_top[1,"id"]
  return(search_top_id)
}

get_spotify_access <- function() {
  
  require(spotifyr)
  
  # Access Spotify API
  Sys.setenv(SPOTIFY_CLIENT_ID = '582705fb4bc84785889943f4c7c9daea')
  Sys.setenv(SPOTIFY_CLIENT_SECRET = 'b2cba29273a94cca8c576152625e756b')
  
  access_token <- get_spotify_access_token()
  
  return(access_token)
  
}

get_track_info <- function(track_id) {
  
  require(spotifyr)
  
  access_token <- get_spotify_access()
  
  # Example of how to find Track ID from URL: https://open.spotify.com/track/0AQquaENerGps8BQmbPw14?si=8dabce3769b34a09
  
  # Using spotifyr API wrapper package, see: https://www.rcharlie.com/spotifyr/index.html
  track_info <- get_track(c(track_id))
  
  return(track_info)
  
}

get_track_audio_data <- function(track_id) {
  
  require(spotifyr)
  
  # Example of how to find Track ID from URL: https://open.spotify.com/track/0AQquaENerGps8BQmbPw14?si=8dabce3769b34a09
  
  # Using spotifyr API wrapper package, see: https://www.rcharlie.com/spotifyr/index.html
  track_audio_analysis <- get_track_audio_analysis(track_id)
  return(track_audio_analysis)
}


prep_beats_data <- function(track_audio_analysis) {
  
  require(dplyr)
  
  # Get data on beats
  beats <- track_audio_analysis$beats
  noise <- runif(nrow(beats), -1.5, 2)
  beats <- beats %>% mutate(t=duration+noise) %>% 
    mutate(start_norm=(start-min(start))/(max(start)- min(start)))
  
  return(beats)
  
}


prep_notes_data <- function(track_audio_analysis) {
  
  require(dplyr)
  require(tidyr)
  
  # Get data on chords and notes
  chromatic_scale <- c("A", "A#/Bb", "B", "C", "C#/Db", "D", "D#/Eb", "E", "F", "F#/Gb", "G", "G#/Ab")
  
  # Pitches are in a nested list, so we need to use lapply to convert each row to a dataframe and then combine them together.
  # t() is used to transpose because each of the 12 list items we actually want to become columns
  pitches <- lapply(track_audio_analysis$segments$pitches, function(x) as.data.frame(t(x)))
  pitches <- bind_rows(pitches)
  colnames(pitches) <- chromatic_scale
  
  # Extract other interesting data on the notes
  start <- unlist(track_audio_analysis$segments$start)
  duration <- unlist(track_audio_analysis$segments$duration)
  loudness <- unlist(track_audio_analysis$segments$loudness_max)
  
  # Put it all together
  segments <- cbind(start, duration, pitches, loudness)
  
  # Pivot to tidy dataframe so we can visualize!
  segments_long <- segments %>% tidyr::pivot_longer(cols=c(3:14), names_to="pitch")
  
  # Filter so we just have the most prominent (actual) notes in each chord, 
  #  then normalize so we have the option to have all values on the same scale between 0 and 1 for easier viz
  topnotes <- segments_long %>% filter(value>.50) %>% arrange(start, value) %>% 
    mutate(start_norm=(start-min(start))/(max(start)- min(start)),
           duration_norm=(duration-min(duration))/(max(duration)- min(duration)),
           loudness_norm=(loudness-min(loudness))/(max(loudness)- min(loudness)))
  return(topnotes)
  
}


generate_track_art <- function(track_id) {
  
  df <- get_track_audio_data(track_id)
  
  beats <- df %>% prep_beats_data(.)
  # print(head(beats))
  
  topnotes <- df %>% prep_notes_data(.)
  # print(head(topnotes))
  
  plot <- ggplot(topnotes, aes(x=start)) + 
    geom_bar(aes(group=pitch, 
                 color=pitch, 
                 size=duration,
                 alpha=loudness_norm),
             position = "fill") +
    geom_line(data=beats, aes(x=start,
                              y=confidence+.5, alpha=duration), color="white") +
    # scale_color_viridis(option="magma",discrete=T) + 
    coord_polar() + 
    theme_void() + 
    theme(legend.position = "none") +
    theme(plot.background = element_rect(fill = "black"))
  
  return(plot)
  
}


generate_track_art("0AQquaENerGps8BQmbPw14?si=8dabce3769b34a09")
generate_track_art("1nRTH500HbZX8PYwT4ZMby?si=addcc1b5de7e4b22")
generate_track_art("3xKsf9qdS1CyvXSMEid6g8?si=eb603ea54e034d30")