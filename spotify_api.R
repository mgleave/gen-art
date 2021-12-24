# Spotify API Call

#install.packages('spotifyr')
#devtools::install_github('charlie86/spotifyr')

#install.packages("remotes")
#library(remotes)
remotes::install_github("marcusvolz/ggart")

library(spotifyr)
library(ggplot2)
library(dplyr)

Sys.setenv(SPOTIFY_CLIENT_ID = '582705fb4bc84785889943f4c7c9daea')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'b2cba29273a94cca8c576152625e756b')

access_token <- get_spotify_access_token()

#https://open.spotify.com/track/0AQquaENerGps8BQmbPw14?si=8dabce3769b34a09
big_iron_track <- get_track_audio_analysis("0AQquaENerGps8BQmbPw14?si=8dabce3769b34a09")
big_iron_features <- get_track(c("0AQquaENerGps8BQmbPw14?si=8dabce3769b34a09"))

marty_robbins <- get_artist_audio_features(artist="Marty Robbins")

noise <- runif(nrow(big_iron_track$beats), -1.5, 2)

test <- big_iron_track$beats %>% mutate(t=duration+noise)

ggplot(big_iron_track$beats, aes(x=start, y=duration)) + geom_point()
ggplot(big_iron_track$beats, aes(x=start, y=duration, color=confidence)) + geom_point()

ggplot(big_iron_track$beats, aes(x=start, y=duration, color=confidence)) + 
  geom_point() + theme_void()

chromatic_scale <- c("A", "A#/Bb", "B", "C", "C#/Db", "D", "D#/Eb", "E", "F", "F#/Gb", "G", "G#/Ab")

pitches <- lapply(big_iron_track$segments$pitches, function(x) as.data.frame(t(x)))
pitches <- bind_rows(pitches)
colnames(pitches) <- chromatic_scale
head(pitches)

start <- unlist(big_iron_track$segments$start)
duration <- unlist(big_iron_track$segments$duration)
loudness <- unlist(big_iron_track$segments$loudness_max)

segments <- cbind(start, duration, pitches, loudness)
str(segments)
summary(segments)

segments_long <- segments %>% tidyr::pivot_longer(cols=c(3:14), names_to="pitch")

# angle <- 19 * pi / 180
# t <- (1:nrow(segments_long))*angle
# segments_long <- cbind(segments_long, t)
# ggplot(segments_long, aes(y=value, group=pitch, color=pitch, size=duration, alpha=value)) + geom_histogram(position="dodge") + theme_void() 
# ggplot(segments_long, aes(x=sin(start*t)*t^2, y=cos(value*t)*t^3, group=pitch, color=pitch, size=duration, alpha=value)) + geom_col(position="dodge") +theme_void() 
# ggplot(segments_long, aes(x=start, y=value, group=pitch, color=pitch, size=duration, alpha=value)) + geom_col(postion="stacked") 
# ggplot(segments_long, aes(x=sin(start*t)*t^2, y=cos(value*t)*t^3, group=pitch, color=pitch, size=duration, alpha=value)) + geom_col(position="dodge") + theme_void() + coord_polar() + theme(legend.position = "none") 


# x <- sin(start)
# y <- cos(value)
# angle <- 19 * pi / 180
# t <- (1:nrow(segments))*angle
# 
# df <- cbind(x,y,t)

####
topnotes <- segments_long %>% filter(value>.50) %>% arrange(start, value)

topnotes <- topnotes %>% mutate(start_norm=(start-min(start))/(max(start)- min(start)),
                                duration_norm=(duration-min(duration))/(max(duration)- min(duration)),
                                loudness_norm=(loudness-min(loudness))/(max(loudness)- min(loudness)))




#ggplot(topnotes, aes(x=sin(start*t)*t^2, y=cos(value*t)*t^3, group=pitch, color=pitch, size=duration, alpha=value)) + geom_col(position="dodge") +theme_void() 
#ggplot(topnotes, aes(x=sin(start*t^5)*t^2, y=cos(value*t^2)*t^3, group=pitch, color=pitch, size=duration, alpha=value)) + geom_col(position="dodge") + theme_void() + coord_polar() + theme(legend.position = "none") 

ggplot(topnotes, aes(x=start, y=duration, group=pitch, color=pitch, alpha=value)) + geom_col() + theme_void() + coord_polar()  + theme(legend.position = "none") 


ggplot(topnotes, aes(x=start, 
                     xend = start + duration,
                     y=  value, 
                     yend =  value + duration,
                     group=pitch, 
                     color=pitch, alpha=value)) + 
  geom_segment() +
  coord_polar()  + 
  theme_void() + 
  theme(legend.position = "none") 

ggplot(topnotes, aes(x=start_norm, 
                     y= start_norm + duration_norm, 
                     group=pitch, 
                     color=pitch, alpha=value)) + 
  geom_point() +
 #  coord_polar()  + 
  theme_void() + 
  theme(legend.position = "none") 

library(viridis)

seq(-3,3, by = 0.01) %>% 
  expand.grid(x=., y=.) %>% 
  ggplot(aes(x = 1- x-sin(y^2), y = 1 + y - cos(x^2), color=x*y))+
  geom_point(alpha = 0.05, shape = 20, size = 0)+
  theme_void()+
  coord_polar()+
  labs(subtitle = "Flower")

backdrop <- seq(-1,1, by = 0.01) %>% 
  expand.grid(x=., y=.)



ggplot(topnotes, aes(x=start)) + 
  geom_bar(aes(group=pitch, 
               color=pitch, 
               size=duration,
               alpha=loudness_norm),
           position = "fill") +
  geom_line(data=test, aes(x=start,
                            y=confidence+.5, alpha=duration), color="white") +
 # scale_color_viridis(option="magma",discrete=T) + 
  coord_polar() + 
  theme_void() + 
  theme(legend.position = "none") +
  theme(plot.background = element_rect(fill = "black"))



