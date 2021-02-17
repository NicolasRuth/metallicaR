# Getting data from Spotify using the Spotify developer API

### Packages ###

library(tidyverse) # for data wrangling
if (!require(spotifyr)){
  install.packages("spotifyr", devtools::install_github("charlie86/spotifyr"))
} #
library(spotifyr) # use Spotify API
library(knitr) # for report generation

### Spotify Access ###

# Please go to Spotify for developers and register. https://developer.spotify.com/
# Then you can use your own ID

Sys.setenv(SPOTIFY_CLIENT_ID = "yourID")

# and your own developer secret

Sys.setenv(SPOTIFY_CLIENT_SECRET = "yourSecret")

# Generating an access token to use Spotify’s API

access_token <- get_spotify_access_token(Sys.getenv("SPOTIFY_CLIENT_ID"),
                                         Sys.getenv("SPOTIFY_CLIENT_SECRET"))


# Get all available information for the major Metallica LPs
kill_em_all <- get_album_tracks("1aGapZGHBovnmhwqVNI6JZ")
ride_the_lightning <- get_album_tracks("05DePtm7oQMdL3Uzw2Jmsc")
master_of_puppets <- get_album_tracks("2Lq2qX3hYhiuPckC8Flj21")
justice_for_all <- get_album_tracks("6jZ1z25PyF4Yd3kHxt9rl1")
black_album <- get_album_tracks("2Kh43m04B1UkVcpcRa1Zug")
load <- get_album_tracks("6ndH0UlQbyCOVqByMXXhdV")
reload <- get_album_tracks("0Ip2GlQPoAIgdkqCO2YkMa")
garage_inc <- get_album_tracks("0vshXZYhBkbIoqxyC2fXcF")
st_anger <- get_album_tracks("0HKpzK9ZoJ0oVA43E5gewM")
death_magnetic <- get_album_tracks("3bK58rVcWBh3V3vxMLzi0V")
hardwired <- get_album_tracks("7LwifLL1anaEd9eIIfIkx7")

metallica_lps <- rbind(kill_em_all, ride_the_lightning, master_of_puppets,
                       justice_for_all, black_album, load, reload, garage_inc,
                       st_anger, death_magnetic, hardwired)

lps <- c("Kill 'Em All", "Ride The Lightning", "Master of Puppets",
         "...And Justice For All", "Death Magnetic", "Garage Inc.",
         "Hardwired…To Self-Destruct", "Load", "Metallica", "Reload", "St. Anger")

metallica_data1 <- get_track_audio_features(metallica_lps$id[1:50])
metallica_data2 <- get_track_audio_features(metallica_lps$id[51:127])

metallica_aa_data1 <- get_tracks(metallica_lps$id[1:50])
metallica_aa_data2 <- get_tracks(metallica_lps$id[51:100])
metallica_aa_data3 <- get_tracks(metallica_lps$id[101:127])

metallica_aa_data <- rbind(metallica_aa_data1, metallica_aa_data2, metallica_aa_data3)

metallica_data <- rbind(metallica_data1, metallica_data2)

metallicaR <- merge(metallica_data, metallica_aa_data, by = "id")

rm(kill_em_all, ride_the_lightning, master_of_puppets, justice_for_all,
   black_album, load, reload, garage_inc, st_anger, death_magnetic,
   hardwired, metallica_data2, metallica_data1, metallica_lps,
   metallica_aa_data1, metallica_aa_data2, metallica_aa_data3)

# Select the interesting variables
metallicaR <- select(metallicaR, c(name, track_number, album.release_date,
                            id, album.name, popularity,
                            danceability, energy, key, loudness, mode,
                            speechiness, acousticness, instrumentalness,
                            liveness, valence, tempo, duration_ms.x,
                            explicit))
metallicaR <- mutate(metallicaR, key_name = pitch_class_lookup[key + 1],
                      mode_name = case_when(mode == 1 ~ 'major', mode == 0 ~ 'minor', TRUE ~ as.character(NA)),
                      key_mode = paste(key_name, mode_name))

metallicaR$album.name <- gsub("Remastered", "", metallicaR$album.name)
metallicaR$album.name <- gsub("\\()", "", metallicaR$album.name)

metallicaR$album.name <- factor(metallicaR$album.name,
                                levels = c("Kill 'Em All ", "Ride The Lightning ",
                          "Master Of Puppets ", "…And Justice for All ", "Metallica",
                          "Load", "Reload", "Garage Inc.", "St. Anger", "Death Magnetic",
                          "Hardwired…To Self-Destruct"))
metallicaR <- rename(metallicaR, duration = duration_ms.x)
metallicaR$duration <- round(metallicaR$duration / 1000)

write_csv(metallicaR, "./data/metallicaR.csv")
