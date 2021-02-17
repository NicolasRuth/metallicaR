### Plotting some interesting variables ###

library(tidyverse)
library(ggridges)
library(ggthemes)
library(RColorBrewer)

# First we will look at the mean length of all songs.

mean_length_plot <- ggplot(metallicaR,
                           aes(x = duration))

mean_length_plot +
  geom_histogram(binwidth = 10) +
  geom_vline(aes(xintercept = mean(duration)),
             color = "red",
             size = 1) +
  theme_hc(style = "darkunica") +
  xlab("Duration in seconds") +
  ylab("Number of songs with this length") +
  ggtitle("Histogram of duration of all songs in seconds with mean duration")

ggsave("./img/mean_length_plot.png")

# Next, the mean loudness per LP is examined

metallicaR <- metallicaR %>% arrange(album.release_date)

loudness_album_plot <- ggplot(metallicaR,
                              aes(x = factor(album.name,
                                             levels = unique(album.name)),
                                  y = loudness))

loudness_album_plot +
  geom_bar(stat = "summary",
           aes(fill = factor(album.name,
                             levels = unique(album.name))),
           width = .5,
           fun = "mean") +
  scale_fill_brewer(palette = "RdGy",
                    name = "Album titles") +
  theme(axis.text.x = element_text(angle = 75,
                                   vjust = 0.6)) +
  ylab("Loudness in dB") +
  xlab("Album titles") +
  ggtitle("Bar plot of loudness per album in dB")

ggsave("./img/loudness_album_plot.png")

# Now, we will plot boxplots to look at the average distribution of tempo across LPs

tempo_albums_plot <- ggplot(metallicaR,
                            aes(factor(album.name,
                                       levels = unique(album.name)),
                                tempo))

tempo_albums_plot +
  geom_boxplot(aes(fill = factor(album.name,
                                 levels = unique(album.name)))) +
  theme(axis.text.x = element_text(angle = 75,
                                   vjust = 0.6)) +
  labs("Tempo of LPs",
       "Each box represents the mean tempo of an album with standard deviation") +
  ylab("Tempo") +
  xlab("Album titles") +
  scale_fill_brewer(palette = "RdGy",
                    name = "Album titles") +
  ggtitle("Boxplots representing tempo per album in bpm")

ggsave("./img/tempo_albums_plot.png")

# Did Metallica get meaner ver time? Let's look at valence per LP

metallicaR <- metallicaR %>% arrange(desc(album.release_date))

valence_albums_plot <- ggplot(metallicaR,
                              aes(x = valence,
                                  y = factor(album.name,
                                             levels = unique(album.name)),
                                  fill = ..x..))
valence_albums_plot +
  geom_density_ridges_gradient(scale = 0.9) +
  scale_fill_gradient(low = "red", high = "black") +
  theme_fivethirtyeight() +
  theme(panel.background = element_rect(fill = "white")) +
  theme(plot.background = element_rect(fill = "white")) +
  xlim(0,1) +
  theme(legend.position = "none") +
  ggtitle("Ridgeline plot of valence (from 0 - negative to 1 - positive) per album")

ggsave("./img/valence_albums_plot.png")

# Ok, in general is there a track position where Metallica
# usually puts the most energetic song?

tracknumber_energy_plot <- ggplot(metallicaR,
                                  aes(x = factor(track_number),
                                      y = energy),
                                  fill = ..x..)
tracknumber_energy_plot +
  geom_boxplot(aes(fill = factor(track_number))) +
  ylab("Energy") +
  xlab("Track position") +
  theme_pander() +
  scale_fill_grey(name = "Track position")

ggsave("./img/tracknumber_energy_plot.png")

# The moment we were all waiting for. What is the most popular Metallica LP?

metallicalbum <- metallicaR %>%
  group_by(album.name) %>%
  select(popularity) %>%
  summarize_all("mean")

metallicalbum$popularity_z <- round((metallicalbum$popularity -
                                    mean(metallicalbum$popularity))/
                                   sd(metallicalbum$popularity), 2)

# First some preparations for nice diverging bars

metallicalbum$pop_type <- ifelse(metallicalbum$popularity_z < 0, "below", "above")

metallicalbum <- metallicalbum[order(metallicalbum$popularity_z), ]

metallicalbum$album.name <- factor(metallicalbum$album.name,
                                levels = metallicalbum$album.name)

popular_lps_plot <- metallicalbum %>%
  group_by(album.name, popularity_z) %>%
  ggplot(aes(x = album.name,
             y = popularity_z,
             label = popularity_z))

popular_lps_plot +
  geom_bar(stat = "summary",
           fun = "mean",
           aes(fill = pop_type),
           width=.5)  +
  scale_fill_manual(name="Popularity",
                    labels = c("Above Average", "Below Average"),
                    values = c("above"="red", "below"="black")) +
  labs(subtitle = "Liking above and below mean popularity of Metallica albums on Spotify",
       title = "Order of Metallica albums according to popularity") +
  ylab("Average popularity on Spotify") +
  xlab("Album titles") +
  theme_bw() +
  coord_flip()

ggsave("./img/popular_lps_plot.png")
