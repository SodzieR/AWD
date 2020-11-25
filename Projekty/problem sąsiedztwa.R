
#
library(tidyverse)

obliczDystans <- function(df1, df2) {
  x1 <- df1$x
  y1 <- df1$y
  z1 <- df1$z
  x2 <- df2$x
  y2 <- df2$y
  z2 <- df2$z
  dystans <- sqrt((x2-x1)^2+(y2-y1)^2+(z2-z1)^2)
  return(dystans)
}

set.seed(424242)

# 1. - wylosuj 10k punktów z rozkładu normalnego (0, I).
ilosc_punktow <- 10000

myList <- list()

for (i in 1:ilosc_punktow) {
  myList[[i]] <- runif(3, 0, 1)
  names(myList[[i]]) <- c('x', 'y', 'z')
}

merged_data_frame <- data.frame(do.call(rbind, myList))

# 2. -  Dla n=20 punktów policz mediana+-mad(policz 
# stosunek odległości (1, 10, 100, 1000) najbliższego sąsiada do najdalszego sąsiada)

# https://www.calculatorsoup.com/calculators/geometry-solids/distance-two-points.php

# wylosuj 20 punktów z 10k punktów
set.seed(424242)

wybrane20pkt <- round(runif(20, 1, 10000))

lista20pkt <- list()

index <- 1

for (i in wybrane20pkt) {
  lista20pkt[[index]] <- myList[[i]]
  index = index + 1
}

merged_data_frame_20pkt <- data.frame(do.call(rbind, lista20pkt)) %>% 
  mutate(id_punktu = (1:20)) %>% 
  select(id_punktu, everything())

#
library(tidyverse)

obliczDystans <- function(df1, df2) {
  x1 <- df1$x
  y1 <- df1$y
  z1 <- df1$z
  x2 <- df2$x
  y2 <- df2$y
  z2 <- df2$z
  dystans <- sqrt((x2-x1)^2+(y2-y1)^2+(z2-z1)^2)
  return(dystans)
}

set.seed(424242)

# 1. - wylosuj 10k punktów z rozkładu normalnego (0, I).
ilosc_punktow <- 10000

myList <- list()

for (i in 1:ilosc_punktow) {
  myList[[i]] <- runif(3, 0, 1)
  names(myList[[i]]) <- c('x', 'y', 'z')
}

merged_data_frame <- data.frame(do.call(rbind, myList))

# 2. -  Dla n=20 punktów policz mediana+-mad(policz 
# stosunek odległości (1, 10, 100, 1000) najbliższego sąsiada do najdalszego sąsiada)

# https://www.calculatorsoup.com/calculators/geometry-solids/distance-two-points.php

# wylosuj 20 punktów z 10k punktów
set.seed(424242)

wybrane20pkt <- round(runif(20, 1, 10000))

lista20pkt <- list()

index <- 1

for (i in wybrane20pkt) {
  lista20pkt[[index]] <- myList[[i]]
  index = index + 1
}

merged_data_frame_20pkt <- data.frame(do.call(rbind, lista20pkt)) %>% 
  mutate(id_punktu = (1:20)) %>% 
  select(id_punktu, everything())
