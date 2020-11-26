
# -------------------------------------------------------------------------


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


# Final -------------------------------------------------------------------

library(philentropy)

problem_sasiedztwa <- function(d) {
  
  nazwy_wspolrzednych_dla_punktu <- paste(c("x"), 1:d, sep="")
  
  set.seed(424242)
  
  n = 100
  
  myList <- list()
  
  for (i in 1:n) {
    myList[[i]] <- runif(d, 0, 1)
    names(myList[[i]]) <- nazwy_wspolrzednych_dla_punktu
  }
  
  merged_data_frame <- do.call(rbind, myList)
  merged_data_frame <- as.data.frame(merged_data_frame)
  rownames(merged_data_frame) <- paste(c("p"), 1:n, sep="")
  
  set.seed(424242)
  
  wybrane20pkt <- round(runif(5, 1, 100))
  
  lista20pkt <- list()
  
  i = 1
  
  for (value in wybrane20pkt) {
    lista20pkt[[i]] <- merged_data_frame[value, ]
    i = i + 1
  }
  
  merged_data_frame_20pkt <- do.call(rbind, lista20pkt)
    
  
  merged_data_frame_20pkt_setdiff <- setdiff(merged_data_frame, merged_data_frame_20pkt)
  
  
  macierz_odleglosci <- distance(rbind(merged_data_frame_20pkt, merged_data_frame_20pkt_setdiff), 
           method = 'euclidean', use.row.names = TRUE)
  
  list_distances <- list()
  
  i2 = 1
  
  for (value in rownames(merged_data_frame_20pkt)) {
    
    wybrane_punkty <- data.frame(macierz_odleglosci) %>% 
      select(rownames(merged_data_frame_20pkt)[i2]) 
    
    wybrane_punkty2 <- wybrane_punkty %>% 
      slice(-i2) 
    
    wybrane_punkty2_max <- wybrane_punkty2 %>% 
      max()
    
    wybrane_punkty2_min <- wybrane_punkty2 %>% 
      min()
    
    stosunek_odleglosci <- wybrane_punkty2_min/wybrane_punkty2_max
    
    zwracana_tabela <- data.frame(punkt = value,
                                  stosunek_odleglosci = stosunek_odleglosci)
    
    list_distances[[i2]] <- zwracana_tabela
    i2 = i2 + 1
  }
  
  merged_list_distances <- do.call(rbind, list_distances)
  merged_list_distances <- as.data.frame(merged_list_distances)
  
  mad_ps <- mad(merged_list_distances$stosunek_odleglosci)
  
  median_ps <- median(merged_list_distances$stosunek_odleglosci)
  
  return(data.frame(mad_ps, median_ps, median_ps-mad_ps, median_ps+mad_ps))
}



















