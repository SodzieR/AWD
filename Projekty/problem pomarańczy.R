
set.seed(424242)

library(plotrix) # install.packages('plotrix')

# Define orange 

r_total = 2 # orange radius

r_skorka = r_total*0.1 # orange peel

# plot
plot(-2:2,-2:2,type="n",xlab="x",ylab="y",main="Problem skórki pomarañczy", asp = 1)
draw.circle(0,0, r_total)
draw.circle(0,0, r_total - r_skorka)


# expr_total for the inside of an orange in 3D
expr_total <- expression(x^2 + y^2 + z^2 <= (r_total-r_skorka)^2)

# exppr_skorka for the N dots on the peel
expr_skorka <- expression((x^2 + y^2 + z^2 >= (r_total-r_skorka)^2) & (x^2 + y^2 + z^2 <= r_total^2))

# get random x-y-z coordinates from uniform distribution for times n

n = 5000
myList <- list()

for (i in 1:n) {
  myList[[i]] <- runif(3, -r_total, r_total)
  #print(myList[[i]])
  names(myList[[i]]) <- c('x', 'y', 'z')
}

merged_data_frame <- do.call(rbind, myList)

merged_data_frame <- as.data.frame(merged_data_frame)
merged_data_frame$r <- r_total

#' 4. Count the number of points which fall within the sphere, by checking whether the 
#' coordinates for each point satisfies x^2 + y^2 + z^2 <= r^2. Let this number of points be x.

x_miazsz <- ifelse(eval(expr_total, merged_data_frame), 1, 0)

ilosc_kropek_w_miazszu <- length(x_miazsz[x_miazsz == TRUE])

x_skorka <- ifelse(eval(expr_skorka , merged_data_frame), 1, 0)

ilosc_kropek_w_skorce <- length(x_skorka[x_skorka == TRUE])

# add circle plots

for (i in 1:n) {
  draw.circle(myList[[i]][1],myList[[i]][2], .01)
}

objetosc_miazszu_do_objetosci_skorki <- ilosc_kropek_w_miazszu/ilosc_kropek_w_skorce

print(paste0('Objetosc miazszu do objetosci skorki wynosi: ', objetosc_miazszu_do_objetosci_skorki))


# Final -------------------------------------------------------------------

problem_pomaranczy <- function(d, n, r_total) {
  
  set.seed(424242)
  
  library(plotrix) # install.packages('plotrix')
  
  # Define orange 
  
  r_skorka = r_total*0.1 # orange peel
  
  nazwy_wspolrzednych_dla_punktu <- paste( c("x"), 1:d, sep="")
  
  wyrazenie_dla_danego_wymiaru <- paste(nazwy_wspolrzednych_dla_punktu, collapse = '^(d-1) + ')
  
  wyrazenie_z_poprawka <- paste(wyrazenie_dla_danego_wymiaru, '^(d-1)', sep = '')
  
  wyrazenie_z_poprawka_skorka_pomaranczy <- paste(wyrazenie_z_poprawka, '>= (r_total-r_skorka)^(d-1))') 
  
  wyrazenie_z_poprawka_skorka_pomaranczy2 <- paste0('(', wyrazenie_z_poprawka_skorka_pomaranczy)
  
  wyrazenie_z_poprawka_skorka_pomaranczy3 <- paste(wyrazenie_z_poprawka, '<= (r_total^2)^(d-1))')
  
  wyrazenie_z_poprawka_skorka_pomaranczy4 <- paste0('(', wyrazenie_z_poprawka_skorka_pomaranczy3)
  
  wyrazenie_z_poprawka_skorka_pomaranczy5 <- paste(wyrazenie_z_poprawka_skorka_pomaranczy2,
                                                   wyrazenie_z_poprawka_skorka_pomaranczy4,
                                                   sep = ' & ')
  
  wyrazenie_z_poprawka_srodek_pomaranczy <- paste(wyrazenie_z_poprawka, '<= (r_total-r_skorka)^(d-1)')
  
  # expr_total for the inside of an orange in 3D
  # expr_total <- expression(x^2 + y^2 + z^2 <= (r_total-r_skorka)^2)
  
  # exppr_skorka for the N dots on the peel
  # expr_skorka <- expression((x^2 + y^2 + z^2 >= (r_total-r_skorka)^2) & (x^2 + y^2 + z^2 <= r_total^2))
  
  # get random x-y-z coordinates from uniform distribution for times n
  
  # n = 5000
  myList <- list()
  
  for (i in 1:n) {
    myList[[i]] <- runif(d, -r_total, r_total)
    #print(myList[[i]])
    names(myList[[i]]) <- nazwy_wspolrzednych_dla_punktu
  }
  
  merged_data_frame <- do.call(rbind, myList)
  
  merged_data_frame <- as.data.frame(merged_data_frame)
  merged_data_frame$r <- r_total
  merged_data_frame$d <- d
  merged_data_frame$r_total <- r_total
  merged_data_frame$r_skorka <- r_skorka
  
  #' 4. Count the number of points which fall within the sphere, by checking whether the 
  #' coordinates for each point satisfies x^2 + y^2 + z^2 <= r^2. Let this number of points be x.
  
  x_miazsz <- ifelse(eval(parse(text = wyrazenie_z_poprawka_srodek_pomaranczy), merged_data_frame), 1, 0)
  
  ilosc_kropek_w_miazszu <- length(x_miazsz[x_miazsz == TRUE])
  
  x_skorka <- ifelse(eval(parse(text = wyrazenie_z_poprawka_skorka_pomaranczy5), merged_data_frame), 1, 0)
  
  ilosc_kropek_w_skorce <- length(x_skorka[x_skorka == TRUE])
  
  
  objetosc_miazszu_do_objetosci_skorki <- ilosc_kropek_w_miazszu/ilosc_kropek_w_skorce
  
  return(objetosc_miazszu_do_objetosci_skorki)
  
}
