
set.seed(424242)

library(plotrix) # install.packages('plotrix')

# Define orange 

r_miazsz = 2 # orange radius

r_skorka = r_miazsz*0.1 # orange peel

# plot
plot(-2:2,-2:2,type="n",xlab="x",ylab="y",main="Problem skórki pomarañczy", asp = 1)
draw.circle(0,0, r_miazsz)
draw.circle(0,0, r_miazsz - r_skorka)


# expr_miazsz for the inside of an orange in 3D
expr_miazsz <- expression(x^2 + y^2 + z^2 <= (r_miazsz-r_skorka)^2)

# exppr_skorka for the N dots on the peel
expr_skorka <- expression((x^2 + y^2 + z^2 >= (r_miazsz-r_skorka)^2) & (x^2 + y^2 + z^2 <= r_miazsz^2))

# get random x-y-z coordinates from uniform distribution for times n

n = 5000
myList <- list()

for (i in 1:n) {
  myList[[i]] <- runif(3, -r_miazsz, r_miazsz)
  #print(myList[[i]])
  names(myList[[i]]) <- c('x', 'y', 'z')
}

merged_data_frame <- do.call(rbind, myList)

merged_data_frame <- as.data.frame(merged_data_frame)
merged_data_frame$r <- r_miazsz

#' 4. Count the number of points which fall within the sphere, by checking whether the 
#' coordinates for each point satisfies x^2 + y^2 + z^2 <= r^2. Let this number of points be x.

x_miazsz <- ifelse(eval(expr_miazsz, merged_data_frame), 1, 0)

ilosc_kropek_w_miazszu <- length(x_miazsz[x_miazsz == TRUE])

x_skorka <- ifelse(eval(expr_skorka , merged_data_frame), 1, 0)

ilosc_kropek_w_skorce <- length(x_skorka[x_skorka == TRUE])

# add circle plots

for (i in 1:n) {
  draw.circle(myList[[i]][1],myList[[i]][2], .01)
}

objetosc_miazszu_do_objetosci_skorki <- ilosc_kropek_w_miazszu/ilosc_kropek_w_skorce

print(paste0('Objetosc miazszu do objetosci skorki wynosi: ', objetosc_miazszu_do_objetosci_skorki))
