
set.seed(424242)

#' 1. Assuming the sphere is centered at x=0, y=0, z=0, where r is the radius of the sphere.
expr <- expression(x^2 + y^2 + z^2 <= r^2)
#' 2. Define a cube with side length 2r. Your sphere is hence contained within this cube.

#'3. Simulate n number of points from this cube, by drawing 3 numbers from the uniform 
#'distribution [0, 2r] to form the x, y, and z coordinates of a point within the cube, 
#'and doing this n times.

r = 2

n = 5000
myList <- list()

for (i in 1:n) {
  myList[[i]] <- runif(3, 0, 2*r)
  #print(myList[[i]])
  names(myList[[i]]) <- c('x', 'y', 'z')
}

merged_data_frame <- do.call(rbind, myList)

merged_data_frame <- as.data.frame(merged_data_frame)
merged_data_frame$r <- 2

#' 4. Count the number of points which fall within the sphere, by checking whether the 
#' coordinates for each point satisfies x^2 + y^2 + z^2 <= r^2. Let this number of points be x.

x <- ifelse(eval(expr, merged_data_frame), 1, 0)

x2 <- length(x[x==1]) 

#'5. x/n is hence an estimate of the ratio of the volume of the sphere to the volume of the cube.
#'Using this ratio, the volume of the sphere can be calculated as x/n * (2r)^3.

x2/n 

#'The accuracy of the estimate increases with n.

x2/n*(2*r)^3

# plot


plot(-3:3,-3:3,type="n",xlab="x",ylab="y",main="Problem k¹tów przestrzeni", asp = 1)
rect( -2, -2, 2, 2) 
draw.circle( 0, 0, r)

for (i in 1:n) {
  draw.circle(myList[[i]][1],myList[[i]][2], .01)
}













