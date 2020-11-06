
# Dependencies ------------------------------------------------------------

library(tidyverse)
library(ggbiplot)

# Setup -------------------------------------------------------------------


# Load --------------------------------------------------------------------

iris_pca <- prcomp(iris[, -5], center = TRUE, scale. = TRUE)

summary(iris_pca)

ggbiplot(iris_pca, groups = iris$Species, obs.scale = 1)

# 

temp <- summary(iris_pca)

temp$importance[3,]

plot(temp$importance[3,], 
     ylim = c(0,1))

#




ggplot(aes(1:4, temp$importance[3,]))+
  geom_point()


# Cwiczenia 2 - analiza korelacji kanonicznej -----------------------------

#' @dependencies
library(CCA)   #install.packages('[package_name_in_square_brackets]')
library(GGally)

#' @data
iris_sepal <- iris[1:2]
iris_petal <- iris[3:4]

#' @initiral_analysis
ggpairs(iris_sepal)
ggpairs(iris_petal)
#
ggpairs(data.frame(iris_sepal, iris_petal))

#' @CCA_analysis
CCA_iris <- matcor(iris_sepal, iris_petal)
img.matcor(CCA_iris, type = 2)


CCA_plot.cc <- cc(iris_sepal, iris_petal) 
plt.cc(CCA_plot.cc, var.label = TRUE)












