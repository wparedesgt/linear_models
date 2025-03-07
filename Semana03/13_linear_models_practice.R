##Linear Models in Practice Exercises

# SE =  sqrt(var(diff))
# var(diff) = (1/nx + 1/ny) (sum {(x_i - mu_x)^2} + sum{(y_i - mu_y)^2}) / (nx + ny - 2)
# 
# ( (X^T X)^-1 )[2,2] = (1/nx + 1/ny)


nx <- 5
ny <- 7

X <- cbind(rep(1,nx + ny),
          rep(c(0,1),
              c(nx, ny)))

t(X)%*%X
