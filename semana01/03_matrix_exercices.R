#Ejercicios de Matrices

X <- matrix(1:12,4,3)
X

t(t(X))
X %*% matrix(1,ncol(X))
X*1
X%*%diag(ncol(X))


#Solve the following system of equations using R:
#3a + 4b - 5c + d = 10
#2a + 2b + 2c - d = 5
#a - b + 5c - 5d = 7
#5a + d = 4
#What is the solution for c?


# Create the coefficient matrix A
A <- matrix(c(
  3, 4, -5, 1,
  2, 2, 2, -1,
  1, -1, 5, -5,
  5, 0, 0, 1
), nrow = 4, byrow = TRUE)

# Create the constants vector b
b <- c(10, 5, 7, 4)

# Solve the system using the solve function
solution <- solve(A, b)

# Name the solution components
names(solution) <- c("a", "b", "c", "d")

# Display the solution
solution


#cargar las siguientes matrices en R

a <- matrix(1:12, nrow = 4)
b <- matrix(1:15, nrow = 3)

a%*%b

a
a[3,]
b
b[,2]

sum(a[3,]*b[,2])

## QUIZ

f <- function(h0, v0, t) {
  return(h0 + v0*t - 0.5*9.8*t^2)
}

# Initial conditions
h0 <- 8848  # Initial height (Mt. Everest) in meters
v0 <- 0     # Initial velocity in m/s
t <- 10     # Time in seconds

# Calculate height at t = 10s
height_10s <- f(h0, v0, t)
height_10s


c(seq(1,2),seq(3,4))

