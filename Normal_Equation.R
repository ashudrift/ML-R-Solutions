## Solving Linear Regression by Normal Equations

## Reading the data

Data <- read.csv("Question2.csv")
y <- Data[,1]
x <- cbind(rep(1,length(y)),Data[,2:4])

## Initializing theta names

thetas = c()
for (i in 1:(length(x)+1))
{
  theta = paste("theta",i-1)
  thetas = c(thetas,theta)
}


## Converting Data type

y <- as.matrix(y)
x<- as.matrix(x)
## Solution

param_values <- solve((t(x)%*%x))%*%t(x)%*%y

thetas <- cbind(thetas,param_values)

## The equation would be theta0 + theta1*B + theta2*C + theta3*D
thetas

## Checking for errors and graph between predict_y and y

ap1 = function(a)
{sol = a*as.numeric(thetas[,2])
 return(sol)}
arg = apply(x,1,ap1)
predict_y = apply(arg,2,sum)
error = sum(predict_y - y)/length(predict_y)
print(paste("The mean error in the model is",error))
plot(y,predict_y)