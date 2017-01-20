## Reading the data

Data <- read.csv("Question2.csv")
y <- Data[,1]
x <- Data[,2:4]
x = cbind(1,x)

## Regression function

Reg <- function(alpha = 0.002, max_iter = 300, y, x)
{ 
  
 
## Initializing counter for theta
  init = c()
  thetas = c()
  for (i in 1:(length(x)))
  {
    theta = paste("theta",i-1)
    thetas = c(thetas,theta)
    init = c(init,1)
  }
thetas = cbind(thetas,init)

## Function to multiply each row of x values with corresponding thetas
  ap1 = function(a)
    {sol = a*as.numeric(thetas[,2])
    return(sol)}
arg = apply(x,1,ap1)
predict_y = apply(arg,2,sum)
Cost_Function = sum((y - predict_y)^2)/length(predict_y)
## Gradient Descent Function

  Gradient_Desc <- function(thetas, x, y, alpha)
  {
    del = c("del0")
    val0 = (Cost_Function/length(y))*alpha
    val = c()
    for(i in 2:length(thetas[,1]))
    {
      del = c(del,paste("del",(i-1)))
      val_temp = alpha * (sum((predict_y - y)*x[,(i-1)])/length(y))
      val = c(val,val_temp)
      
    }
    val = c(val0,val)
    del = cbind(del,val)
    thetas[,2] = as.numeric(thetas[,2]) - as.numeric(del[,2])
    return(thetas)
  }

## Actual iterations
iterations = c()
Cost_Fn = c()
for (i in 1:max_iter)
{
  arg = apply(x,1,ap1)
  predict_y = apply(arg,2,sum)
  Cost_Function = sum((y - predict_y)^2)/length(predict_y)
  thetas = Gradient_Desc(thetas, x, y, alpha)
  iterations = c(iterations,i)
  Cost_Fn = c(Cost_Fn,Cost_Function)
  Performance = cbind(iterations,Cost_Fn)
}
output = list(thetas,Performance)
return(output)

}

output = Reg(y = y ,x = x)
thetas = output[[1]]
## the values of thetas for the model are
thetas
## eq: theta0 + theta1*B + theta2*c + theta3*C

Performance = output[[2]]
## Looking for predicted y and mean difference in predicted and actual y

ap1 = function(a)
{sol = a*as.numeric(thetas[,2])
 return(sol)}
arg = apply(x,1,ap1)
predict_y = apply(arg,2,sum)
error = sum(predict_y - y)/length(predict_y)
print(paste("The mean error in the model is",error))

# good points about the model

## The model is a good model as it gives relatively small error value of order -0.006 only

## Also looking at the plot of predicted y and actual we see a linear relationship
par(mfrow = c(1,2))
plot(y,predict_y)

## Looking at the plot of cost function with iterations we see gradually reducing value
## of cost function without any erratic movement.

plot(Performance)

## The learning parameter self adjusts (reduces) as it approached the optimum value

## It can handle larger number of parameters than normal equations