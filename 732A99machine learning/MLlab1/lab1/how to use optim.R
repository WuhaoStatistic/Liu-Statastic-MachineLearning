# IN ALL
# optim() helps find the minimum value of input function, and also corresponding parameters 

# this is the data
data=data.frame(x=c(1,2,3,4,5,6), y=c(1,3,5,6,8,12))

# this is the function that need to be optimized
# it should return a single value
min.RSS <- function(dat, par) {with(dat, sum((par[1] + par[2] * x - y)^2))} 

# par is the initial value of all parameter.
# in this example formula has 2 parameters par[1], par[2] , so vector par here should also length 2 
# and in this example , initial value is 0 and 1.
#
# funtion is min.RSS
# since in the function min.RSS,we need a variable called dat, so we have to add dat = data
result <- optim(c(0, 1), fn = min.RSS, dat = data)

# the result is about the min value of function(perhaps its local minimum) with the corresponding parameters
# we can also choose different method to get the minimum value
# for this example is -1.1266486 and 2.028620
#######################
# test
x <-c(1,2,3,4,5,6)
y <-c(1,3,5,6,8,12)

sum((-1.266846+2.028620*x-y)^2) 
result[['value']]
#######################
# simple plot to test
# remain p1 unchanged, p2 from 1.51 to 2.5
# When reach the lowest value, p2 is about 2.02 
p1 <- -1.266486
p2 <- 2.028620
l1 <- c()
for (p2 in 151:250/100)
{
  l1 <- c(l1,sum((p1 + p2 * x - y)^2))
}
plot(1:100,l1)
