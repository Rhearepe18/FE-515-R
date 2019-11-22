# Quesion 2

#2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder. 
#Now you need to create a function to find the smallest positive number that is evenly divisible by two numbers you input into the function.

#The least common multiple (L.C.M.) of two numbers is the smallest positive integer that is perfectly divisible by the two given numbers.
# Program to find the L.C.M. of two input number

lcm <- function(x, y) {
  # choose the greater number
  if(x > y) {
    greater = x
  } else {
    greater = y
  }
  while(TRUE) {
    if((greater %% x == 0) && (greater %% y == 0)) {
      lcm = greater
      break
    }
    greater = greater + 1
  }
  return(lcm)
}
# take input from the user
num1 = as.integer(readline(prompt = "Enter first number: "))
num2 = as.integer(readline(prompt = "Enter second number: "))
print(paste("The L.C.M. of", num1,"and", num2,"is", lcm(num1, num2)))


#Question 3

#Read JPM 
setwd("/Users/rhearepe/Downloads")
JPM <- read.csv("JPM.csv", header = T)
head(JPM)

#Sub-table which only contains Open, High, Low and Close
JPM.OHLC <- JPM[,1:5]
head(JPM.OHLC)

#Using ”sapply” function we mentioned in class to calculate mean value for each column and save it as a vector
mean.JPM <- sapply(JPM.OHLC[,2:5],mean)
mean.JPM

#Using ”apply” function we mentioned in class to calculate mean value for each row and save it as a 3 by 5 matrix, 
#the data should be assigned by row.

mean.byrow <- apply(JPM.OHLC[2:5],1,mean)
matrix(mean.byrow,nrow=3, ncol=5, byrow=T)

#Question 4

     #1. What’s the difference between ”mapply” and ”lapply”?

#lapply returns a list of the same length as X, 
#each element of which is the result of applying FUN to the corresponding element of X

#mapply is a multivariate version of sapply. 
#mapply applies FUN to the first elements of each … argument, the second elements, the third elements, and so on. 
#Arguments are recycled if necessary.

      #2.How to use ”mapply”? Write an example.
# mapply function in R
mapply(sum, 1:4, 1:4, 1:4)
#mapply sums up all the first elements(1+1+1) ,sums up all the second elements(2+2+2) and so on so the result will be:
#[1]  3   6   9   1

      #3. Can you use ”mapply” to the function you created in Question 2?
#If yes, assign two vectors as inputs for the self-defined function. If not, explain why.
#Yes, mapply can be used to the function created in question 2
#Function from question 2 is:
lcm <- function(x, y) {
  # choose the greater number
  if(x > y) {
    greater = x
  } else {
    greater = y
  }
  while(TRUE) {
    if((greater %% x == 0) && (greater %% y == 0)) {
      lcm = greater
      break
    }
    greater = greater + 1
  }
  return(lcm)
}
# take input from the user
#once the inputs are taken from the user, mapply can be used to directly find the answer
num1 = as.integer(readline(prompt = "Enter first number: "))
num2 = as.integer(readline(prompt = "Enter second number: "))
  mapply(lcm, num1, num2)   #mapply function can be used in this way
print(paste("The L.C.M. of", num1,"and", num2,"is", lcm(num1, num2))) 



#Question 5
#Write a self-define function which can distinguish an input number is prime number or not. 
#Please give a example, for this example, use a number which is lower than 100.


# prime numbers are greater than 1
checkprime <- function(num) {
  if(num == 2) {
    print("Number is prime")
    } else {
    if(num %% (2:(num-1)) != 0) {
      print("Number is prime")
    } else { 
      print("Number is not prime")
      
}}}

checkprime(101)  




# Question 2 

Num <- function(x, y) {

  a <- seq(x, y) # creates the sequence from the x and y values in the function
  b <- 1 
  while (TRUE) { 
    t <- b%%a  
    if (sum(t) == 0) {
      return(b) 
      break 
    } else { 
      b <- b + 1 
    }
  }
}


Num(1, 10)












