1)a

coins <- c(rep("Gold", 20), rep("Silver", 30), rep("Bronze", 50))
drawn_coins <- sample(coins, 10, replace = FALSE)  
print(drawn_coins)


1)b

outcomes <- c("Success", "Failure")
probs <- c(0.9, 0.1)
surgery_results <- sample(outcomes, 10, replace = TRUE, prob = probs)


2)a

# Function to estimate probability using simulation
birthday_simulation <- function(n, trials=10000) {
  count <- 0  # Count how many times at least two people share a birthday
  
  for (i in 1:trials) {
    birthdays <- sample(1:365, n, replace=TRUE)  # Random birthdays for n people
    if (length(unique(birthdays)) < n) {  # If a duplicate birthday exists
      count <- count + 1
    }
  }
  
  return(count / trials)  # Probability estimate
}

# Simulate for different values of n
for (n in c(5, 10, 20, 23, 30, 50)) {
  cat("For n =", n, "Estimated Probability:", birthday_simulation(n), "\n")
}


2)b


# Function to estimate the probability of at least two people sharing a birthday
birthday_simulation <- function(n, trials=10000) {
  count <- 0  # Counter for successful birthday matches

  for (i in 1:trials) {
    birthdays <- sample(1:365, n, replace=TRUE)  # Generate random birthdays
    if (length(unique(birthdays)) < n) {  # If at least one birthday repeats
      count <- count + 1
    }
  }

  return(count / trials)  # Return estimated probability
}

# Find the smallest n where probability > 0.5
find_n_for_50_percent <- function() {
  n <- 1
  while (birthday_simulation(n) < 0.5) {  # Keep checking until probability > 0.5
    n <- n + 1
  }
  return(n)
}

# Compute and print the result
smallest_n <- find_n_for_50_percent()
cat("Smallest n where probability > 0.5:", smallest_n, "\n")


3)

# Function to compute conditional probability using Bayes' Theorem
bayes_conditional_prob <- function(event_prob, given_event_prob, total_given_prob) {
  return ((given_event_prob * event_prob) / total_given_prob)
}

# Given probabilities
prob_rain <- 0.2           # Probability of rain (P(Rain))
prob_cloudy <- 0.4         # Probability of cloudy weather (P(Cloudy))
prob_cloudy_given_rain <- 0.85  # Probability of cloudy given rain (P(Cloudy | Rain))

# Compute P(Rain | Cloudy) using Bayes' Theorem
prob_rain_given_cloudy <- bayes_conditional_prob(prob_rain, prob_cloudy_given_rain, prob_cloudy)

# Print the result
cat("P(Rain | Cloudy) =", prob_rain_given_cloudy, "\n")


4)

data<-iris


5)

getmode <- function(v) {  
  uniqv <- unique(v)  
  uniqv[which.max(tabulate(match(v, uniqv)))]  
}

v <- c(2,1,2,3,1,2,3,4,1,5,5,8,2,3)  
mode <- getmode(v)  
print(mode)





pmf cdf sketch




len=seq(0,31,1)
n=31
p=0.447
pmf_values=numeric()
for(i in 1:length(len))
{
  pmf_values[i]=dbinom(len[i],n,p)
}
plot(len,pmf_values)

len=seq(0,31,1)
n=31
p=0.447
cmf_values=numeric()
for(i in 1:length(len))
{
  cmf_values[i]=pbinom(len[i],n,p)
}
plot(len,cmf_values)




Assign 4




1)

x=c(0,1,2,3,4)
p=c(0.41,0.37,0.16,0.05,0.01)
f1=sum(x*p)
print(f1)
f2=weighted.mean(x,p)
print(f2)
f3=c(x%*%p)
print(f3)


2)

f=function(t){
s=(t)*0.1*exp((-0.1)*(t))
return(s)
}
integrate(f,0,Inf)$value

3)# Define the number of books sold and corresponding probabilities
x = c(0, 1, 2, 3)  # Number of books sold
p = c(0.1, 0.2, 0.2, 0.5)  # Probabilities

# Compute net revenue correctly
y = (12 * x) + (3 - x) * 2 - 18  

# Compute expected value E(Y)
expected_Y = sum(y * p)  

print(expected_Y)  # Should print 7.4


4)

f1 = function(x){
s = (x) * (0.5 * exp(-(abs(x))));
return (s);
}
first = integrate(f1,1,10)$value;
print(first);
f2 = function(x){
r = (x^2) * (0.5 * exp(-(abs(x))));
return (r);
}
second = integrate(f2,1,10)$value;
print(second);
print('Mean is');
print(first);
print('Variance is');
print(second-(first^2));



5)

f = function(x){
return ((3/4) * (1/4)^(x-1));
}
p = function(y){
x = sqrt(y);
if(x == floor(x)){
return (f(x));
}else{
return (0);
}
}
prob = p(9);
print(prob);
v = c(1:5);
e = sum(v, x^2 * f(x));
print(e);
e_sqrt = sum(v, (x^2) ^ 2 * f(x));
print(e_sqrt);
var = e_sqrt - (e ^ 2);
print(var);


