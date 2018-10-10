#Reed Magleby
#Lab Assignemnt 3

library(tidyverse)
library(gapminder)

#Problem 1
#Recreate Graph

x1 <- seq(-3, 3, by=0.01)
x2 <- seq(0, 6, by=0.01)
x1_area <-seq(qnorm(0.95),3,by=0.01)
x2_area <- seq(0, qnorm(0.05, mean=3.3, sd=1), by=0.01)

norm1 <- data.frame(x = x1, y = dnorm(x1, mean = 0, sd = 1))
norm1_area <- data.frame(x = x1_area, y = dnorm(x1_area, mean = 0, sd = 1), "type"="Type I error")
norm2 <- data.frame(x = x2, y = dnorm(x2, mean = 3.3, sd = 1))
norm2_area <- data.frame(x = x2_area, y = dnorm(x2_area, mean = 3.3, sd = 1),"type"="Type II error")

ggplot(data=norm1_area, aes(x=x,y=y)) +
  geom_line(data = norm1, aes(x=x,y=y), color="blue") +
  geom_line(data = norm2, aes(x=x,y=y), color="red") +
  geom_ribbon(data = norm1_area, aes(x=x, ymax=y, ymin=0, fill="blue"), alpha=0.5, show.legend=TRUE) +
  geom_ribbon(data = norm2_area, aes(x=x, ymax=y, ymin=0, fill="red"), alpha=0.5, show.legend=TRUE) +
  scale_fill_manual(values=c("blue","red"), labels=c("Type I Error","type II Error"), name=NULL) +
  theme_classic() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor=element_blank(), 
        legend.position=c(0,1), 
        legend.justification = c(0,1),
        legend.background = element_rect(colour = "black", fill=NA),
        panel.background = element_rect(colour = "black", fill=NA)) + 
  geom_vline(xintercept = qnorm(0.95), linetype = "dashed") +
  labs(y=NULL,x=NULL) +
  scale_x_continuous(labels = c(expression(paste(theta,"o")), expression(paste(theta,"a"))), breaks = c(0,3.3)) +
  annotate("text", x=1, y=0.015, label = expression(beta)) +
  annotate("text", x=2.3, y=0.015, label = expression(alpha), color="white") 

#Problem 2
#Implement a function that will check if a given positive integer is a prime number.

prime <- function(x){
  if (x == 2){
    print(paste0(x, " is a prime number"))
  } else if (any(x %% 2:(x-1)==0)){
    print(paste0(x, " is not a prime number"))
  } else {
    print(paste0(x, " is a prime number"))
  }
}

#test prime function
prime(67)
