library(ggplot2)
library(reshape2)
#provided data
option <- data.frame(Strike = c(50, 45, 20),
                     type = c("C", "C","P"),
                     optionPrice = c(1.62,1.50, 0.01),
                     futurePrice = c(48.03,45.0, 48.03),
                     time_to_expiry = c(0.1423,0.15, 0.1423)
)
#convert to integer and use it for later in bsm 
option$type <- ifelse(option$type == "C", 1.0, -1.0)

#set user initial defaults
#risk free wasn't provided; precision wasn't provided
option["sigma"] <- 5
option["dummy"] <- 10
d_rate = 0.05
default_tolerance = 0.0001
start <- proc.time()

#generalized bsm model, input df and volatility
black <- function(x, sigma){
  x['d1'] = (log10(x['futurePrice']/x['Strike']) + ((d_rate+(sigma**2)/2)*x['time_to_expiry']))/sigma*sqrt(x['time_to_expiry'])
  x['d2'] = x['d1'] - sigma*sqrt(x['time_to_expiry'])
  x['sigma'] <- sigma
  
  #dummy option price
  #inverse norm or norm
  x['n_d1'] = dnorm(as.numeric(unlist(x['type']*x['d1'])), mean = 0, sd = 1, log = TRUE)
  x['n_d2'] = dnorm(as.numeric(unlist(x['type']*x['d2'])), mean = 0, sd = 1, log = TRUE)
  
  if(x['type'] == 1) 
    x['dummy'] =  x['n_d1']*x['futurePrice'] - x['n_d2']*x['Strike']*exp(-d_rate*x['time_to_expiry'])
  if(x['type']== -1) 
    x['dummy'] =  x['n_d2']*x['Strike']*exp(-d_rate*x['time_to_expiry']) - x['n_d1']*x['futurePrice']
   return (x)
}

plotImpliedVol=function(option){
  bsm <- apply(option, 1, function(x) {
    
    #using Olog(n) to bisect find sigma; match dummy to listed option price
    opti <- function(x, lower, upper){
      mid = (lower+ upper)/2
      
      #bisect, then move left or right and compare left and right
      #walk left of midpt
      l_low = lower
      l_high = mid
      left = (l_low+mid)/2
      
      #walk right of midpt
      r_low = mid
      r_high = upper
      right = (mid+r_high)/2
      
      left_x <- black(x, left)
      right_x <-black(x, right)
      
      c_dummy = c(left_x['dummy'], right_x['dummy'])
      
      #go left or right of price
      tolerance = abs(c_dummy- x['optionPrice'] )
      min = which.min(tolerance)
      
      #handle of negative price?
      for (i in 1:length(tolerance)){
        if(tolerance[i] > default_tolerance){
          if (min == 1 ){ 
            x['sigma'] <- left
            low = l_low
            high = l_high
          }
          else if (min == 2 ){
            x['sigma'] <- right
            low = r_low
            high = r_high
          }
        }
        else{
          break
        }
        x<- opti(black(x, x['sigma']), low, high)
      }
      
      return(x)
    }
    
    #initialize lower and upper bound random numbers
    opti(x, 0,10)
  })
}

plot <- function (plot_df){
  
  option <- as.data.frame(t(plot_df))
  min_loop = 1000
  # have options axis length the same to plot on the same graph; use min just so it looks nice per the question
  for (i in 1:nrow(option)){
    data <- (option[i,])
    vol_increment <- seq(0.01, as.numeric(ceiling(data$sigma/0.05)*0.05 ), by = 0.01)
    min_loop = min(length(vol_increment),min_loop)
  }
  
  
  #for call = 0 to optionPrice, calculate volatility to option$sigma
  vol_increment <- seq(0.01, min_loop*0.01, by = 0.01)
  graph_option <- data.frame(volatility = c(vol_increment))
  
  # For each option we will create a new set of df; each new df is then plotted
  # Question didn't ask to plot what to volatility; will assume price? 
  # but pretty much this can graph any column since every calculation is stored
  d = NULL
  for (i in 1:nrow(option)){
      data <- (option[i,])
      data$groupID <- i
      
      graph <- merge(graph_option, data,  all=T)
      
      #calculate each volatility increment
      calc_grph <- as.data.frame(t(apply(graph, 1, function(graph) black(graph,graph['volatility']))))
      d = rbind(d, calc_grph)
  }
  graph2 <- melt(d, id=c("groupID","dummy","volatility"))
  plot <- ggplot(graph2, aes(dummy, volatility, group=groupID, colour=groupID )) + 
    geom_line(size=2) +
    labs(x="Option Price",y="Implied Volatility") + 
    ggtitle("Price-Volatility Chart") 
  
  # plot <- qplot(dummy, volatility, data=graph2, group=groupID, 
  #               colour=groupID, geom='line', 
  #               main = "Price-Volatility Chart",
  #               xlab="Price", ylab="Implied Volatility") +  
  #               labs(colour = 'Group ID') + 
  #               geom_smooth(aes(group=interaction(groupID))) 
  
  print(plot)
  print("Finished Running.")
}

#call functions to calculate and print the result and plot
result <- plotImpliedVol(option)
print(result)
print(proc.time() - start)
plot (result)

#clean any variables and restart R
.rs.restartR()