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
  x['d1'] = (log10(x['futurePrice']/x['Strike']) + ((d_rate+(sigma^2)/2)*x['time_to_expiry']))/(sigma*sqrt(x['time_to_expiry']))
  x['d2'] = x['d1'] - sigma*sqrt(x['time_to_expiry'])
  x['sigma'] <- sigma
  
  #dummy option price; inverse norm or norm
  x['n_d1'] = pnorm(as.numeric(unlist(x['type']*x['d1'])), mean = 0, sd = 1)
  x['n_d2'] = pnorm(as.numeric(unlist(x['type']*x['d2'])), mean = 0, sd = 1)
  
  if(x['type'] == 1) 
    x['dummy'] =  x['n_d1']*x['futurePrice'] - x['n_d2']*x['Strike']*exp(-d_rate*x['time_to_expiry'])
  if(x['type']== -1) 
    x['dummy'] =  x['n_d2']*x['Strike']*exp(-d_rate*x['time_to_expiry']) - x['n_d1']*x['futurePrice']
   return (x)
}

plotImpliedVol=function(option){
  bsm <- apply(option, 1, function(x) {
    
    #use bisect method to find sigma; match dummy to listed option price
    #find midpt then move left or right and compare left and right tolerance
    opti <- function(x, lower, upper){
      mid = (lower+ upper)/2
      
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
      
      #go left or right of price based on tolerance
      tolerance = abs(c_dummy- x['optionPrice'] )
      min = which.min(tolerance)
      
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
          x<- opti(black(x, x['sigma']), low, high)
        }
        else{
          break
        }
      }
      
      return(x)
    }
    
    #initialize lower and upper bound random numbers
    opti(x, 0,10)
  })
}

#make option plots
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
  d_graph = NULL
  for (i in 1:nrow(option)){
      data <- (option[i,])
      data$groupID <- i
      
      graph <- merge(graph_option, data,  all=T)
      
      #calculate each volatility increment
      calc_grph <- as.data.frame(t(apply(graph, 1, function(graph) black(graph,graph['volatility']))))
      d_graph = rbind(d_graph, calc_grph)
  }
  graph2 <- melt(d_graph, id=c("groupID","dummy","volatility"))
  
  diagram <- ggplot(graph2, aes(volatility,dummy, group=factor(groupID), colour=factor(groupID) )) + 
    geom_line(size=2) +
    labs(x="Implied Volatility" ,y="Option Price") + 
    ggtitle("Price-Volatility Chart") +
    labs(color='Option Number') +
    facet_wrap(~groupID, ncol=2, scales = "free")
  
  print(diagram)
  print("Finished Running. Wait for plot.")
}

#call functions to calculate and print the result and plot
result <- plotImpliedVol(option)
print(t(result))
print(proc.time() - start)
plot (result)

#clean any variables and restart R
.rs.restartR()