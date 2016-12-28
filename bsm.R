library(ggplot2)
#The function plotImpliedVol:
option <- data.frame(Strike = c(50, 45, 20),
                    type = c("C", "C","P"),
                    optionPrice = c(1.62,1.50, 0.01),
                    futurePrice = c(48.03,45.0, 48.03),
                    time_to_expiry = c(0.1423,0.15, 0.1423)
                    )

option$type <- ifelse(option$type == "C", 1.0, -1.0)

#computes implived volatility and graph the implied volatility
plotImpliedVol=function(option){
  #set user initial defaults
  option["sigma"] <- 5
  option["dummy"] <- 100000
  d_rate = 0.05
  default_tolerance = 0.001
  
  bsm <- apply(option, 1, function(x) {
    
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
      if(x['type']== -1) {
        x['dummy'] =  x['n_d2']*x['Strike']*exp(-d_rate*x['time_to_expiry']) - x['n_d1']*x['futurePrice']
      }
      
      return (x)
    }
    
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
      tolerance = abs(c_dummy- x['optionPrice'])
      min = which.min(tolerance)
      
      for (i in 1:length(tolerance)){
        if(tolerance[i] > default_tolerance){
          if (min == 1 ){ #go left
            x['sigma'] <- left
            low = l_low
            high = l_high
          }
          else if (min == 2){ #go right
            x['sigma'] <- right
            low = r_low
            high = r_high
          }else{
            low = lower
            high = upper
          }
        }
        else{
          break
        }
        x<- opti(black(x, x['sigma']), low, high)
      }
      
      #finalize everything
      option <- as.data.frame(t(x))
      #for call = 0 to optionPrice, calculate volatility to option$sigma
      vol_increment <- seq(0.01, as.numeric(ceiling(option['sigma']/0.05)*0.05 ), by = 0.01)
      
      graph_option <- data.frame(volatility = c(vol_increment))
      graph <- merge(graph_option, option,  all=T)
      
      #calculate each volatility increment
      graph <- as.data.frame(t(apply(graph, 1, function(graph) black(graph,graph['volatility']))))
      plot <- ggplot(data=graph, aes(x=volatility, y=dummy))+ geom_line()
      print(plot)
      return(x)
    }
    #initialize lower and upper bound
    opti(x, 0,10)
  })
}


result <- plotImpliedVol(option)

.rs.restartR()