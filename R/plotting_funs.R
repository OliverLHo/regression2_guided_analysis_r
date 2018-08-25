resid_plot <- function(model, bins = 50, type = c("normal", "standard", 
                                                  "student")){
  require(ggplot2)
  type <- match.arg(type)
  if(type == "normal"){
    plotdf <- data.frame(resid = residuals(model))
  } else if(type == "standard"){
    plotdf <- data.frame(resid = rstandard(model))
  } else if(type == "student"){
    plotdf <- data.frame(resid = rstudent(model))
  }
  min_x <- min(plotdf$resid)
  max_x <- max(plotdf$resid)
  bw <- (max_x-min_x) / bins
  max_x <- max(c(abs(min_x), abs(max_x)))
  min_x <- -max_x
  
  p1 <- ggplot(plotdf, aes(x = resid)) + 
    geom_histogram(breaks = seq(min_x, max_x, bw), 
                   color = I("black"), 
                   fill = I("white")) + 
    stat_function(
      fun = function(x, mean, sd, n, bw){
        dnorm(x = x, mean = mean(plotdf$resid), 
              sd = sd(plotdf$resid)) * nrow(plotdf) * bw
      }, 
      args = c(mean = mean, sd = sd, n = n, bw = bw)) + 
    geom_vline(xintercept = 0, linetype = 2, color = I("dark red")) + 
    geom_vline(xintercept = median(plotdf$resid), linetype = 1, 
               color = I("dark green")) +
    theme_bw()
  print(p1)
}
