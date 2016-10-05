# Borrowed from: http://stackoverflow.com/questions/4357031/qqnorm-and-qqline-in-ggplot2
# With Slight modifications to match our other ggplot2 plots!
# Thanks so much to stack overflow user floo0!
gg_qq <- function(x, distribution = "norm", ..., line.estimate = NULL, conf = 0.95,
                  labels = names(x)){
  q.function <- eval(parse(text = paste0("q", distribution)))
  d.function <- eval(parse(text = paste0("d", distribution)))
  x <- na.omit(x)
  ord <- order(x)
  n <- length(x)
  P <- ppoints(length(x))
  df <- data.frame(ord.x = x[ord], z = q.function(P, ...))
  
  if(is.null(line.estimate)){
    Q.x <- quantile(df$ord.x, c(0.25, 0.75))
    Q.z <- q.function(c(0.25, 0.75), ...)
    b <- diff(Q.x)/diff(Q.z)
    coef <- c(Q.x[1] - b * Q.z[1], b)
  } else {
    coef <- coef(line.estimate(ord.x ~ z))
  }
  
  zz <- qnorm(1 - (1 - conf)/2)
  SE <- (coef[2]/d.function(df$z)) * sqrt(P * (1 - P)/n)
  fit.value <- coef[1] + coef[2] * df$z
  df$upper <- fit.value + zz * SE
  df$lower <- fit.value - zz * SE
  
  if(!is.null(labels)){ 
    df$label <- ifelse(df$ord.x > df$upper | df$ord.x < df$lower, labels[ord],"")
  }
  
  p <- ggplot(df, aes(x=z, y=ord.x)) +
    geom_point() + 
    geom_abline(intercept = coef[1], slope = coef[2], col="turquoise4") +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha=0.2) + 
    theme(panel.background = element_rect(fill = "gray98"),
          panel.grid.minor = element_blank(),
          axis.line.y   = element_line(colour="grey80"),
          axis.line.x = element_line(colour="grey80")) +
    labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
    
  if(!is.null(labels)) p <- p + geom_text( aes(label = label))
  print(p)
  coef
}
