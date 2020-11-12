
## Function plotting population pyramids 

ggpyramid <- function(data = data,
                      age = age,
                      pop = pop,
                      gender = gender, 
                      men = "Men",
                      women = "Women",
                      fill = gender,
                      annotate = FALSE, 
                      percent = TRUE) {
  if(percent){
    data$pop_pct <-  100 * {{ data$pop }} / sum( {{ data$pop }})
    p <- ggplot() + 
      geom_bar(data = subset( {{ data }}, {{ gender }} == women ),
               aes(x = {{ age }},
                   y = pop_pct,
                   fill = {{ fill }}), 
               stat = "identity", 
               width = 1) + 
      geom_bar(data = subset( {{ data }}, {{ gender }} == men ),
               aes(x = {{ age }},
                   y = - pop_pct,
                   fill = {{ fill }}), 
               stat = "identity",
               width = 1) + 
      geom_hline(yintercept = 0, colour = "grey10") +
      scale_x_discrete(name = "Age") +
      coord_flip() 
  }
  else {
    y_max <- ceiling(max(abs(data$pop/1000)))
    p <- ggplot() + 
      geom_bar(data = subset( {{ data }}, {{ gender }} == women ),
               aes(x = {{ age }},
                   y = {{ pop }} / 1000,
                   fill = {{ fill }}), 
               stat = "identity", 
               width = 1) + 
      geom_bar(data = subset( {{ data }}, {{ gender }} == men ),
               aes(x = {{ age }},
                   y = - {{ pop }} / 1000,
                   fill = {{ fill }}), 
               stat = "identity",
               width = 1) + 
      geom_hline(yintercept = 0, colour = "grey10") +
      scale_x_discrete(name = "Age") +
      coord_flip() 
  }
  
  if(percent & annotate) {
    p +
      annotate(geom = "text",
               y = max(data$pop_pct) / 2,
               x = max(as.numeric(data$age)),
               label = women,
               fontface = "bold") +
      annotate(geom = "text",
               y = -max(data$pop_pct)/ 2 ,
               x = max(as.numeric(data$age)),
               label = men,
               fontface = "bold") 
   } else{
     if(!percent & annotate) {
    p +
      annotate(geom = "text",
               y = max(data$pop) / 2000,
               x = max(as.numeric(data$age)),
               label = women,
               fontface = "bold") +
      annotate(geom = "text",
               y = -max(data$pop) / 2000 ,
               x = max(as.numeric(data$age)),
               label = men,
               fontface = "bold")
     } else{ 
    p
     }
   }
}
