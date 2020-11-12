
## Aux. function plotting population pyramids 

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
    data$pop <-  100 * {{ data$pop }} / sum( {{ data$pop }})
    brks_y <- seq(-100, 100, 1)
    lmts_y = c(min(brks_y), max(brks_y))
    lbls_y <- paste0(as.character(abs(brks_y)), "%")
    p <- ggplot() + 
      geom_bar(data = subset( {{ data }}, {{ gender }} == women ),
               aes(x = {{ age }},
                   y = pop,
                   fill = {{ fill }}), 
               stat = "identity", 
               width = 1) + 
      geom_bar(data = subset( {{ data }}, {{ gender }} == men ),
               aes(x = {{ age }},
                   y = - pop,
                   fill = {{ fill }}), 
               stat = "identity",
               width = 1) + 
      geom_hline(yintercept = 0, colour = "grey10") +
      scale_y_continuous(name = NULL, breaks = brks_y, labels = lbls_y) +
      scale_x_discrete(name = "Age") +
      coord_flip() 
  }
  else {
    y_max <- ceiling(max(abs(data$pop/1000)))
    brks_y <- seq(-200, 200, 50)
    lbls_y <- paste0(as.character(abs(brks_y)), "k")
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
      scale_y_continuous(name = NULL, breaks = brks_y, labels = lbls_y) +
      scale_x_discrete(name = "Age") +
      coord_flip() 
  }
  
  if(percent & annotate) {
    p +
      annotate(geom = "text",
               y = max(data$pop) / 2,
               x = max(as.numeric(data$age)) - 1,
               label = women,
               fontface = "bold") +
      annotate(geom = "text",
               y = -max(data$pop)/ 2 ,
               x = max(as.numeric(data$age)) - 1,
               label = men,
               fontface = "bold") 
   } else{
     if(!percent & annotate) {
    p +
      annotate(geom = "text",
               y = max(data$pop) / 2000,
               x = max(as.numeric(data$age)) - 1,
               label = women,
               fontface = "bold") +
      annotate(geom = "text",
               y = -max(data$pop) / 2000 ,
               x = max(as.numeric(data$age)) - 1,
               label = men,
               fontface = "bold")
     } else{ 
    p
     }
   }
}
