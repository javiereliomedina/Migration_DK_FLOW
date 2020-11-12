
## Function plotting population pyramids 

ggpyramid <- function(data = data,
                      age = age,
                      pop = pop,
                      gender = gender, 
                      men = "Men",
                      women = "Women",
                      fill = gender) {
  p <- ggplot() + 
    geom_bar(data = subset( {{ data }}, {{ gender }} == women ),
             aes(x = {{ age }},
                 y = {{ pop }},
                 fill = {{ fill }}), 
             stat = "identity", 
             width = 1) + 
    geom_bar(data = subset( {{ data }}, {{ gender }} == men ),
             aes(x = {{ age }},
                 y = - {{ pop }},
                 fill = {{ fill }}), 
             stat = "identity",
             width = 1) + 
    geom_hline(yintercept = 0, colour = "grey10") +
    scale_x_discrete(name = "Age") +
    coord_flip() 
}
