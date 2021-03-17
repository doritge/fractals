############################
#
# Regression fractal tree
# Based on David Shiffman's The Code Train: Coding Challenge #14
# testing changes
#
###########################
library(tidyverse)

# The regression function - branches 2 segments out of the current branch
# a jitter is introduced for tree inclination
branch <- function(current,  # current branch
                   rate,     # rate of decreasing length of branches
                   a,        # angle of branches
                   levels)   # number of levels in iteration
  {

  if(current$level == levels) return(current)
  
  left <- current %>%
    mutate(x = xend,
           y = yend,
           angle = angle + a + jitter(-0.21, amount = 0.2),
           xend = x + len * cos(angle),
           yend = y + len * sin(angle),
           level = level + 1,
           len = rate * len) %>%
    branch(rate, a, levels)
  
  right <- current %>%
    mutate(x = xend,
           y = yend,
           angle = angle - a  + jitter(-0.21, amount = 0.2),
           xend = x +len * cos(angle),
           yend = y + len * sin(angle),
           level = level + 1,
           len = rate * len) %>%
    branch(rate, a, levels)
  
  rbind(current, right, left)
}

### generating the tree by using the regresiion branch function
branches <- tibble(x = 100,
                   y = 0,
                   xend = 100,
                   yend = 80,
                   angle = pi/2,
                   level = 1) %>%
  mutate(len = sqrt((xend - x)^2 + (yend - y)^2)) %>%
  branch(0.7, pi/6, 8)

### generating leaves at the last branches
leaves <- branches %>%
  filter(level == max(level)) %>%
  select(xend, yend)

### Plotting
p <- ggplot() +
  geom_curve(data = branches,
             aes(x = x, y = y, xend = xend, yend = yend, size = 1/level),
             curvature = 0.03,
             color = "black") +                                            # branches
  geom_point(data = leaves, aes(x = xend, y = yend), color = "#e62e00") +  # leaves
  geom_point(aes(x = 320, y = 340), size = 30, color = "#ffffe6") +        # moon
  coord_equal() +
  scale_x_continuous(limit = c(-80, 400)) +
  scale_y_continuous(limit = c(0, 400)) +
  theme(legend.position  = "none",
        panel.background = element_rect(fill = "grey30"),
        panel.grid       = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())

ggsave("fractal_tree.png", p, height = 6, width = 8)



                     
  



