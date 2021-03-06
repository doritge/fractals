############################
#
# Recurssive fractal tree
#
###########################
library(tidyverse)

# Generate a right or left sub-branch
sub_branch <- function(current,     # current branch
                       direction,   # left / right
                       g_rate,      # growth rate of sub-branch
                       a)           # angle of sub-branch
{
  
  if (direction == "right") a <- -a
  
  current %>%
    tail(1) %>%
    mutate(level = level + 1,
           len = g_rate * len,
           angle = angle + a + jitter(-0.21, amount = 0.05),
           x = xend,
           y = yend,
           xend = x + len * cos(angle),
           yend = y + len * sin(angle))
}

# Main regression function - branches 2 segments out of the current branch
# a jitter is introduced for a more natural look
branch <- function(current,    # current branch
                   g_rate,     # growth rate of branches
                   a,          # angle of branches
                   t_levels)   # number of tree levels
{
  
  if(tail(current, 1)$level == t_levels) return(current)
  
  left <- sub_branch(current, "left", g_rate,  a) %>%
    branch(g_rate, a, t_levels)
  
  right <- sub_branch(current, "right", g_rate,  a) %>%
    branch(g_rate, a, t_levels)
  
  rbind(current, right, left)
}

### generating the tree by using the regression function
t_levels <- 12
tree <- tibble(x = 100,
                   y = 0,
                   xend = 100,
                   yend = 80,
                   len = 80,
                   angle = pi/2,
                   level = 1) %>%
  branch(0.7, pi/6, t_levels)

### generating leaves at the last branches
cherries <- tree %>%
  filter(level == max(level)) %>%
  select(xend, yend)

### Plotting
p <- ggplot() +
  geom_point(aes(x = 350, y = 320), size = 35, color = "#ffffe6") +        # moon
  geom_curve(data = tree,
             aes(x = x, y = y, xend = xend, yend = yend, size = 1/level),  # tree
             curvature = 0.03,
             color = "black") +
  geom_point(data = cherries, aes(x = xend, y = yend), color = "#e62e00") +  # cherries
  coord_equal() +
  scale_x_continuous(limit = c(-80, 430)) +
  scale_y_continuous(limit = c(0, 400)) +
  theme(legend.position  = "none",
        panel.background = element_rect(fill = "grey30"),
        panel.grid       = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())

ggsave(str_c("images/recursive_tree_", as.character(t_levels), ".png"),
       p, height = 20, width = 20, units = "cm", dpi = 800)
