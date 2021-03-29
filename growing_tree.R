############################
#
# Animating the recurssive fractal tree - The growing tree
# This animation uses the transition_layer function
#   for smooth transition need add frames by spliting the branches into segments
#
###########################
library(tidyverse)
library(gganimate)

# Generate a right or left sub-branch (segmentation added)
sub_branch <- function(current,     # current branch
                       direction,   # left / right
                       n_segments,  # number of segments per branch
                       g_rate,      # growth rate of sub-branch
                       a)           # angle of sub-branch
{
  if (direction == "right") a <- -a
  
  b <- current %>%
    filter(row_number() == nrow(current)) %>%
    mutate(level = level + 1,
           len = g_rate * len,
           angle = angle + a + jitter(-0.21, amount = 0.05),
           x = xend,
           y = yend,
           xend = x + len / n_segments * cos(angle),
           yend = y + len / n_segments * sin(angle),
           layer = layer + 1)
  
  # partition branch to segments
  if (n_segments > 1){
    b <- accumulate(1:(n_segments - 1),
                    .f = function(prev, y){
                      mutate(prev,
                             x = xend,
                             y = yend,
                             xend = x + len / n_segments * cos(angle),
                             yend = y + len / n_segments * sin(angle),
                             layer = layer + 1)
                    },
                    .init = b) %>%
      bind_rows()
  }
  
  return(b)
}

# Main regression function - n_segment parameter added
branch <- function(current,    # current branch
                   g_rate,     # growth rate of branches
                   a,          # angle of branches
                   t_levels,   # number of tree levels
                   n_segments) # number of segments per level  
{
  if(tail(current, 1)$level == t_levels) return(current)
   
  left <- sub_branch(current, "left", n_segments, g_rate, a) %>%
    branch(g_rate, a, t_levels, n_segments)
  
  right <- sub_branch(current, "right", n_segments, g_rate, a) %>%
    branch(g_rate, a, t_levels, n_segments)
  
  rbind(current, right, left)
}

## generating the tree as a layer list by using the regression function
t_levels <- 12
n_segments <- 4  # n_segments = 1 is the original recursive tree
g_rate <- 0.7
a <- pi/6

tree <- tibble(x = 100,
                   y = 0,
                   xend = 100,
                   yend = 80 / n_segments,
                   angle = pi/2,
                   level = 1,
                   layer = 0,
                   len = 80)

if (n_segments > 1){
  tree <- accumulate(1:(n_segments - 1),
                  .f = function(prev, y){
                    mutate(prev, x = xend,
                           y = yend,
                           xend = x + len / n_segments * cos(angle),
                           yend = y + len / n_segments * sin(angle),
                           layer = layer + 1)
                  },
                  .init = tree) %>%
    bind_rows()
}

tree <- tree %>%
  branch(g_rate, a, t_levels, n_segments)

### generating leaves at the last branches
cherries <- tree %>%
  filter(layer == max(layer)) %>%
  mutate(layer = layer + row_number() %% 10) %>%
  select(xend, yend, layer)

## Plotting
p <- ggplot() +
  geom_point(aes(x = 350, y = 320), size = 35, color = "#ffffe6") +
  geom_curve(data = tree,
             aes(x = x, y = y, xend = xend, yend = yend, size = 1/level),
             curvature = 0.03,
             color = "black") +
  geom_point(data = cherries, aes(x = xend, y = yend, group = interaction(xend, layer)),
             color = "#e62e00") +
  coord_equal() +
  scale_x_continuous(limit = c(-80, 430)) +
  scale_y_continuous(limit = c(0, 400)) +
  theme(legend.position  = "none",
        panel.background = element_rect(fill = "grey30"),
        panel.grid       = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())

# animate a growing tree
anim <- p +
  transition_states(layer, transition_length = 1, state_length = 1) +
  ease_aes("sine-in-out") +
  shadow_mark()

animate(anim, renderer = magick_renderer(loop = FALSE), nframes = 150, fps = 25,
        height = 400, width = 600)

anim_save("images/growing_tree.gif", animattion = last_animation())  # default
