############################
#
# Animating the recurssive fractal tree - Starry starry night
#
###########################
library(tidyverse)
library(gganimate)

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

### generating stars
stars_state <- tibble(x = sample(-70:420, 40), y = sample(280:390, 40))
stars <- do.call("rbind", replicate(10, stars_state, simplify = FALSE)) %>%
  mutate(x = jitter(x, amount = 0.5),
         y = jitter(y, amount = 0.5),
         state = rep(1:10, each = 40))


### Plotting
p <- ggplot() +
  geom_curve(data = tree,
             aes(x = x, y = y, xend = xend, yend = yend, size = 1/level),  # tree
             curvature = 0.03,
             color = "black") +
  geom_point(data = cherries, aes(x = xend, y = yend),
             color = "#e62e00") +                      # cherries
  geom_point(data = stars, aes(x = x, y = y, group = interaction(x, state)),
             size = 2, color = "#ffff66") +            # stars
  coord_equal() +
  scale_x_continuous(limit = c(-80, 430)) +
  scale_y_continuous(limit = c(0, 400)) +
  theme(legend.position  = "none",
        panel.background = element_rect(fill = "grey30"),
        panel.grid       = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())

anim <- p +
  transition_states(state, transition_length = 2, state_length = 1) +
  ease_aes("sine-in-out") +
  enter_recolor(color = "#ffff66") +
  exit_recolor(color = "#ffffcc")

animate(anim, renderer = magick_renderer(loop = TRUE), nframes = 150, fps = 25,
        height = 400, width = 600)

anim_save("images/stars_tree.gif", animattion = last_animation())  # default

