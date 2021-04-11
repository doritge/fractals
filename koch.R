######################################
#
#  Koch snowflakes and more
#  The standard Koch snowflake algorithm works for triangles.
#  The code here supports n-side polygons
#  Each iterations builds a new polygon on the middle of the previous segment
#
#####################################
library(tidyverse)
library(ggforce)
library(gganimate)

split_segment <- function(s, n){
  # Each segments is split to 3 and on the middle a new polygon is built.
  # Overall n+1 segments are generated from each segment
  
  len <- s$len/3
  
  iterations <- c(s$iter, rep(s$iter + 1, n-1), s$iter)
  
  # The angles on the first and last sub-segments remain as is. The angles of the new polygon
  # are calculated
  angles <- s$angle
  for (i in 1:(n-1)) angles <- c(angles, s$angle + (2*i - n)*pi/n)
  angles <- c(angles, s$angle)
  
  # generate sub-segments
  tibble(x = accumulate(angles[1:n], ~.x + len*cos(.y), .init = s$x),
         y = accumulate(angles[1:n], ~.x + len*sin(.y), .init = s$y),
         len = len,
         iter = iterations,
         angle = angles)
}

#### Process shape
n_iter <- 4   # number of iterations
n <- 3        # number of sides
len <- 1      # inital length of side

# The initial polygon
first_poly <- tibble(x = accumulate(1:(n-1), ~.x + len*cos(.y*2*pi/n), .init = 0),
                     y = accumulate(1:(n-1), ~.x + len*sin(.y*2*pi/n), .init = 0),
                     len = len,
                     iter = 0,
                     angle = accumulate(2:n, ~.y*2*pi/n, .init = 2*pi/n))

# Iterate
s_list <- first_poly %>%
  split(seq(nrow(.)))

for (i in 1:n_iter){
  segments <- s_list %>%
    map2_dfr(n, split_segment)
  s_list <- segments %>%
    split(seq(nrow(.)))
}

# Plot
p <- segments %>%
  ggplot() +
  geom_shape(aes(x = x, y = y),
             color = "white", fill = NA, size = 1) +
  coord_equal() +
  theme(legend.position  = "none",
      panel.background = element_rect(fill = "black"),
      panel.grid       = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank())

#### Animate
# Need to collect all segments not only of last iteration

all_segs <- first_poly %>%
  mutate(iteration = 0)

s_list <- first_poly %>%
  split(seq(nrow(.)))

for (i in 1:n_iter){
  segments <- s_list %>%
    map2_dfr(n, split_segment)
  s_list <- segments %>%
    split(seq(nrow(.)))
  all_segs <- all_segs %>%
    rbind(mutate(segments, iteration = i))
}

# Plot

p_all <- all_segs %>%
  ggplot() +
  geom_shape(aes(x = x, y = y, group = interaction(len, iteration)),
             color = "white", fill = NA, size = 1.5) +
  coord_equal() +
  theme(legend.position  = "none",
        panel.background = element_rect(fill = "black"),
        panel.grid       = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())

anim <- p_all +
  transition_states(iteration, transition_length = 1, state_length = 4) +
  ease_aes("sine-in-out")

animate(anim, renderer = magick_renderer(loop = TRUE))

#### Saving
ggsave(str_c("images/koch_", as.character(n), "_", as.character(n_iter), ".png"),
       p, height = 20, width = 20, units = "cm", dpi = 600)

anim_save("images/koch_anim_3_4.gif", animattion = last_animation())
