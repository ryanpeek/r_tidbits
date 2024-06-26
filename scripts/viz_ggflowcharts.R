#flowcharts

library(tidyverse)
library(igraph)
library(showtext)
library(rcartocolor)

# example from here: https://nrennie.rbind.io/blog/2022-06-06-creating-flowcharts-with-ggplot2/
goldilocks <- tibble(from = c("Goldilocks",
                              "Porridge", "Porridge", "Porridge",
                              "Just right",
                              "Chairs", "Chairs", "Chairs",
                              "Just right2",
                              "Beds", "Beds", "Beds",
                              "Just right3"),
                     to = c("Porridge",
                            "Too cold", "Too hot", "Just right",
                            "Chairs",
                            "Still too big", "Too big", "Just right2",
                            "Beds",
                            "Too soft", "Too hard", "Just right3",
                            "Bears!"))


g <- graph_from_data_frame(goldilocks, directed = TRUE)
coords <- layout_as_tree(g)
colnames(coords) = c("x", "y")

output_df <- as_tibble(coords) %>%
  mutate(step = vertex_attr(g, "name"),
         label = gsub("\\d+$", "", step),
         x = x*-1,
         type = factor(c(1, 2, 3, 2, 3, 2, 3, 3, 3, 3, 3, 3, 3, 1)))

# make boxes
plot_nodes <- output_df %>%
  mutate(xmin = x - 0.35,
         xmax = x + 0.35,
         ymin = y - 0.25,
         ymax = y + 0.25)


plot_edges <- goldilocks %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = c("from", "to"),
               names_to = "s_e",
               values_to = "step") %>%
  left_join(plot_nodes, by = "step") %>%
  select(-c(label, type, y, xmin, xmax)) %>%
  mutate(y = ifelse(s_e == "from", ymin, ymax)) %>%
  select(-c(ymin, ymax))

p <- ggplot() +
  geom_rect(data = plot_nodes,
            mapping = aes(xmin = xmin, ymin = ymin,
                          xmax = xmax, ymax = ymax,
                          fill = type, colour = type),
            alpha = 0.5)
p

# fonts
font_add_google(name = "Henny Penny", family = "henny")
showtext_auto()

p <- p +
  geom_text(data = plot_nodes,
            mapping = aes(x = x, y = y, label = label),
            family = "henny",
            color = "#585c45")
p

# arrows
(p <- p +
  geom_path(data = plot_edges,
            mapping = aes(x = x, y = y, group = id),
            colour = "#585c45",
            arrow = arrow(length = unit(0.3, "cm"), type = "closed")))

(p <- p +
    scale_fill_carto_d(palette = "Antique") +
    scale_colour_carto_d(palette = "Antique"))


# add title
(p <- p +
  labs(title = "The Goldilocks \nDecision Tree",
       caption = "N. Rennie\n\nData: Robert Southey. Goldilocks and the Three Bears.
       1837.\n\nImage: New York Public Library\n\n#30DayChartChallenge"))


# play with theme and color picker for background:

(p <- p +
  theme_void() +
  theme(plot.margin = unit(c(1, 1, 0.5, 1), "cm"),
        legend.position = "none",
        plot.background = element_rect(colour = "#f2e4c1", fill = "#f2e4c1"),
        panel.background = element_rect(colour = "#f2e4c1", fill = "#f2e4c1"),
        plot.title = element_text(family = "henny", hjust = 0, face = "bold",
                                  size = 40, color = "#585c45",
                                  margin = margin(t = 10, r = 0, b = 10, l = 0)),
        plot.caption = element_text(family = "henny", hjust = 0,
                                    size = 10, color = "#585c45",
                                    margin = margin(t = 10))))


p


# Add Image ---------------------------------------------------------------

library(magick)
bears <- image_read("https://nrennie.rbind.io/blog/2022-06-06-creating-flowcharts-with-ggplot2/img.jpg?raw=true") #%>%
  #image_resize("200x300")

library(cowplot)
ggdraw(p) +
  draw_image(bears, x = 0.32, y = 0.2, scale = 0.4)

