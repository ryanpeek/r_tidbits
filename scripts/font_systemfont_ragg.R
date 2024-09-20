# dealing with fonts
library(systemfonts)
library(ragg)
library(glue)
library(ggplot2)
library(hrbrthemes)

# see reasonable colors: https://www.reasonable.work/colors/

# select "AGG" graphics option in RStudio for rendering:
# Tools > Global Options > General > Graphics > Select "AGG" under Backend

# can download and install fonts directly using Google Font:
# download zip, unzip, and install following system req's

# pick a font!
fnt <- "Irish Grover"
# Barlow,  Roboto, Schoolbell, Public Sans, Roboto Mono, Roboto Slab,
# Roboto Condensed, IBM Plex Sans, Titillium Web

# Download or load a font:
library(showtext)
showtext_opts(dpi=300)
showtext_auto(enable=TRUE)

# add from google:
font_add_google(fnt)
# add locally:
# font_add(family = "<family_name>", regular = "/path/to/font/file")
# showtext_auto()


# get some data to play with:
(gg1 <- ggplot() +
    geom_point(
      data = mtcars,
      aes(mpg, wt, color = factor(cyl))
    ) +
    geom_label(
      aes(
        x = 15, y = 5.48,
        label = "<- Wow this point is interesting,\n    a heavy but mildly \n    fuel efficient vehicle."),
      family = fnt,
      label.size = 0, hjust = 0, vjust = 1
    ) +
    labs(
      x = "Fuel efficiency (mpg)", y = "Weight (tons)",
      title = glue("{fnt}: ggplot2 scatterplot example"),
      subtitle = "A plot that is only useful for demonstration purposes",
      caption = glue("Brought to you by {fnt}")
    ) +
    #cowplot::theme_cowplot(font_family = fnt))
    hrbrthemes::theme_ipsum(fnt))

# save with ggplot2
ggsave(gg1, filename = glue::glue("figs/font_tst_{fnt}.pdf"), width = 11, height = 8, device = cairo_pdf, dpi = 300)
ggsave(gg1, filename = glue::glue("figs/font_tst_{fnt}.png"), width = 11, height = 8, dpi = 300, bg = "white")


# save with agg
agg_png(filename=glue("figs/font_tst_{fnt}.png"),
        width = 11, height = 8, units = "in",
        background = "white", res = 300)
gg1
dev.off()
