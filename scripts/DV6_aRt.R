# Artful distributions

# making aRt because its a friday
library(usethis)
pr_init(branch = "DV6_aRt")
pr_push()

### Call source script----

source(here::here(path = "scripts/0_data_import.R"))

### Packages ----

library(ggstream)

### aRt ----

ggplot(traits%>%
         unite(
           plot,
           c(site, treatment),
           sep = " ", remove = FALSE
         )) +
  geom_stream(aes(
    x = plot_id,
    y = value,
    fill = plot
  ),
  geom = 'polygon') +
  coord_polar(theta = "y",
              clip = "off",
              start = 0,
              direction = -1)  +
  scale_fill_manual(name = "Plot",
                    values = colours_site$c,
                    breaks = colours_site$t) +
  theme_void() +
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = colorspace::darken("#7E605E", 0.4),
                                        colour = colorspace::darken("#7E605E", 0.4)),
        plot.background = element_rect(fill = colorspace::darken("#7E605E", 0.4),
                                       colour = colorspace::darken("#7E605E", 0.4)),
        legend.background = element_rect(fill = colorspace::darken("#7E605E", 0.4),
                                         colour = colorspace::darken("#7E605E", 0.4)),
        text = element_text(colour = "grey96"),
        strip.background = element_rect(fill = colorspace::darken("#7E605E", 0.6),
                                        colour = NA),
        strip.text = element_text(colour = "grey96")) +
  labs(title = "Its a circle")

ggsave(here(path = "output/aRt.png"),
       height = 15.5, width = 15,
       units = "in", dpi = 300)  
