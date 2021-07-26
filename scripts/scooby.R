
# setup -------------------------------------------------------------------

library(tidyverse)
library(extrafont)


# import data from web -------------------------------------------------------------

# scoobydoo_raw <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-13/scoobydoo.csv',
                          # na = "NULL")

# write_rds(scoobydoo_raw, 'dados/scoobydoo.rds')


# import data locally -----------------------------------------------------

scoobydoo_raw <- read_rds('dados/scoobydoo.rds')

glimpse(scoobydoo_raw)


# wrangling! --------------------------------------------------------------

scooby <- scoobydoo_raw %>% 
  mutate(trap_work_first = as.logical(trap_work_first)) 

glimpse(scooby)

scooby %>% 
  # filter(monster_amount > 0) %>% 
  drop_na(imdb) %>% 
  ggplot() +
  geom_jitter(aes(suspects_amount, imdb, color = date_aired),width = .5, height = .25, size = 1.5, alpha = .75, stroke = 0)


# visualizing! ------------------------------------------------------------


colors <- c("#698B22",
            "#CDB38B",
            "#9B30FF",
            "#FF6347",
            "#68838B",
            "#8EE5EE",
            "#BDBDBD")

scooby_movies <- scooby %>% 
  filter(format %in% c("Movie", "Movie (Theatrical)"))

scooby_movies %>%
  filter(monster_amount > 0) %>%
  separate_rows(monster_type, monster_type, sep = ",") %>%
  mutate(
    monster_type = str_trim(monster_type),
    monster_type = as_factor(monster_type),
    monster_type = fct_lump(monster_type, n = 6, other_level = "Other")
  ) %>%
  count(monster_type) %>%
  mutate(
    monster_type = fct_reorder(monster_type, n),
    monster_type = fct_relevel(monster_type, "Other", after = 0)
  ) %>%
  ggplot(aes(monster_type, n)) +
  geom_col(aes(fill = monster_type),
           width = .85,
           show.legend = FALSE) +
  coord_flip(ylim = c(0, 40),
             expand = FALSE,
             clip = "off") +
  geom_text(
    aes(
      y = if_else(monster_type == "Animal", n + .6, n - .6),
      x = monster_type,
      label = if_else(monster_type == "Animal", "26 total appearances", paste(n)),
      hjust = if_else(monster_type == "Animal", 0, 1)
    ),
    family = "Commissioner",
    size = 8
  ) +
  scale_fill_manual(values = colors[7:1]) +
  labs(
    title = "If it wasn't for those stupid zombies!",
    subtitle = "Monster types in Scooby Doo movies",
    y = "Number of monsters",
    caption = paste(
      "By @brunocbreis for #TidyTuesday | Source: ScoobyPedia | Data from",
      nrow(scooby_movies),
      "movies"
    )
  ) +
  theme_void(base_family = "Commissioner") +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(
      size = 30,
      face = "bold",
      margin = margin(0, 0, 5, 0)
    ),
    plot.subtitle = element_text(size = 13, margin = margin(0, 0, 25, 0)),
    plot.caption.position = "plot",
    plot.caption = element_text(margin = margin(25, 0, 0, 0)),
    plot.margin =  margin(30, 30, 15, 30),
    axis.title.x = element_blank(),
    axis.text.y = element_text(
      hjust = 1,
      margin = margin(0, 10, 0, 0),
      size = 13
    ),
    panel.spacing = margin(10, 10, 10, 10)
  )

ggsave('stupid_zombies_graph.png',
       width = 12,
       height = 9)
