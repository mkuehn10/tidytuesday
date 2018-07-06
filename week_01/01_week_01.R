library(tidyverse)
library(readxl)
library(scales)
library(magrittr)
library(maps)
library(usmap)
library(geofacet)

tuition_data <- read_xlsx('week_01/us_avg_tuition.xlsx')

tuition_data %>%
  gather(var, value, -State) %>%
  group_by(State) %>%
  summarise(avg_tuition = mean(value))

tuition_data %>%
  gather(var, value, -State) %>%
  group_by(var) %>%
  summarise(avg_tuition = mean(round(value, -1)))

tuition_data %>%
  mutate(
    pct_change = round((`2015-16` - `2010-11`) / (`2010-11`) * 100, 0),
    change_cat2 = case_when(
      pct_change < -0.1 ~ "From -2% to -0.1%",
      between(pct_change, 0, 2) ~ "From 0% to +2%",
      between(pct_change, 3, 7) ~ "From +3% to +7%",
      between(pct_change, 8, 12) ~ "From +8% to 12%",
      between(pct_change, 13, 17) ~ "From +13% to +17%",
      between(pct_change, 18, 22) ~ "From +18% to +22%",
      between(pct_change, 22, 26) ~ "From +22% to +26%",
      pct_change > 26 ~ "More than +26%"
    ),
    change_cat2 = factor(
      change_cat2,
      levels = c(
        "From -2% to -0.1%",
        "From 0% to +2%",
        "From +3% to +7%",
        "From +8% to 12%",
        "From +13% to +17%",
        "From +18% to +22%",
        "From +22% to +26%",
        "More than +26%"
      )
    )
  ) %>%
  select(State, `2015-16`, change_cat2) -> tuition_cats

tuition_cats %>%
  ggplot() +
  geom_bar(stat = 'identity', aes(
    x = reorder(State, `2015-16`),
    y = `2015-16`,
    fill = change_cat2
  )) +
  geom_text(aes(
    x = reorder(State, `2015-16`),
    label = dollar(round(`2015-16`,-1)),
    y = `2015-16`
  ),
  angle = 0,
  hjust = 0) +
  theme(axis.text.x = element_text(
    angle = 90,
    vjust = 0.5,
    hjust = 1
  )) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "#7b91ba",
      "#bcbdbf",
      "#e0c791",
      "#f3c355",
      "#f1ad00",
      "#e06d2c",
      "#d22425",
      "#9f0f19"
    )
  ) +
  theme_light()

plot_usmap(data = tuition_cats %>% rename(state = State), values = "change_cat2") +
  scale_fill_manual(
    values = c(
      "#7b91ba",
      "#bcbdbf",
      "#e0c791",
      "#f3c355",
      "#f1ad00",
      "#e06d2c",
      "#d22425",
      "#9f0f19"
    ),
    na.translate = FALSE
  ) +
  theme(legend.position = "top")


tuition_cats %>%
  mutate(state_rank = row_number(`2015-16`)) %>%
  ggplot() +
  #annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, aes(fill = change_cat2)) +
  geom_rect(xmin = -1, xmax = 2, ymin = -1, ymax = 2, aes(fill = change_cat2)) +
  geom_label(aes(x = 0.25, y = .75, label = dollar(round(`2015-16`,-1)))) +
  geom_label(aes(x = 0.25, y = 0.25, label = state_rank)) +
  facet_geo(~ State) +
  scale_fill_manual(
    values = c(
      "#7b91ba",
      "#bcbdbf",
      "#e0c791",
      "#f3c355",
      "#f1ad00",
      "#e06d2c",
      "#d22425",
      "#9f0f19"
    )
  ) +
  theme_light() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "top") +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1))
