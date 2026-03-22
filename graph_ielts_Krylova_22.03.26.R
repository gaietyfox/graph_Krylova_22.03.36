# Радиальный график токенов
# по проекту "Corpus IELTS - Writing Task 1"

# Данная "композиция" вдохновлена wordcloud, материалами курса и 
# постом Drew Steen с примерами графиков

# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("readr")
# install.packages("tidyr")
# install.packages("stringr")


library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)


corpus <- read_csv("corpus_parsed_clean.csv", show_col_types = FALSE", show_col_types = FALSE)


# Преобразуем леммы в длинный формат, где
# одна строка = один токен

tokens_clean <- corpus %>%
  select(chart_type, lemmas_no_stop) %>%
  mutate(lemmas_no_stop = gsub("\\[|\\]|'", "", lemmas_no_stop)) %>%
  separate_rows(lemmas_no_stop, sep = ",\\s*") %>%
  rename(token = lemmas_no_stop) %>%
  filter(!is.na(token), token != "")


# Выберем 3 самых частотных типа chart_type

top_types <- tokens_clean %>%
  count(chart_type, sort = TRUE, name = "type_total") %>%
  slice_head(n = 3) %>%
  mutate(
    type_rank = row_number(),
    plot_colour = c("red", "green", "blue")
  )

# Выбираем три основных типа графиков,
# считаем частоту токенов внутри каждого из них
# и оставляем по 15 самых частотных слов на каждый тип
top_words <- tokens_clean %>%
  filter(chart_type %in% top_types$chart_type) %>%
  count(chart_type, token, sort = TRUE, name = "freq") %>%
  group_by(chart_type) %>%
  slice_head(n = 15) %>%
  ungroup() %>%
  left_join(top_types, by = "chart_type")


# Закрепляем случайное расположение слов
# (Иначе при каждом запуске будут разные значения)
set.seed(31)


# Задаём базовый радиус для каждого из трёх типов,
# чтобы они располагались на разных кольцах

top_words <- top_words %>%
  mutate(
    ring = case_when(
      type_rank == 1 ~ 1.4,
      type_rank == 2 ~ 2.6,
      type_rank == 3 ~ 3.3
    )
  )

# Распределяем слова по кругу внутри каждого типа графика
# Для этого задаём угол, радиус, поворот текста и выравнивание
# Дополнительная случайность нужна, чтобы график выглядел хаотично и менее читаемо

# Каждому слову назначается место на круге (theta),
# задаётся расстояние от центра (radius),
# слово случайно поворачивается (angle_text),
# а также слово случайно сдвигается по выравниванию (hjust_text)

top_words <- top_words %>%
  group_by(chart_type) %>%
  mutate(
    id = row_number(),
    theta = seq(0, 2 * pi, length.out = n() + 1)[1:n()],
    theta = theta + runif(n(), -0.20, 0.20),
    radius = ring + runif(n(), -0.35, 0.35),
    angle_text = (theta * 180 / pi) +
      sample(c(-120, -90, -60, -30, 0, 30, 60, 90, 120, 180), n(), replace = TRUE),
    hjust_text = sample(c(0, 0.5, 1), n(), replace = TRUE)
  ) %>%
  ungroup()


# Создаём координаты для лучей, идущих от центра к словам
segments_df <- top_words %>%
  mutate(
    x = theta,
    y = radius,
    x0 = theta,
    y0 = 0
  )

# Создадим лишние фоновые кольца
# - на каких расстояниях от центра должны быть кольца
rings_df <- data.frame(y = seq(0.6, 4.2, by = 0.35))


# Строим график
p <- ggplot(top_words, aes(x = theta, y = radius, colour = chart_type)) +
  
  # Лишние круги на фоне
  geom_hline(
    data = rings_df,
    aes(yintercept = y),
    inherit.aes = FALSE,
    colour = "yellow",
    linewidth = 0.5,
    alpha = 0.55
  ) +
  
  # Лучи к словам
  geom_segment(
    data = segments_df,
    aes(x = x0, y = y0, xend = x, yend = y),
    linewidth = 0.6,
    alpha = 0.45,
    show.legend = TRUE
  ) +
  
  # Добавим полупрозрачные точки в позициях слов,
  # чтобы сделать график ещё более трудным в восприятии
  geom_point(
    aes(size = freq),
    alpha = 0.3,
    show.legend = FALSE
  ) +
  
  # Слова - сделаем их жирным шрифтом
  geom_text(
    aes(
      label = token,
      size = freq,
      angle = angle_text,
      hjust = hjust_text
    ),
    fontface = "bold",
    alpha = 0.95,
    show.legend = FALSE
  ) +
  
  # Полярные координаты
  coord_polar(theta = "x") +
  
  # Цвета по типам
  scale_colour_manual(
    values = setNames(top_types$plot_colour, top_types$chart_type)
  ) +
  
  # Размер слов
  scale_size(range = c(3.5, 11)) +
  
  # Показываем легенду по топ-3 по частотности типам графиков в корпусе
  guides(
    size = "none",
    colour = guide_legend(title = "Топ-3 chart_types")
  ) +
  
  # Подписи 
  labs(
    title = "Радиальный график с лучами из центра",
    subtitle = "(Чем слово больше, тем чаще оно встречается)",
    caption = "**График также вдохновлён концептом Word Cloud"
  ) +
  
  # Оформление - выберем цвета, сложно воспринимающиеся глазу
  theme_void() +
  theme(
    plot.background = element_rect(fill = "blue3", colour = NA),
    plot.margin = margin(5, 5, 5, 5),
    panel.background = element_rect(fill = "brown", colour = NA),
    legend.background = element_rect(fill = "blue3", colour = NA),
    legend.key = element_rect(fill = "blue3", colour = NA),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 12, colour = "brown"),
    legend.text = element_text(size = 11, colour = "brown"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, colour = "brown", margin = margin(b = 15)),
    plot.subtitle = element_text(size = 12, hjust = 0.5, colour = "brown", margin = margin(b = 15)),
    plot.caption = element_text(size = 10, face = "italic", hjust = 1, colour = "orange")
  )



# Сохраняем итоговую версию
ggsave(
  filename = "Krylova_22.03.26.png",
  plot = p,
  width = 11,
  height = 11,
  dpi = 300
)