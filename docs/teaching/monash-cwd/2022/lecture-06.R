library(tidyverse)
library(ozmaps)
library(colorspace)
library(grid)
library(gridSVG)
library(vcd)


# functions ---------------------------------------------------------------

read_aust_census_files <- function(files) {
  map_dfc(files, ~{
    read_csv(.x) %>%
      select(-1)
  })
}

read_ancestry <- function(data, year) {
  data %>%
    pivot_longer(everything()) %>%
    filter(str_detect(name, "Tot_[Rr]esp$")) %>%
    select(ancestry = name, count = value) %>%
    mutate(ancestry = str_replace(ancestry, "_Tot_[Rr]esp$", ""),
           ancestry = str_replace(ancestry, "_", " "),
           ancestry = case_when(ancestry=="Aust" ~ "Australia",
                                ancestry=="Aust Abor" ~ "Aboriginal",
                                ancestry=="Ancestry NS" ~ "Not Stated",
                                ancestry=="Ancestry ns" ~ "Not Stated",
                                ancestry=="NZ" ~ "New Zealand",
                                ancestry=="Tot P" ~ "Total",
                                ancestry=="Sth African" ~ "South African",
                                TRUE ~ ancestry)) %>%
    filter(ancestry!="Tot responses") %>%
    arrange(desc(count)) %>%
    mutate(percentage = count / count[1] * 100,
           census = year)
}

read_birthplace <- function(data, year) {
  data %>%
    pivot_longer(everything()) %>%
    filter(str_detect(name, "_(P|Tot)$"),
           str_detect(name, "^P_")) %>%
    select(birth = name, count = value) %>%
    mutate(birth = str_replace(birth, "_(P|Tot)$", ""),
           birth = str_replace(birth, "^P_", ""),
           birth = str_replace_all(birth, "_", " "),
           birth = case_when(birth=="Country birth not stated" ~ "Not Stated",
                             birth=="COB NS" ~ "Not Stated",
                             birth=="UK Channel Islands Isle Man" ~ "United Kingdom",
                             birth=="Born elsewhere" ~ "Other",
                             birth=="Elsewhere" ~ "Other",
                             birth=="USA" ~ "United States America",
                             birth=="China excl SARs Taiwan" ~ "China",
                             birth=="Hong Kong SAR China" ~ "Hong Kong",
                             birth=="Hong Kong SAR Ch" ~ "Hong Kong",
                             birth=="PNG" ~ "Papua New Guinea",
                             birth=="Nthern Ireland" ~ "Northern Ireland",
                             birth=="Frmr Yug Repc Mac FYROM" ~ "Former Yugoslav Republic of Macedonia",
                             birth=="FYROM" ~ "Former Yugoslav Republic of Macedonia",
                             birth=="South Eastern Europe nfd" ~ "South Eastern Europe",
                             birth=="SE Europe nfd" ~ "South Eastern Europe",
                             birth=="Korea Republic South" ~ "Korea South",
                             birth=="Tot" ~ "Total",
                             TRUE ~ birth)) %>%
    arrange(desc(count)) %>%
    mutate(percentage = count / count[1] * 100,
           census = year)
}

read_birthplace_with_gender <- function(data, year) {
  data %>%
    pivot_longer(everything()) %>%
    filter(str_detect(name, "_(P|Tot)$"),
           str_detect(name, "^[MF]_")) %>%
    select(birth = name, count = value) %>%
    mutate(birth = str_replace(birth, "_(P|Tot)$", ""),
           sex = ifelse(str_detect(birth, "^F"), "Female", "Male"),
           birth = str_replace(birth, "^[MF]_", ""),
           birth = str_replace_all(birth, "_", " "),
           birth = case_when(birth=="Country birth not stated" ~ "Not Stated",
                             birth=="COB NS" ~ "Not Stated",
                             birth=="UK Channel Islands Isle Man" ~ "United Kingdom",
                             birth=="Born elsewhere" ~ "Other",
                             birth=="Elsewhere" ~ "Other",
                             birth=="USA" ~ "United States America",
                             birth=="China excl SARs Taiwan" ~ "China",
                             birth=="Hong Kong SAR China" ~ "Hong Kong",
                             birth=="Hong Kong SAR Ch" ~ "Hong Kong",
                             birth=="PNG" ~ "Papua New Guinea",
                             birth=="Nthern Ireland" ~ "Northern Ireland",
                             birth=="Frmr Yug Repc Mac FYROM" ~ "Former Yugoslav Republic of Macedonia",
                             birth=="FYROM" ~ "Former Yugoslav Republic of Macedonia",
                             birth=="South Eastern Europe nfd" ~ "South Eastern Europe",
                             birth=="SE Europe nfd" ~ "South Eastern Europe",
                             birth=="Korea Republic South" ~ "Korea South",
                             birth=="Tot" ~ "Total",
                             TRUE ~ birth)) %>%
    arrange(desc(count)) %>%
    group_by(sex) %>%
    mutate(percentage = count / count[1] * 100,
           census = year) %>%
    ungroup()
}


# ancestry data --------------------------------------------------------------------

ancestry <- list()
ancestry[["2011"]] <- read_aust_census_files(c(here::here("lectures/data/2011Census_B08A_AUST_short.csv"),
                                               here::here("lectures/data/2011Census_B08B_AUST_short.csv"))) %>%
  read_ancestry(2011)
ancestry[["2016"]]  <- read_aust_census_files(here::here("lectures/data/2016Census_G08_AUS.csv")) %>%
  read_ancestry(2016)
ancestry[["2021"]]  <- read_aust_census_files(here::here("lectures/data/2021Census_G08_AUS_AUS.csv")) %>%
  read_ancestry(2021)

write_csv(bind_rows(!!!ancestry), file = here::here("lectures/data/census-ancestry.csv"))


# bop data ----------------------------------------------------------------

bop <- list()

#bop[["2011"]] <- read_aust_census_files(here::here("lectures/data/2011Census_B09_AUST_short.csv")) %>%
#  read_birthplace(2011)
bop[["2016"]] <- read_aust_census_files(map_chr(LETTERS[1:8],
                                               ~here::here(glue::glue("lectures/data/2016Census_G09{.x}_AUS.csv")))) %>%
  read_birthplace(2016)
bop[["2021"]] <- read_aust_census_files(map_chr(LETTERS[1:8],
                                                ~here::here(glue::glue("lectures/data/2021Census_G09{.x}_AUS_AUS.csv")))) %>%
  read_birthplace(2021)

write_csv(bind_rows(!!!bop), file = here::here("lectures/data/census-birthplace.csv"))


bopsex <- read_aust_census_files(map_chr(LETTERS[1:8],
                               ~here::here(glue::glue("lectures/data/2021Census_G09{.x}_AUS_AUS.csv")))) %>%
  read_birthplace_with_gender(2021)

write_csv(bopsex, file = here::here("lectures/data/census-birthplace-by-sex.csv"))


# dummy data --------------------------------------------------------------

n1 <- 50
df1 <- tibble(x = runif(n1, 0, 10),
              y =  3 * x + rnorm(n1, 0, 10),
              z = rnorm(n1, 0, 2),
              g = sample(letters[1:4], size = n1, replace = TRUE))
df2 <- expand_grid(x = LETTERS[1:4], g = letters[1:3]) %>%
  mutate(y = sample(10 * (1:10), size = 12, replace = TRUE),
         x = fct_reorder(x, y, function(x) -sum(x)))
theme1 <- theme_void() +
  theme(plot.background = element_rect(fill = "#FFE2EC", color = NA),
        plot.margin = margin(10, 10, 10, 10),
        plot.title.position = "plot",
        plot.title = element_text(margin = margin(t = -4, b = 10),
                                  size = 9.5, face = "bold")
  )
# as(hex2RGB("#FFE2EC"), "polarLUV")
theme1A <- theme1 + theme(plot.background = element_rect(fill = hex(polarLUV(80, 16.77085, 346.3558)), color = NA))
theme2A <- theme1 + theme(plot.background = element_rect(fill = hcl2hex(66.02879, 31.95793, 80), color = NA))
theme2B <- theme1 + theme(plot.background = element_rect(fill = hcl2hex(66.02879, 31.95793, 88), color = NA))

theme2 <- theme1 + theme(plot.background = element_rect(fill = "#FFEDC8", color = NA))
theme3 <- theme1 + theme(plot.background = element_rect(fill = "#D7FBCD", color = NA))
theme4 <- theme1 + theme(plot.background = element_rect(fill = "#B6FEF5", color = NA))
theme5 <- theme1 + theme(plot.background = element_rect(fill = "#D5F4FF", color = NA))
theme6 <- theme1 + theme(plot.background = element_rect(fill = "#FFE5FF", color = NA))
theme7 <- theme1 + theme(plot.background = element_rect(fill = "#FFE2EC", color = NA))

w <- h <- 1.8
h1 <- 1.8
w1 <- 2.3

# generate 5 and discard the tails which are too white
reds <- sequential_hcl(n = 5, palette = "Reds 2")
yellows <- c("#6A3F00", "#97742F", "#BAA588", "#D4CCC3")
cols <- c("#BBDEB1", "#99E2D8", "#B8D8F8", "#EDC8F5", "#FFC5D0")

grid_setup <- function(label) {
  grid.newpage()
  grid.rect(gp = gpar(lty = "solid", lwd = 3, fill = "white"),
            width = unit(0.9, "npc"),
            height = unit(0.9, "npc"))
  grid.text(label, x = 0.1, y = 0.9,
            gp = gpar(fontsize = 13), just = c("left", "top"))
  pushViewport(plotViewport(c(2, 3, 4, 3)))
}

# vis-barplot-horizontal
g <- df2 %>%
  group_by(x) %>%
  summarise(y = sum(y)) %>%
  ggplot(aes(x, y)) +
  geom_col(fill = reds[1]) +
  theme1A +
  theme(axis.line.y = element_line(color = "black", size = 1),
        axis.ticks.y = element_line(color = "black", linetype = "solid", size = 1, lineend = NULL),
        axis.ticks.length.y = unit(0.3, "lines")) +
  ggtitle("BARPLOT\n(HORIZONTAL)")
ggsave(here::here("lectures/images/lecture-06/vis-barplot-horizontal.svg"), g, width = w, height = h)

# vis-barplot-vertical
g <- df2 %>%
  group_by(x) %>%
  summarise(y = sum(y)) %>%
  ggplot(aes(y, x)) +
  geom_col(fill = reds[1]) +
  theme1A +
  theme(axis.line.x = element_line(color = "black", size = 1),
        axis.ticks.x = element_line(color = "black", linetype = "solid", size = 1, lineend = NULL),
        axis.ticks.length.x = unit(0.3, "lines")) +
  ggtitle("BARPLOT\n(VERTICAL)")
ggsave(here::here("lectures/images/lecture-06/vis-barplot-vertical.svg"), g, width = w, height = h)


# vis-piechart
g <- df2 %>%
  group_by(x) %>%
  summarise(y = sum(y)) %>%
  ggplot(aes("", y, fill = x)) +
  geom_col(position = "fill") +
  theme1A +
  guides(fill = FALSE) +
  ggtitle("PIE CHART") +
  coord_polar("y") +
  scale_fill_manual(values = reds[1:4]) +
  theme(plot.margin = margin(t=20, b=20, l=17, r=17),
        plot.title = element_text(margin = margin(t = -13, l = -20)))
ggsave(here::here("lectures/images/lecture-06/vis-piechart.svg"), g, width = w, height = h)

# vis-stacked-barplot
g <- ggplot(df2, aes(x, y, fill = g)) +
  geom_col() +
  theme1 +
  guides(fill = "none") +
  theme(axis.line.y = element_line(color = "black", size = 1),
        axis.ticks.y = element_line(color = "black", linetype = "solid", size = 1, lineend = NULL),
        axis.ticks.length.y = unit(0.3, "lines")) +
  ggtitle("STACKED BARPLOT") +
  scale_fill_manual(values = reds[1:4])
ggsave(here::here("lectures/images/lecture-06/vis-stacked-barplot.svg"), g, width = w, height = h)


# vis-grouped-barplot
g <- ggplot(df2, aes(x, y, fill = g)) +
  geom_col(position = "dodge") +
  theme1 +
  guides(fill = "none") +
  theme(axis.line.y = element_line(color = "black", size = 1),
        axis.ticks.y = element_line(color = "black", linetype = "solid", size = 1, lineend = NULL),
        axis.ticks.length.y = unit(0.3, "lines")) +
  ggtitle("GROUPED BARPLOT") +
  scale_fill_manual(values = reds[1:4])
ggsave(here::here("lectures/images/lecture-06/vis-grouped-barplot.svg"), g, width = w, height = h)


# vis-stacked-percentage-barplot
g <- ggplot(df2, aes(x, y, fill = g)) +
  geom_col(position = "fill") +
  theme1 +
  guides(fill = "none") +
  theme(axis.line.y = element_line(color = "black", size = 1),
        axis.ticks.y = element_line(color = "black", linetype = "solid", size = 1, lineend = NULL),
        axis.ticks.length.y = unit(0.3, "lines")) +
  ggtitle("STACKED PERCENTAGE\nBARPLOT") +
  scale_fill_manual(values = reds[1:4])
ggsave(here::here("lectures/images/lecture-06/vis-stacked-percentage-barplot.svg"), g, width = w, height = h)

# vis-histogram
g <- ggplot(faithful, aes(eruptions)) +
  geom_histogram(fill = "#006400", bins = 10, color = "white") +
  theme3 +
  ggtitle("HISTOGRAM") +
  theme(axis.line = element_line(color = "black", size = 1),
        axis.ticks = element_line(color = "black", linetype = "solid", size = 1, lineend = NULL),
        axis.ticks.length = unit(0.3, "lines"))
ggsave(here::here("lectures/images/lecture-06/vis-histogram.svg"), g, width = w, height = h)

# vis-density
g <- ggplot(faithful, aes(eruptions)) +
  geom_density(fill = "#006400", color = NA) +
  theme3 +
  ggtitle("DENSITY PLOT\n(FILLED)") +
  theme(axis.line = element_line(color = "black", size = 1),
        axis.ticks = element_line(color = "black", linetype = "solid", size = 1, lineend = NULL),
        axis.ticks.length = unit(0.3, "lines"))
ggsave(here::here("lectures/images/lecture-06/vis-density.svg"), g, width = w, height = h)

# vis-boxplot
g <- ggplot(faithful, aes(eruptions, "")) +
  geom_boxplot(color = "#006400", width = 0.3) +
  theme3 +
  ggtitle("BOXPLOT") +
  theme(axis.line.x = element_line(color = "black", size = 1),
        axis.ticks.x = element_line(color = "black", linetype = "solid", size = 1, lineend = NULL),
        axis.ticks.length.x = unit(0.3, "lines"))
ggsave(here::here("lectures/images/lecture-06/vis-boxplot.svg"), g, width = w, height = h)

# vis-violin
g <- ggplot(faithful, aes(eruptions, "")) +
  geom_violin(fill = "#006400", color = "#006400") +
  theme3 +
  ggtitle("VIOLIN PLOT") +
  theme(axis.line.x = element_line(color = "black", size = 1),
        axis.ticks.x = element_line(color = "black", linetype = "solid", size = 1, lineend = NULL),
        axis.ticks.length.x = unit(0.3, "lines"))
ggsave(here::here("lectures/images/lecture-06/vis-violin.svg"), g, width = w, height = h)


# vis-dot
g <- ggplot(faithful, aes(eruptions)) +
  geom_dotplot(color = "#006400") +
  theme3 +
  ggtitle("DOT PLOT") +
  theme(axis.line.x = element_line(color = "black", size = 1),
        axis.ticks.x = element_line(color = "black", linetype = "solid", size = 1, lineend = NULL),
        axis.ticks.length.x = unit(0.3, "lines"))
ggsave(here::here("lectures/images/lecture-06/vis-dot.svg"), g, width = w, height = h)


# vis-beeswarm
g <- ggplot(faithful, aes("", eruptions)) +
  ggforce::geom_sina(color =  "#006400", alpha = 0.5) +
  coord_flip() +
  theme3 +
  ggtitle("SINA PLOT") +
  theme(axis.line.x = element_line(color = "black", size = 1),
        axis.ticks.x = element_line(color = "black", linetype = "solid", size = 1, lineend = NULL),
        axis.ticks.length.x = unit(0.3, "lines"))
ggsave(here::here("lectures/images/lecture-06/vis-beeswarm.svg"), g, width = w, height = h)

# vis-scatter
df_scatter <- diamonds %>%
  sample_n(80)
g <- ggplot(df_scatter, aes(carat, price)) +
  geom_point(color = yellows[1]) +
  theme2A +
  ggtitle("SCATTER PLOT") +
  theme(axis.line = element_line(color = "black", size = 1),
        axis.ticks = element_line(color = "black", linetype = "solid", size = 1, lineend = NULL),
        axis.ticks.length = unit(0.3, "lines"))
ggsave(here::here("lectures/images/lecture-06/vis-scatter.svg"), g, width = w, height = h)

# vis-bubble
g <- ggplot(df_scatter, aes(carat, price, size = depth)) +
  geom_point(color = yellows[1], alpha = 0.3) +
  theme2 +
  ggtitle("BUBBLE CHART") +
  theme(axis.line = element_line(color = "black", size = 1),
        axis.ticks = element_line(color = "black", linetype = "solid", size = 1, lineend = NULL),
        axis.ticks.length = unit(0.3, "lines")) +
  guides(size = "none")
ggsave(here::here("lectures/images/lecture-06/vis-bubble.svg"), g, width = w, height = h)


# vis-hex
g <- ggplot(diamonds, aes(carat, price)) +
  geom_hex(bins = 8) +
  theme2B +
  ggtitle("HEX PLOT") +
  theme(axis.line = element_line(color = "black", size = 1),
        axis.ticks = element_line(color = "black", linetype = "solid", size = 1, lineend = NULL),
        axis.ticks.length = unit(0.3, "lines")) +
  scale_fill_continuous_sequential(h1 = 42, c1 = 45, c2 = 45, l1 = 30, l2 = 60) +
  guides(fill = "none")
ggsave(here::here("lectures/images/lecture-06/vis-hex.svg"), g, width = w, height = h)


# vis-line
g <- tsibble::as_tsibble(nhtemp) %>%
  ggplot(aes(index, value)) +
  geom_line(color = "#151B8D") +
  theme5 +
  ggtitle("LINE PLOT") +
  theme(axis.line = element_line(color = "black", size = 1),
        axis.ticks = element_line(color = "black", linetype = "solid", size = 1, lineend = NULL),
        axis.ticks.length = unit(0.3, "lines"))
ggsave(here::here("lectures/images/lecture-06/vis-line.svg"), g, width = w, height = h)


# vis-heatmap
g <- ToothGrowth %>%
  mutate(dosef = factor(dose)) %>%
  group_by(dosef, supp) %>%
  summarise(mlen = mean(len)) %>%
  ggplot(aes(dosef, supp, fill = mlen)) +
  geom_tile(color = "black", size = 1.2) +
  theme6 +
  ggtitle("HEATMAP") +
  guides(fill = "none") +
  scale_fill_continuous_sequential(palette = "Purples 3")
ggsave(here::here("lectures/images/lecture-06/vis-heatmap.svg"), g, width = w, height = h)


# vis-map
library(ozmaps)
oz_sf <- ozmap_data("states")
g <- oz_sf %>%
  mutate(value = rnorm(n())) %>%
  ggplot(aes(fill = value)) +
  geom_sf() +
  theme7 +
  ggtitle("CHOROPLETH MAP") +
  guides(fill = "none") +
  theme(plot.margin = margin(t=20, b=20, l=7, r=7),
        plot.title = element_text(margin = margin(t = -10, b = 10))) +
  scale_fill_continuous_sequential(palette = "Reds 3")
ggsave(here::here("lectures/images/lecture-06/vis-map.svg"), g, width = w, height = h)


# vis-volchart
grid.newpage()
grid.rect(gp = gpar(lwd = 0, fill = "#FFE2EC"),
          width = unit(2, "npc"),
          height = unit(2, "npc"))
grid.text("VOLUME CHART", x = 0.1, y = 0.9,
          gp = gpar(fontsize = 10, fontface = "bold"), just = c("left", "top"))
pushViewport(plotViewport(c(2, 3, 4, 3)))
# stick 1
grid.polyline(x = c(0, 0, 0.3, 0.3, 0),
              y = c(0, 0.3, 0.3, 0, 0),
              gp=gpar(lwd = 3))
grid.polyline(x = c(0.3, 0.4, 0.4, 0.1, 0),
              y = c(0, 0.1, 0.4, 0.4, 0.3),
              gp=gpar(lwd = 3))
grid.polyline(x = c(0.3, 0.4), y = c(0.3, 0.4),
              gp=gpar(lwd = 3))
# stick 2
grid.polyline(x = c(0.7, 0.7, 1, 1, 0.7),
              y = c(0, 0.7, 0.7, 0, 0),
              gp=gpar(lwd = 3))
grid.polyline(x = c(1, 1.3, 1.3, 1, 0.7),
              y = c(0, 0.4, 1.1, 1.1, 0.7),
              gp=gpar(lwd = 3))
grid.polyline(x = c(1, 1.3), y = c(0.7, 1.1),
              gp=gpar(lwd = 3))
# dimension not correct
grid.export(here::here("lectures/images/lecture-06/vis-volchart.svg"), res = 72)



# task-position-common-scale ----------------------------------------------
grid_setup("POSITION\nCOMMON SCALE")
pushViewport(dataViewport(c(0, 1), c(0, 2)))
grid.yaxis(at = 0:2)
grid.points(c(0.2, 0.2), c(1.2, 0.3), pch = 19)


# task-position-non-aligned-scale -----------------------------------------
grid_setup("POSITION\nNON-ALIGNED SCALE")
pushViewport(viewport(layout = grid.layout(1, 2, widths = unit(0.5, "npc"),
                                           heights = unit(1, "npc")), name = "row1-col2"),
             viewport(layout.pos.col = 1, layout.pos.row = 1),
             dataViewport(c(0, 1), c(0, 2), extension = 0))
grid.yaxis(at = 0:2)
grid.rect(y = 0, x = 0.2, just = "bottom", gp = gpar(fill = "black"),
          width = unit(2, "mm"),
          height = 0.5)
seekViewport("row1-col2")
pushViewport(viewport(layout.pos.col = 2, layout.pos.row = 1),
             plotViewport(c(0, 1, 0, 0)),
             dataViewport(c(0, 1), c(0, 5), extension = 0))
grid.yaxis(at = 0:5)
grid.rect(y = 0, x = 0.25, just = "bottom", gp = gpar(fill = "black"),
          width = unit(2, "mm"),
          height = 0.5)

# task-length
grid_setup("LENGTH")
grid.lines(x = c(0.5, 0.5), y = c(0, 0.8), gp = gpar(lwd = 3))
grid.lines(x = c(0.3, 0.3), y = c(0, 0.4), gp = gpar(lwd = 3))

# task-direction
grid_setup("DIRECTION")
grid.lines(x = c(0.5, 0.6), y = c(0.2, 0.8), gp = gpar(lwd = 3),
           arrow = arrow())
grid.lines(x = c(0.3, 0), y = c(0.2, 0.7), gp = gpar(lwd = 3),
           arrow = arrow())

# task-angle
grid_setup("ANGLE")
grid.lines(x = c(0.8, 1), y = c(0.2, 0.8), gp = gpar(lwd = 3))
grid.lines(x = c(0.8, 0.6), y = c(0.2, 0.8), gp = gpar(lwd = 3))
grid.lines(x = c(0.3, 0.4), y = c(0.2, 0.8), gp = gpar(lwd = 3))
grid.lines(x = c(0.3, 0.3), y = c(0.2, 0.8), gp = gpar(lwd = 3))

# task-area
grid_setup("AREA")
grid.circle(gp = gpar(fill = "black"))
grid.circle(gp = gpar(fill = "black"), x = 0, y = 0.9, r = 0.1)

# task-volume
grid_setup("VOLUME")
# stick 1
grid.polyline(x = c(0, 0, 0.3, 0.3, 0),
              y = c(0, 0.3, 0.3, 0, 0),
              gp=gpar(lwd = 3))
grid.polyline(x = c(0.3, 0.4, 0.4, 0.1, 0),
              y = c(0, 0.1, 0.4, 0.4, 0.3),
              gp=gpar(lwd = 3))
grid.polyline(x = c(0.3, 0.4), y = c(0.3, 0.4),
              gp=gpar(lwd = 3))
# stick 2
grid.polyline(x = c(0.5, 0.5, 0.8, 0.8, 0.5),
              y = c(0.1, 0.8, 0.8, 0.1, 0.1),
              gp=gpar(lwd = 3))
grid.polyline(x = c(0.8, 0.9, 0.9, 0.6, 0.5),
              y = c(0.1, 0.2, 0.9, 0.9, 0.8),
              gp=gpar(lwd = 3))
grid.polyline(x = c(0.8, 0.9), y = c(0.8, 0.9),
              gp=gpar(lwd = 3))

# task-curvature
grid_setup("CURVATURE")
grid.bezier(c(0.5, 0.5, 0.8, 0.8), c(0.4, 0.8, 0.8, 0.4),
            gp = gpar(lwd = 3))
grid.bezier(c(0.2, 0.2, 0.4, 0.4), c(0.2, 0.4, 0.4, 0.2),
            gp = gpar(lwd = 3))

# task-texture
grid_setup("TEXTURE")
grid.rect(0.1, 0.3, name = "rect1", width = 0.5, height = 0.4,
          gp = gpar(lwd = 3))
grid.rect(0.5, 0.95, name = "rect2", width = 0.3, height = 0.4,
          gp = gpar(lwd = 3))
grid.rect(0.95, 0.3, name = "rect3", width = 0.3, height = 0.5,
          gp = gpar(lwd = 3))
pat1 <- pattern(linesGrob(gp=gpar(lwd=5)),
                width = unit(0.04, "npc"), height = unit(0.04, "npc"),
                dev.width = 0.5, dev.height = 0.4)
pat2 <- pattern(linesGrob(c(0, 3), c(0, 3), gp=gpar(lwd=5)),
                width = unit(0.1, "npc"), height = unit(0.1, "npc"),
                dev.width = 0.3, dev.height = 0.4)
pat3 <- pattern(circleGrob(r = 0.3, gp=gpar(fill="black")),
                dev.width = 0.05, dev.height = 0.08)
grid.patternFill("rect1", pattern = pat1)
grid.patternFill("rect2", pattern = pat2)
grid.patternFill("rect3", pattern = pat3)
# pattern only shows up if below is done
grid.export(here::here("lectures/images/lecture-06/task-texture.svg"))


# task-shape
grid_setup("SHAPE")
grid.points(x = rep((1:4)/4.5, each = 4), y = rep((1:4)/4.5, times = 4), pch = c(1:16),
            size = unit(0.75, "char"))

# task-color-hue
grid_setup("COLOR HUE")
grid.points(x = rep((1:4)/4.5, each = 4), y = rep((1:4)/4.5, times = 4),
            size = unit(1, "char"), pch = 16,
            gp = gpar(col = hcl2hex(seq(0, 360, length.out = 16), 100, 60)))

# task-color-chroma
grid_setup("COLOR CHROMA")
grid.points(x = rep((1:4)/4.5, each = 4), y = rep((1:4)/4.5, times = 4),
            size = unit(1, "char"), pch = 16,
            gp = gpar(col = hcl2hex(0, seq(0, 360, length.out = 16), 60)))

# task-color-luminance
grid_setup("COLOR LUMINANCE")
grid.points(x = rep((1:4)/4.5, each = 4), y = rep((1:4)/4.5, times = 4),
            size = unit(1, "char"), pch = 16,
            gp = gpar(col = hcl2hex(0, 35, seq(0, 100, length.out = 16))))

# task-color-saturation
grid_setup("COLOR SATURATION")
grid.points(x = rep((1:4)/4.5, each = 4), y = rep((1:4)/4.5, times = 4),
            size = unit(1, "char"), pch = 16,
            gp = gpar(col = hex(HSV(0, seq(0, 1, length.out = 16), 0.6))))

# task-color-shade
grid_setup("COLOR SHADE")
grid.points(x = rep((1:4)/4.5, each = 4), y = rep((1:4)/4.5, times = 4),
            size = unit(1, "char"), pch = 16,
            gp = gpar(col = hex(HSV(0, 0.6, seq(0, 1, length.out = 16)))))


# preattentive ------------------------------------------------------------

n <- 30
df2 <- tibble(x = runif(n), y = runif(n),
              type = sample(c(TRUE, FALSE),
                            replace = TRUE, size = n,
                            prob = c(0.1, 0.9)))
ggplot(df2, aes(x, y)) +
  geom_point(aes(shape = type),
             color = "#006DAE", size = 5) +
  theme_docs + labs(x = "", y = "") +
  guides(shape = FALSE) +
  theme(axis.text = element_blank())

ggplot(df2, aes(x, y)) +
  geom_text(aes(angle = ifelse(type, 0, 90), label = "T"),
            color = "#746FB2", size = 5, fontface = "bold") +
  theme_docs + labs(x = "", y = "") +
  guides(angle = FALSE) +
  theme(axis.text = element_blank())

ggplot(df2, aes(x, y)) +
  geom_point(aes(color = type), size = 5) +
  theme_docs + labs(x = "", y = "") +
  guides(color = FALSE) +
  scale_color_manual(values = c("black", "#C8008F")) +
  theme(axis.text = element_blank())



# gesalt-proximity --------------------------------------------------------

df3 <- as.data.frame(HairEyeColor) %>%
  mutate(Hair = fct_reorder(Hair, Freq, function(x) mean(x, na.rm = TRUE)))
g1 <- ggplot(df3, aes(Hair, Freq, fill = Sex)) +
  geom_col(position = "dodge") +
  scale_fill_discrete_qualitative()  +
  theme(plot.title.position = "plot") +
  labs(y = "Frequency")
g2 <- ggplot(df3, aes(Sex, Freq, fill = Hair)) +
  geom_col(position = "dodge") +
  scale_fill_discrete_qualitative() +
  theme(plot.title.position = "plot")+
  labs(y = "Frequency")
g1 + g2 + plot_annotation(title = "Survey of Hair Color and Gender of Statistics Students")



# proximity-labels --------------------------------------------------------

dfp <- penguins %>%
  group_by(year, species) %>%
  summarise(mean_bill_length = mean(bill_length_mm, na.rm = T)) %>%
  ungroup()
g1 <- ggplot(dfp, aes(as.factor(year), mean_bill_length, color = species, group = species)) +
  geom_line(size = 1.3) +
  scale_color_manual(values = c("darkorange", "purple", "cyan4")) + labs(x = "Year", y = "Mean bill length (mm)", color = "Species")
g2 <- dfp %>%
  ggplot(aes(as.factor(year), mean_bill_length, color = species, group = species)) +
  geom_line(size = 1.3) +
  scale_color_manual(breaks = c("Chinstrap", "Gentoo", "Adelie"),
                     values = c("purple", "cyan4", "darkorange")) + labs(x = "Year", y = "Mean bill length (mm)", color = "Species")
g3 <- ggplot(dfp, aes(year, mean_bill_length, color = species, group = species)) +
  geom_line(size = 1.3) +
  scale_color_manual(values = c("darkorange", "purple", "cyan4")) + labs(x = "Year", y = "Mean bill length (mm)") +
  guides(color = FALSE) +
  geom_text_repel(data = filter(dfp, year == "2009"),
                  aes(color = species, label = species),
                  nudge_x = 1, xlim = c(2009.1, Inf)) +
  scale_x_continuous(breaks = c(2007:2009),limits = c(2006, 2010.7))
g1 + g2 + g3 +  plot_annotation(title = "Palmer penguins")





