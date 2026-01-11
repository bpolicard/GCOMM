library(tidyverse)
library(scales)
library(lubridate)
library(gtrendsR)
library(ggrepel)
library(here)
options(warnPartialMatchArgs = FALSE)

dir.create(here("11_reproducibility", "data"))
dir.create(here("11_reproducibility", "data", today()))
dir.create(here("11_reproducibility", "output"))
dir.create(here("11_reproducibility", "output", today()))

set.seed(7140)
gtrends(
  keyword = c(URLdecode("%2Fm%2F026jxvv"), URLdecode("%2Fm%2F02pjzvh")),
  geo = "US",
  time = "2014-01-01 2019-04-13"
) %>% 
  .[["interest_over_time"]] %>% 
  as_tibble() %>% 
  mutate(
    search_interest = case_when(hits == "<1" ~ 0, TRUE ~ as.numeric(hits)),
    keyword = case_when(
      keyword == URLdecode("%2Fm%2F026jxvv") ~ 
        "Virginia Cavaliers Men's Basketball",
      keyword == URLdecode("%2Fm%2F02pjzvh") ~ 
        "Villanova Wildcats Men's Basketball"
    ) 
  ) %>% 
  select(month = date, keyword, search_interest) ->
  interest

write_csv(
  x    = interest, 
  path = here(
    "11_reproducibility",
    "data",
    today(),
    "road-to-redemption.csv"
  )
)

interest %>% 
  mutate(
    annotation = case_when(
      month == ymd("2016-04-01") & 
        keyword == "Villanova Wildcats Men's Basketball" ~
        "Villanova wins the 2016 NCAA Men's Basketball Championship",
      month == ymd("2018-03-01") & 
        keyword == "Virginia Cavaliers Men's Basketball" ~
        "16-seed UMBC beats 1-seed UVA",
      month == ymd("2018-04-01") & 
        keyword == "Villanova Wildcats Men's Basketball" ~
        "Villanova wins the 2018 NCAA Men's Basketball Championship",
      month == ymd("2019-04-01") & 
        keyword == "Virginia Cavaliers Men's Basketball" ~
        "UVA wins the 2019 NCAA Men's Basketball Championship"
    )
  ) %>% 
  filter(!is.na(annotation)) ->
  big_event

interest %>% 
  ggplot(aes(x = month, y = search_interest, color = keyword)) +
  # Annotate where Villanova wins the 2016 NCAA Men's Basketball Championship. 
  geom_vline(
    xintercept = as.POSIXct("2016-04-01"), 
    size = 2, 
    color = "white"
  ) +
  # Annotate where 16-seed UMBC beats 1-seed UVA.
  geom_vline(
    xintercept = as.POSIXct("2018-03-01"), 
    size = 2, 
    color = "white"
  ) +
  # Annotate where Villanova wins the 2018 NCAA Men's Basketball Championship.
  geom_vline(
    xintercept = as.POSIXct("2018-04-01"), 
    size = 2, 
    color = "white"
  ) +
  # Annoatate where UVA wins the 2019 NCAA Men's Basketball Championship.
  geom_vline(
    xintercept = as.POSIXct("2019-04-01"), 
    size = 2, 
    color = "white"
  ) +
  # Add lines to the graph to capture each team's monthly search interest.
  geom_line(size = 1.5) +
  # Change the colors of the lines to match the universities' team colors.
  scale_color_manual(values = c("#13B5EA", "#E57200")) +
  # Annotate the graph with the events documented in the big_event tibble.
  ggrepel::geom_label_repel(
    aes(label = annotation), 
    face = "bold",
    size = 7,
    data = big_event,
    label.size = 0,
    segment.color = NA,
    show.legend = FALSE
  ) +
  # Make the x-axis breaks and labels meaningful by focusing on the month of 
  # March.
  scale_x_datetime(
    breaks = seq(as.POSIXct("2014-03-01"), as.POSIXct("2019-03-01"), "year"),
    date_labels = "%B %Y"
  ) +
  # Adjust the size of the text and move the legend to underneath the plot.
  theme(
    plot.title      = element_text(size = 22, face = "bold"), 
    plot.subtitle   = element_text(size = 16),
    axis.title      = element_text(size = 16, face = "bold"),
    axis.text       = element_text(size = 14), 
    legend.title    = element_text(size = 16, face = "bold"),
    legend.text     = element_text(size = 16), 
    plot.caption    = element_text(size = 15), 
    legend.position = "bottom"
  ) +
  # Add a BLUF title and subtitle as well as meaningful labels and a caption.
  labs(
    title    = "The University of Virginia Men's Basketball Team's Road to Redemption",
    subtitle = "If you learn to use adversity right, it will buy you a ticket to a place you couldn't have gone any other way",
    x        = "Month",
    y        = "Google Search Interest Over Time",
    color    = "Topic",
    caption  = "Data Source: Google Trends (https://google.com/trends)."
  )
