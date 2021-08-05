# Load packages and connect to BGT data on SCIPRO Oracle database @JRC ----
source("./Code/1- Load packages and connect database.R")

# additional packages (not previously included)
library(arrow) # read/write fast "feather" binary data on disk
library(treemapify) # plotting tree-maps
library(patchwork) # combine ggplot in a custom grid layout

# Load occupation data ---------------------------------------------------------------

# Titles and groups for SOC Standard Occupation Codes
labels_soc_occupations <- read_rds("Metadata/labels_soc_occupations.rds")

# Copy main ads table (ID, title, employer, occupation, industry) from database to disk ----
if (file.exists("Data/ads_main.feather")){
  ads_main <- read_feather("Data/ads_main.feather", col_select = c("year", "SOC"))
} else {
  bgt_main %>%
    select(JOB_ID, JOB_DATE, JOB_TITLE = CLEAN_JOB_TITLE, EMPLOYER = CANON_EMPLOYER, SOC = UKSOC_CODE, SIC = SIC_CODE) %>%
    collect() %>% # collect() copies the table in local memory
    mutate(
      # encode JOB_DATE as `date` instead of `dttm`
      JOB_DATE = as_date(JOB_DATE),
      year = year(JOB_DATE),
      # encode literal "na" as <NA> (missing value)
      across(where(is.character), ~ na_if(., "na"))
    ) %>%
    write_feather("./Data/ads_main.feather")
}

n_ads <- 60725519 # shortcut to value, to avoid slow computation


# Representativeness: compare ads with NOMIS LFS by occupation ---

# UK employment by SOC from NOMIS LFS census
# https://www.nomisweb.co.uk/datasets/168_1/about
empl_soc_nomis <- read_excel("./Metadata/NOMIS UK employment by SOC.xlsx") %>% 
  # Split SOC code digits from occupation label
  extract(SOC, c("SOC", "occupation_name"), r"{^(\d+)\s(.*)$}") %>%  
  gather(estimate, employment, -SOC, -occupation_name) %>%
  separate(estimate, into = c("year", "estimate"), convert = TRUE) %>% 
  filter(estimate == "number") %>% 
  select(-estimate)

# compute number of ads per SOC per year
empl_soc_bg <- ads_main %>% 
  group_by(year, SOC) %>% 
  summarise(n_ads = n(), .groups = "drop")

# create table employment and ads by SOC 4-digit occupations
# join ads table with Nomis table, excluding 2020 (incomplete)
empl_soc <- empl_soc_nomis %>%
  filter(year <= 2019, str_length(SOC) == 4) %>% 
  left_join(empl_soc_bg, by = c("SOC", "year")) %>% 
  arrange(year, SOC) %>% 
  # Add labels for SOC-1 Occupation major groups
  mutate(SOC_1 = str_sub(SOC, 1, 1)) %>% 
  left_join(labels_soc_occupations %>% select(SOC, soc_group), by = c("SOC_1" = "SOC")) %>% 
  select(year, SOC, occupation_name, occupation_group = soc_group, employment, n_ads)


# Occupation structure tree-map ----

## Employed people in NOMIS ----
plot_empl_nomis <- empl_soc %>%
  filter(year == 2019) %>% 
  mutate(soc_label = paste0(occupation_name, "\n",  scales::comma(employment))) %>% 
  ggplot(aes(area = employment, label = soc_label, fill = occupation_group, subgroup = occupation_group)) +
  geom_treemap(alpha = 0.6, color = "white") +
  geom_treemap_subgroup_text(colour = "white", place = "centre", grow = T, reflow = T, alpha = 0.9, fontface = "bold") +
  geom_treemap_text(colour = "black", place = "topleft", reflow = T) +
  geom_treemap_subgroup_border(colour = "white", alpha = 0.8) +
  # coord_equal() +
  scale_fill_brewer(palette = "Paired") +
  theme(legend.position = "none") +
  labs(
    title = "Structure of UK employment by occupation in 2019, Nomis labour force survey data",
    subtitle = "Employment in occupations (SOC-4 Unit Groups), coloured by Occupation Major Group (SOC 1)"
  )


ggsave(plot_empl_nomis, "./Figures/UK occupation structure.png", scale = 1.3, width = 8, height = 5)
ggsave(plot_empl_nomis, "./Figures/UK occupation structure.pdf", scale = 1.3, width = 8, height = 5)


## Number of ads in BG NOVA UK data ----
plot_empl_bgt <- empl_soc %>%
  filter(year == 2019) %>% 
  ggplot(aes(area = n_ads, label = occupation_name, fill = occupation_group, subgroup = occupation_group)) +
  geom_treemap(alpha = 0.6, color = "white") +
  geom_treemap_subgroup_text(colour = "white", place = "centre", grow = T, reflow = T, alpha = 0.9, fontface = "bold") +
  geom_treemap_text(colour = "black", place = "topleft", reflow = T) +
  geom_treemap_subgroup_border(colour = "white", alpha = 0.8) +
  coord_equal() +
  scale_fill_brewer(palette = "Paired") +
  theme(legend.position = "none") +
  labs(
    title = "Job ads by occupation in 2019, Burning Glass UK data",
    subtitle = "Employment in occupations (SOC-4 Unit Groups), coloured by Occupation Major Group (SOC 1)"
  )

ggsave(plot_empl_bgt, "./Figures/UK occupation ads.png", scale = 1.2, width = 8.5, height = 8.5)
ggsave(plot_empl_bgt, "./Figures/UK occupation ads.pdf", scale = 1.2, width = 8.5, height = 8.5)

## Combined plot ----
plot_empl_nomis +
  labs(
    title = NULL,
    subtitle = "Nomis labour force survey data"
  ) + 
  plot_empl_bgt +
  labs(
    title = NULL,
    subtitle = "Number of job advertisements in BGT Nova UK"
  ) 
ggsave("./Figures/UK occupation structure comparison.png", width = 10, height = 5)

# Plot sampling rates ads-occupations ----

soc_ad_rate <- empl_soc %>% 
  filter(year == 2019) %>% 
  mutate(ad_rate = n_ads / employment) 

median_ad_rate <- soc_ad_rate$ad_rate %>% median()

bind_rows(
  soc_ad_rate %>% slice_max(n = 10, order_by = ad_rate),
  soc_ad_rate %>% slice_min(n = 10, order_by = ad_rate) %>% arrange(desc(ad_rate))
) %>% 
  mutate(
    occupation_name = factor(occupation_name) %>% fct_inorder(),
    occupation_group = factor(occupation_group, levels = unique(empl_soc$occupation_group))
  ) %>%  
  ggplot(aes(x = occupation_name %>% fct_rev(), y = ad_rate, color = occupation_group)) +
  geom_point(aes(size = employment)) + geom_linerange(aes(ymin = median_ad_rate, ymax = ad_rate, x = occupation_name %>% fct_rev())) +
  geom_hline(aes(yintercept = median_ad_rate), linetype = "dashed") +
  coord_flip() + 
  scale_y_log10(breaks = c(0.2, median_ad_rate, 1, 10, 100, 1000), labels = c("1:5", "1:7.6\n(median)", "1:1", "", "100:1", "1,000:1"), minor_breaks = NULL) +
  scale_color_brewer("Occupation Major Groups (SOC-1)", palette = "Paired") +
  scale_size_area("Employment (Nomis 2019)", labels = comma) + 
  labs(
    title = "The representation of occupations in job ads varies wildly",
    subtitle = "Number of employees in Nomis for each ad in Burning Glass: top 10 and bottom 10",
    x = "Occupations (SOC-4)", y = "N Employment : N ads\n(log scale)"
  )
ggsave("Figures/ad rate top bottom.png")

plot_soc_ad_rate <- soc_ad_rate %>%
  ggplot(aes(x = ad_rate, y = occupation_group %>% fct_rev())) +
  geom_point(aes(size = employment, color = occupation_group), alpha = 0.7) +
  geom_vline(aes(xintercept = median_ad_rate), linetype = "dashed") +
  scale_fill_brewer(palette = "Paired") +
  scale_x_log10(breaks = c(1e-3, 1e-2, 1e-1, median_ad_rate, 1), labels = c("1:1,000", "1:100", "", "1:7.6\n(median)", "1:1"), minor_breaks = NULL ) +
  scale_size_area("Employment\n(Nomis 2019)", labels = comma, breaks = c(1e4, 5e4, 1e5, 5e5, 1e6)) +
  scale_color_brewer("Occupation Major Groups (SOC-1)", palette = "Paired", guide = NULL) +
  theme(legend.text.align = 1, plot.title.position = "plot") +
  annotate("text", y = 0.6, x = 1, label = "rel. over-represented", alpha = 0.6) +
  annotate("text", y = 0.6, x = 0.01, label = "rel. under-represented", alpha = 0.6) +
  annotate(geom = "text",  x = 0.358,   y = 8.5, label = "Nurses", hjust = "right", size = 2, fontface = "italic") +
  annotate(geom = "curve", x = 0.358,   y = 8.5, xend = 0.358, yend = 8, curvature = -1.3, arrow = arrow(length = unit(1, "mm")), color = "grey70", alpha = 0.5) +
  annotate(geom = "text",  x = 0.00148, y = 6.5, label = "Finance officers", hjust = "left", size = 2, fontface = "italic") + 
  annotate(geom = "curve", x = 0.00148, y = 6.5, xend = 0.00148, yend = 6, curvature = 1.3, arrow = arrow(length = unit(1, "mm")), color = "grey70", alpha = 0.5) + 
  annotate(geom = "text",  x = 4.57,    y = 3.5, label = "Sales-related\noccupations n.e.c.", hjust = "right", size = 2, fontface = "italic") +
  annotate(geom = "curve", x = 4.57,    y = 3.5, xend = 4.57, yend = 3, curvature = -1.3, arrow = arrow(length = unit(1, "mm")), color = "grey70", alpha = 0.5) +
  annotate(geom = "text",  x = 0.0658,  y = 3.5, label = "Sales and\nretail assistants", hjust = "right", size = 2, fontface = "italic") +
  annotate(geom = "curve", x = 0.0658,  y = 3.5, xend = 0.0658, yend = 3, curvature = -1.3, arrow = arrow(length = unit(1, "mm")), color = "grey70", alpha = 0.5) + 
  annotate(geom = "text",  x = 0.224,   y = 4.5, label = "Care workers\n and home carers", hjust = "left", size = 2, fontface = "italic") +
  annotate(geom = "curve", x = 0.224,   y = 4.5, xend = 0.224, yend = 4, curvature = 1.3, arrow = arrow(length = unit(1, "mm")), color = "grey70", alpha = 0.5) +
  annotate(geom = "text",  x = 0.00331, y = 4.5, label = "Air travel assistants", hjust = "left", size = 2, fontface = "italic") +
  annotate(geom = "curve", x = 0.00331, y = 4.5, xend = 0.00331, yend = 4, curvature = 1.3, arrow = arrow(length = unit(1, "mm")), color = "grey80", alpha = 0.5) + 
  labs(
    title = "Professional occupations are better represented in online job advertisments",
    subtitle = "Number of ads for each employed person, across all SOC-4 occupations in 2019",
    x = "Occupation major groups (SOC-1)",
    y = "N ads : N Employees\n(log scale)"
  )

ggsave(plot_soc_ad_rate + labs(title = NULL), filename = "Figures/ad rate occupations.png", height = 4, width = 8)
ggsave(plot_soc_ad_rate, "Figures/ad rate occupations.pdf", height = 5, width = 8, bg = "transparent", device=cairo_pdf)
ggsave(plot_soc_ad_rate, "Figures/ad rate occupations.svg", height = 5, width = 8, bg = "transparent")


# Plotly ad rates ----

soc_ad_rate %>% 
  mutate(
    soc_label = paste0(occupation_name, "<br>N advertisements: ", comma(n_ads, accuracy = 1.0), "<br>N employed: ", comma(employment, accuracy = 1.0)),
    occupation_group = occupation_group %>% fct_rev()
  ) %>% 
  plot_ly() %>% 
  add_markers(
    type = 'scatter',
    y = ~occupation_group,
    x = ~ad_rate,
    color = ~occupation_group,
    text = ~soc_label,
    size = ~n_ads,
    colors = brewer.pal(9, "Paired") %>% rev(),
    showlegend = F,
    hoverinfo = 'text',
    marker = list(opacity = 0.8, sizemode = 'area')
  ) %>% 
  layout(xaxis = list(type = "log")) %>%  
  layout(
    title = "Number of ads in BGT UK data for every employee in the labour market <br>For each SOC-4 occupation in 2019", 
    yaxis = list(title = "Occupation major groups (SOC-1)"),
    xaxis = list(title = "N ads : N Employees\n(log scale)")
  )


# Job titles frequency ----------------------------------------------------

if (file.exists("Data/titles_soc.feather")) {
  titles_soc <- read_feather("Data/titles_soc.feather")
} else {
  ads_titles <- read_feather("Data/ads_main.feather", col_select = c("JOB_ID", "JOB_TITLE", "SOC"))
  
  # Count how many times a specific combination of JOB_TITLE x SOC occurs,
  # then count how many different SOC the job titles is in 
  titles_soc <- ads_titles %>%
    count(JOB_TITLE, SOC, name = "count") %>%
    add_count(JOB_TITLE, name = "split")
  
  write_feather(titles_soc, "Data/titles_soc.feather")
}

if (file.exists("Data/titles_frequency.feather")){
  titles_frequency <- read_feather("Data/titles_frequency.feather")
} else {
  ## Most common job titles ----
  titles_frequency <- titles_soc %>%
    group_by(JOB_TITLE) %>% 
    summarise(
      count = sum(count),
      SOC_splits = min(split),
      .groups = "drop"
    ) %>% 
    arrange(desc(count)) %>% 
    mutate(share_ads = count / n_ads)
}


titles_frequency %>% 
  slice(1:10) %>% 
  select(-SOC_splits) %>% 
  mutate(
    share_ads = percent(share_ads),
    count = comma(count)
  ) %>% 
  xtable::xtable(include.rownames = F)

# filter job titles including symbols
titles_frequency %>% 
  filter(str_detect(JOB_TITLE, r"{[^a-zA-Z\d\s&\.-]}"))

titles_rank_frequency <- titles_frequency %>% 
  select(count_ads = count, -SOC_splits, -share_ads) %>% 
  count(count_ads, name = "freq") %>% 
  arrange(count_ads) %>% 
  mutate(
    total_freq = count_ads*freq,
    cum_freq = cumsum(total_freq)
  )

titles_rank_frequency %>% 
  ggplot(aes(x = count_ads, y = cum_freq)) + 
  geom_step() + 
  scale_x_log10(
    name = "How many times jobs titles occur\n(logarithmic scale)",
    label = comma_format(accuracy = 1),
    breaks = c(10^c(0:6), 60, 212599),
    sec.axis = sec_axis(
      ~ . / n_ads,
      name = "Relative frequency of individual job titles across all ads",
      breaks = c(0.000000016, 0.0000001, 0.00001, 0.001, 0.00350),
      labels = c("0.0000016%", "0.00001%", "0.001%", "0.1%", "0.35%")
    )
  ) +
  scale_y_continuous(
    name = "Cumulative number of all ads (millions)",
    limits = c(1, NA),
    breaks = c(0, 10347228, 2e7, 3e7, 4e7, 6e7),
    label = comma_format(accuracy = 1, scale = 1e-6),
    sec.axis = sec_axis(
      ~ . / n_ads,
      name = "Cumulative share of all ads",
      breaks = c(0, 0.17, 0.25, 0.5, 0.75, 1),
      label = percent_format(accuracy = 1)
    )
  ) + 
  # annotation_logticks(sides = "b") +
  labs(
    title = "Frequency of job titles",
    subtitle = "Cumulative frequency of job titles, from least to most common"
  ) +
  annotate(geom = "text",  x = 3,   y = 6e6, hjust = "left", size = 3, fontface = "italic", label = "The least common job titles occur only once.\nCollectively, they account for 10m ads, or 17% of the total") +
  annotate(geom = "curve", x = 3,   y = 6e6, xend = 1, yend = 1e7, curvature = -0.4, arrow = arrow(length = unit(2, "mm")), color = "grey50", alpha = 0.5) + 
  annotate(geom = "text",  x = 130, y = 2.7e7, hjust = "left", size = 3, fontface = "italic", label = "50% of all ads (around 30m) contain job titles\nthat appear at most 60 times in total.") +
  annotate(geom = "curve", x = 130, y = 2.7e7, xend = 60, yend = 3e7, curvature = -0.4, arrow = arrow(length = unit(2, "mm")), color = "grey50", alpha = 0.5) +
  annotate(geom = "text",  x = 2e5, y = 4.5e7, hjust = "right", size = 3, fontface = "italic", label = "The most common job title\nappears in 212,599 ads,\nor only in 0.35% of the total") +
  annotate(geom = "curve", x = 2e5, y = 4.55e7, xend = 213e3, yend = 6e7, curvature = 0.2, arrow = arrow(length = unit(2, "mm")), color = "grey50", alpha = 0.5) 

ggsave("Figures/job title frequency.pdf", height = 5, width = 10, bg = "transparent", device = cairo_pdf)

## Split classification: same job title classified in different SOCs! ----

if (file.exists("Data/titles_split.feather")) {
  titles_split <- read_feather(titles_split, "Data/titles_split.feather")
} else {
  titles_split <- titles_soc %>%
    filter(split > 1) %>%
    arrange(desc(split, JOB_TITLE)) %>% 
    left_join(labels_soc_occupations, by = "SOC")
  
  write_feather(titles_split, "Data/titles_split.feather")
}

titles_split_summary <- titles_split %>% 
  group_by(JOB_TITLE) %>% 
  arrange(desc(count)) %>% 
  summarise(
    occurrences = sum(count),
    splits = head(split, 1),
    SOCs = paste0(replace_na(SOC, "none"), collapse = ", "),
    .groups = "drop"
  ) %>% 
  arrange(desc(occurrences))

titles_split_summary %>% 
  mutate(SOCs = str_trunc(SOCs, 31, ellipsis = ", …")) %>% 
  slice(1:100) %>% 
  write_csv("Metadata/split titles soc.csv")
