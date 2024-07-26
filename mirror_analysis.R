library(here)
library(readr)
library(tidyverse)
library(glue)
library(ggpubr)
library(e1071)
library(npmlreg)
library(AnthroTools)

get_lr <- function(get_what, with_what) {
  the_lr <- tryCatch(
    {
      round(with_what[[get_what]], 2)
    },
    error = function(cond) { return(NA) }
  )
  return(the_lr)
}

get_count <- function(get_what) {
  the_count <- tryCatch(
    {
      get_what
    },
    error = function(cond) { return(NA) },
    warning = function(cond) {return(NA) }
  )
  return(the_count)
}

character_frame <- function(character_name) {
  lower_name <- tolower(character_name)
  col_name <- glue("{lower_name}_")
  pattern_name <- glue("[{col_name}]")
  the_frame <- survey_frame |>
    select(starts_with(col_name)) |>
    mutate(name = character_name) |>
    mutate(id = row_number()) |>
    rename_with(~ str_remove(., col_name)) |>
    separate_rows(pronouns, sep = ",")

  the_frame$locale <- recode(the_frame$locale, "City" = "city", "Suburb" = "suburb", "Town" = "town", "Rural Area" = "rural")
  the_frame$pronouns <- recode(the_frame$pronouns, "He/him" = "he", "She/her" = "she", "They/them" = "they")

  re_frame <- the_frame |>
    select(-c(pronouns, ses, locale)) |>
    unique() |>
    pivot_longer(!c(id, name), names_to = "aspect", values_to = "value") |>
    filter(aspect != c("locale", "ses", "pronouns")) |>
    mutate(aspect = as.factor(aspect)) |>
    mutate(value = as.numeric(value))

  re_lhr <- allvc(
    value ~ aspect,
    random = ~ 1 | id,
    data = re_frame,
    random.distribution = "np",
    k = 6,
    verbose = FALSE,
    plot.opt = 0
  )

  re_lhr <- re_lhr$coefficients
  
  rev_frame <- data.frame(name = character_name) |>
    mutate(rev_aina = get_lr("aspectaina", re_lhr)) |>
    mutate(rev_asian = get_lr("aspectasian", re_lhr)) |>
    mutate(rev_black = get_lr("aspectblack", re_lhr)) |>
    mutate(rev_latine = get_lr("aspectlatine", re_lhr)) |>
    mutate(rev_nhpi = get_lr("aspectnhpi", re_lhr)) |>
    mutate(rev_white = get_lr("aspectwhite", re_lhr))

  re_rank_frame <- rev_frame |>
    pivot_longer(!name, names_to = "aspect", values_to = "value") |>
    mutate(rank = dense_rank(desc(value))) |>
    pivot_wider(id_cols = "name", values_from = "rank", names_from = "aspect") |>
    rename_with(~ str_replace(., "rev_", "rank_")) |>
    select(-name)

  rev_frame <- rev_frame |>
    select(-name)
  
  locale_frame <- the_frame |>
    select(id, name, locale) |>
    unique() |>
    group_by(name, locale) |>
    tally() |>
    ungroup() |>
    mutate(rank = dense_rank(desc(n))) |>
    pivot_wider(
      id_cols = "name",
      names_from = "locale",
      values_from = "rank",
      names_prefix = "locale_"
    )
  
  pronouns_frame <- the_frame |>
    select(id, name, pronouns) |>
    group_by(name, pronouns) |>
    tally() |>
    ungroup() |>
    mutate(rank = dense_rank(desc(n))) |>
    pivot_wider(
      id_cols = "name",
      names_from = "pronouns",
      values_from = "rank",
      names_prefix = "pronouns_"
    )

  new_frame <- data.frame(
    name = character_name,
    ses = mean(round(na.omit(the_frame$ses), 0))
  ) |>
    mutate(locale_city = get_count(locale_frame$locale_city)) |>
    mutate(locale_suburb = get_count(locale_frame$locale_suburb)) |>
    mutate(locale_town = get_count(locale_frame$locale_town)) |>
    mutate(locale_rural = get_count(locale_frame$locale_rural)) |>
    mutate(pronouns_he = get_count(pronouns_frame$pronouns_he)) |>
    mutate(pronouns_she = get_count(pronouns_frame$pronouns_she)) |>
    mutate(pronouns_they = get_count(pronouns_frame$pronouns_they)) |>
    bind_cols(rev_frame) |>
    bind_cols(re_rank_frame)

  return(new_frame)
}

######

learner_image_key <- c(
  7,
  6,
  5,
  4,
  3,
  2,
  1
)
names(learner_image_key) <- c(
  "IM_507T9Zv5QZCi36m",
  "IM_4OW5fRHLlR3QhOS",
  "IM_ehQr4txnBQ4Oe4C",
  "IM_8IddW11pJlsI62O",
  "IM_2mnG07uPgiLIDsi",
  "IM_9oFiGDmvIgB21mK",
  "IM_cwHsrsckcyaq4vA"
)

survey_frame <<- read_csv("data/mirror_data_0917.csv", show_col_types = FALSE) |>
  mutate(self_learner = recode(
    self_learner,
    !!!learner_image_key
  )) |>
  mutate(story1_self = recode(
    story1_self,
    !!!learner_image_key
  )) |>
  mutate(story2_self = recode(
    story2_self,
    !!!learner_image_key
  )) |>
  mutate(story3_self = recode(
    story3_self,
    !!!learner_image_key
  ))


ava_frame <- character_frame("Ava")
ben_frame <- character_frame("Ben")
alex_frame <- character_frame("Alex")
bailey_frame <- character_frame("Bailey")
david_frame <- character_frame("David")
sarah_frame <- character_frame("Sarah")

characters_frame <- bind_rows(
  ava_frame,
  ben_frame,
  alex_frame,
  bailey_frame,
  david_frame,
  sarah_frame
)

characters_frame <- characters_frame |>
  mutate(ses = round(ses, 0)) |>
  mutate(ses = case_when(
    ses %in% 271:300 ~ 1,
    ses %in% 241:270 ~ 2,
    ses %in% 211:240 ~ 3,
    ses %in% 181:210 ~ 4,
    ses %in% 151:180 ~ 5,
    ses %in% 121:150 ~ 6,
    ses %in% 91:120 ~ 7,
    ses %in% 61:90 ~ 8,
    ses %in% 31:60 ~ 9,
    ses %in% 0:30 ~ 10
  ))

#####

re_rank_frame <- characters_frame |>
  select(
    name,
    #rank_aina,
    rank_asian,
    rank_black,
    rank_latine,
    rank_nhpi,
    rank_white) |>
  pivot_longer(!name, names_to = "aspect", values_to = "rank")

nb_re_rank <- naiveBayes(
  rank ~ .,
  data = re_rank_frame
)

pn_rank_frame <- characters_frame |>
  select(
    name,
    pronouns_he,
    pronouns_she,
    pronouns_they
  ) |>
  pivot_longer(!name, names_to = "aspect", values_to = "rank")

nb_pn_rank <- naiveBayes(
  rank ~ .,
  data = pn_rank_frame
)


nb_re_rank_frame <- as.data.frame(nb_re_rank$tables$aspect) |>
  pivot_wider(
    id_cols = "Y",
    names_from = "aspect",
    values_from = "Freq"
  ) |>
  rename(
    "rank" = 1,
    "asian" = 2,
    "black" = 3,
    "latine" = 4,
    "nhpi" = 5,
    "white" = 6
  )

#####

mds_characters <- characters_frame |>
  select(-c(rev_aina, rev_asian, rev_black, rev_latine, rev_nhpi, rev_white)) |>
  mutate(across(everything(), as.character)) |>
  mutate(ses = glue("ses_{ses}")) |>
  mutate(locale_city = glue("city_{locale_city}")) |>
  mutate(locale_suburb = glue("suburb_{locale_suburb}")) |>
  mutate(locale_town = glue("town_{locale_town}")) |>
  mutate(locale_rural = glue("rural_{locale_rural}")) |>
  mutate(pronouns_he = glue("he_{pronouns_he}")) |>
  mutate(pronouns_she = glue("she_{pronouns_she}")) |>
  mutate(pronouns_they = glue("they_{pronouns_they}")) |>
  mutate(rank_aina = glue("aina_{rank_aina}")) |>
  mutate(rank_asian = glue("asian_{rank_asian}")) |>
  mutate(rank_black = glue("black_{rank_black}")) |>
  mutate(rank_latine = glue("latine_{rank_latine}")) |>
  mutate(rank_nhpi = glue("nhpi_{rank_nhpi}")) |>
  mutate(rank_white = glue("white_{rank_white}"))

rownames(mds_characters) <- mds_characters$name
mds_characters$name <- NULL

#   mds_frame <- mds_characters |>
#   dist() |>         
#   cmdscale() |>
#   as_tibble()
# colnames(mds_frame) <- c("Dim_1", "Dim_2")
# ggscatter(mds_frame, x = "Dim_1", y = "Dim_2", 
#           label = rownames(mds_characters),
#           size = 1,
#           repel = TRUE)
library(factoextra)
library(FactoMineR)
res.mca <- MCA(mds_characters, graph = TRUE)
eig.val <- get_eigenvalue(res.mca)
fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 40))

head(round(var$contrib,2), 4)

var <- get_mca_var(res.mca)
var

fviz_mca_var(res.mca, alpha.var="cos2",
             repel = TRUE,
             ggtheme = theme_minimal())

library("corrplot")
corrplot(var$cos2, is.corr=FALSE)

fviz_eig(
  res.mca,
  addlabels = TRUE,
  ylim = c(0, 30),
  ggtheme = theme_minimal()
)
