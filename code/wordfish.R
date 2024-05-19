
# Small test of wordfish on euromanifesto

## Load packages

needs(tidyverse, fs, here, quanteda, tidytext, quanteda.textmodels, quanteda.textstats, quanteda.textplots, topicmodels)

# Import data

manifestos_2024 <- fs::dir_ls("raw_data/EUR2024/txt", regexp = ".txt", recurse = T) |> 
    # Import txt file by having all the text in the same column named text and add file name
    map_dfr(~ read_delim(.x, delim = "\t", col_names = FALSE, col_types = "c", id = "file")) |> 
    rename(text = X1) |> 
    mutate(
        election = 'EURO',
        year = 2024,
        party = str_extract(file, "(?<=EUR2024_).*(?=_)")) |> 
    group_by(party) |> 
    mutate(text = paste0(text, collapse = " "), 
           doc = str_c(party, "_", year)) |> 
    unique()

manifestos_2019 <- fs::dir_ls("raw_data/EUR2019/txt/MAN", regexp = ".txt", recurse = T) |> 
    # Import txt file by having all the text in the same column named text and add file name
    map_dfr(~ read_delim(.x, delim = "\t", col_names = FALSE, col_types = "c", id = "file")) |> 
    rename(text = X1) |> 
    mutate(
        election = 'EURO',
        year = 2019,
        party = str_extract(file, "(?<=EUR2019_).*(?=_)")) |> 
    group_by(party) |> 
    mutate(text = paste0(text, collapse = " "), 
           doc = str_c(party, "_", year)) |> 
    unique() 

pf_2022 <- fs::dir_ls("raw_data/PRE2022/txt") |> 
    map_dfr(~ read_delim(.x, delim = "\t", col_names = FALSE, col_types = "c", id = "file"))  |> 
    rename(text = X1) |> 
    mutate(
        election = 'PRE',
        year = 2022,
        party = str_extract(file, "(?<=PF_).*(?=_)")) |> 
    group_by(party) |> 
    mutate(text = paste0(text, collapse = " "), 
           doc = str_c(party, "_", year)) |> 
    unique() 


manifestos <- bind_rows(manifestos_2024, manifestos_2019) |> 
    mutate(text = str_remove_all(text, "#") |> str_replace_all("l'", ""))

manifestos_sentences <- manifestos |> 
    ungroup() |> 
    unnest_tokens(sentence, text, token = "sentences", to_lower = FALSE)

write_csv(manifestos_sentences, here("data", "euromanifesto_sentences.csv"))

# Worscores based on EU position

manifestos <- manifestos |> 
    mutate(ches_lr_eco = case_when(
        party == "PCF" & year == 2019 ~ 1.12,
        party == "LFI" & year == 2019 ~ 0.87,
        party == "EELV" & year == 2019 ~ 2.87,
        party == "PS" & year == 2019 ~ 6.12,
        party == "LREM" & year == 2019 ~ 6.33,
        party == "LR" & year == 2019 ~ 8.12,
        party == "RN" & year == 2019 ~ 6.87,
    ), 
    ches_lr_gen = case_when(
        party == "PCF" & year == 2019 ~ 1.12,
        party == "LFI" & year == 2019 ~ 1.25,
        party == "EELV" & year == 2019 ~ 2.5,
        party == "PS" & year == 2019 ~ 3,
        party == "LREM" & year == 2019 ~ 6.33,
        party == "LR" & year == 2019 ~ 7.87,
        party == "RN" & year == 2019 ~ 9.75,
    ))

write_csv(manifestos, here("data", "euromanifesto.csv"))

manifesto_dfm <- corpus(manifestos, text_field = "text", docid_field = "doc") |> 
    quanteda::tokens() |> 
    quanteda::dfm() |> 
    quanteda::dfm_remove(stopwords("fr"))

tmod_ws <- textmodel_wordscores(manifesto_dfm, y = manifestos$ches_lr_gen, smooth = 1)

x <- tmod_ws$wordscores

wordscores <- tibble(words = words <- attr(x, "names") , 
                     scores = as_tibble_col(x))

wordscores |> view()

summary(tmod_ws)

pred_ws <- predict(tmod_ws, se.fit = TRUE, newdata = manifesto_dfm)

pred_ws$fit

textplot_scale1d(pred_ws)

# Wordfish scaling

manifesto_dfm_2019 <- corpus(manifestos_2019, text_field = "text", docid_field = "doc") |> 
    quanteda::tokens() |> 
    quanteda::dfm() |> 
    quanteda::dfm_remove(stopwords("fr")) |> 
    # remove numbers and punctuation
    quanteda::dfm_remove(pattern = "[0-9]") |>
    quanteda::dfm_remove(pattern = "[[:punct:]]")

manifesto_dfm_2024 <- corpus(manifestos_2024, text_field = "text", docid_field = "doc") |> 
    quanteda::tokens() |> 
    quanteda::dfm() |> 
    quanteda::dfm_remove(stopwords("fr")) |> 
    # remove numbers and punctuation
    quanteda::dfm_remove(pattern = "[0-9]") |>
    quanteda::dfm_remove(pattern = "[[:punct:]]")

manifesto_wordfish_2019 <- manifesto_dfm_2019 |>  
    quanteda.textmodels::textmodel_wordfish()

manifesto_wordfish_2024 <- manifesto_dfm_2024 |>  
    quanteda.textmodels::textmodel_wordfish()

wf_docs_2019 <- tibble(
    docs = manifesto_wordfish_2019$docs,
    theta = manifesto_wordfish_2019$theta,
    se = manifesto_wordfish_2019$se.theta,
    year = 2019

)

wf_docs_2024 <- tibble(
    docs = manifesto_wordfish_2024$docs,
    theta = manifesto_wordfish_2024$theta,
    se = manifesto_wordfish_2024$se.theta,
    year = 2024
    
)
manifesto_wordfish <- manifesto_dfm |> 
    quanteda.textmodels::textmodel_wordfish()

wf_all <- tibble(
    docs = manifesto_wordfish$docs,
    theta = manifesto_wordfish$theta,
    se = manifesto_wordfish$se.theta,
    year = 2024
)

wf_docs <- bind_rows(wf_docs_2019, wf_docs_2024)

wf_all |> 
    ggplot(aes(fct_reorder(docs, theta), theta,  ymin = theta - se, ymax = theta + se)) +
    geom_pointrange() +
    coord_flip() +
    theme_light() +
    labs(
        title = "Wordfish scaling of Euro manifestos",
        x = "Document",
        y = "Estimated theta"
    )

manifesto_wordfish$psi

wf_output_2019 <- tibble(
    features = manifesto_wordfish_2019$features,
    beta = manifesto_wordfish_2019$beta,
    psi = manifesto_wordfish_2019$psi,
    year = 2019
    
)

wf_output_2024 <- tibble(
    features = manifesto_wordfish_2024$features,
    beta = manifesto_wordfish_2024$beta,
    psi = manifesto_wordfish_2024$psi,
    year = 2024

)

wf_output_all <- tibble(
    features = manifesto_wordfish$features,
    beta = manifesto_wordfish$beta,
    psi = manifesto_wordfish$psi

)

wf_output <- bind_rows(wf_output_2019, wf_output_2024)

needs(gghighlight)

wf_output_2019 |>  
    ggplot(aes(beta, psi, label = features)) +
    geom_text() +
    theme_light() +
    facet_wrap(~year, scales = "free") +
    geom_vline(xintercept = 0, linetype = 2) +
    # Highlight only the word islam
    gghighlight(features %in% c("agriculteurs", "climat", "nations", "agriculture", "sécurité", "écologie", "immigration", "climatique", "woke", "écologique", "ukraine", "punitive", "palestine", "fossiles", "frontières", "souveraineté"))

wf_output_2024 |> 
    ggplot(aes(beta, psi, label = features)) +
    geom_text() +
    theme_light() +
    facet_wrap(~year, scales = "free") +
    geom_vline(xintercept = 0, linetype = 2) +
    # Highlight only the word islam
    gghighlight(features %in% c("agriculteurs", "climat", "nations", "agriculture", "sécurité", "écologie", "immigration", "climatique", "woke", "écologique", "ukraine", "punitive", "palestine", "fossiles", "frontières", "souveraineté"))


# Create docsvars is party == RN or not

manifestos$rn <- ifelse(manifestos$party == "RN", 1, 0)

manifesto_dfm <- corpus(manifestos, text_field = "text", docid_field = "doc") |> 
    quanteda::tokens() |> 
    quanteda::dfm() |> 
    quanteda::dfm_remove(stopwords("fr")) |> 
    # remove numbers and punctuation
    quanteda::dfm_remove(pattern = "[0-9]") |>
    quanteda::dfm_remove(pattern = "[[:punct:]]")

words <- textstat_fighting_words(manifesto_dfm, group.var = "rn")[[1]] |> 
    as_tibble()

words |> 
    ggplot(aes(log(n_reference), z_score)) +
    geom_point() +
    ggrepel::geom_text_repel(aes(label = feature), box.padding = 0.5)


# Correspondence analysis

tmod_ca <- manifesto_dfm |> 
    dfm_remove(stopwords("fr")) |> 
    textmodel_ca()


tibble(
    dim1 = coef(tmod_ca, doc_dim = 1)$coef_document,
    dim2 = coef(tmod_ca, doc_dim = 2)$coef_document,
    dim3 = coef(tmod_ca, doc_dim = 3)$coef_document,
    doc = tmod_ca$rownames
) |> 
    ggplot(aes(dim1, dim2)) +
    geom_point() +
    geom_text(aes(label = doc), check_overlap = TRUE, vjust = -0.5, hjust = -0.2) +
    labs(
        title = "Correspondence analysis of Euro manifestos",
        x = "Dimension 1",
        y = "Dimension 2"
) +
    theme_light() +
    theme(legend.position = "none") +
    scale_x_continuous(limits = c(-5,5))
# LExical diversity

tstat_lexdiv <- textstat_lexdiv(manifesto_dfm)

tstat_lexdiv |> 
    ggplot(aes(fct_reorder(document, TTR), TTR)) +
    geom_col() +
    coord_flip()

tstat_dist <- as.dist(textstat_dist(manifesto_dfm))
clust <- hclust(tstat_dist)
plot(clust, xlab = "Distance", ylab = NULL)

# Keyness

man_rn <- manifesto_dfm |> 
    dfm_subset(party == "PCF")

man_rn |> 
    quanteda.textstats::textstat_keyness(target = man_rn$year == 2024) |> 
    textplot_keyness()


tstat_key <- textstat_keyness(manifesto_dfm, 
                              target = year(manifesto_dfm$year) >= 2024)




## Let's try a topic model

stop_words <- tibble(word = stopwords::stopwords("fr")) |> 
    mutate(word = as.character(word))

man_dtm <- manifestos |> 
    ungroup() |> 
    unnest_tokens(word, text) |> 
    anti_join(stop_words) |>
    count(doc, word, sort = TRUE) 


view(man_dtm)
man_lda <- LDA(man_dtm, k = 10, control = list(seed = 1234))

man_lda |> 
    group_by(topic) |>
    slice_max(beta, n = 10) %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(term, beta)) +
    geom_col() +
    scale_x_reordered() +
    facet_wrap(vars(topic), scales = "free_x")

