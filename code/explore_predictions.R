
needs(tidyverse)

theme_set(theme_light())
# Read data

manifesto_classified <- read_csv("data/euromanifesto_sentences_cap_classified.csv")

# Create dic to map classes to topics

class_dic <- c(
    "1" = "Macroeconomics",
    "2" = "Civil Rights",
    "3" = "Health",
    "4" = "Agriculture",
    "5" = "Labor",
    "6" = "Education",
    "7" = "Environment",
    "8" = "Energy", 
    "9" = "Immigration",
    "10" = "Transport",
    "12" = "Law and Crime",
    "13" = "Social Welfare",
    "14" = "Housing",
    "15" = "Domestic Commerce",
    "16" = "Defense",
    "17" = "Technology",
    "18" = "Foreign Trade",
    "19" = "International Affairs",
    "20" = "Government Operations", 
    "23" = "Culture")

# Reverse dictionnary, to map topics to classes

class_dic_rev <- class_dic |> 
    enframe(name = "class", value = "topic") |> 
    mutate(class = as.numeric(class)) |> 
    deframe()

# Map classes to topics

# Look at environmental sentences

# Create a heatmap of the predictions according to row_number



manifesto_classified <- manifesto_classified |> 
    mutate(predicted = as.character(predicted)) |> 
    mutate(topic = class_dic[predicted])

manifesto_classified |> 
    filter(year == 2019) |> 
    ggplot(aes(sentence_num, topic)) +
    geom_tile() +
    # remove legend
    theme(legend.position = "none") +
    # fill by black and white
    scale_fill_grey() +
    facet_wrap(~doc, scales = "free_x")

manifesto_classified |> 
    filter( party == "LR" & topic == "Environment") |> view()

man_probs_year <- manifesto_classified |> 
    ungroup() |> 
    group_by(year, doc) |> 
    count(topic) |> 
    mutate(perc = n / sum(n)*100) |> 
    # Compute mean by year
    group_by(year, topic) |>
    summarise(perc = mean(perc))

man_probs_year |> 
    ggplot(aes(topic, perc, fill = as.factor(year))) +
    geom_col(position = "dodge2") +
    scale_color_brewer("Topic", palette = "Set1") +
    scale_y_continuous("Share")  +
    coord_flip()

man_class_probs <- manifesto_classified |> 
    group_by(year, party, doc) |> 
    count(topic) |> 
    mutate(perc = n / sum(n)*100)  |> 
    ungroup()

man_class_probs |> 
    filter(topic %in% c("Environment")) |> 
    ggplot(aes(fct_reorder(party, perc), perc, fill = as.factor(year))) +
    geom_col(position = "dodge2") +
    facet_wrap(~topic) +
    scale_fill_brewer("Year", palette = "Set1") +
    scale_y_continuous("Share") +
    scale_x_discrete("Party") 

man_class_probs |> 
    filter(party == "REN") |> 
    ggplot(aes(fct_reorder(topic, perc), perc, fill = as.factor(year))) +
    geom_col(position = "dodge2") +
    scale_fill_brewer("Year", palette = "Set1") +
    scale_y_continuous("Share") +
    facet_wrap(~party) +
    coord_flip()












if (input$plot_type == "Bar Plot" & input$vis_category == "All" & input$vis_party == "All" & input$vis_year == "All") {
    plot_data <- data %>%
        count(predicted) %>%
        mutate(share = n / sum(n) * 100)
    
    p <- ggplot(plot_data, aes(x = fct_reorder(predicted, share), y = share)) +
        geom_col() +
        theme_light() +
        labs(title = "Share of Predictions by Category", x = "Category", y = "Share (%)") +
        coord_flip()
    
    p
}
# Now if want to filter by party
else if (input$plot_type == "Bar Plot" & input$vis_category == "All" & input$vis_party != "All" & input$vis_year == "All") {
    plot_data <- data %>%
        filter(party == input$vis_party) %>%
        count(predicted) %>%
        mutate(share = n / sum(n) * 100)
    
    p <- ggplot(plot_data, aes(x = fct_reorder(predicted, share), y = share)) +
        geom_col() +
        theme_light() +
        labs(title = paste("Share of Predictions by Category for", input$vis_party), x = "Category", y = "Share (%)") +
        coord_flip()
    
    p
}
# Now if want to filter by year
else if (input$plot_type == "Bar Plot" & input$vis_category == "All" & input$vis_party == "All" & input$vis_year != "All") {
    plot_data <- data %>%
        filter(year == input$vis_year) %>%
        count(predicted) %>%
        mutate(share = n / sum(n) * 100)
    
    p <- ggplot(plot_data, aes(x = fct_reorder(predicted, share), y = share)) +
        geom_col() +
        theme_light() +
        labs(title = paste("Share of Predictions by Category for", input$vis_year), x = "Category", y = "Share (%)") +
        coord_flip()
    
    p
}
# Now if I choose one party in a given year
else if (input$plot_type == "Bar Plot" & input$vis_category == "All" & input$vis_party != "All" & input$vis_year != "All") {
    plot_data <- data %>%
        filter(party == input$vis_party & year == input$vis_year) %>%
        count(predicted) %>%
        mutate(share = n / sum(n) * 100)
    
    p <- ggplot(plot_data, aes(x = fct_reorder(predicted, share), y = share)) +
        geom_col() +
        theme_light() +
        labs(title = paste("Share of Predictions by Category for", input$vis_party, "in", input$vis_year), x = "Category", y = "Share (%)") +
        coord_flip()
    
    p
}
# Now if choose one topic. Compute share of predicted topic by party
else if (input$plot_type == "Bar Plot" & input$vis_category != "All" & input$vis_party == "All" & input$vis_year == "All") {
    plot_data <- data %>%
        filter(predicted == input$vis_category) %>%
        count(party) %>%
        mutate(share = n / sum(n) * 100)
    
    p <- ggplot(plot_data, aes(x = fct_reorder(party, share), y = share)) +
        geom_col() +
        theme_light() +
        labs(title = paste("Share of Predictions by Party for", input$vis_category), x = "Party", y = "Share (%)") +
        coord_flip()
    
    p
}
