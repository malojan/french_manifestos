# InstTous necessary packages if not already instToused

# Load packages
library(shiny)
library(DT)
library(dplyr)
library(stringr)
library(ggplot2)
library(forcats)  # For fct_reorder
library(RColorBrewer)  # For scale_fill_brewer
library(quanteda)
library(quanteda.textmodels)
library(gghighlight)

# Import manifestos_classifeid

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


# Map classes to topics

# Look at environmental sentences

# Create a heatmap of the predictions according to row_number



manifesto_classified <- manifesto_classified |> 
    mutate(predicted = as.character(predicted)) |> 
    mutate(topic = class_dic[predicted])

manifesto_classified |> 
    count(topic)
# Translate topics intro french

class_dic_en <- c(
    "Macroeconomics" = "Economie",
    "Civil Rights" = "Droits de l'homme, libertés, discriminations",
    "Health" = "Santé",
    "Agriculture" = "Agriculture",
    "Labor" = "Travail",
    "Education" = "Education",
    "Environment" = "Environnement",
    "Energy" = "Energie",
    "Immigration" = "Immigration",
    "Transport" = "Transport",
    "Law and Crime" = "Justice et criminalité",
    "Social Welfare" = "Protection sociale",
    "Housing" = "Logement",
    "Domestic Commerce" = "Commerce intérieur",
    "Defense" = "Défense",
    "Technology" = "Technologie",
    "Foreign Trade" = "Commerce extérieur",
    "International Affairs" = "Affaires internationales",
    "Government Operations" = "Fonctionnement de l'Etat & Autres",
    "Culture" = "Culture"
)


# Replace topics with french translation

manifesto_classified <- manifesto_classified |> 
    mutate(topic = class_dic_en[topic])


# Assuming you have a data frame named classified_df
classified_df <- manifesto_classified |> 
    mutate(sentence = str_to_sentence(sentence)) |> 
    select(year, sentence, party, topic, doc) |> 
    rename(text = sentence, 
           predicted = topic)

classified_df |> count(predicted)

# Define UI
ui <- navbarPage("Explorateur de programme : élections européennes",

                 tabPanel("Visualiser les enjeux",
                          sidebarLayout(
                              sidebarPanel(
    
                                  selectInput("vis_category", "Enjeu:",
                                              choices = c("Tous", unique(classified_df$predicted)),
                                              selected = "Tous"),
                                  selectInput("vis_party", "Parti:",
                                              choices = c("Tous", unique(classified_df$party)),
                                              selected = "Tous"),
                                  selectInput("vis_year", "Année:",
                                              choices = c("Tous", "Comparaison", unique(classified_df$year)),
                                              selected = "Tous")
                              ),
                              mainPanel(
                                  plotOutput("plot")
                              )
                          )
                 ),
                 tabPanel("Explorer le texte",
                          sidebarLayout(
                              sidebarPanel(
                                  selectInput("category", "Enjeu:",
                                              choices = c("Tous", unique(classified_df$predicted)),
                                              selected = "Tous"),
                                  selectInput("party", "Parti:",
                                              choices = c("Tous", unique(classified_df$party)),
                                              selected = "Tous"),
                                  selectInput("year", "Année:",
                                              choices = c("Tous", unique(classified_df$year)),
                                              selected = "Tous")
                              ),
                              mainPanel(
                                  DTOutput("table")
                              )
                          )
                 )
                 
)

# Define server logic
server <- function(input, output) {
    # Reactive expression to filter data based on selected category, party, and year
    filtered_data <- reactive({
        data <- classified_df
        if (input$category != "Tous") {
            data <- data[data$predicted == input$category, ]
        }
        if (input$party != "Tous") {
            data <- data[data$party == input$party, ]
        }
        if (input$year != "Tous") {
            data <- data[data$year == input$year, ]
        }
        data
    })
    
    # Render data table
    output$table <- renderDT({
        datatable(filtered_data(), options = list(pageLength = 10))
    })
    
    # Reactive expression to filter data for visualizations
    vis_data <- reactive({
        data <- classified_df
        if (!input$vis_category %in% c("Tous")) {
            data <- data[data$predicted == input$vis_category, ]
        }
        if (input$vis_party != "Tous") {
            data <- data[data$party == input$vis_party, ]
        }
        if (!input$vis_year %in% c("Tous", "Comparaison")) {
            data <- data[data$year == input$vis_year, ]
        }
        data
    })
    
    # Render plot
    output$plot <- renderPlot({
        data <- vis_data()
        
        shares <- classified_df |> 
            count(predicted) |> 
            mutate(share = n / sum(n) * 100) |> 
            ungroup()
        
        party_topic_shares <- classified_df |> 
            group_by(party) |> 
            count(predicted) |> 
            mutate(share = n / sum(n) * 100) |> 
            ungroup()
            
        year_topic_shares <- classified_df |> 
            group_by(year) |> 
            count(predicted) |> 
            mutate(share = n / sum(n) * 100) |> 
            ungroup()
        
        party_year_topic_shares <- classified_df |> 
            group_by(party, year) |>
            count(predicted) |>
            mutate(share = n / sum(n) * 100) |> 
            ungroup()
        
        # If category, party, and year are Tous "Tous", plot shares
        
        if (input$vis_category == "Tous" & input$vis_party == "Tous" & input$vis_year == "Tous") {
            ggplot(shares, aes(x = fct_reorder(predicted, share), y = share)) +
                geom_col() +
                scale_fill_brewer(palette = "Set1") +
                labs(title = "Distribution des enjeux dans les programmes",
                     x = "Enjeu",
                     y = "Proportion (%)") +
                coord_flip()
        }
        # If Comparaison is selected in Year, plot year topic_shares
        
        else if (input$vis_category == "Tous" & input$vis_party == "Tous" & input$vis_year == "Comparaison") {
            ggplot(year_topic_shares, aes(fct_reorder(predicted, share), share, fill = as.factor(year))) +
                geom_col(position = "dodge2") +
                scale_fill_brewer("Year", palette = "Set1") +
                labs(title = "Distribution des enjeux dans les programmes par année",
                     x = "Enjeu",
                     y = "Proportion (%)") +
                coord_flip()
        }
        # If a given year is selected, plot year topic shares and filter
        
        else if (input$vis_category == "Tous" & input$vis_party == "Tous" & !input$vis_year %in% c("Tous", "Comparaison")) {
            
            year_topic_shares |> 
                filter(year == input$vis_year) |>
                ggplot(aes(fct_reorder(predicted, share), share)) +
                geom_col() +
                scale_fill_brewer(palette = "Set1") +
                labs(title = paste("Distribution des enjeux dans les programmes pour", input$vis_year),
                     x = "Enjeu",
                     y = "Proportion (%)") +
                coord_flip()
        }
        # If a given party is chosen and category is "Tous", plot party topic shares
        
        else if (input$vis_category == "Tous" & !input$vis_party %in% c("Tous") & input$vis_year == "Tous") {
            
            party_topic_shares |> 
                filter(party == input$vis_party) |>
                ggplot(aes(fct_reorder(predicted, share), share)) +
                geom_col(position = "dodge2") +
                labs(title = "Distribution des enjeux dans les programmes par parti",
                     x = "Enjeu",
                     y = "Proportion (%)") +
                coord_flip()
        }
        # If a given party is chosen with a given year and category is "Tous", plot party year topic shares
        
        else if (input$vis_category == "Tous" & !input$vis_party %in% c("Tous") & !input$vis_year %in% c("Tous", "Comparaison")) {
            ggplot(party_year_topic_shares, aes(fct_reorder(predicted, share), share)) +
                geom_col(position = "dodge2") +
                labs(title = paste("Distribution des enjeux dans les programmes par parti et année", input$vis_year),
                     x = "Enjeu",
                     y = "Proportion (%)") +
                coord_flip()
        }
        # If party is chosen with Comparaison
        else if (input$vis_category == "Tous" & !input$vis_party %in% c("Tous") & input$vis_year == "Comparaison") {
            party_year_topic_shares |> 
                filter(party == input$vis_party) |>
                ggplot(aes(fct_reorder(predicted, share), share, fill = as.factor(year))) +
                geom_col(position = "dodge2") +
                scale_fill_brewer("Year", palette = "Set1") +
                labs(title = "Distribution des enjeux dans les programmes par parti et année",
                     x = "Enjeu",
                     y = "Proportion (%)") +
                coord_flip()
        }
        # If a given category is chosen, plot topic shares by party
        
        else if (!input$vis_category %in% c("Tous") & input$vis_party == "Tous" & input$vis_year == "Tous") {
            party_topic_shares |> 
                filter(predicted == input$vis_category) |>
                ggplot(aes(fct_reorder(party, share), share)) +
                geom_col() +
                labs(title = paste("Distribution des enjeux dans les programmes pour", input$vis_category),
                     x = "Parti",
                     y = "Proportion (%)") +
                coord_flip()
        }
        # If a given category is chosen with a given year, plot topic shares by party
        
        else if (!input$vis_category %in% c("Tous") & input$vis_party == "Tous" & !input$vis_year %in% c("Tous", "Comparaison")) {
            party_year_topic_shares |> 
                filter(predicted == input$vis_category & year == input$vis_year) |>
                ggplot(aes(fct_reorder(party, share), share)) +
                geom_col() +
                labs(title = paste("Distribution des enjeux dans les programmes pour", input$vis_category, "for Year", input$vis_year),
                     x = "Parti",
                     y = "Proportion (%)") +
                coord_flip()
        }
        # Tousow Comparaison for one category across years with party_year_topics_shares
        
        else if (!input$vis_category %in% c("Tous") & input$vis_party == "Tous" & input$vis_year == "Comparaison") {
            party_year_topic_shares |> 
                filter(predicted == input$vis_category) |>
                ggplot(aes(fct_reorder(party, share), share, fill = as.factor(year))) +
                geom_col(position = "dodge2") +
                scale_fill_brewer("Year", palette = "Set1") +
                labs(title = paste("Distribution des enjeux dans les programmes pour", input$vis_category, "by Year"),
                     x = "Parti",
                     y = "Proportion (%)") +
                coord_flip()

        }
        # If only one party selected with a given topic 
        else if (!input$vis_category %in% c("Tous") & !input$vis_party %in% c("Tous") & input$vis_year == "Tous") {
            party_topic_shares |> 
                filter(predicted == input$vis_category & party == input$vis_party) |>
                ggplot(aes(fct_reorder(predicted, share), share)) +
                geom_col() +
                labs(title = paste("Distribution des enjeux dans les programmes pour", input$vis_category, "by", input$vis_party),
                     x = "Enjeu",
                     y = "Proportion (%)") +
                coord_flip()
        }
        # Do the same but by year 
        else if (!input$vis_category %in% c("Tous") & !input$vis_party %in% c("Tous") & !input$vis_year %in% c("Tous", "Comparaison")) {
            party_year_topic_shares |> 
                filter(predicted == input$vis_category & party == input$vis_party & year == input$vis_year) |>
                ggplot(aes(fct_reorder(predicted, share), share)) +
                geom_col() +
                labs(title = paste("Distribution des enjeux dans les programmes pour", input$vis_category, "by", input$vis_party, "for Year", input$vis_year),
                     x = "Enjeu",
                     y = "Proportion (%)") +
                coord_flip()
        }
        # and with Comparaison
        else if (!input$vis_category %in% c("Tous") & !input$vis_party %in% c("Tous") & input$vis_year == "Comparaison") {
            party_year_topic_shares |> 
                filter(predicted == input$vis_category & party == input$vis_party) |>
                ggplot(aes(fct_reorder(predicted, share), share, fill = as.factor(year))) +
                geom_col(position = "dodge2") +
                scale_fill_brewer("Year", palette = "Set1") +
                labs(title = paste("Distribution des enjeux dans les programmes pour", input$vis_category, "by", input$vis_party, "by Year"),
                     x = "Enjeu",
                     y = "Proportion (%)") +
                coord_flip()
        }
    
    })
  
}



# Run the application 
shinyApp(ui = fluidPage(ui, theme = shinythemes::shinytheme("cosmo")), server = server)
