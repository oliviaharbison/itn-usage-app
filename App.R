# Packages ----
library(shiny)
library(tidyverse)
library(plotly)

# Data ----
insect_net <- read_rds(file = "data/insect_net.rds")

gender <- insect_net %>%
  filter(category == "gender")

res_area <- insect_net %>%
  filter(category == "residence_area")

wealth <- insect_net %>%
  filter(category == "wealth_quintile")

edu <- insect_net %>%
  filter(category == "education_level")

# UI ----
ui <- fluidPage(# App title ----
                titlePanel("Mosquito Net Usage"),
                tabsetPanel(
                  # Panel 1 ----
                  tabPanel(
                    "Data Visualization",
                    
                      # Sidebar layout with input and output definitions ----
                    sidebarLayout(
                      position = "left",
                      
                      # Sidebar panel for inputs
                      sidebarPanel(
                        p(
                          "Please select any of the variables below to see each of them visualized."
                        ),
                        #Input: radio buttons for fill
                        radioButtons(
                          inputId = "var_fill",
                          label = "Select Fill Variable:",
                          choices = list(
                            "Gender",
                            "Residence Area",
                            "Wealth Quintiles",
                            "Education Level"
                          )
                        ),
                        br(),
                        br(),
                        br(),
                        h6(
                          "Note: For comparison purposes, you can find a map of countries with and without malaria transmission on the second tab of this app."
                        ),
                        width = 2
                      ),
                      
                      # Main panel for displaying outputs ----
                      mainPanel(
                        h4(strong(
                          "Child mosquito net usage in countries with malaria."
                        )),
                        # Output: Barplot
                        plotOutput(
                          outputId = "barPlot",
                          width = 800,
                          height = 1200
                        )
                      )
                    )
                  )
                  ,
                  # Panel 2 ----
                  tabPanel(
                    "Map",
                    img(
                      src = "MalariaEndemicity_2020.png",
                      width = 772,
                      height = 480
                    ),
                    sidebarPanel(
                      p(
                        "This map, created by the CDC, shows the countries in which malaria transmission occurs throughout, occurs partially, or does not occur."
                      ),
                      br(),
                      br(),
                      p(
                        "You can find this map and more information on ",
                        a(href = "https://www.cdc.gov/malaria/about/distribution.html", "the CDC's webpage.")
                      )
                    )
                  ),
                  # Panel 3 ----
                  tabPanel(
                    "Additional Information",
                    mainPanel(
                      h4("Additional Information"),
                      br(),
                      br(),
                      div(
                        "Malaria is one of the leading causes of death for children under 5 years old,
                             with almost 500,000 children dying each year. In fact, three out of four malaria
                             deaths are children under 5 years old. Although the total elimination of malaria
                             is a complicated endeavor, there are more simple mitigating efforts that can be
                             put in place to diminish childhood risk. One of the most common, effective,
                             and easy preventative measures is sleeping under insecticide treated nets (ITNs).
                             For a very low cost, these nets are a fairly easy way to limit exposure to ",
                        em("Anopheles"),
                        " mosquitos, which carry and transmit malaria. Since ",
                        em("Anopheles"),
                        " mosquitos are mostly active at night, ITNs can greatly decrease malaria rates.",
                        br(),
                        br(),
                        br(),
                        
                        "The visualization in the first tab of this app shows the percent of children
                             under 5 years old sleeping under ITNs. This information is by country and split
                             by several different variables (of the user's choosing). There are several interesting
                             patterns in this data. For example, it seems that children in families with higher levels
                              of education are more likely to sleep under an ITN. Furthermore, it seems like children
                              in families of higher wealth status are also more likely to sleep under ITNs. These patterns
                             are not surprising but are important to recognize.",
                        br(),
                        br(),
                        br(),
                        
                        "Furthermore, it is helpful to compare the data in the visualization to the map
                             in the second tab of this app for additional helpful context. For example, it's not surprising the number of children in Pakistan
                             using ITNs is small because there is low malaria transmission. On the other hand, Djibouti
                             also has low percentages of ITN usage, but it is a higher malaria transmission area.",
                        br(),
                        br(),
                        br(),
                        "This data tells the story of where ITNs are used (high income homes, etc.) and where more
                             ITNs could be needed.",
                        style = "background-color: #E9F1F7; padding: 15px"
                      ),
                      br(),
                      br(),
                      br(),
                      br(),
                      
                      div(
                        "This data is sourced from the WHO and the UN, and can be found",
                        a(href = "https://data.un.org/Data.aspx?q=sleep&d=WHO&f=MEASURE_CODE%3aitnchild", "here."),
                        "Other
                               included information is sourced from the",
                        a(href = "https://www.cdc.gov/malaria/about/distribution.html", "CDC,"),
                        a(href = "https://ourworldindata.org/malaria-introduction#:~:text=Only%20a%20small%20fraction%20of,four%20malaria%20victims%20are%20children.&text=Malaria%20is%20one%20of%20the,a%20million%20children%20every%20year.&text=That's%201320%20dead%20children%20on%20any%20average%20day.", "Our World in Data,"),
                        "and",
                        a(href = "https://malariajournal.biomedcentral.com/articles/10.1186/s12936-020-3106-2#:~:text=At%20high%20levels%20of%20coverage,users%20of%20ITNs%20%5B3%5D", "Malaria Journal.")
                      ),
                      br(),
                      br(),
                      width = 10
                    )
                  )
                ))



# Server ----
server <- function(input, output) {
  output$barPlot <- renderPlot({
    # Deal with input variables ----
    fill <- switch(
      input$var_fill,
      "Gender" = gender$selection,
      "Residence Area" = res_area$selection,
      "Wealth Quintiles" = wealth$selection,
      "Education Level" = edu$selection
    )
    
    df <- switch(
      input$var_fill,
      "Gender" = gender,
      "Residence Area" = res_area,
      "Wealth Quintiles" = wealth,
      "Education Level" = edu
    )
    
    # Legend label ----
    
    label <- input$var_fill
    
    # Barplot ----
    ggplot(df, aes(
      x = percent_net,
      y = fct_reorder(country, percent_net),
      fill = fill
    )) +
      geom_bar(stat = "identity",
               position = position_dodge(.7),
               width = 0.6) +
      theme_minimal() +
      labs(
        x = "Percent of children under 5\nthat use insecticide treated mosquito nets",
        y = NULL,
        caption = "Source: UN Data",
        fill = label
      ) +
      lims(x = c(0, 100)) +
      scale_fill_brewer(palette = "Set2") +
      guides(x = guide_axis(angle = 30)) +
      scale_x_continuous(position = "top") +
      theme(
        axis.title.x.top = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14),
        legend.justification = "top"
      )
    
    
    
  })
  
}


# Create Shiny app ----
shinyApp(ui = ui, server = server)