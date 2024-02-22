
#load packages
library(shiny)
library(shinyjs)
library(ggplot2)
library(dplyr) 
library(tidyr)
library(tidyverse)

#get data 
encoded <- readRDS("encoded.rds")
encodedx <- readRDS("encodedx.rds")


# Define UI for application that draws a histogram
ui <- fluidPage(
  
    useShinyjs(),
    tags$style(
      HTML("
      /* Add padding below the title panel */
      .title-panel-padding {
        padding-bottom: 16px;
      }
    ")
    ),

    # Application title
    div(class = "title-panel-padding",
        titlePanel("Tamayo Physical Therapy Patient Dashboard"), 
        h5("Visualizing data from 75 patients, Dec 2022 - Dec 2023")),
    
  
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      
      #input: select variables to plot
        sidebarPanel(
          
          # Select if viewing histograms or box plots 
          selectInput(
            inputId = "plotType", 
            label = "Select First Plot Type", 
            choices = c("Histogram", "Box Plot", "Scatterplot"),
            selected = "Histogram"
            ),
          
          # UI for histograms 
          conditionalPanel(
              condition = "input.plotType == 'Histogram'",
              # Select variable for histogram
              selectInput(
                inputId = "histype",
                label = "Histogram to view:",
                choices = c("Total Units", "Total Visits", "Age", "First DOS"),
                selected = "Age"
              ),
              #select bin size
              sliderInput("bins",
                            "Number of bins:",
                            min = 1,
                            max = 30,
                            value = 10)
          ),
          # UI for Scatterplot
          conditionalPanel(
            condition = "input.plotType == 'Scatterplot'",
            # Select variable for y axis
            selectInput(
              inputId = "y",
              label = " Y-axis",
              choices = c("Total Units", "Total Visits", "Age", "First DOS"),
              selected = "Total Units"
            ),
            # Select variable for x axis
            selectInput(
              inputId = "x",
              label = " X-axis",
              choices = c("Total Units", "Total Visits", "Age", "First DOS"),
              selected = "Total Visits"
            )
            
          ),
  
          # UI for box plots
          conditionalPanel(
            condition = "input.plotType == 'Box Plot'",
            #select variable for boxplot
            selectInput(
              inputId = "boxtype",
              label = "Boxplot to view:",
              choices = c("Total Units", "Total Visits"),
              selected = "Total Visits"
            )
          ),

          # UI for heatmap
  
            #select variable for heatmap
            selectInput(
              inputId = "heatype",
              label = "Heatmap to view:",
              choices = c("Therapists", "Diagnosis"),
              selected = "Diagnosis"
            ),
            conditionalPanel(
              condition = "input.heatype == 'Diagnosis'",
              sliderInput(
                inputId = "numDiag", 
                label = "Number of Diagnoses to Display:", 
                min = 1, 
                max = 35, 
                value = 15)
            ), 
          conditionalPanel(
            condition = "input.heatype == 'Therapists'",
            sliderInput(
              inputId = "numTherp",
              label = "Number of Therapists to Display:", 
              min = 1, 
              max = 10, 
              value = 5)
          ),
          # Show User Guide 
          checkboxInput(inputId = "showguide",
                        label = "Show User Guide", 
                        value = FALSE)
          
          
        ),
          mainPanel(
            conditionalPanel(
              condition = "input.plotType == 'Histogram' && input.showguide == false",
              plotOutput("histPlot")  # Output for histograms
            ),
            conditionalPanel(
              condition = "input.plotType == 'Box Plot' && input.showguide == false",
              plotOutput("boxPlot")  # Output for box plots
            ),
            conditionalPanel(
              condition = "input.plotType == 'Scatterplot' && input.showguide == false",
              plotOutput("scatterPlot")  # Output for scatterplots
            ), 
            conditionalPanel(
              condition = "input.heatype == 'Diagnosis' && input.showguide == false",
              plotOutput("heatPlot")  # Output for heatmap
            ),
            conditionalPanel(
              condition = "input.heatype == 'Therapists' && input.showguide == false",
              plotOutput("heatTPlot")  # Output for heatmap
            ),
            conditionalPanel(
              condition = "input.showguide == true",
              htmlOutput("userguide") # Output for heatmap
            )
            
          )
        
    )
)


server <- function(input, output) {
  
    output$userguide <- renderUI({
      HTML("
    <h2>User Guide</h2>
    <h3>Overview and Purpose</h3>
      <p> The Physical Therapy Patient Data Visualizer is a tool designed 
      to help therapists at Tamayo Physical therapy track and analyze patient 
      information effectively. The visualizer samples the 75 patients treated 
      from Dec 2022 - Dec 2023. </p>
    <h3>Visualizations</h3>
      <h4> Histogram </h4>
        <ul>
          <li>The histogram display visually represents frequency of patient metrics — Total Units, Total Visits, Age, and First DOS.  </li>
          <li>Bins, or intervals, organize the data, and users can adjust the number of bins using a slider for detailed insights into each metric's distribution </li>
        </ul>
      <h4> Box Plot </h4>
        <ul>
          <li> Box plots visually represent data distribution and central tendencies within two age groups: under 65 and over 65.</li>
        </ul>
      <h4> Scatterplot </h4>
        <ul>
          <li> The scatterplots feature flexible axes, allowing users to explore relationships within the selected metrics of Total Units, Total Visits, Age, and First DOS.</li>
        </ul>
      <h4> Heatmap </h4>
      <ul>
        <li> The heatmaps visually represent data intensity through color gradients, with a slider enabling control over the number of values displayed on the y-axis. </li>
        <li> Data is sorted by frequency, so the therapists or diagnosis with higher counts will appear first when displaying more values. </li>
      </ul>
  ")
    })

    output$histPlot <- renderPlot({
      
        # generate bins based on input$bins from ui.R
        selected_column <- input$histype
        x    <- encoded[[selected_column]]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        hist(encoded[[input$histype]], 
             breaks = bins, col = "darkgray", 
             border = 'white',
             ylab = 'Count',
             xlab = input$histype,
             main = paste("Histogram of", selected_column))
    })
    
    output$boxPlot <- renderPlot({
      selected_column <- input$boxtype
      varb    <- encoded[[selected_column]]
      mytitle <- paste("Box Plot of", input$boxtype, "by Age Group")
      ggplot(encoded, aes(x = age_group, y = varb)) +
        geom_boxplot(fill = "darkgrey", color = "black") +
        labs(title = mytitle, 
             x = "Age Group", 
             y = input$boxtype) +
        theme(text = element_text(size = 14))
    })
    
    output$scatterPlot <- renderPlot({
      if (input$x == "Total Units") {
        selected_x <- encoded$total_units
      } else if (input$x == "Total Visits") {
        selected_x <- encoded$total_visits
      } 
      else if (input$x == "First DOS") {
        selected_x <- encoded$first_dos
      } else if (input$x == "Age") {
        selected_x <- encoded$Age
      } 
      if (input$y == "Total Units") {
        selected_y <- encoded$total_units
      } else if (input$y == "Total Visits") {
        selected_y <- encoded$total_visits
      } else if (input$y == "First DOS") {
        selected_y <- encoded$first_dos
      } else if (input$y == "Age") {
        selected_y <- encoded$Age
      } 
      
      mytitle <- paste("Scatter Plot of", input$x, "vs.", input$y)
      ggplot(encoded, aes(x = selected_x, y = selected_y)) +
        geom_point(color = "black") +
        labs(title = mytitle, x = input$x, y = input$y)
    })
    
    
    output$heatPlot <- renderPlot({

      heatmap_data_long <- encodedx %>%
        pivot_longer(cols = ends_with("_group"), names_to = "Diagnosis", values_to = "Diagnosis_Treated") %>%
        group_by(Age, Diagnosis) %>%
        summarise(Frequency = sum(Diagnosis_Treated), .groups = "drop") %>%
        group_by(Diagnosis) %>%
        mutate(Total_Frequency = sum(Frequency)) %>%
        group_by(Age) %>% arrange(desc(Total_Frequency)) %>% 
        slice_head(n = input$numDiag)
      
      mytitle <- paste("Age-Diagnosis Heatmap (Top ", input$numDiag, ")")
      ggplot(heatmap_data_long, aes(x = Age, y = Diagnosis, fill = Frequency)) +
        geom_tile() +
        scale_fill_gradient(low = "white", high = "blue") +  # Adjust color scale if needed
        labs(title = mytitle, x = "Age", y = "Diagnosis")
    })
    output$heatTPlot <- renderPlot({
      
      heatmap_data_long <- encodedx %>%
        pivot_longer(cols = starts_with("PT_"), names_to = "Therapists", values_to = "PT_Treated") %>%
        group_by(Age, Therapists) %>%
        summarise(Frequency = sum(PT_Treated), .groups = "drop") %>%
        group_by(Therapists) %>%
        mutate(Total_Frequency = sum(Frequency)) %>%
        group_by(Age) %>% arrange(desc(Total_Frequency)) %>% 
        slice_head(n = input$numTherp)
      
      mytitle <- paste("Age-Therapist Heatmap (Top ", input$numTherp, ")")
      ggplot(heatmap_data_long, aes(x = Age, y = Therapists, fill = Frequency)) +
        geom_tile() +
        scale_fill_gradient(low = "white", high = "blue") +  # Adjust color scale if needed
        labs(title = mytitle, x = "Age", y = "Diagnosis")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
