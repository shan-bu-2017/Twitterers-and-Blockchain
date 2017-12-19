#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(devtools)
library(twitteR)
library(ggplot2)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(reshape2)
library(lubridate)
library(ggvis)
library(shiny)
## read data
data <- read.csv("blockchain.csv",stringsAsFactors=FALSE)

bingneg <- get_sentiments("bing") %>% 
  filter(sentiment == "negative")

bingpos <- get_sentiments("bing") %>% 
  filter(sentiment == "positive")



## tokenize and remove stop words


tidy_data <-  data %>% 
  unnest_tokens(word,text) 

# Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel("Twitter Analysis of blockchain"),
  
  navbarPage(title="BLOCKCHAIN",
             tabPanel("Introduction",
                      h1("Project Introduction"),
                      br(), # space
                      hr(), # draw a line
                      p("Blockchain is the world's leading software platform for digital assets . Google Trends shows that Blockchain has a boost in the year 2017
                        How about Twitterers' attitudes toward blockchain? My project analyzes Twitter data with the keyword 'blockchain' from 2017-01-01 to 2017-12-18. To explore the brand perception of “blockchain” on Twitter. Word frequency analysis, sentiment analysis, mapping analysis are conducted for exploring people’s attitudes.
"),
                      img(src="blockchain.jpg",align="center"),
                      p("Pic source: https://hbr.org/2017/03/the-blockchain-will-do-to-banks-and-law-firms-what-the-internet-did-to-media?referral=03758&cm_vc=rr_item_page.top_right.shtml")
                      ),
             tabPanel("Word Frequency",
                      h1("Word Frequency"),
                      fluidRow(
                        column(5,
                               plotOutput("wordcloud1")
                        ),
                        column(6,
                               dataTableOutput("wordfreqtable")
                        )
                      )
             ),
             tabPanel("Sword of Damocles?",
                      h1("Sentiment Analysis--Is Blockchain the Sword of Damocles? "),
                      p("The sentiment analyses present that Some people regards Blockchain as a innovation 
while others regard it risky. From the twitters, the word dark is mentioned is 61 times, indicating it is an impressive negative sentiment of people towards blockchain.

                        The word bonus appeared over 60 times. This is also an important sentiment here. 
                        
                        Words like glad, and easier may indicate something about people's expectaiton of blockchain. 
                        
                        Therefore, it seems that people holds a half-and-half attitudes towards blockchain."),
                      fluidRow(
                        column(4,
                               plotOutput("wordcloud2")
                        ),
                        column(4,
                               plotOutput("posplot")
                        ),
                        column(4,
                               plotOutput("negplot")
                        )
                      )
             ), #end of tabpanel sentiment
             
             tabPanel("A Summit Day ",
                      h1("A summit day: Dec 17 2017"),
                      fluidRow(
                        column(7,
                               ggvisOutput("bydate")
                        ),
                        column(4,
                               p(""),
                               p(""),
                               p("The number of tweets about blockchain on December 17 reached the highest amount than other dates.
I used the criteria (created day of Twitter)
Time span:  2017/01/01-2017/01/18
")
                        ),
                        dataTableOutput("datetable")
                      )
             ),
             tabPanel("Mapping ",
                      h1("Where are blockchain Twitterers?"),
                      p("The map displays that east part of U.S., such as New York; and the west part like California, have the
                                                                       most people twittering the blockchain topic."),
                   
                      p(img(src="map.png",align="center")))
  )#end of navbar
)#end of fluidpage
             

# Define server logic
server <- function(input, output) {
  
  
  output$wordcloud1 <- renderPlot({
    
    # draw the wordcloud1
    tidy_data %>%
      count(word) %>%
      with(wordcloud(word, n, max.words = 200))
    
  }) #end of wordcloud1
  
  output$wordfreqtable <- renderDataTable({
    tidy_data %>%  count(word, sort=TRUE)
    
  })
  
  output$wordcloud2 <-renderPlot({
    tidy_data %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort = TRUE) %>%
      acast(word ~ sentiment, value.var = "n", fill = 0) %>%
      comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                       max.words = 100)
  })
  
  output$posplot <- renderPlot({
    
    poslist <- tidy_data %>%
      inner_join(bingpos) %>%
      count(word, sort = TRUE)
    
    poslist %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n)) +
      geom_col(fill="red") +
      xlab(NULL) +
      coord_flip() +
      scale_y_continuous(limits = c(0,50))
    
  })
  
  output$negplot <- renderPlot({
    
    neglist <- tidy_data %>%
      inner_join(bingneg) %>%
      count(word, sort = TRUE)
    
    neglist %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n)) +
      geom_col(fill="blue") +
      xlab(NULL) +
      coord_flip() +
      scale_y_continuous(limits = c(0,50))
  })
  
  # create date table
  datedt <-data %>%
    mutate( date1 = date(created)) %>%
    group_by(date1) %>%
    summarise(date_n = n()) %>%
    mutate(Date = as.character(date1))
  
  datedt$id <- 1:nrow(datedt)  # Add an id column to use ask the key
  
  
  # create all_values function for ggvis
  hover1 <- function(x) {
    if(is.null(x)) return(NULL)
    rw <- datedt$date_n[datedt$id == x$id]
    paste0(rw)
  }
  
  # ggvis date 
  datedt %>% ggvis(x = ~Date, y = ~date_n, key := ~id,stroke:="#1DA1F2") %>%
    layer_lines(strokeWidth := 3) %>%
    layer_points() %>%
    add_tooltip(hover1, "hover") %>%
    bind_shiny("bydate")
  
  
  output$datetable <- renderDataTable({
    
    data
  })
  
  # hour ggvis
  # create hour table
  hourly <- data %>%
    mutate(hour=hour(created)) %>%
    group_by(hour) %>%
    summarise(hour_n = n()) %>%
    mutate(Hour=as.integer(hour))
  
  hourly$id <- 1:nrow(hourly)  # Add an id column to use ask the key
  
  
  # create hover2 function for ggvis
  hover2 <- function(x) {
    if(is.null(x)) return(NULL)
    row <- hourly$hour_n[hourly$id == x$id]
    paste0(row)
  }
  
  # ggvis date 
  hourly %>% ggvis(x = ~Hour, y = ~hour_n, key := ~id,stroke:="#1DA1F2") %>%
    layer_lines(strokeWidth := 2) %>%
    layer_points() %>%
    add_tooltip(hover2, "hover") %>%
    bind_shiny("byhour")
  
}#end of ifinteractive



# Run the application 
shinyApp(ui = ui, server = server)

