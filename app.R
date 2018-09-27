library(shiny)
library(tidyverse)

dfm1 <- readRDS("unigram.rds")
dfm2 <- readRDS("bigram.rds")
dfm3 <- readRDS("trigram.rds")

search_function <- function(x, ...){
        x <- tolower(x) # lower case of the string. 
        
        x <- str_trim(x) # trim the whitespaces.  
        
        w_minus_2 <- paste0("^", word(x, -2), "$") # the second word in the string from the right. 
        
        w_minus_1 <- paste0("^", word(x, -1), "$") # the first word in the string from the right. 
        
        # Find trigrams that match the two words from the right of the input string. 
        find_trigram <- str_detect(dfm3$w3, pattern = w_minus_2) & str_detect(dfm3$w2, pattern = w_minus_1)
        
        # find bigrams that match the first word of the input string from the right. 
        find_bigram <- str_detect(dfm2$w2, pattern = w_minus_1) 
        
        # find unigrams that match the first words of the input string from the right. 
        find_unigram <- str_detect(dfm1$w1, pattern = w_minus_1)
        
        if(isTRUE(sum(find_trigram) > 0)){ # words are matched in trigram. 
                
                t = dfm3[find_trigram, ]  #subset the trigram. 
                
                # calculate the score in stupid backoff model. 
                ## numerator： the count of trigram starting with the two words to the total number of *trigrams*.  
                ## denominator: the count of bigram with the two words to the total number of *bigrams*. 
                t$score =  t[, 4]/sum(dfm2[find_bigram, ]$freq)
                
                # create an output dataframe. 
                ## next_word is the third word in the trigrams that match the first words. 
                ## log_score is the log calculation of the score in stupid backoff. 
                df <- data.frame(next_word = t$w1, log_score = log(t$score))
                
                #print the first 6 row of the prediction. 
                print(head(df))
                
        }else{# If the first two words are not matched in the trigram. 
                
                if(isTRUE(sum(find_bigram) >0)){# detect if the last words in the string can be found in bigram. 
                        
                        t = dfm2[find_bigram, ] # Subset the bigram that start with the last words in input string. 
                        
                        # calculate the score in stupid backoff model.
                        ## numerator： the proportion of bigrams starting with the last word to the total number of bigrams.
                        ## denominator: the proportion of unigram with the last word to the total number of unigrams. 
                        # 0.4 as the empirical discount in stupid backoff. 
                        t$score = 0.4 * t[, 3]/sum(dfm1[find_unigram, ]$freq) 
                        
                        # create an output dataframe.
                        ## next_word is the third word in the bigrams that match the first words.
                        ## log_score is the log calculation of the score in stupid backoff. 
                        df <- data.frame(next_word = t$w1, 
                                         log_score = log(t$score))    
                        
                        print(head(df))
                        
                }else{# if the bigrams starting with the last word in the string are not found. 
                        t = head(dfm1) # select the top words in unigram. 
                        # calculate the score of the most common words
                        # two recursive 0.4, i.e.,0.4^2 is the discount factor in stupid backoff. 
                        t$score = 0.4^2 * t[, 2]/854743 # 854743 as the total number of corpus. 
                        
                        
                        # create an output dataframe.
                        ## next_word is the third word in the bigrams that match the first words.
                        ## log_score is the log calculation of the score in stupid backoff. 
                        df = data.frame(next_word = t$w1, 
                                        log_score = log(t$score))
                        
                        print(head(df))
                        
                }
        }
}


ui <- fluidPage(
        tags$h1("Datascinece Capstone Project: Predicting the next word."), 
        tags$h5("You can find the final Rmarkdown report at: "), 
        tags$a(href = "https://github.com/Rui-Lian/datascience-capstone/blob/master/Datascinece%20final%20report.Rmd", "Final Report"), 
        tags$h5("and the shiny application code at: "),
        tags$a(href = "https://github.com/Rui-Lian/datascience-capstone/blob/master/app.R", "Shiny application"), 

        sidebarLayout(
                sidebarPanel(
                        textInput(inputId = "text", 
                                  label = "input word string in the text box below.", 
                                  value = "")
                ), 
                mainPanel(
                        tags$h5("output here"), 
                        tableOutput("prediction")
                )
        )

)

server <- function(input, output){
        data <- reactive(input$text)
        output$prediction <- renderTable({
                search_function(data())
        })
}

shinyApp(ui = ui, server = server)


