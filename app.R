# This is with vectors
# datasets n>4

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(tidyr)
library(shinythemes)
beg <- Sys.time() # starts calculating the time for loading the data sets
load("single.RData")
load("bigrams.RData")
load("trigrams.RData")
end <- Sys.time() # end calculating time
print(end - beg) # print time difference in the R console


# functions

# makes first letter of a string capital
firstcap <- function(mystr) {
    paste0(toupper(substr(mystr,1,1)),substr(mystr,2,nchar(mystr)))
}

# append punctuation to the last word if separated
lastappend <- function(vector) {
    last <- vector[length(vector)]
    seclast <- vector[length(vector)-1]
    if(last == "." | last == "!" | last == "?"){
        vector <- vector[-length(vector)]
        vector[length(vector)] <- paste0(seclast,last)
    }
    return(vector)
}

# checks for punctuation on the last element
checkend <- function(vector) {
    last <- vector[length(vector)]
    lastCh <- substr(last, nchar(last),nchar(last)+1)
    if(lastCh  == "." | lastCh == "!" | lastCh == "?") {
        return(T)
    } else {
        return(F)
    }
}

# Define UI for application 
ui <- fluidPage(theme = shinytheme("united"),
                tabsetPanel(
                    tabPanel("Application", 
                             # Application title
                             fluidRow(h1("Text entry prediction application.", align = "center")),
                             # application interface
                             fluidRow(column(7, offset = 2, textInput(inputId = "textentry",label = h3("Type your text here:"), width = "80%"), align = "center"),
                                      column(2, actionButton(inputId = "space", label = "[ space ]"), actionButton(inputId = "erase", "[ <-back ]"))),
                             fluidRow( tags$div(id = "numbs", h3("1 2 3")), align = "center"),
                             fluidRow(column(4, offset = 4, uiOutput("moreControls"), align = "center")),
                             # output text
                             fluidRow(h3(textOutput(outputId = "finaltext"), align = "center")),
                             
                             # css styles
                             tags$style(type='text/css', 
                                        "#space { margin-top: 60px;}",
                                        "#erase { margin-top: 60px;}",
                                        "#numbs { letter-spacing: 10px;}",
                                        "#info { padding-left: 20%;
                                        padding-right:20%;
                                        text-align: justify;
                                        line-height: 1.5;}"),
                             
                             # JavaScripts
                             tags$script(HTML('
                                              $(document).keydown (function (e) {
                                              if (e.keyCode == 32) {
                                              $("#space").click();
                                              } else if (e.keyCode == 97 || e.altKey && e.keyCode == 49) {
                                              $("#word1").click();
                                              } else if (e.keyCode == 98 || e.altKey && e.keyCode == 50) {
                                              $("#word2").click();
                                              } else if (e.keyCode == 99 || e.altKey && e.keyCode == 51) {
                                              $("#word3").click();
                                              } else if (e.keyCode == 8){
                                              $("#erase").click();
                                              }
                                              });
                                              '))
                             ),
                    tabPanel("Instructions",
                             fluidRow(h2("How to use the Text entry prediction application.", align = "center")),
                             # Application explanation
                             fluidRow(h4(HTML("<div id = 'info'><p>
                                              This application predicts the next word in the sentence.<br>
                                              You should type words in the text entry box, the application will predict the next word in the sentence.<br> 
                                              It is suggesting 3  possible words, to add a word from the sugested you can click on the <b>word</b>, pres <b>Num1</b>, <b>Num2</b> or <b>Num3</b> or pres <b>Alt+1</b>, <b>Alt+2</b> or <b>Alt+3</b>.<br> 
                                              Clicking on <b>[space]</b> button  or <b>space key</b> on the keyboard will add the word in the textentry box to the sentence.<br>
                                              To remove the last letter from the word in the textentry box click <b>[<-back]</b> button or <b>backspace key</b> on the keyboard.
                                              If the textentry box is empty pressing <b>[<-back]</b> button or <b>backspace key</b> on the keyboard will remove the last word from the sentence.
                                              </p></div>"), align = "center"))
                             )
                             )
                             )





# Define server logic 
server <- function(input,output, session){ 
    
    text  <- reactiveValues(data = c(""))
    inputlenght <- reactive(nchar(input$textentry))
    
    # give the first 3 words from single words list
    first3_s <- function(data) {
        slice(data, grep(paste0("^", input$textentry), word) ) %>%
            slice(1:3) %>% 
            select(word) %>% 
            unlist(use.names = F)
    }
    
    # give the first 3 words from bigrams list
    first3_b <- function(data) {
        filter(data, word1 == text$data[length(text$data)]) %>% 
            slice(grep(paste0("^",input$textentry), word2)) %>%
            slice(1:3) %>% 
            select(word2) %>% 
            unlist(use.names = F)
    }
    
    # give the first 3 words from trigrams list
    first3_t <- function(data){
        filter(data,word1 == text$data[length(text$data)-1] , word2 == text$data[length(text$data)]) %>% 
            slice(grep(paste0("^",input$textentry), word3)) %>%
            slice(1:3) %>% 
            select(word3) %>% 
            unlist(use.names = F)
    }
    
    # adds words from bigrams and single when there are missing (NA) in the first 3 words
    reorder <- function(tri, bi, single){
        beg <- Sys.time() # start calculating time to find words
        tri_new <- vector("character")
        if(missing(tri)) {
            tri_new <- append(tri_new, bi[!is.na(bi)])
            tri_new <- append(tri_new, single)
            tri_new <- unique(tri_new)
        } else{
            tri_new <- append(tri_new, tri[!is.na(tri)])
            tri_new <- append(tri_new, bi[!is.na(bi)])
            tri_new <- append(tri_new, single)
            
            tri_new <- unique(tri_new)
        }
        end <- Sys.time() # end calculating time
        print(end - beg) # print time difference in the R console
        tri_new
    }
    
    # make list of the 3 most likely words
    first3 <- reactive({
        if(length(text$data) == 1){
            first3_s(single)
        } else if (length(text$data) == 2){
            reorder(bi = first3_b(bigrams), single = first3_s(single))
        } else if(length(text$data) > 2){
            reorder(first3_t(trigrams) , first3_b(bigrams), first3_s(single))
        }
    })
    
    
    # draws the word entry buttons
    output$moreControls <- renderUI({
        tagList(
            actionButton(inputId = "word1", first3()[1]),
            actionButton(inputId = "word2", first3()[2]),
            actionButton(inputId = "word3", first3()[3])
        )
    })
    
    # adding words to output text when word button or space is activated 
    observeEvent(input$word1, {
        if(length(text$data) < 2 | checkend(text$data) == T){
            text$data = append(text$data,firstcap(first3()[1]) )
            updateTextInput(session, "textentry", value = "")
        } else {
            text$data = append(text$data,first3()[1] )
            updateTextInput(session, "textentry", value = "")
        }
    })
    observeEvent(input$word2, {
        if(length(text$data) < 2 | checkend(text$data) == T){
            text$data = append(text$data,firstcap(first3()[2]) )
            updateTextInput(session, "textentry", value = "")
        } else {
            text$data = append(text$data,first3()[2] )
            updateTextInput(session, "textentry", value = "")
        }
    })
    observeEvent(input$word3, {
        if(length(text$data) < 2 | checkend(text$data) == T){
            text$data = append(text$data,firstcap(first3()[3]) )
            updateTextInput(session, "textentry", value = "")
        } else {
            text$data = append(text$data,first3()[3] )
            updateTextInput(session, "textentry", value = "")
        }
    })
    observeEvent(input$space, {
        if(length(text$data) < 2 | checkend(text$data) == T) {   # to be implemeted to the words!!!!!!
            print(checkend(text$data))
            text$data = append(text$data,firstcap(input$textentry))
            updateTextInput(session, "textentry", value = "")
        } else {
            text$data = append(text$data,input$textentry)
            text$data = lastappend(text$data)
            updateTextInput(session, "textentry", value = "")
        }
        
    })
    
    # functionality for erase words or letters
    observeEvent(input$erase, {
        if(inputlenght() > 0 ) {
            updateTextInput(session, "textentry", value = substr(input$textentry,1, inputlenght()-1))
        } else if(length(text$data) > 1) {
            text$data <- text$data[-length(text$data)]
        }
        
    })
    
    # drows the output text
    output$finaltext <- renderText({
        text$data
    })
    
}    


shinyApp(ui, server)

