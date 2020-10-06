#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tm)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Opiniones parlamentarias"),
    
    sidebarLayout(
        sidebarPanel(
            
            tags$div(class="header", checked=NA,
                     list(
                         tags$p("Actualmente, la app cuenta con las intervenciones de 22 diputados/as en ejercicio. 
                                Puedes leer más sobre ellos en el siguiente enlace:"),
                         tags$a(href="https://www.bcn.cl/index_html", "Biblioteca del Congreso"),
                         tags$p("Primero, elige a un diputado o diputada en esta lista:")
                     )
            ),
            
            # Selector de diputado
            selectInput("persona", "¿Cuál es la opinión de:",
                        c("Gabriel Boric Font" = "Gabriel Boric Font",
                          "Camila Vallejo Dowling " = "Camila Vallejo Dowling",
                          "Jorge Brito Hasbún" = "Jorge Brito Hasbún",
                          "Claudia Mix Jiménez" = "Claudia Mix Jiménez",
                          "Luciano Cruz-Coke Carvallo" = "Luciano Cruz-Coke Carvallo",
                          "Renato Garín González" = "Renato Garín González",
                          "Gonzalo Winter Etcheberry" = "Gonzalo Winter Etcheberry",
                          "Pamela Jiles Moreno" = "Pamela Jiles Moreno",
                          "Daniella Cicardini Milla" = "Daniella Cicardini Milla",
                          "Gastón Saavedra Chandía" = "Gastón Saavedra Chandía",
                          "Camila Flores Oporto" = "Camila Flores Oporto",
                          "Pedro Alvarez-Salamanca Ramírez" = "Pedro Alvarez-Salamanca Ramírez",
                          "Gael Yeomans Araya" = "Gael Yeomans Araya",
                          "Vlado Mirosevic Verdugo" = "Vlado Mirosevic Verdugo",
                          "Maya Fernández Allende" = "Maya Fernández Allende",
                          "Leonardo Soto Ferrada" = "Leonardo Soto Ferrada",
                          "Andrés Longton Herrera" = "Andrés Longton Herrera",
                          "Maite Orsini Pascal" = "Maite Orsini Pascal",
                          "Florcita Alarcón Rojas" = "Florcita Alarcón Rojas",
                          "Patricio Rosas Barrientos" = "Patricio Rosas Barrientos",
                          "Jaime Tohá González" = "Jaime Tohá González",
                          "Jorge Alessandri Vergara" = "Jorge Alessandri Vergara")),
            
            tags$div(class="header", checked=NA,
                     list(
                         tags$p("Ahora, elige un tema de interés. Puede que no todos/as los/as diputados/as tengan
                                una opinión disponible en la base de datos.")
                     )
            ),
            
            # Selector de temática
            selectInput("about", "sobre el tema del/la:",
                        c("Aborto" = "aborto",
                          "Constitución" = "constitucion",
                          "Aguas" = "agua",
                          "Dictadura" = "dictadura",
                          "Salud" = "salud",
                          "Educación" = "educación",
                          "Lucro" = "lucro",
                          "Estallido social" = "estallido")),
            
            tags$div(class="header", checked=NA,
                     list(
                         tags$p("Por último, elige cuántas opiniones sobre el diputado/a quieres leer.
                                Estas son un resumen extraido de su intervención parlamentaria.")
                     )
            ),
            
       #     numericInput("n_textos", "Opinión Nº:",
        #                 value = 1,
         #                min = 1,
          #               max = 3,
           #              step = 1)
            radioButtons("n_textos", "Opinión Nº:",
                         choices = list("Intervención 1" = 1,
                                        "Intervención 2" = 2,
                                        "Intervención 3" = 3)
            
        ),
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            htmlOutput("titular"),
            htmlOutput("data"),
            htmlOutput("link"),
            plotOutput("analisis")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    library(tidytext)
    library(tm)
    library(textrank)
    
    diputados1 <- readRDS("datos.RDS") %>% 
        rename(texto = ...8) %>% 
        select(ID, fecha, tema, legislatura, enlace, diputado, texto) %>% 
        na.omit()
    
    query <- function(q,tfidftdm,k,fixed)
    {
        ## q        : query
        ## tfidftdm : TermDocumentMatrix with tf-idf
        ## k        : number of documents to retrieve
        ## fixed    : strict (science -> science) or flexible search (science -> science, neuroscience, ...)
        
        ## implementing the logic
        ## 1- splitting the string on "OR" into AND logic blocks
        or.split <- unlist(strsplit(q, " or "))
        
        ## 2- group the terms queried via "AND" 
        terms.list <- sapply(or.split, function(x) strsplit(x, " and "))
        
        ## 3- Initializing the results vector and searching through every "logic
        ## block" ( -> "OR")
        results <- c()
        for (l in terms.list)
        {    
            ## if we should match the query words.
            if (fixed)
            {
                ## find the rows/words in the             
                rows <- which(rownames(tfidftdm) %in% l)
                
                ## if the word is not in the corpus, stop
                if(length(rows) == 0) stop("¡Ups! Parece ser que no tengo una opinión pública al respecto. Bip Bop.")            
                
                ## list of documents containing the words in the query
                docs <- list()
                for (r in rows)
                {
                    sub.matrix <- as.matrix(tfidftdm[r,])
                    d <- which(sub.matrix != 0)
                    docs <- append(docs, list(d))
                }
                
                ## performing the "AND" logic by intersect the datasets
                cols <- Reduce(intersect, docs)
                
            }
            ## We add some flexibility to the search by also including documents
            ## that have words of a similar root than those in the query.
            ## For example, querying "science" will also consider "neuroscience"
            else
            {            
                
                ## list of documents containing the words in the query
                docs <- list()
                for (t in l)
                {                
                    ## stemming the query-word(s) for flexible search 
                    new.rows <- stemDocument(t)
                    
                    ## find the rows/words with "similar" words
                    rows <- which(grepl(new.rows,rownames(tfidftdm)))
                    
                    ## if the word is not in the corpus, stop               
                    if(length(rows) == 0) stop("¡Ups! Parece ser que no tengo una opinión pública al respecto. Bip Bop.")            
                    
                    ## initialise an aggregator of all files with "similar" words
                    agg <- c()
                    for (r in rows)
                    {                    
                        sub.matrix <- as.matrix(tfidftdm[r,])
                        d <- which(sub.matrix != 0)
                        agg <- unique(c(agg,d))
                        
                    }
                    
                    docs <- append(docs,list(agg))
                } 
                
                ## performing the "AND" logic by intersect the datasets
                cols <- Reduce(intersect, docs)
            }
            
        }
        
        ## 4-finally, adding the tfidf scores...
        scores <- apply(as.matrix(tfidftdm[rows,cols]), 2, sum)
        
        ## adding names to the vector...
        doc.names <- colnames(tfidftdm)[cols]
        names(scores) <- doc.names
        
        ## aggregating results..
        results <- c(results,scores)
        
        ## sorting results based on score, descending order...
        results <- sort(results, decreasing = TRUE)
        
        ## and drop duplicated from the "OR" join and return k documents
        is.duplicated <- which(duplicated(names(results)))
        if (any(is.duplicated))   results <- results[-is.duplicated]
        
        return(results[1:k])
        
    }
    
    custom_stop_words <- tibble(word = quanteda::stopwords("spanish"))
    
    output$data <- renderUI({
        #review_corpus = Corpus(VectorSource(diputados1$texto))
        #review_corpus = tm_map(review_corpus, content_transformer(tolower))
        #review_corpus = tm_map(review_corpus, removeNumbers)
        #review_corpus = tm_map(review_corpus, removePunctuation)
        #review_corpus = tm_map(review_corpus, removeWords, c("diputado", "diputada",
        #                                                   "señor", "señora", "presidente",
        #                                                    stopwords("spanish")))
        #review_corpus =  tm_map(review_corpus, stripWhitespace)
        #tfidftdm <- TermDocumentMatrix(review_corpus, control=list(stopwords=TRUE,
        #                                                           weighting=weightTfIdf))
        
        selecto <- diputados1 %>% 
            filter(diputado == input$persona)
        
        
        docs <- data.frame(doc_id = selecto$ID,
                           text = selecto$texto,
                           diputado = selecto$diputado,
                           dmeta1 = selecto[2:4],
                           stringsAsFactors = T)
        ds <- DataframeSource(docs)
        ds <- Corpus(ds)
        ds = tm_map(ds, content_transformer(tolower))
        ds = tm_map(ds, removeNumbers)
        ds = tm_map(ds, removePunctuation)
        ds = tm_map(ds, removeWords, c("diputado", "diputada",
                                       "señor", "señora", "presidente",
                                       stopwords("spanish")))
        ds =  tm_map(ds, stripWhitespace)
        
        dip_elegido <- meta(ds, "diputado") == input$persona
        dip_opinion <- TermDocumentMatrix(ds[dip_elegido], control=list(stopwords=TRUE,
                                                                        weighting=weightTfIdf))
        
        
        fnames.list <- selecto$ID
        colnames(dip_opinion) <- fnames.list
        words <- rownames(dip_opinion)
        
        q1 <- query(input$about, 
                    dip_opinion, 
                    k = 5,
                    fixed = F) %>% 
            attr('names') %>% 
            na.omit()
        
        diputados1 %>% 
            filter(ID == q1) %>% 
            .$texto -> piensa
        
        diputados1 %>% 
            filter(ID == q1) %>% 
            .$fecha -> fechaopinion
        
        diputados_sentences <- tibble(piensa) %>%
            #filter(ID == q1) %>% 
            unnest_tokens(sentence, piensa, token = "sentences") %>%
            mutate(sentence_id = row_number()) %>%
            select(sentence_id, sentence)
        
        diputados_wordssentences <- diputados_sentences %>%
            unnest_tokens(word, sentence)
        
        diputados_wordssentences <- diputados_wordssentences %>%
            anti_join(custom_stop_words, by = "word")
        
        article_summary <- textrank_sentences(data = diputados_sentences, 
                                              terminology = diputados_wordssentences)
        
        HTML(paste0("<ul><li><i>",summary(article_summary)[as.numeric(input$n_textos)],"</i></li></ul>"))
        
        
        
        
    })
    
    output$link <- renderUI(
        {
            
            selecto <- diputados1 %>% 
                filter(diputado == input$persona)
            
            
            docs <- data.frame(doc_id = selecto$ID,
                               text = selecto$texto,
                               diputado = selecto$diputado,
                               dmeta1 = selecto[2:4],
                               stringsAsFactors = T)
            ds <- DataframeSource(docs)
            ds <- tm::Corpus(ds)
            ds = tm_map(ds, content_transformer(tolower))
            ds = tm_map(ds, removeNumbers)
            ds = tm_map(ds, removePunctuation)
            ds = tm_map(ds, removeWords, c("diputado", "diputada",
                                           "señor", "señora", "presidente",
                                           stopwords("spanish")))
            ds =  tm_map(ds, stripWhitespace)
            
            dip_elegido <- meta(ds, "diputado", type = "indexed") == input$persona
            dip_opinion <- TermDocumentMatrix(ds[dip_elegido], control=list(stopwords=TRUE,
                                                                            weighting=weightTfIdf))
            
            
            fnames.list <- selecto$ID
            colnames(dip_opinion) <- fnames.list
            words <- rownames(dip_opinion)
            
            q1 <- query(input$about, 
                        dip_opinion, 
                        k = 5,
                        fixed = F) %>% 
                attr('names')
            
            diputados1 %>% 
                filter(ID == q1) %>% 
                .$enlace -> opinioncompleta
            
            paste("Puedes leer mi opinión completa en", opinioncompleta)
        }
    )
    
    output$titular <- renderUI(
        {
            
            selecto <- diputados1 %>% 
                filter(diputado == input$persona)
            
            
            docs <- data.frame(doc_id = selecto$ID,
                               text = selecto$texto,
                               diputado = selecto$diputado,
                               dmeta1 = selecto[2:4],
                               stringsAsFactors = T)
            ds <- DataframeSource(docs)
            ds <- Corpus(ds)
            ds = tm_map(ds, content_transformer(tolower))
            ds = tm_map(ds, removeNumbers)
            ds = tm_map(ds, removePunctuation)
            ds = tm_map(ds, removeWords, c("diputado", "diputada",
                                           "señor", "señora", "presidente",
                                           stopwords("spanish")))
            ds =  tm_map(ds, stripWhitespace)
            
            dip_elegido <- meta(ds, "diputado") == input$persona
            dip_opinion <- TermDocumentMatrix(ds[dip_elegido], control=list(stopwords=TRUE,
                                                                            weighting=weightTfIdf))
            
            
            fnames.list <- selecto$ID
            colnames(dip_opinion) <- fnames.list
            words <- rownames(dip_opinion)
            
            q1 <- query(input$about, 
                        dip_opinion, 
                        k = 5,
                        fixed = F) %>% 
                attr('names')
            
            # Recupera la fecha de la emision
            diputados1 %>% 
                filter(ID == q1) %>% 
                .$fecha %>% 
                as.Date()-> fechaopinion
            
            HTML(paste0("<h2>","¡Hola! Soy un resumen de ", input$persona," ¡bip bop!","</h2>"),
                 paste0("<h4>", "El día ", format(fechaopinion, "%d-%m-%y"), " indiqué esto en torno al tema del/la ",
                        input$about, ":", "</h4>"))
            
            
        }
    )
    
    output$analisis <- renderPlot(
        {
            diputados1 %>%
                filter(diputado == input$persona) %>% 
                #select(-c(enlace, tema)) %>% 
                unnest_tokens(word, texto)  %>%
                anti_join(custom_stop_words) %>% 
                count(word, sort = T)  %>% 
                mutate(word = reorder(word, n)) %>% 
                head(20) %>% 
                ggplot(aes(x = word, 
                       y = n)) +
                geom_col() +
                xlab(NULL) +
                coord_flip()
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
