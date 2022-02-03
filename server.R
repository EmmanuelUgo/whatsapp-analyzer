server <- function(input, output, session) {
    
    
    observe({
        
        req(input$v_upload)
        
        #Processing the file importation
        progress <- shiny::Progress$new()
        progress$set(message = "File processing", value = 0)
        on.exit(progress$close())
        progress$set(detail = "2 minutes...", value = 0.5)
        data <- input$v_upload
        if(is.null(data)){return(NULL)} 
        if(grepl("\\.zip$", data)=="TRUE"){
            df_data<-rwa_read(unzip(data$datapath,files="_chat.txt")) %>% 
                filter(!is.na(author), !text %in% c("Se eliminó este mensaje")) %>% 
                mutate(time = as.Date(time))
        } else {
            df_data<-rwa_read(data$datapath) %>% 
                filter(!is.na(author), !text %in% c("Se eliminó este mensaje")) %>% 
                mutate(time = as.Date(time))
        }
        
        
        
        output$date_range <- renderUI({
            
            dateRangeInput("daterange1", "Date range:",
                           start = min(as.Date(df_data$time), na.rm = TRUE),
                           end   = max(as.Date(df_data$time), na.rm = TRUE),
                           width = '50%')
            
        })
        
        filter_data <- reactive({
            
            validate(
                need(input$daterange1, 'Input valid dates')
            )
            
            df_data %>% 
                filter(between(time,input$daterange1[1], input$daterange1[2]))
            
        })
        
        
        output$n_messages <- renderInfoBox({
            nrow <-NROW(df_data)
            value_box(prettyNum(nrow, big.mark = ","),
                      subtitle = tags$p(strong("Total number of messages"),
                                        style="font-family:cambria;font-size: 100%;font-weight: bold"),
                      icon = icon("envelope"),
                      color = ""
            )
            
        })
        
        output$n_users <- renderInfoBox({
            
            req(filter_data)
            
            target_df <- filter_data()
            users <- length(unique(target_df[!is.na(target_df$author),]$author))
            value_box(prettyNum(users, big.mark = ","),
                      subtitle = tags$p(strong("Total number of Users"),
                                        style="font-family:cambria;font-size: 100%;font-weight: bold"),
                      icon = icon("users"),
                      color = ""
            )
            
        })
        
        output$top_user <- renderInfoBox({
            req(filter_data)
            
            target_df <- filter_data()
            top_user <- slice(count(target_df[!is.na(target_df$author),], author, sort = TRUE),1)
            user <- as.character(top_user$author)
            count <- prettyNum(top_user$n, big.mark = ",")
            value_box(tags$p(user, style = "font-size: 50%;"),
                      subtitle = tags$p(paste("# of Chats: ", count),
                                        style="font-family:cambria;font-size: 100%;font-weight: italic"),
                      color = ""
            )
            
        })
        
        output$wordcloud <- renderPlot({
            
            validate(
                need(input$n_words, 'Please select a number')
            )
            
            filter_data() %>%
                mutate(type = ifelse(text == "<Multimedia omitido>","sticker","text")) %>% 
                filter(type == "text") %>% 
                unnest_tokens("word","text") %>% 
                anti_join(stop_words) %>% 
                select(word) %>% 
                filter(!str_detect(word, "\\d"),
                       !word %in% c("dey", "yea","ni", "no", "na","nah", "https")) %>% 
                count(word, sort = TRUE) %>% 
                with(wordcloud(word, n, max.words = input$n_words,colors = T))
        })
        
        output$emoji_plot <- renderPlotly({
            req(filter_data)
            
            g <-  filter_data() %>% 
                mutate(type = ifelse(text == "<Multimedia omitido>","sticker","text")) %>% 
                filter(type == "text") %>% 
                count(author, sort = T) %>% 
                slice_max(n = 20, order_by = n, with_ties = FALSE) %>% 
                mutate(author = fct_reorder(author, n)) %>% 
                ggplot(aes(author, n)) +
                geom_col() +
                coord_flip() +
                labs(title = "Top 20 Users",
                     subtitle = "This data is based on text only.\nIt excludes stickers, images, etc",
                     x = "",
                     y = "count\n") 
            
            ggplotly(g)
        })
        
    })
}
