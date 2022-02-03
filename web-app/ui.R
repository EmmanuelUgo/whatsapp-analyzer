ui <- semanticPage(title = "Whatsapp Analyzer", 
                   includeCSS("www/styles.css"),
                   
                   h2("Whatsapp Analyzer"),
                   sidebar_layout(
                       
                       sidebar_panel(
                           
                           file_input("v_upload",
                                      "Upload WhatsApp Chat Data:",
                                      multiple = FALSE,
                                      accept = c(".zip",".txt")),
                           
                           tags$br(),
                           
                           uiOutput("date_range"),
                           
                           tags$br(),
                           
                           p("Select No of Words"),
                           conditionalPanel("output.n_messages != NULL",
                                            slider_input("n_words",value = 100, min = 50, 
                                                         max = 200,step= 50, class = "labeled ticked")),
                           
                           hr()
                           
                       ),
                       
                       main_panel(
                           segment(class = "piled segment",
                                   
                                   cards(
                                       class = "three",
                                       
                                       card(class = "red",
                                            div(class = "content",
                                                div(class = "header", "Message Information"),
                                                div(class = "description", value_box_output("n_messages")))),
                                       
                                       card(class = "blue",
                                            div(class = "content",
                                                div(class = "header", "User Information"), 
                                                div(class = "description",  value_box_output("n_users")))),
                                       
                                       card(class = "orange",
                                            div(class = "content",
                                                div(class = "header", "Top User"), 
                                                div(class = "description",  value_box_output("top_user"))))
                                   )
                           ),
                           
                           segment(class = "piled segment",
                                   cards(
                                       class = "two",
                                       
                                       card(class = "blue",
                                            plotOutput("wordcloud")),
                                       
                                       card(class = "blue",
                                            plotlyOutput("emoji_plot"))
                                   ))
                       )
                   )
)

