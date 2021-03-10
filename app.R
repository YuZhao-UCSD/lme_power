#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
library(shiny)

ui <- fluidPage(
    
    # Application title
    headerPanel("Power curve for longitudinal study"),
    mainPanel(width=8,plotOutput("distPlot")),
    fluidRow(
        column(4,numericInput("power_goal", "Power (%) you want to achieve:",80,min = 5,max = 100),
               strong('Minimum sample size for group 1 and group 2 with the design below (Scenario 1) to achieve power above is: '),
               verbatimTextOutput("n_need"),
               strong('Minimum sample size for group 1 and group 2 with the design below (Scenario 2) to achieve power above is: '),
               verbatimTextOutput("n_need_2")),
        
        ),
    tabsetPanel(tabPanel("Design",
                         fluidRow(
                             column(width = 6, titlePanel("Scenario 1"),
                                    sidebarPanel(width = 12,sliderInput("nvis",
                                                                        "Number of follow-up visits:",
                                                                        min = 1,
                                                                        max = 30,
                                                                        value = 6)
                                    ),
                                    column(6,numericInput("t_interval", "Time between visits (in years):",0.25)),
                                    column(6,numericInput("p", "Subject attrition rate for group 1:",0,min = 0)),
                                    column(6,numericInput("p_gp2", "Subject attrition rate for group 2:",0,min = 0)),
                                    column(6,numericInput("lambda", "Allocation ratio (= (sample size group 1)/(sample size group 2)):",1,min = 0)),
                                    column(6,numericInput("delta", "Effect size (the amount of slope change you want to detect):",.25*4.0579))
                             ),
                             column(width = 6, titlePanel("Scenario 2 (optional)"),
                                    sidebarPanel(width = 12,
                                                 sliderInput("nvis_2",
                                                             "Number of follow-up visits:",
                                                             min = 1,
                                                             max = 30,
                                                             value = 6)
                                    ),
                                    column(6,numericInput("t_interval_2", "Time between visits (in years):",0.25)),
                                    column(6,numericInput("p_2", "Subject attrition rate for group 1:",0,min = 0)),
                                    column(6,numericInput("p_2_gp2", "Subject attrition rate for group 2:",0,min = 0)),
                                    column(6,numericInput("lambda_2", "Allocation ratio (= (sample size group 1)/(sample size group 2)):",1,min = 0)),
                                    column(6,numericInput("delta_2", "Effect size (the amount of slope change you want to detect):",.25*4.0579))
                             ),
                         )),
                tabPanel("Covariance Parameters",
                         fluidRow(
                                column(width = 6,
                                    titlePanel("Scenario 1"),
                                    column(6,numericInput("sig2.s", "Random slope variance:",15.7150,min = 0)),
                                    column(6,numericInput("sig2.int", "Random intercept variance:",55.24,min = 0)),
                                    column(6,numericInput("sig2.e", "Random error variance:",13.731,min = 0)),
                                    column(6,numericInput("sig.b0b1", "Random intercept and slope covariance:",13.70)),
                                    column(6,numericInput("sig2.s_gp2", "Random slope variance (group 2):",15.7150,min = 0)),
                                    column(6,numericInput("sig2.int_gp2", "Random intercept variance (group 2):",55.24,min = 0)),
                                    column(6,numericInput("sig2.e_gp2", "Random error variance (group 2):",13.731,min = 0)),
                                    column(6,numericInput("sig.b0b1_gp2", "Random intercept and slope covariance (group 2):",13.70))
                             ),
                             column(width = 6, titlePanel("Scenario 2 (optional)"),
                                    column(6,numericInput("sig2.s_2", "Random slope variance:",15.7150,min = 0)),
                                    column(6,numericInput("sig2.int_2", "Random intercept variance:",55.24,min = 0)),
                                    column(6,numericInput("sig2.e_2", "Random error variance:",13.731,min = 0)),
                                    column(6,numericInput("sig.b0b1_2", "Random intercept and slope covariance:",13.70)),
                                    column(6,numericInput("sig2.s_2_gp2", "Random slope variance (group 2):",15.7150,min = 0)),
                                    column(6,numericInput("sig2.int_2_gp2", "Random intercept variance (group 2):",55.24,min = 0)),
                                    column(6,numericInput("sig2.e_2_gp2", "Random error variance (group 2):",13.731,min = 0)),
                                    column(6,numericInput("sig.b0b1_2_gp2", "Random intercept and slope covariance (group 2):",13.70))
                             ),
                         )),
                tabPanel('Plot Setting',
                         fluidRow(
                             column(4,numericInput("nsamp_low", "Sample size from:",50,min = 1)),
                             column(4,numericInput("nsamp_up", "to:",1000,min = 1)),
                             column(4,numericInput("nsamp_by", "by:",10,min = 1)),
                             column(4,textInput("col_cur", "Color of Scenario 1 curve:",'darkred')),
                             column(4,textInput("col_cur_2", "Color of Scenario 2 curve:",'darkblue')),
                         ))
                
                ),

)


server <- function(input, output) {
    
    output$distPlot <- renderPlot({
        library(longpower)
        library(ggplot2)
        t = input$t_interval*0:input$nvis
        t_2 = input$t_interval_2*0:input$nvis_2
        nsamp_gp1 = round(seq(input$nsamp_low,input$nsamp_up,by=input$nsamp_by)/(1+1/input$lambda))
        nsamp_gp1_2 = round(seq(input$nsamp_low,input$nsamp_up,by=input$nsamp_by)/(1+1/input$lambda_2))
        nsamp_fig_3 = seq(input$nsamp_low,input$nsamp_up,by=input$nsamp_by)
        t_fig_3_1 = edland.linear.power(nsamp_gp1,input$delta,t=t,lambda = input$lambda,
                                        sig2.int=input$sig2.int,sig.b0b1=input$sig.b0b1,sig2.s=input$sig2.s,
                                        sig2.e=input$sig2.e,
                                        sig2.int_2=input$sig2.int_gp2,sig.b0b1_2=input$sig.b0b1_gp2,sig2.s_2=input$sig2.s_gp2,
                                        sig2.e_2=input$sig2.e_gp2,
                                        p=c(rep(input$p,length(t)-1),1-sum(rep(input$p,length(t)-1))),
                                        p_2=c(rep(input$p_gp2,length(t)-1),1-sum(rep(input$p_gp2,length(t)-1))))$power
        t_fig_3_2 = edland.linear.power(nsamp_gp1_2,input$delta_2,t=t_2,lambda = input$lambda_2,
                                        sig2.int=input$sig2.int_2,sig.b0b1=input$sig.b0b1_2,sig2.s=input$sig2.s_2,
                                        sig2.e=input$sig2.e_2,
                                        sig2.int_2=input$sig2.int_2_gp2,sig.b0b1_2=input$sig.b0b1_2_gp2,sig2.s_2=input$sig2.s_2_gp2,
                                        sig2.e_2=input$sig2.e_2_gp2,
                                        p=c(rep(input$p_2,length(t_2)-1),1-sum(rep(input$p_2,length(t_2)-1))),
                                        p_2=c(rep(input$p_2_gp2,length(t_2)-1),1-sum(rep(input$p_2_gp2,length(t_2)-1))))$power
        cols = c("Scenario 1"=input$col_cur,"Scenario 2"=input$col_cur_2)
        
        ggplot() +
            geom_line(aes(x=nsamp_fig_3,y=t_fig_3_1,color="Scenario 1"),size=0.7) +
            geom_line(aes(x=nsamp_fig_3,y=t_fig_3_2,color="Scenario 2"),size=0.7) +
            geom_hline(yintercept = input$power_goal*0.01,colour = "black",linetype=2)+
            scale_color_manual(name="",values=cols) +
            ggtitle('') + xlab('Total Sample Size') + ylab('Power') + theme(plot.title = element_text(hjust = 0.5),legend.key = element_blank(),
                                                                            panel.background = element_rect(
                                                                                colour = "black", size = 0.5, linetype = "solid"),
                                                                            panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                                                                                            colour = "white"), 
                                                                            panel.grid.minor = element_line(size = 0.5, linetype = 'solid',
                                                                                                            colour = "white"))
        
       
    })
    
    output$n_need <- renderText({ 
        t = input$t_interval*0:input$nvis
        n_min <- edland.linear.power(delta = input$delta,power = 0.01*input$power_goal,t=t,lambda = input$lambda,
                            sig2.int=input$sig2.int,sig.b0b1=input$sig.b0b1,sig2.s=input$sig2.s,
                            sig2.e=input$sig2.e,
                            sig2.int_2=input$sig2.int_gp2,sig.b0b1_2=input$sig.b0b1_gp2,sig2.s_2=input$sig2.s_gp2,
                            sig2.e_2=input$sig2.e_gp2,
                            p=c(rep(input$p,length(t)-1),1-sum(rep(input$p,length(t)-1))),
                            p_2=c(rep(input$p_gp2,length(t)-1),1-sum(rep(input$p_gp2,length(t)-1))))$n
        paste(ceiling(n_min[1]),', ',ceiling(n_min[2]),sep = '')
    })
    
    output$n_need_2 <- renderText({ 
        t_2 = input$t_interval_2*0:input$nvis_2
        n_min_2 <- edland.linear.power(delta = input$delta_2,power = 0.01*input$power_goal,t=t_2,lambda = input$lambda_2,
                                     sig2.int=input$sig2.int_2,sig.b0b1=input$sig.b0b1_2,sig2.s=input$sig2.s_2,
                                     sig2.e=input$sig2.e_2,
                                     sig2.int_2=input$sig2.int_2_gp2,sig.b0b1_2=input$sig.b0b1_2_gp2,sig2.s_2=input$sig2.s_2_gp2,
                                     sig2.e_2=input$sig2.e_2_gp2,
                                     p=c(rep(input$p_2,length(t_2)-1),1-sum(rep(input$p_2,length(t_2)-1))),
                                     p_2=c(rep(input$p_2_gp2,length(t_2)-1),1-sum(rep(input$p_2_gp2,length(t_2)-1))))$n
        paste(ceiling(n_min_2[1]),', ',ceiling(n_min_2[2]),sep = '')
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

