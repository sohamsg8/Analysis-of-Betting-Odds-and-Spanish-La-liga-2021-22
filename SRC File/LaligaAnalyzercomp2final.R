library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyverse)
library(gridExtra)
library(dashboardthemes)
ui <- dashboardPage(
  
  dashboardHeader(title="Project component 2: La Liga Analyzer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About the dataset",tabName = "menu3"),
      menuItem("Match-Team wise Analysis",tabName = "menu1"),
      menuItem("Betting Analysis",tabName = "menu2"),
      menuItem("Conclusion",tabName = "menu4")
    )
  ),
  dashboardBody(shinyDashboardThemes(
    theme = "flat_red"
  ),
    tabItems(
      tabItem("menu1",h1("Match-Team wise Analysis"),fluidPage(
        sidebarLayout(
          sidebarPanel(
            selectInput("coloumn","choose team",choices = c("Match_Results","Half_Time_Leading_Team","Full_time_Result","Home_Team_Goal_Stats",
                                                            "Away_Team_Goal_Stats","Home_Team_Discipline_Stats", "Away_Team_Discipline_Stats","Team_wise_goal_stat","Team_Wise_Discipline_Stats")),
            actionButton("go","apply")
          ),
          mainPanel(plotOutput("chart"),textOutput("write"))
        )
      )),
      tabItem("menu2",h1("Betting Plot"),fluidPage(
        sidebarLayout(
          sidebarPanel(
            selectInput("coloumn1","choose betting provider",choices = c("B365_Home","BWH_Home",
                                                                          "IWH_Home","PSH_Home","WHH_Home","VCH_Home","Max_Home","Average_Home",
                                                                         "B365_Draw","BWH_Draw",
                                                                         "IWH_Draw","PSH_Draw","WHH_Draw","VCH_Draw","Max_Draw","Average_Draw",
                                                                         "B365_Away","BWH_Away",
                                                                         "IWH_Away","PSH_Away","WHH_Away","VCH_Away","Max_Away","Average_Away")),
            actionButton("go1","Apply")
          ),
          mainPanel(plotlyOutput("chart1"),verbatimTextOutput("write1"))
        )
      )),
      tabItem("menu3",h1("About the Project"),fluidPage(
        tags$b(tags$h2("Introduction")),
        tags$ul(tags$h4("Sports betting is a rapidly growing industry that obtained a worldwide market size of over 200 billion United States (US) dollars in 2019 (Ibisworld, 2020). 
        In total, there are over 30,000 sports-betting-related businesses globally (Ibisworld, 2020). Prior to the COVID-19 pandemic, the sports-betting industry in the regions of Asia, the Middle East, and South America had grown at above-average rates (Ibisworld, 2020), while in 2021 weekly sports betting in the United States doubled (Morning Consult, 2022).
        Football is the most popular sport in the world , evidently betting in football also is quite common. In view of unpredictability of the game , betting providers use much statistical approach in estimating the odds. The birth and subsequent explosion of the internet at the end of the 1990s truly revolutionised the way we bet on football. 
        The new school bookies, such as Betfair and bet365, began to pop up in abundance, offering an entirely online operation. Football data became more easily available and betters at individual level started to try comeup with their own predictions.
        Here we take the data on Spanish top division (La Liga) data for 2021-22 season and see the changes on betting odds provided by different betters with course of match and how they relate to various match results. Also we see the unpredictability of a match by seeing the extent of change in result at full time as compared to that in halftime.")), 
        tags$b(tags$h2("Data Description")),
        tags$ul(tags$h4("The data is obtained from the European Football Database (https://www.football-data.co.uk/). This is a database that have detailed match data of different tiers of europe's
        top 11 football leagues from 1993 onwards. For our purpose , Spanish top division (La Liga) data of 2021-22 season has been taken. The data set provides detailed data of every matcg including result,
                        shots taken , shot on target, fouls committed, disciplinary statistics and more. It also provides betting odds data by different betting providers.")),
        tags$b(tags$h2("Summary Analysis")),
        tags$b(tags$h4("Summary analysis shows that full time home goal for the season to be 1.421 whereas that of away team being 1.082, which signifies home team being more successful in the season. Also each match averages 2.5 goals per match which reveals the competitive nature of the tournament. It also reveals that average half time goals to be just above 1 , which signifies that majority of the goals were scored in the first half in the season."))
        
      )),
      tabItem("menu4",h1("CONCLUSION"),fluidPage(
        tags$b(tags$h4("From the above analysis it is clear that the chance of home team winning is higher hence betting providers tend to give a higher probability (1/odds) to the home team. Also we see that the competition is highly competitive with change in half and full time result being very frequent. For season 2021-22 it has been seen the team with most shot and best disciplinary record throughout the tournament has won the league.
It has been shown that there are much fluctuation in the odds provided by the betting provider at the start of the match to what a actual match may be. It is highly risky to bet given the scenario and betters have to have their own knowledge. Finally , we see that sometimes the betting odds totally gets very eratical especially for away team win or draw , which provides a higher opportunity of upset and hence a greater return."))
      
      ))
      
    )
  )
)
server <- function(input,output){
  data <- read.csv("./data/SP1.csv")
  data$pair <- 0.5*(data$FTHG+data$FTAG)*(data$FTHG+data$FTAG+1)+data$FTAG
  observeEvent(input$go,
               if(input$coloumn=="Match_Results"){
                 pair_freq <- data %>% 
                   count(pair)
                 
                 
               
                 output$chart<-renderPlot({
                   left_join(x = data, y = pair_freq, by = "pair") %>% 
                   ggplot(aes(FTHG, FTAG)) +
                     geom_point(aes(color = FTR, size = n)) +
                     geom_text(aes(label = n, hjust = -1.8)) +
                     guides(size = "none")+
                     labs(title = "Scatter Plot Showing Match Result", x="Home Team Goal", y="Away Team Goal")
                  
                 })
                 output$write<-renderPrint({
                   print("The scatter plot have home team goal on the x axis and away team goal on the y axis , size of the point
corresponds to no. of time that result occurs and the color of the point corresponds to the result of the match.
The plot reveals that most of the match result were concentrated at low scoring games (<3 goals) with 1-0 being the most common score line , followed by 2-0 win for home. We also see that highest goal scored in a match was 2-6 win for away team. Also highest no. of goals scored by a team in a single match being 6, which occurred 3 times. Also , we can clearly see that more matches have been won by the home team.")
                 })}
               else if(input$coloumn=="Full_time_Result"){
                  
                 output$chart<-renderPlot({
                   data %>% 
                     count(FTR) %>% 
                     mutate(n = n/sum(n)) %>%
                   ggplot(aes(y = n, fill = FTR, x = "")) +
                     geom_col() + 
                     coord_polar(theta = "y") + labs(title = "Distribution of result(full time)")
                 })
                 output$write<-renderPrint({
                   print("Most matches have been won by the won team which is just below 50%, followed by drawn matches and then the matches in which away team wins.")
                 })
               }
               else if(input$coloumn=="Half_Time_Leading_Team"){
        
                 output$chart<-renderPlot({
                   data %>% 
                     count(HTR) %>% 
                     mutate(n = n/sum(n)) %>%
                   ggplot(aes(y = n, fill = HTR, x = "")) +
                     geom_col() + 
                     coord_polar(theta = "y") + labs(title = "Distribution of result(half time)")
                 })
                 output$write<-renderPrint({
                   print("Most of the game at half time are draw")
                 })
               }
               else if(input$coloumn=="Home_Team_Goal_Stats"){
                 Hgoal<- sum(data$FTHG)
                 Hshots<- sum(data$HS)
                 Hst<- sum(data$HST)
                 Hcorner<-sum(data$HC)
                 H_goal_stat<-c(Hshots,Hst,Hgoal,Hcorner)
                 M<-c("Home Team Shots", "Home Team Shot on Target","Home Goals","Home Corner")
                 output$chart<-renderPlot({
                   barplot(H_goal_stat,names.arg=M,col="blue",
                           main="Home Team Goal Stat",border="red")
                 })
                 
               }
               else if(input$coloumn=="Away_Team_Goal_Stats"){
                 Agoal<- sum(data$FTAG)
                 Ashots<- sum(data$AS)
                 Ast<- sum(data$AST)
                 Acorner<-sum(data$AC)
                 A_goal_stat<-c(Ashots,Ast,Agoal,Acorner)
                 N<-c("Away Team Shots", "Away Team Shot on Target","Away Goals","Away Corner")
                 output$chart<-renderPlot({
                   barplot(A_goal_stat,names.arg=N,col="red",
                           main="Away Team Goal Stat",border="blue")
                 })
               }
               else if(input$coloumn=="Home_Team_Discipline_Stats"){
                 HFoul<- sum(data$HF)
                 HYellow<- sum(data$HY)
                 HRed<- sum(data$HR)
                 H_discipline<-c(HFoul,HYellow,HRed)
                 O<-c("Home Team Foul","Home Team Yellow Card","Home Team Red Card")
                 output$chart<-renderPlot({
                   barplot(H_discipline,names.arg=O,col="blue",
                           main="Home Team Discipline",border="red")
                 })
                 
               }
               else if(input$coloumn=="Away_Team_Discipline_Stats"){
                 AFoul<- sum(data$AF)
                 AYellow<- sum(data$AY)
                 ARed<- sum(data$AR)
                 A_discipline<-c(AFoul,AYellow,ARed)
                 P<-c("Away Team Foul","Away Team Yellow Card","Away Team Red Card")
                 output$chart<-renderPlot({
                   barplot(A_discipline,names.arg=P,col="red",
                           main="Away Team Discipline",border="blue")
                 })
               }
               else if(input$coloumn=="Team_wise_goal_stat"){
                 teams <- data %>% 
                   distinct(HomeTeam)
                 
                 teams <- as.array(teams$HomeTeam)
                 
                 home_stat = data.frame()
                 for(team in teams){
                   temp = data.frame(team_name = team)
                   temp <- cbind(temp, data %>% 
                                   filter(HomeTeam == team) %>% 
                                   summarise(total_shots = sum(HS),
                                             total_shots_on_target = sum(HST),
                                             total_goals = sum(FTHG)))
                   home_stat <- rbind(home_stat, temp)
                 }
                 away_stat = data.frame()
                 for(team in teams){
                   temp = data.frame(team_name = team)
                   temp <- cbind(temp, data %>% 
                                   filter(AwayTeam == team) %>% 
                                   summarise(total_shots = sum(AS),
                                             total_shots_on_target = sum(AST),
                                             total_goals = sum(FTAG)))
                   away_stat <- rbind(away_stat, temp)
                 }
                 
                 output$chart<-renderPlot({
                   left_join(home_stat, away_stat, by = "team_name") %>% 
                     mutate(total_shots = total_shots.x + total_shots.y,
                            total_shots_on_target = total_shots_on_target.x + total_shots_on_target.y,
                            total_goals = total_goals.x + total_goals.y) %>% 
                     select(total_shots, total_shots_on_target, total_goals, team_name) %>% 
                     pivot_longer(total_shots:total_goals, names_to = "category", values_to = "value")%>%
                     ggplot(aes(fill = category, x = team_name, y = value)) +
                     geom_bar(position = "stack", stat = "identity") + 
                     theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1))+labs(title = "Distribution of Shot and goal per team")
                 })
                 output$write<-renderPrint({
                   print("Plot gives us an indepth analysis of no. of shot taken , shot on target and goal scored by each team in the competition . It is quite evident that the maximum no. of shot taken , as well as shot on target and goal scored was by the eventual league winner Real Madrid, which is not surprising. Followed by Barcelona who finished second. Lowest no. of shots were taken by Elche who finished 13th , whereas lowest goals were scored by Alaves who finished last.")
                 })
               }
               else if(input$coloumn=="Team_Wise_Discipline_Stats"){
                 teams <- data %>% 
                   distinct(HomeTeam)
                 
                 teams <- as.array(teams$HomeTeam)
                 
                 home_stat = data.frame()
                 for(team in teams){
                   temp = data.frame(team_name = team)
                   temp <- cbind(temp, data %>% 
                                   filter(HomeTeam == team) %>% 
                                   summarise(total_foul = sum(HF),
                                             total_yellow = sum(HY),
                                             total_red = sum(HR)))
                   home_stat <- rbind(home_stat, temp)
                 }
                 away_stat = data.frame()
                 for(team in teams){
                   temp = data.frame(team_name = team)
                   temp <- cbind(temp, data %>% 
                                   filter(AwayTeam == team) %>% 
                                   summarise(total_foul = sum(AF),
                                             total_yellow = sum(AY),
                                             total_red = sum(AR)))
                   away_stat <- rbind(away_stat, temp)
                 }
                 output$chart<-renderPlot({
                   left_join(home_stat, away_stat, by = "team_name") %>% 
                     mutate(total_foul = total_foul.x + total_foul.y,
                            total_yellow = total_yellow.x + total_yellow.y,
                            total_red = total_red.x + total_red.y) %>% 
                     select(total_foul, total_yellow, total_red, team_name) %>% 
                     pivot_longer(total_foul:total_red, names_to = "category", values_to = "value")%>%
                     ggplot(aes(fill = category, x = team_name, y = value)) +
                     geom_bar(position = "stack", stat = "identity") + 
                     theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1))+labs(title = "Distribution of fouls and cards")
                 })
                 output$write<-renderPrint({
                   print("Plot gives us an indepth analysis of disciplinary statistics of each team throughout the season which consists of no. of fouls committed , yellow card received and red card received. It is quite evident from the plot that highest no. of fouls commited along with highest yellow and red card was received by Valencia who finished 9th. On contrary the most disciplined team of the tournament being the champion Real Madrid. This plot also shows that high correlation between fouls commited and no. of cards received.")
                 })
               }
               
  )
  
  observeEvent(input$go1,
               if(input$coloumn1=="B365_Home"){
                 data$B365H_change<-data$B365H-data$B365CH
                 data$sl <- c(seq(1,380))
                 output$chart1<-renderPlotly({
                   ggplot(data,aes(x=sl)) + geom_line(aes(y = B365H_change), color = "red")+
                     labs(title = "Bet365 Home team Change")
                   
                 })
                 output$write1<-renderText({
                   print("Change in Odds for Home team before and at the closing moment")
                 })
               }
               else if(input$coloumn1=="BWH_Home"){
                 data$BWH_change<-data$BWH-data$BWCH
                 data$sl <- c(seq(1,380))
                 output$chart1<-renderPlotly({
                   ggplot(data,aes(x=sl)) + geom_line(aes(y = BWH_change), color = "blue")+
                     labs(title = "Bet&Win Home team Change")
                 })
                 output$write1<-renderText({
                   print("Change in Odds for Home team before and at the closing moment")
                 })
               }
               else if(input$coloumn1=="IWH_Home"){
                 data$IWH_change<-data$IWH-data$IWCH
                 data$sl <- c(seq(1,380))
                 output$chart1<-renderPlotly({
                   ggplot(data,aes(x=sl)) + geom_line(aes(y = IWH_change), color = "orange")+
                     labs(title = "Interwetten Home Change")
                 })
                 output$write1<-renderText({
                   print("Change in Odds for Home team before and at the closing moment")
                 })
               }
               else if(input$coloumn1=="PSH_Home"){
                 data$PSH_change<-data$PSH-data$PSCH
                 data$sl <- c(seq(1,380))
                 output$chart1<-renderPlotly({
                   ggplot(data,aes(x=sl)) + geom_line(aes(y = PSH_change), color = "green")+
                     labs(title = "Pinnacle Home Change")
                 })
                 output$write1<-renderText({
                   print("Change in Odds for Home team before and at the closing moment")
                 })
               }
               else if(input$coloumn1=="WHH_Home"){
                 data$WHH_change<-data$WHH-data$WHCH
                 data$sl <- c(seq(1,380))
                 output$chart1<-renderPlotly({
                   ggplot(data,aes(x=sl)) + geom_line(aes(y = WHH_change), color = "black")+
                     labs(title = "William Hill Home Change")
                 })
                 output$write1<-renderText({
                   print("Change in Odds for Home team before and at the closing moment")
                 })
               }
               else if(input$coloumn1=="VCH_Home"){
                 data$VCH_change<-data$VCH-data$VCCH
                 data$sl <- c(seq(1,380))
                 output$chart1<-renderPlotly({
                   ggplot(data,aes(x=sl)) + geom_line(aes(y = VCH_change), color = "purple")+
                     labs(title = "VC Bet Home Change")
                 })
                 output$write1<-renderText({
                   print("Change in Odds for Home team before and at the closing moment")
                 })
               }
               else if(input$coloumn1=="Max_Home"){
                 data$MaxH_change<-data$MaxH-data$MaxCH
                 data$sl <- c(seq(1,380))
                 output$chart1<-renderPlotly({
                   ggplot(data,aes(x=sl)) + geom_line(aes(y = MaxH_change), color = "purple")+
                     labs(title = "Maximum Bet Home Change")
                 })
                 output$write1<-renderText({
                   print("Change in Odds for Maximum bet on Home team before and at the closing moment")
                 })
               }
               else if(input$coloumn1=="Average_Home"){
                 data$Avg_changeH<-data$AvgH-data$AvgCH
                 data$sl <- c(seq(1,380))
                 output$chart1<-renderPlotly({
                   ggplot(data,aes(x=sl)) + geom_line(aes(y = Avg_changeH), color = "Red")+
                     labs(title = "Home")
                 })
                 output$write1<-renderText({
                   print("Change in Odds for Average bet on Home team before and at the closing moment")
                 })
               }
               else if(input$coloumn1=="B365_Draw"){
                 data$B365D_change<-data$B365D-data$B365CD
                 data$sl <- c(seq(1,380))
                 output$chart1<-renderPlotly({
                   ggplot(data,aes(x=sl)) + geom_line(aes(y = B365D_change), color = "red")+
                     labs(title = "Bet365 Draw Change")
                   
                 })
                 output$write1<-renderText({
                   print("Change in Odds for Draw team before and at the closing moment")
                 })
               }
               else if(input$coloumn1=="BWH_Draw"){
                 data$BWD_change<-data$BWD-data$BWCD
                 data$sl <- c(seq(1,380))
                 output$chart1<-renderPlotly({
                   ggplot(data,aes(x=sl)) + geom_line(aes(y = BWD_change), color = "blue")+
                     labs(title = "Bet&Win Draw Change")
                 })
                 output$write1<-renderText({
                   print("Change in Odds for Draw before and at the closing moment")
                 })
               }
               else if(input$coloumn1=="IWH_Draw"){
                 data$IWD_change<-data$IWD-data$IWCD
                 data$sl <- c(seq(1,380))
                 output$chart1<-renderPlotly({
                   ggplot(data,aes(x=sl)) + geom_line(aes(y = IWD_change), color = "orange")+
                     labs(title = "Interwetten Draw Change")
                 })
                 output$write1<-renderText({
                   print("Change in Odds for Draw before and at the closing moment")
                 })
               }
               else if(input$coloumn1=="PSH_Draw"){
                 data$PSD_change<-data$PSD-data$PSCD
                 data$sl <- c(seq(1,380))
                 output$chart1<-renderPlotly({
                   ggplot(data,aes(x=sl)) + geom_line(aes(y = PSD_change), color = "green")+
                     labs(title = "Pinnacle Draw Change")
                 })
                 output$write1<-renderText({
                   print("Change in Odds for Draw before and at the closing moment")
                 })
               }
               else if(input$coloumn1=="WHD_Home"){
                 data$WHD_change<-data$WHD-data$WHCD
                 data$sl <- c(seq(1,380))
                 output$chart1<-renderPlotly({
                   ggplot(data,aes(x=sl)) + geom_line(aes(y = WHD_change), color = "black")+
                     labs(title = "William Hill Draw Change")
                 })
                 output$write1<-renderText({
                   print("Change in Odds for Draw before and at the closing moment")
                 })
               }
               else if(input$coloumn1=="VCH_Draw"){
                 data$VCD_change<-data$VCD-data$VCCD
                 data$sl <- c(seq(1,380))
                 output$chart1<-renderPlotly({
                   ggplot(data,aes(x=sl)) + geom_line(aes(y = VCD_change), color = "purple")+
                     labs(title = "VC Bet Draw Change")
                 })
                 output$write1<-renderText({
                   print("Change in Odds for Draw before and at the closing moment")
                 })
               }
               else if(input$coloumn1=="Max_Draw"){
                 data$MaxD_change<-data$MaxD-data$MaxCD
                 data$sl <- c(seq(1,380))
                 output$chart1<-renderPlotly({
                   ggplot(data,aes(x=sl)) + geom_line(aes(y = MaxD_change), color = "purple")+
                     labs(title = "Maximum Bet Draw Change")
                 })
                 output$write1<-renderText({
                   print("Change in Odds for Maximum bet on Draw before and at the closing moment")
                 })
               }
               else if(input$coloumn1=="Average_Draw"){
                 data$Avg_changeD<-data$AvgD-data$AvgCD
                 data$sl <- c(seq(1,380))
                 output$chart1<-renderPlotly({
                   ggplot(data,aes(x=sl)) + geom_line(aes(y = Avg_changeD), color = "Red")+
                     labs(title = "Draw")
                 })
                 output$write1<-renderText({
                   print("Change in Odds for Average bet on Draw before and at the closing moment")
                 })
               }
               else if(input$coloumn1=="B365_Away"){
                 data$B365A_change<-data$B365A-data$B365CA
                 data$sl <- c(seq(1,380))
                 output$chart1<-renderPlotly({
                   ggplot(data,aes(x=sl)) + geom_line(aes(y = B365A_change), color = "red")+
                     labs(title = "Bet365 Away team Change")
                   
                 })
                 output$write1<-renderText({
                   print("Change in Odds for Away team before and at the closing moment")
                 })
               }
               else if(input$coloumn1=="BWH_Away"){
                 data$BWA_change<-data$BWA-data$BWCA
                 data$sl <- c(seq(1,380))
                 output$chart1<-renderPlotly({
                   ggplot(data,aes(x=sl)) + geom_line(aes(y = BWA_change), color = "blue")+
                     labs(title = "Bet&Win Away team Change")
                 })
                 output$write1<-renderText({
                   print("Change in Odds for Away team before and at the closing moment")
                 })
               }
               else if(input$coloumn1=="IWH_Away"){
                 data$IWA_change<-data$IWA-data$IWCA
                 data$sl <- c(seq(1,380))
                 output$chart1<-renderPlotly({
                   ggplot(data,aes(x=sl)) + geom_line(aes(y = IWA_change), color = "orange")+
                     labs(title = "Interwetten Away Change")
                 })
                 output$write1<-renderText({
                   print("Change in Odds for Away team before and at the closing moment")
                 })
               }
               else if(input$coloumn1=="PSH_Away"){
                 data$PSA_change<-data$PSA-data$PSCA
                 data$sl <- c(seq(1,380))
                 output$chart1<-renderPlotly({
                   ggplot(data,aes(x=sl)) + geom_line(aes(y = PSA_change), color = "green")+
                     labs(title = "Pinnacle Away Change")
                 })
                 output$write1<-renderText({
                   print("Change in Odds for Away team before and at the closing moment")
                 })
               }
               else if(input$coloumn1=="WHH_Away"){
                 data$WHA_change<-data$WHA-data$WHCA
                 data$sl <- c(seq(1,380))
                 output$chart1<-renderPlotly({
                   ggplot(data,aes(x=sl)) + geom_line(aes(y = WHA_change), color = "black")+
                     labs(title = "William Hill Away Change")
                 })
                 output$write1<-renderText({
                   print("Change in Odds for Away team before and at the closing moment")
                 })
               }
               else if(input$coloumn1=="VCH_Away"){
                 data$VCA_change<-data$VCA-data$VCCA
                 data$sl <- c(seq(1,380))
                 output$chart1<-renderPlotly({
                   ggplot(data,aes(x=sl)) + geom_line(aes(y = VCA_change), color = "purple")+
                     labs(title = "VC Bet Away Change")
                 })
                 output$write1<-renderText({
                   print("Change in Odds for Away team before and at the closing moment")
                 })
               }
               else if(input$coloumn1=="Max_Away"){
                 data$MaxA_change<-data$MaxA-data$MaxCA
                 data$sl <- c(seq(1,380))
                 output$chart1<-renderPlotly({
                   ggplot(data,aes(x=sl)) + geom_line(aes(y = MaxA_change), color = "purple")+
                     labs(title = "Maximum Bet Away Change")
                 })
                 output$write1<-renderText({
                   print("Change in Odds for Maximum bet on Away team before and at the closing moment")
                 })
               }
               else if(input$coloumn1=="Average_Away"){
                 data$Avg_changeA<-data$AvgA-data$AvgCA
                 data$sl <- c(seq(1,380))
                 output$chart1<-renderPlotly({
                   ggplot(data,aes(x=sl)) + geom_line(aes(y = Avg_changeA), color = "Red")+
                     labs(title = "Away")
                 })
                 output$write1<-renderText({
                   print("Change in Odds for Average bet on Away team before and at the closing moment")
                 })
               }
  )

  
  
  
  
}

shinyApp(ui,server)
