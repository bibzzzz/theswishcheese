library(shiny)
#library(XML)
library(plyr)
library(stringr)
# library(data.table)
library(ggplot2)
library(RCurl)


n_iters <- 50
n_sec_iters <- 50
high_perc <- 0.25
low_perc <- 0.75
n_samples <- 500



# player_universe_size <- 600
# player_proj_tables_prefix <- "http://games.espn.go.com/fba/freeagency?leagueId=103954&startIndex="
# player_proj_tables_suffix <- "&context=freeagency&version=projections&avail=-1"
# sec_player_proj_tables_prefix <- "http://www1.fantasypros.com/nba/projections/avg-overall.php"
# data <- read.csv("/Users/amydonaldson/Documents/Habib/swish cheese/2016_valuations_complete.csv",header = TRUE)
# data <- data.table(data)

# player_proj <- NULL
# i <- 0
# while (i<=player_universe_size){
#   
#   table_url_i <- paste0(player_proj_tables_prefix,i,player_proj_tables_suffix)
#   table_i <- readHTMLTable(table_url_i)
#   player_table_i <- table_i$playertable_0
#   names(player_table_i) <- as.character(data.frame(lapply(player_table_i[1,], as.character)
#                                                    , stringsAsFactors=FALSE))
#   player_table_i <- player_table_i[2:nrow(player_table_i),c(1,6:18)]
#   i <- i + nrow(player_table_i)
#   
#   player_proj <- rbind(player_proj,player_table_i)
# }
# 
# player_proj <- unique(player_proj)
# player_proj$value <- 1
# player_proj <- player_proj[player_proj$PTS!='--',]
# 
# player_proj <- player_proj[,c(1,5:ncol(player_proj))]
# names(player_proj)[1] <- "player"
# names(player_proj)[2] <- "FGMA"
# names(player_proj)[4] <- "FTMA"
# names(player_proj)[6] <- "TPM"
# 
# player_proj$player <- sapply(strsplit(as.character(player_proj$player), ","), `[[`, 1)
# player_proj$player <- gsub("*","", player_proj$player , fixed=TRUE)
# player_proj$player <- gsub(" ","", player_proj$player , fixed=TRUE)
# player_proj$player <- gsub(".","", player_proj$player , fixed=TRUE)
# player_proj$player <- gsub("-","", player_proj$player , fixed=TRUE)
# player_proj$player <- gsub("'","", player_proj$player , fixed=TRUE)
# 
# 
# player_proj$FGM <- as.numeric(sapply(strsplit(as.character(player_proj$FGMA), "/"), `[[`, 1))
# player_proj$FGA <- as.numeric(sapply(strsplit(as.character(player_proj$FGMA), "/"), `[[`, 2))
# player_proj$FTM <- as.numeric(sapply(strsplit(as.character(player_proj$FTMA), "/"), `[[`, 1))
# player_proj$FTA <- as.numeric(sapply(strsplit(as.character(player_proj$FTMA), "/"), `[[`, 2))
# player_proj$REB <- as.numeric(as.character(player_proj$REB))
# player_proj$AST <- as.numeric(as.character(player_proj$AST))
# player_proj$STL <- as.numeric(as.character(player_proj$STL))
# player_proj$BLK <- as.numeric(as.character(player_proj$BLK))
# player_proj$PTS <- as.numeric(as.character(player_proj$PTS))
# player_proj[,"TPM"] <- as.numeric(as.character(player_proj[,"TPM"]))
# player_proj$GP <- 82
# 
# ### INCORPORATE SECONDARY DATA SOURCE ###
# 
# table_url_i <- paste0(sec_player_proj_tables_prefix)
# table_i <- readHTMLTable(table_url_i)
# sec_player_proj <- table_i$data
# 
# sec_player_proj <- unique(sec_player_proj)
# #sec_player_proj$value <- 1
# #sec_player_proj <- sec_player_proj[sec_player_proj$PTS!='--',]
# 
# names(sec_player_proj)[1] <- "player"
# names(sec_player_proj)[9] <- "TPM"
# 
# sec_player_proj$player <- gsub("(",")", sec_player_proj$player , fixed=TRUE)
# sec_player_proj$player <- sapply(strsplit(as.character(sec_player_proj$player), ')'), `[[`, 1)
# sec_player_proj$player <- gsub("*","", sec_player_proj$player , fixed=TRUE)
# sec_player_proj$player <- gsub(" ","", sec_player_proj$player , fixed=TRUE)
# sec_player_proj$player <- gsub(".","", sec_player_proj$player , fixed=TRUE)
# sec_player_proj$player <- gsub("-","", sec_player_proj$player , fixed=TRUE)
# sec_player_proj$player <- gsub("'","", sec_player_proj$player , fixed=TRUE)
# 
# sec_player_proj <- sec_player_proj[,c("player","PTS","REB","AST","BLK","STL","TPM","GP")]
# 
# 
# sec_player_proj$REB <- as.numeric(as.character(sec_player_proj$REB))
# sec_player_proj$AST <- as.numeric(as.character(sec_player_proj$AST))
# sec_player_proj$STL <- as.numeric(as.character(sec_player_proj$STL))
# sec_player_proj$BLK <- as.numeric(as.character(sec_player_proj$BLK))
# sec_player_proj$PTS <- as.numeric(as.character(sec_player_proj$PTS))
# sec_player_proj[,"TPM"] <- as.numeric(as.character(sec_player_proj[,"TPM"]))
# sec_player_proj$GP <- as.numeric(as.character(sec_player_proj$GP))
# sec_player_proj$GP <- ifelse(sec_player_proj$GP>82,82,sec_player_proj$GP)
# 
# temp_player_proj <- merge(player_proj,sec_player_proj,by="player",all.x=TRUE)
# 
# # gp_table <- data.frame(player=temp_player_proj$player,GP=(temp_player_proj$GP.x+temp_player_proj$GP.y)/2)
# gp_table <- data.frame(player=temp_player_proj$player[!is.na(temp_player_proj$GP.y)],GP=temp_player_proj$GP.y[!is.na(temp_player_proj$GP.y)])
# player_proj <- player_proj[,names(player_proj)!="GP"]
# 
# player_proj <- data.frame(player=temp_player_proj$player,
#                           FGM=temp_player_proj$FGM,
#                           FGA=temp_player_proj$FGA,
#                           FTM=temp_player_proj$FTM,
#                           FTA=temp_player_proj$FTA,
#                           "TPM"=ifelse(is.na(temp_player_proj[,"TPM.y"]),temp_player_proj[,"TPM.x"],(temp_player_proj[,"TPM.x"]+temp_player_proj[,"TPM.y"])/2),
#                           REB=ifelse(is.na(temp_player_proj[,"REB.y"]),temp_player_proj[,"REB.x"],(temp_player_proj[,"REB.x"]+temp_player_proj[,"REB.y"])/2),
#                           AST=ifelse(is.na(temp_player_proj[,"AST.y"]),temp_player_proj[,"AST.x"],(temp_player_proj[,"AST.x"]+temp_player_proj[,"AST.y"])/2),
#                           STL=ifelse(is.na(temp_player_proj[,"STL.y"]),temp_player_proj[,"STL.x"],(temp_player_proj[,"STL.x"]+temp_player_proj[,"STL.y"])/2),
#                           BLK=ifelse(is.na(temp_player_proj[,"BLK.y"]),temp_player_proj[,"BLK.x"],(temp_player_proj[,"BLK.x"]+temp_player_proj[,"BLK.y"])/2),
#                           PTS=ifelse(is.na(temp_player_proj[,"PTS.y"]),temp_player_proj[,"PTS.x"],(temp_player_proj[,"PTS.x"]+temp_player_proj[,"PTS.y"])/2))
# 
# write.csv(gp_table,'/Users/amydonaldson/Documents/Habib/dev/gp_table.csv',row.names=FALSE)
# write.csv(player_proj,'/Users/amydonaldson/Documents/Habib/dev/player_proj.csv',row.names=FALSE)


gp_url <- getURL('https://raw.githubusercontent.com/bibzzzz/theswishcheese/master/gp_table.csv')
gp_table <- read.csv(text = gp_url)
player_proj_url <- getURL('https://raw.githubusercontent.com/bibzzzz/theswishcheese/master/player_proj.csv')
player_proj <- read.csv(text = player_proj_url)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  player_proj_input <- reactive({
    return (data.frame(player_proj,value=(input$budget*input$n_teams*input$squad_size)/(input$budget*input$n_teams*input$squad_size)))
  })
  interim_valuation_table <- reactive({
    
    progress <- shiny::Progress$new(session, min=1, max=n_iters + n_sec_iters)
    on.exit(progress$close())
    
    progress$set(message = 'Simulation in progress',
                 detail = '\n This may take a while... (see progress underneath the address bar)')
    
    player_proj <- player_proj_input()
    
    valuation_table <- NULL
    for (iter in 1:n_iters){  
      playerlist <- NULL
      agg_team_proj <- NULL
      i <- 1
      while (i <= n_samples){
        sample_i <- sample(1:nrow(player_proj),input$squad_size,replace=FALSE)
        team_proj_i <- player_proj[sample_i,]
        team_perf_i <- colSums(team_proj_i[,2:12])
        if (team_perf_i[11]<=input$budget){
          playerlist[i] <- list(team_proj_i$player)
          agg_team_proj <- rbind(agg_team_proj,team_perf_i)
          print(paste0(iter,"/",n_iters," - ",i,"/",n_samples))
          i <- i + 1}
      }
      
      agg_team_proj <- data.frame(agg_team_proj)
      
      agg_team_proj$FGPCT <- agg_team_proj$FGM/agg_team_proj$FGA
      agg_team_proj$FTPCT <- agg_team_proj$FTM/agg_team_proj$FTA
      
      agg_team_proj <- agg_team_proj[,c("TPM","REB","AST","STL","BLK","PTS","FGPCT","FTPCT")]
      
      mean_matrix <- matrix(apply(agg_team_proj, 2, mean),n_samples,8,byrow=TRUE)
      sd_matrix <- matrix(apply(agg_team_proj, 2, sd),n_samples,8,byrow=TRUE)
      std_matrix <- (agg_team_proj-mean_matrix)/sd_matrix
      scored_projections <- data.frame(id=seq(1,n_samples,1),std_matrix,score=rowSums(std_matrix))
      scored_projections$rank <- rank(-scored_projections$score)
      top_squads <- scored_projections$id[scored_projections$rank<=n_samples*high_perc]
      inc_players <- data.frame(player=unlist(playerlist[top_squads]))
      increase_table <- count(inc_players)
      names(increase_table)[2] <- "increase_points"
      bottom_squads <- scored_projections$id[scored_projections$rank>=n_samples*low_perc]
      dec_players <- data.frame(player=unlist(playerlist[bottom_squads]))
      decrease_table <- count(dec_players)
      names(decrease_table)[2] <- "decrease_points"
      
      new_valuation_table <- merge(player_proj[,c("player","value")],increase_table,by=c("player"),all=TRUE)
      new_valuation_table <- merge(new_valuation_table,decrease_table,by=c("player"),all=TRUE)
      new_valuation_table$increase_points <- ifelse(is.na(new_valuation_table$increase_points),0,new_valuation_table$increase_points)
      new_valuation_table$decrease_points <- ifelse(is.na(new_valuation_table$decrease_points),0,new_valuation_table$decrease_points)
      new_valuation_table <- data.frame(new_valuation_table,adjustment=new_valuation_table$increase_points-new_valuation_table$decrease_points)
      new_valuation_table <- data.frame(new_valuation_table,
                                        new_value=ifelse(
                                          new_valuation_table$value+(new_valuation_table$adjustment-mean(new_valuation_table$adjustment))/sd(new_valuation_table$adjustment)<=0
                                          ,0
                                          ,new_valuation_table$value+(new_valuation_table$adjustment-mean(new_valuation_table$adjustment))/sd(new_valuation_table$adjustment)))
      player_proj <- merge(player_proj,new_valuation_table[,c("player","new_value")])
      player_proj$value <- player_proj$new_value
      valuation_table <- rbind(valuation_table,data.frame(player=player_proj$player,value=player_proj$new_value,iteration=iter))
      player_proj <- player_proj[,names(player_proj)!="new_value"]
      #print(paste0(iter,"/",n_iters))
      progress$set(value = iter)
    }
    
    
    ##### ONCE TOP 120 PLAYERS ISOLATED RERUN VALUATIONS #####
    player_proj <- player_proj[rank(-player_proj$value, ties.method = "random")<=input$n_teams*input$squad_size,]
    player_proj$value <- rep(1,length(player_proj$value))
    
    valuation_table <- NULL
    for (iter in 1:n_sec_iters){  
      playerlist <- NULL
      agg_team_proj <- NULL
      i <- 1
      while (i <= n_samples){
        sample_i <- sample(1:nrow(player_proj),input$squad_size,replace=FALSE)
        team_proj_i <- player_proj[sample_i,]
        team_perf_i <- colSums(team_proj_i[,2:12])
        if (team_perf_i[11]<=input$budget){
          playerlist[i] <- list(team_proj_i$player)
          agg_team_proj <- rbind(agg_team_proj,team_perf_i)
          print(paste0(iter,"/",n_sec_iters," - ",i,"/",n_samples))
          i <- i + 1}
      }
      
      agg_team_proj <- data.frame(agg_team_proj)
      
      agg_team_proj$FGPCT <- agg_team_proj$FGM/agg_team_proj$FGA
      agg_team_proj$FTPCT <- agg_team_proj$FTM/agg_team_proj$FTA
      
      agg_team_proj <- agg_team_proj[,c("TPM","REB","AST","STL","BLK","PTS","FGPCT","FTPCT")]
      
      mean_matrix <- matrix(apply(agg_team_proj, 2, mean),n_samples,8,byrow=TRUE)
      sd_matrix <- matrix(apply(agg_team_proj, 2, sd),n_samples,8,byrow=TRUE)
      std_matrix <- (agg_team_proj-mean_matrix)/sd_matrix
      scored_projections <- data.frame(id=seq(1,n_samples,1),std_matrix,score=rowSums(std_matrix))
      scored_projections$rank <- rank(-scored_projections$score)
      top_squads <- scored_projections$id[scored_projections$rank<=n_samples*high_perc]
      inc_players <- data.frame(player=unlist(playerlist[top_squads]))
      increase_table <- count(inc_players)
      names(increase_table)[2] <- "increase_points"
      bottom_squads <- scored_projections$id[scored_projections$rank>=n_samples*low_perc]
      dec_players <- data.frame(player=unlist(playerlist[bottom_squads]))
      decrease_table <- count(dec_players)
      names(decrease_table)[2] <- "decrease_points"
      
      new_valuation_table <- merge(player_proj[,c("player","value")],increase_table,by=c("player"),all=TRUE)
      new_valuation_table <- merge(new_valuation_table,decrease_table,by=c("player"),all=TRUE)
      new_valuation_table$increase_points <- ifelse(is.na(new_valuation_table$increase_points),0,new_valuation_table$increase_points)
      new_valuation_table$decrease_points <- ifelse(is.na(new_valuation_table$decrease_points),0,new_valuation_table$decrease_points)
      new_valuation_table <- data.frame(new_valuation_table,adjustment=new_valuation_table$increase_points-new_valuation_table$decrease_points)
      new_valuation_table <- data.frame(new_valuation_table,
                                        new_value=ifelse(
                                          new_valuation_table$value+(new_valuation_table$adjustment-mean(new_valuation_table$adjustment))/sd(new_valuation_table$adjustment)<=0
                                          ,0
                                          ,new_valuation_table$value+(new_valuation_table$adjustment-mean(new_valuation_table$adjustment))/sd(new_valuation_table$adjustment)))
      player_proj <- merge(player_proj,new_valuation_table[,c("player","new_value")])
      player_proj$value <- player_proj$new_value
      player_proj$value <- (input$budget*input$n_teams)*(player_proj$value/sum(player_proj$value))
      valuation_table <- rbind(valuation_table,data.frame(player=player_proj$player,value=player_proj$new_value,iteration=iter))
      player_proj <- player_proj[,names(player_proj)!="new_value"]
      #print(paste0(iter,"/",n_sec_iters))
      progress$set(value = iter + n_iters)
    }
    return (valuation_table)
  })
  
  final_valuation_table <- reactive({
    valuation_table <- interim_valuation_table()[interim_valuation_table()$iter==n_sec_iters,]
    full_valuation_table <- merge(valuation_table,gp_table,all.x=TRUE)
    full_valuation_table$GP <- ifelse(is.na(full_valuation_table$GP),mean(gp_table$GP),full_valuation_table$GP)
    full_valuation_table$final_value <- (full_valuation_table$GP/sum(full_valuation_table$GP))*full_valuation_table$value
    full_valuation_table$final_value <- (input$budget*input$n_teams)*(full_valuation_table$final_value/sum(full_valuation_table$final_value))
    return (full_valuation_table)
  })
  output$player_proj_table <- renderTable({
    table <- interim_valuation_table()[interim_valuation_table()$iteration==n_sec_iters,]
    return (table[,c("player","value")])
  }, include.rownames=FALSE)
  output$valueplot <- renderPlot({ 
    plot_data <- data.frame(final_valuation_table(),rank=rank(-final_valuation_table()$final_value,ties.method="first"))
    plot_data <- data.frame(plot_data,group=ceiling(plot_data$rank/10))
    plot_data$group <- paste(plot_data$group*10-9,"-",plot_data$group*10)
    plot_data <- plot_data[with(plot_data, order(rank)), ]
    plot_data$group <- factor(plot_data$group, levels = unique(plot_data$group[sort(plot_data$rank)]))
    plot_data$player <- factor(plot_data$player, levels = plot_data$player[rank(-plot_data$final_value,ties.method = "first")])
    plot_data$highlight <- factor(ifelse(plot_data$player%in%input$sel_players,1,0))
    ggplot(plot_data,aes(x=player,y=final_value,fill=highlight)) +
      geom_bar(stat="identity") +
      scale_fill_manual(values=c("#78c9a1","#ffc800")) +
      xlab("Player") +
      ylab("Value") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
            , legend.position="none"
            , axis.ticks=element_blank()) +
      facet_wrap(~group, ncol=4, scales="free_x")
  }, height = 800, width = 800 )
  output$iterplot <- renderPlot({ 
    ggplot(interim_valuation_table()[interim_valuation_table()$player%in%c("JamesHarden","KyleLowry", "TimDuncan","StephenCurry", "LeBronJames", "ChrisPaul", "AndreDrummond","MikeConley","KevinDurant","AnthonyDavis"),]
           ,aes(x=iteration,y=value,colour=player)) +
      geom_path()
  })
  output$team_value <- renderText({ 
    final_val_table <- final_valuation_table()[final_valuation_table()$player%in%input$sel_players,]
    team_value <- sum(final_val_table$final_value)
    return (paste0("The current selection of players is valued at ",round(team_value,0)," under the current league settings"))
  })
  output$sel_playerlist <- renderUI({
    selectizeInput("sel_players","Highlight players:", choices = levels(final_valuation_table()$player), selected = NULL, multiple = TRUE,
                   options = NULL)
  })
})
