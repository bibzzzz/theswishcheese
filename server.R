library(shiny)
#library(XML)
library(plyr)
library(stringr)
# library(data.table)
library(ggplot2)
library(RCurl)

high_perc <- 0.25
low_perc <- 0.75

gp_url <- getURL('https://raw.githubusercontent.com/bibzzzz/theswishcheese/master/gp_table.csv')
gp_table <- read.csv(text = gp_url)
player_proj_url <- getURL('https://raw.githubusercontent.com/bibzzzz/theswishcheese/master/player_proj.csv')
player_proj <- read.csv(text = player_proj_url)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  player_proj_input <- eventReactive(input$go, {
    return (data.frame(player_proj,value=(input$budget)/(input$squad_size)))
  })
  interim_valuation_table <- eventReactive(input$go, {
    
    progress <- shiny::Progress$new(session, min=1, max=input$n_iters + input$n_iters)
    on.exit(progress$close())
    
    progress$set(message = 'Simulation in progress',
                 detail = '\n This may take a while... (see progress bar)')
    
    player_proj <- player_proj_input()
    
    valuation_table <- NULL
    for (iter in 1:input$n_iters){  
      playerlist <- NULL
      agg_team_proj <- NULL
      i <- 1
      while (i <= input$n_samples){
        sample_i <- sample(1:nrow(player_proj),input$squad_size,replace=FALSE)
        team_proj_i <- player_proj[sample_i,]
        team_perf_i <- colSums(team_proj_i[,2:12])
        if ((team_perf_i[11]<=input$budget)&(team_perf_i[11]>=0.9*input$budget)){
          playerlist[i] <- list(team_proj_i$player)
          agg_team_proj <- rbind(agg_team_proj,team_perf_i)
          print(paste0(iter,"/",input$n_iters," - ",i,"/",input$n_samples))
          i <- i + 1}
      }
      
      agg_team_proj <- data.frame(agg_team_proj)
      
      agg_team_proj$FGPCT <- agg_team_proj$FGM/agg_team_proj$FGA
      agg_team_proj$FTPCT <- agg_team_proj$FTM/agg_team_proj$FTA
      
      agg_team_proj <- agg_team_proj[,c("TPM","REB","AST","STL","BLK","PTS","FGPCT","FTPCT")]
      
      scored_projections <- rowSums(sapply(agg_team_proj, function(x) rank(x)))
      
      scored_projections_table <- data.frame(id=1:input$n_samples,z_score=(scored_projections-mean(scored_projections))/sd(scored_projections))
      scored_projections_table$rank <- rank(-scored_projections_table$z_score)
      top_squads <- scored_projections_table$id[scored_projections_table$rank<=input$n_samples*high_perc]
      inc_players <- data.frame(player=unlist(playerlist[top_squads]))
      increase_table <- count(inc_players)
      names(increase_table)[2] <- "increase_points"
      bottom_squads <- scored_projections_table$id[scored_projections_table$rank>=input$n_samples*low_perc]
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
      #print(paste0(iter,"/",input$n_iters))
      progress$set(value = iter)
    }
    
    
    ##### ONCE TOP 120 PLAYERS ISOLATED RERUN VALUATIONS #####
    player_proj <- player_proj[rank(-player_proj$value, ties.method = "random")<=input$n_teams*input$squad_size,]
    player_proj$value <- rep(input$budget/input$squad_size,length(player_proj$value))
    
    valuation_table <- NULL
    for (iter in 1:input$n_iters){  
      playerlist <- NULL
      agg_team_proj <- NULL
      i <- 1
      while (i <= input$n_samples){
        sample_i <- sample(1:nrow(player_proj),input$squad_size,replace=FALSE)
        team_proj_i <- player_proj[sample_i,]
        team_perf_i <- colSums(team_proj_i[,2:12])
        if ((team_perf_i[11]<=input$budget)&(team_perf_i[11]>=0.9*input$budget)){
          playerlist[i] <- list(team_proj_i$player)
          agg_team_proj <- rbind(agg_team_proj,team_perf_i)
          print(paste0(iter,"/",input$n_iters," - ",i,"/",input$n_samples))
          i <- i + 1}
      }
      
      agg_team_proj <- data.frame(agg_team_proj)
      
      agg_team_proj$FGPCT <- agg_team_proj$FGM/agg_team_proj$FGA
      agg_team_proj$FTPCT <- agg_team_proj$FTM/agg_team_proj$FTA
      
      agg_team_proj <- agg_team_proj[,c("TPM","REB","AST","STL","BLK","PTS","FGPCT","FTPCT")]
      
      scored_projections <- rowSums(sapply(agg_team_proj, function(x) rank(x)))
      
      scored_projections_table <- data.frame(id=1:input$n_samples,z_score=(scored_projections-mean(scored_projections))/sd(scored_projections))
      scored_projections_table$rank <- rank(-scored_projections_table$z_score)
      top_squads <- scored_projections_table$id[scored_projections_table$rank<=input$n_samples*high_perc]
      inc_players <- data.frame(player=unlist(playerlist[top_squads]))
      increase_table <- count(inc_players)
      names(increase_table)[2] <- "increase_points"
      bottom_squads <- scored_projections_table$id[scored_projections_table$rank>=input$n_samples*low_perc]
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
      #print(paste0(iter,"/",input$n_iters))
      progress$set(value = iter + input$n_iters)
    }
    return (valuation_table)
  })
  
  final_valuation_table <- eventReactive(input$go, {
    valuation_table <- interim_valuation_table()[interim_valuation_table()$iter==input$n_iters,]
    full_valuation_table <- merge(valuation_table,gp_table,all.x=TRUE)
    full_valuation_table$GP <- ifelse(is.na(full_valuation_table$GP),mean(gp_table$GP),full_valuation_table$GP)
    full_valuation_table$final_value <- (full_valuation_table$GP/sum(full_valuation_table$GP))*full_valuation_table$value
    full_valuation_table$final_value <- (input$budget*input$n_teams)*(full_valuation_table$final_value/sum(full_valuation_table$final_value))
    return (full_valuation_table)
  })
  output$player_proj_table <- renderTable({
    table <- interim_valuation_table()[interim_valuation_table()$iteration==input$n_iters,]
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
      xlab("\nPlayer") +
      ylab("Value\n") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
            , legend.position="none"
            , axis.ticks=element_blank()) +
      facet_wrap(~group, ncol=4, scales="free_x")
  }, height = 800, width = 800 )
  output$iterplot <- renderPlot({ 
    iter_plot_data <- rbind(interim_valuation_table()[,c("player","iteration","value")],data.frame(player=final_valuation_table()$player
                                                                                                   ,iteration=rep(max(interim_valuation_table()$iteration) + 1,length(final_valuation_table()$player))
                                                                                                   ,value=final_valuation_table()$final_value))
    ggplot(iter_plot_data[iter_plot_data$player%in%input$sel_players,]
           ,aes(x=iteration,y=value,colour=player)) +
      geom_path() +
      xlim(1,input$n_iters + 1) +
      xlab("\nIteration") +
      ylab("Player value\n") +
      scale_colour_discrete(name = "Player")
  },  height = 400, width = 800 )
  output$team_value <- renderText({ 
    final_val_table <- final_valuation_table()[final_valuation_table()$player%in%input$sel_players,]
    team_value <- sum(final_val_table$final_value)
    return (paste0("The current selection of players is valued at ",round(team_value,0)," under the current league settings"))
  })
  output$sel_playerlist <- renderUI({
    selectizeInput("sel_players","Selected players:", choices = levels(final_valuation_table()$player), selected = NULL, multiple = TRUE,
                   options = NULL)
  })
  output$downloadData <- downloadHandler(
    filename = function() { paste(input$budget,input$n_teams,input$squad_size,input$n_iters,input$n_samples,'player_value_table.csv', sep='_') },
    content = function(file) {
      write.csv(final_valuation_table()[,c('player','final_value')], file, row.names = FALSE)
    }
  )
})
