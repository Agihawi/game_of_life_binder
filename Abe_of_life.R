library(tidyverse)
library(gganimate)
library(reshape2)

#Rules of the game:
# Any live cell with <2 live neighbours dies, as if by underpopulation.
# Any live cell with 2 or 3 live neighbours lives on to the next generation.
# Any live cell with >3 live neighbours dies, as if by overpopulation.
# Any dead cell with 3 live neighbours becomes a live cell, as if by reproduction.

#set grid size
n = 10

#set a maximum number of iterations to loop through
max_iterations <- 10

#generate factor levels for discrete ordering of  n grid size
factor_levels <- as.character(seq(1,n,1))


#define a function to count the number of neighbours of a cell -- parse the x and y values of matrix, 
count_friends <- function(grid) {
  #change grid to append 0's to end row and column
  grid <- rbind(grid, c(rep(0,n)))
  grid <- cbind(grid, c(rep(0,(n+1))))
  #iterate over x values
  for(i in 1:n){
    for(j in 1:n) {
      #subset the matrix based on i and j coordinates to find all neighbours
      subset_friends <- grid[(i-1):(i+1), (j-1):(j+1)]
      #Count number of neighbours -- subtract actual value of the cell of interest and assign to neighbour matrix
      neighbour_matrix[i,j] <<- sum(subset_friends) - grid[i,j]
    }
  }
}



#set a data frame to append things to later for gganimate
overall_frame<-  data.frame(stringsAsFactors=FALSE,
            row_ID = c("test"),
            variable = c(1),
            value = as.factor(c("0")),
            generation = 0)



#begin loop as long as n>0 - i.e carry out whole thing in a loop
if (n > 0){
  #generate random starting configuration from 1 and 0 
  starting_config <- matrix( sample(c(1,0), size=(n**2), replace=TRUE, prob=c(0.6,0.4)),
                             nrow=n,
                             ncol=n)
} 
  
#set iteration counter
iteration_counter = 1

  #begin another loop to keep going as long as a cell is alive in the starting configuration
  while (sum(starting_config) > 0 & (iteration_counter<=max_iterations) ) {
    #plot starting config here
    plot_df <- as.data.frame(starting_config) %>% 
      #plot row numbers as variable
      rownames_to_column('row_ID') %>%
      #melt so it's plottable
      melt(by='row_ID')
    
    #reformat column names so they are numbers
    plot_df$variable <- as.numeric(gsub( "V", "", as.character(plot_df$variable)))
    #set value as a character so it's binary and can be used as discrete colouring
    plot_df$value <- as.factor(plot_df$value)
    plot_df$generation <- iteration_counter
    
    #add something to append data to a huge data frame so it can be animated with gganimate
    overall_frame <- rbind(overall_frame, plot_df)
    
    #produce the actual plot - dead cells as white
    # actual_plot <- ggplot(plot_df, aes(y=row_ID, x=variable, fill=value)) +
    #   geom_tile(alpha=0.7) +
    #   scale_y_discrete(breaks=factor_levels,
    #                    limits=factor_levels) +
    #   theme(legend.position='none',
    #         panel.grid.minor=element_blank(),
    #         panel.grid.major=element_blank(),
    #         axis.title=element_blank(),
    #         axis.text=element_blank(),
    #         axis.ticks = element_blank(),
    #         panel.background = element_rect('white', 'white')) +
    #   scale_fill_manual(breaks=c('1','0'),
    #                      limits=c('1','0'),
    #                      values=c('darkorchid4', 'white'))
    # 
    # print(actual_plot)
    
    #declare an empty matrices which will be used to assign the number of neighbours
    neighbour_matrix <- matrix(c(rep(0,n**2)), nrow=n)
    new_matrix <- matrix(c(rep(0,n**2)), nrow=n)
    #Invoke function to count neighbours
    count_friends(starting_config)
    
    #iterate over each value assigning new values based on the rules
    for(i in 1:n){
      for(j in 1:n) {
        #set number of neighbours
        num_neighbours <- neighbour_matrix[i,j]
        #if cell is alive
        if (starting_config[i,j] == 1) {
          if(num_neighbours == 2 | num_neighbours == 3) {#live cell with 2 or 3 neighbours lives 
            new_matrix[i,j] <- 1
          }
          #apply rules -  
          if (num_neighbours < 2){# Any live cell with <2 live neighbours dies, as if by underpopulation.
            new_matrix[i,j] <- 0
          }
          else if(num_neighbours > 3) {# Any live cell with >3 live neighbours dies, as if by overpopulation.
            new_matrix[i,j] <-  0
          }
        }
        if (starting_config[i,j] == 0 & num_neighbours ==3) {# Any dead cell with 3 live neighbours becomes a live cell
          new_matrix[i,j] <- 1
        }
      }
    }
    #reassign new configuration to starting configuration to go back through loop
    starting_config <- new_matrix
    
    #increase the iteration counter
    iteration_counter = iteration_counter + 1
  }


#remove dummy variable from overall_frame
overall_frame <- overall_frame %>% filter(generation > 0)

#produce the actual plot - dead cells as white
actual_plot <- ggplot(overall_frame, aes(y=row_ID, x=variable, fill=value)) +
  geom_tile(alpha=0.7) +
  scale_y_discrete(breaks=factor_levels,
                   limits=factor_levels) +
  theme(legend.position='none',
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect('white', 'white')) +
  scale_fill_manual(breaks=c('1','0'),
                     limits=c('1','0'),
                     values=c('darkorchid4', 'white'))

#animate with gganimate
actual_plot + transition_states(generation) +
  enter_appear() +
  exit_disappear()




  #produce a blank plot as game is over
  # ggplot(plot_df, aes(x=row_ID, y=variable)) +
  #   geom_blank() +
  #   labs(title='Game over! Starting Next Round...') +
  #   theme(plot.title=element_text(hjust=0.5),
  #         panel.grid.minor=element_blank(),
  #         panel.grid.major=element_blank(),
  #         axis.title=element_blank(),
  #         axis.text=element_blank(),
  #         axis.ticks = element_blank(),
  #         panel.background = element_rect('white', 'white'))
  # 
  
  
  #sleep for 10 seconds before simulating next round
  #Sys.sleep(10)
