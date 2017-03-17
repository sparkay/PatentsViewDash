#plotting patent data

library(ggplot2)
library(cowplot) #something in ploytly seems to be fighting with cowplot - my cowplots are disapearing if I refresh the dashboard, although they show fine on the first load
library(stringr)

#modified to generate both the total count histograph and the frequency "density" plot sidebyside
#sidebyside is accomplished using cowplot

plot_patbkgrd <- function(plotdf){
  
  #since the data is already binned, plot as an area instead of calculating density
  plt_density <- ggplot(plotdf) + geom_area(aes(x=year, y=freq), color="gray", alpha=0.2) + ggtitle("Density") + theme_bw() + ylab("")
  #consider using wavelets to add smoothed line: http://www.bearcave.com/misl/misl_tech/wavelets/histo/
  
  #create histogram
  plt_hist <- ggplot(plotdf) + geom_bar(aes(x=year, y=count), stat="identity", color="gray", alpha=0.2) +ggtitle("Count") + theme_bw() + ylab("")
  
  #output combined plot
  #plot_grid(plt_hist, plt_density, align='h')
  
  #add title
  #title <- ggdraw() + draw_label(sprintf("Total Patents %d", sum(plotdf$count, na.rm=T)), fontface='bold')
  #plot_grid(title, tmp_p, ncol=1, rel_heights=c(0.1, 1))
  
  ######
  #changed to output list of ggplot objects
  #layout will be handled by shiny
  list(plt_density=plt_density, plt_hist=plt_hist)
  
}

#function to generate prettier breaks for log scale plots
logbreaks <- function(maxct){
  intervals <- matrix(c(2, 5, 10), nrow=1)
  oom <- matrix(10^c(0:floor(max(0,log10(maxct)-1))), ncol=1)
  c(1, sort(matrix(oom %*% intervals, nrow=1)))
}

#assignee count histogram per class
plot_assigneehist <- function(assigneedf) {
  #results are very skewed with most assignees have 1 patent, but a few with >1000
  #custom break/label calculation based on maxct for that class
  mybreaks <- logbreaks(max(assigneedf$patsinclass, na.rm=T))
  
  #note with scale_x_log10() the histogram binwidth is measured in the transformed log scale
  ggplot(assigneedf) + geom_histogram(aes(x=patsinclass)) + scale_x_log10(name="patents in class", breaks=mybreaks, labels=mybreaks) + theme_bw() +  ylab("number of assignees")
}

#assignee count by type
plot_assigneetype <- function(assigneedf) {
  ggplot(assigneedf) + geom_bar(aes(x=assignee_desc, fill=assignee_desc), stat="count") + theme_bw() + scale_x_discrete(labels=str_wrap(as.character(levels(assigneedf$assignee_desc)), width=10), name="") +  ylab("number of assignees") + scale_fill_brewer(name="Assignee Type", palette="Set1", guide=F) 
}

#assignee scatter plot
plot_assigneescatter <- function(assigneedf){
  ggplot(assigneedf) + geom_point(aes(x=totalpats, y=patsinclass, color=assignee_desc, text=assignee_organization)) +
    scale_color_brewer(name="Assignee Type", palette="Set1") + theme_bw() +
    scale_x_continuous(trans="log10", name="Total Patents", labels = scales::comma) +
    scale_y_continuous(trans="log10", name="Patents Received in this Class", breaks=logbreaks(max(assigneedf$patsinclass, na.rm=T))) +
    ggtitle("Distribution of Assignees in this Class by Patent Counts and Org Type")
}