### calls to PatentsView API

library(httr)
library(jsonlite)
library(dplyr)

#function for getting number of papents in a USPTOclass per year

patsperyear <- function(mainclass, org="All"){
    outvars <- c("uspc_mainclass_id","uspc_mainclass_title", "year_id","year_num_patents_for_uspc_mainclass")
    
    #call we are trying to emulate: http://www.patentsview.org/api/uspc_mainclasses/query?q={"uspc_mainclass_id":"292"}&f=["uspc_mainclass_id","uspc_mainclass_title","uspc_total_num_patents"] 
    
    #just not confident in what GET's add_headers is doing
    #  callresults <- GET("http://www.patentsview.org/api/uspc_mainclasses/", 
    #                     query = list(uspc_mainclass_id = mainclass), 
    #                     add_headers(f=sprintf("[\"%s\"]", paste(outvars, collapse='\",\"')))) 
    
    #so i'm pasting together my own string
    baseurl <- "http://www.patentsview.org/api/uspc_mainclasses/"
    querystr <- sprintf("q={\"uspc_mainclass_id\":\"%s\"}",as.character(mainclass))
    bodystr <- sprintf("f=[\"%s\"]", paste(outvars, collapse='\",\"'))
    callstr <- sprintf("%squery?%s&%s", baseurl, querystr, bodystr)
    
    #API call
    callresults <- GET(callstr)
    
    #should probably add some error checking
    
    #json processing of content - return df
    #convert to numeric and calculate freq
    #include patent code and title in the response
    patct <- fromJSON(content(callresults, as="text"))$uspc_mainclasses$years[[1]] %>%
             transmute(year=as.numeric(year_id), count=as.numeric(year_num_patents_for_uspc_mainclass)) %>%
             mutate(freq = count/sum(count),
                    mainclass_code = fromJSON(content(callresults, as="text"))$uspc_mainclasses$uspc_mainclass_id,
                    mainclass_title = fromJSON(content(callresults, as="text"))$uspc_mainclasses$uspc_mainclass_title)

    return(patct)
}

#function to get assignee organizations for main class Y
#decided not to limit count b/c renderDataTable does that for us
#USPC_Mainclass endpoint allows us to get all assignees for a class in the first page, while the Assingees endpoint would have limited the number of assignees per page
assignees <- function(mainclass){
  
  #using the same past-by-hand approach as patsperyear
  outvars <- c("assignee_organization", "assignee_type", "assignee_total_num_patents", "assignee_num_patents_for_uspc_mainclass")
  baseurl <- "http://www.patentsview.org/api/uspc_mainclasses/"
  querystr <- sprintf("q={\"_and\":[{\"uspc_mainclass_id\":\"%s\"},{\"patent_type\":\"utility\"}]}",as.character(mainclass))
  bodystr <- sprintf("f=[\"%s\"]", paste(outvars, collapse='\",\"'))
  sortstr <- "s=[{\"assignee_num_patents_for_uspc_mainclass\":\"desc\"}]"
  callstr <- sprintf("%squery?%s&%s&%s", baseurl, querystr, bodystr, sortstr)
  
  #API call
  callresults <- GET(callstr)
  
  assigneedf <- fromJSON(content(callresults, as="text"))$uspc_mainclasses$assignees[[1]] %>%
              mutate(totalpats = as.numeric(assignee_total_num_patents),
                        patsinclass = as.numeric(assignee_num_patents_for_uspc_mainclass)) %>%
              select(-assignee_total_num_patents, -assignee_num_patents_for_uspc_mainclass)
  
  
  #convert assignee type to text - Note: A "1" appearing before any of these codes signifies part interest
  assignee_type_tbl <- data.frame(assignee_type=as.character(c(2:9)),
                                  assignee_desc=c("US Company or Corporation","Foreign Company or Corporation","US Individual","Foreign Individual","US Government","Foreign Government","Country Government","State Government (US)"),
                                  stringsAsFactors = F) #otherwise it tries to coerce type to factor
  assignee_type_tbl$assignee_desc <- factor(assignee_type_tbl$assignee_desc, levels=assignee_type_tbl$assignee_desc)
  
  assigneedf <- left_join(assigneedf, assignee_type_tbl, by="assignee_type")
  
  return(assigneedf) #return all
}
