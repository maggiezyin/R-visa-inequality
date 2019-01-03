shinyServer(function(input, output) { 
  output$SortBy <- renderUI({
    selectInput("sort", "Sort By", c("Inequality Score" = "rank", "Continent" = "continent", "Country" = "country"))
    
  })
  
  output$ReverseOrder <- renderUI({
    checkboxInput("reverse", "Reverse Order", value = FALSE, width = NULL)    
  })
  
  
  output$barplot <- renderPlotly({
    if(input$BarType=="Visa Inequality")
    {
      if(input$sort == "rank"){
        inequality1 <- mutate(inequality1, display_name = fct_rev(fct_reorder(display_name,as.numeric(rank))))
        print(levels(inequality1$display_name))

      }
      else if (input$sort == "continent")
      {
        print("continent")
        inequality1 <- mutate(inequality1, display_name = fct_rev(fct_reorder(display_name,as.numeric(as.factor(continent.x)))))

      }
      else {
        inequality1 <- mutate(inequality1, display_name = fct_rev(fct_reorder(display_name,as.numeric(as.factor(country)))))
      }
      
      if(input$reverse)
      {
        inequality1$display_name <- fct_rev(inequality1$display_name)
      }
      
      g <- ggplot(inequality1)+
        geom_bar(aes(x=display_name,fill=score,text = paste('Country ', country,
                                                            '<br>Score:', score , 
                                                            '<br>Continent:', continent.x)),position="fill", width = 0.7, na.rm=TRUE)+
        scale_fill_brewer(palette="RdBu" , direction= -1, labels=c("+++++", "++++", "+++","++","+","neutral", "-","--","---", "----","-----" ),name="Inequality Score") +coord_flip() +
        labs(x="code",y="Afghanistan",
             title="")+ #theme_classic()
        theme(axis.line=element_blank(),axis.text.x=element_blank(),
              #axis.text.y=element_blank(),
              axis.text.y = element_text(hjust = 0),
              axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),legend.position="none",
              panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),plot.background=element_blank())
    }
    else if(input$BarType=="Visa To"){
      if(input$sort == "rank")
      {
        travelto1<- mutate(travelto1, display_name = fct_rev(fct_reorder(display_name,as.numeric(rank))))
      }
      else if (input$sort == "continent")
      {
        print("cont")
        travelto1<- mutate(travelto1, display_name = fct_rev(fct_reorder(display_name,as.numeric(as.factor(continent.x)))))
      }
      else {
        travelto1<- mutate(travelto1, display_name = fct_rev(fct_reorder(display_name,as.numeric(as.factor(country)))))
      }
      if(input$reverse)
      {
        travelto1$display_name <- fct_rev(travelto1$display_name)
      }
      
      g <- ggplot(travelto1)+
        geom_bar(aes(x=display_name, fill=category,text = paste('Country ', country,
                                                                '<br>Category:', category , 
                                                                '<br>Continent:', continent.x)),position="fill", width = 0.7,na.rm=TRUE)+
        scale_fill_brewer(palette="RdBu", direction = -1,
                          name="Visa Category")+ coord_flip() +
        labs(x="h",y="a",
             title="")+ #theme_classic()
        theme(axis.line=element_blank(),axis.text.x=element_blank(),
              #axis.text.y=element_blank(),
              axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),legend.position="none",
              panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),plot.background=element_blank())
      
    }
    else if (input$BarType=="Visa From"){ 
      if(input$sort == "rank")
      {
        travelfrom1<- mutate(travelfrom1, display_name = fct_rev(fct_reorder(display_name,as.numeric(rank))))
        #travelto1$display_name<-fct_rev(travelto1$display_name)
        print(levels(travelfrom1$display_name))
        
      }
      else if (input$sort == "continent")
      {
        travelfrom1<- mutate(travelfrom1, display_name = fct_rev(fct_reorder(display_name,as.numeric(as.factor(continent.y)))))
      }
      else {
        travelfrom1<- mutate(travelfrom1, display_name = fct_rev(fct_reorder(display_name,as.numeric(as.factor(country.y)))))
      }
      
      if(input$reverse)
      {
        travelfrom1$display_name <- fct_rev(travelfrom1$display_name)
      }
      
      g <- ggplot(travelfrom1)+
        geom_bar(aes(x=display_name, fill=category,text = paste('Country ', code,
                                                                '<br>Category:', category , 
                                                                '<br>Continent:', continent.x)),position="fill", width = 0.7,na.rm=TRUE)+
        scale_fill_brewer(palette="RdBu", direction = -1,
                          name="Visa Category")+ coord_flip() +
        labs(x="h",y="a",
             title="")+ #theme_classic()
        theme(axis.line=element_blank(),axis.text.x=element_blank(),
              #axis.text.y=element_blank(),
              axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),legend.position="none",
              panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),plot.background=element_blank())
    }
    g<-ggplotly(g, tooltip = "text",axis.text.y = element_text(hjust = 0), height=5000) %>% layout(dragmode = "select")
    g
  })
  
  
  output$hover <- renderPrint({
    d <- event_data("plotly_hover")
  })
  
output$map <- renderPlotly({
  if(input$SelectMap=="Who is allowed to travel to Germany?")({

    m<-ggplot(data=world_german_to)+geom_polygon(aes(x=long,y=lat,group = group, fill=factor(disp),color=factor(disp),size=factor(disp), text=country))+scale_color_manual(values=c("white","white","white","white","white","white","black"),guide=FALSE)+scale_size_manual(values=c(.5,.5,.5,.5,.5,.5,1.2),guide=FALSE)+coord_map("gilbert",xlim=c(-180,180),ylim=c(-90,90))
    
    m<-m+theme(plot.background = element_rect(fill = 'white'),legend.position = "bottom", legend.box="horizantal", plot.title = element_text(colour = "gold2"),legend.background=element_rect(fill="white"),panel.background=element_rect(fill="slateblue4"),legend.key=element_rect(color=NA,fill="white"))+scale_color_manual(values=c("white","white","white","white","white","white","black"),guide=FALSE)
    
    m <- m + labs(fill="Travel Visa Policies")+theme(legend.text = element_text(color="grey81"), legend.title=element_text(color="grey81"), legend.position = "bottom", legend.box = "horizantal")

    m<-m+scale_fill_manual(breaks = levels(factor(world_german_from$disp)), values = c("black", "firebrick2","lightcoral","darkorange2","gold2","skyblue2"),labels = c("Visas not\nrequired","Visa On \n arrival","ETA \n required","eVisa \n required","Visa \n required","Admission \n refused"),na.value="grey")+labs(x="",y="",title=NULL)
    m<-ggplotly(m, tooltip = "text")
     })
  if(input$SelectMap=="Where can Germans Travel to?")({ 

    m<-ggplot(data=world_german_from)+geom_polygon(aes(x=long,y=lat,group = group, fill=factor(disp),color=factor(disp),size=factor(disp), text=country))+scale_color_manual(values=c("white","white","white","white","white","white","black"),guide=FALSE)+scale_size_manual(values=c(.5,.5,.5,.5,.5,.5,1.2),guide=FALSE)+coord_map("gilbert",xlim=c(-180,180),ylim=c(-90,90))
    
    m<-m+theme(plot.background = element_rect(fill = 'white'), legend.position = "bottom", plot.title = element_text(colour = "gold2"),legend.background=element_rect(fill="white"),panel.background=element_rect(fill="slateblue4"),legend.key=element_rect(color=NA,fill="white"), legend.box="horizantal")+scale_color_manual(values=c("white","white","white","white","white","white","black"),guide=FALSE)
    
    m <- m + labs(fill="Travel Visa Policies")+theme(legend.text = element_text(color="grey81"), legend.title=element_text(color="grey81"))
    
    m<-m+scale_fill_manual( breaks = levels(factor(world_german_from$disp)), values = c("black", "firebrick2","lightcoral","darkorange2","gold2","skyblue2"),labels = c("Visas not\nrequired","Visa On \n arrival","ETA \n required","eVisa \n required","Visa \n required","Admission \n refused"),na.value="grey")+labs(x="",y="",title=NULL)
    m<-ggplotly(m, tooltip = "text")
  })
  if(input$SelectMap=="Differences between Traveling to and from Germany")({ 
    
    m<-ggplot()+geom_polygon(data=comp,aes(x=long,y=lat,group = group, fill=neg, text=country))+scale_color_manual(values=c("white","black"),guide=FALSE)+scale_size_manual(values=c(.5,1.2),guide=FALSE)+coord_map("gilbert",xlim=c(-180,180),ylim=c(-90,90))+scale_fill_gradient2(name="Travel Visa\n Inequality",midpoint=0,breaks=c(-3,0,3),labels=c("<more\n disadvantages","neutral","more\nadvantages>"))
    
    m<-m+theme(plot.background = element_rect(fill = 'white'),legend.position = "bottom", legend.box="horizantal",legend.background=element_rect(fill="white"),panel.background=element_rect(fill="slateblue4"),legend.key=element_rect(color=NA,fill="white"))
    
    m <- m+theme(legend.text = element_text(color="grey81"), legend.title=element_text(color="grey81"), legend.position = "bottom", legend.box = "horizantal")+guides(fill = guide_colourbar(barwidth = 15, barheight = 3))
    m<-ggplotly(m, tooltip = "text")
    
    })
  if(input$SelectMap=="Explore Visa Inequality")({ 

    m<-ggplot()+geom_polygon(data=world_ineq,aes(x=long,y=lat,group = group, fill=score, text=paste(country.x, '<br>', dis, "Disadvantages", "<br>", adv, "Advantages", "<br>", "#", rank)))+coord_map("gilbert",xlim=c(-180,180),ylim=c(-90,90))+scale_fill_gradient2(name="Travel Visa\n Inequality Score",
                                                                                                                                                                                                                                                                       midpoint=0,breaks=c(-120,-90,-60,-30,0,30,60,90,120),labels=c("-100","-75","-50","-25","0","+25","+50","+75","+100"))
    m<-m+theme(plot.background = element_rect(fill = 'white'),legend.position = "bottom",legend.background=element_rect(fill="white"),panel.background=element_rect(fill="slateblue4"),legend.key=element_rect(color=NA,fill="white"), legend.box = "horizantal")
    
    m <- m + labs(fill="Travel Visa Policies")+theme(legend.text = element_text(color="grey81"), legend.title=element_text(color="grey81"))+guides(fill = guide_colourbar(barwidth = 15, barheight = 3))
   
    m<-ggplotly(m, tooltip = "text")
    
    })
    m 
})
  
output$text<-renderText({
  if(input$SelectMap=="Where can Germans Travel to?")
    ({ 
    text="Germans can go to many countries without any Visa,but need visas for parts of Africa and Asia."
  })
  if(input$SelectMap=="Who is allowed to travel to Germany?")({
    text="This is quite different.  Many countries have different visa policies can travel to Germany."
  })
  if(input$SelectMap=="Differences between Traveling to and from Germany")({
    text="Blue shows the advantages and red the disadvantages for Germans."
  })
  if(input$SelectMap=="Explore Visa Inequality")({
    text="Hover over a country to compare it with others."
  })
    text
})

})





