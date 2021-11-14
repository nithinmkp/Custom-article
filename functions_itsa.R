#other functions
ols_fn<-function(x,df){
        model_ols<-lm(x~time+level+trend,data = df) 
}
round_fn<-function(x){
        x <-x %>% mutate(across(where(is.numeric),round,3))
        return(x)
}

#rtf-word
tab_fn<-function(x,y){
        addParagraph(tab_name,x,"\n")
        addTable(tab_name,y)
        addParagraph(tab_name,"\n")
}

#Diagnostic plots
gg_corgram<-function(mod,lag.max=24,ci=0.95,large.sample.size=T,numrow=1,clr_bar,
                     clr_line){
        require(cowplot)
        #creating necessary dataframes
        list.acf <- acf(residuals(mod), lag.max = lag.max, type = "correlation", plot = FALSE)
        N <- as.numeric(list.acf$n.used)
        df1 <- data.frame(lag = list.acf$lag, acf = list.acf$acf)
        df1$lag.acf <- dplyr::lag(df1$acf, default = 0)
        df1$lag.acf[2] <- 0
        df1$lag.acf.cumsum <- cumsum((df1$lag.acf)^2)
        df1$acfstd <- sqrt(1/N * (1 + 2 * df1$lag.acf.cumsum))
        df1$acfstd[1] <- 0
        df1 <- dplyr::select(df1, lag, acf, acfstd)
        
        list.pacf <- acf(residuals(mod), lag.max = lag.max, type = "partial", plot = FALSE)
        df2 <- data.frame(lag = list.pacf$lag,pacf = list.pacf$acf)
        df2$pacfstd <- sqrt(1/N)
        
        #plot for large sample
        if(large.sample.size == TRUE) {
                plot.acf <- ggplot(data = df1, aes( x = lag, y = acf)) +
                        geom_area(aes(x = lag, y = qnorm((1+ci)/2)*acfstd), fill = "#B9CFE7") +
                        geom_area(aes(x = lag, y = -qnorm((1+ci)/2)*acfstd), fill = "#B9CFE7") +
                        geom_col(fill = clr_bar, width = 0.7,position = "identity") +
                        scale_x_continuous(breaks = seq(0,max(df1$lag),6)) +
                        scale_y_continuous(name = element_blank(), 
                                           limits = c(min(df1$acf,df2$pacf),1)) +
                        labs(subtitle = "ACF") +
                        theme_bw()
                
                plot.pacf <- ggplot(data = df2, aes(x = lag, y = pacf)) +
                        geom_area(aes(x = lag, y = qnorm((1+ci)/2)*pacfstd), fill = "#B9CFE7") +
                        geom_area(aes(x = lag, y = -qnorm((1+ci)/2)*pacfstd), fill = "#B9CFE7") +
                        geom_col(fill = clr_bar, width = 0.7) +
                        scale_x_continuous(breaks = seq(0,max(df2$lag, na.rm = TRUE),6)) +
                        scale_y_continuous(name = element_blank(),
                                           limits = c(min(df1$acf,df2$pacf),1)) +
                        labs(subtitle = "PACF") +
                        theme_bw()
        }
        #plot for small sample
        else {
                plot.acf <- ggplot(data = df1, aes( x = lag, y = acf)) +
                        geom_col(fill = clr_bar, width = 0.7) +
                        geom_hline(yintercept = qnorm((1+ci)/2)/sqrt(N), 
                                   colour = clr_line,
                                   linetype = "dashed") + 
                        geom_hline(yintercept = - qnorm((1+ci)/2)/sqrt(N), 
                                   colour = clr_line,
                                   linetype = "dashed") + 
                        scale_x_continuous(breaks = seq(0,max(df1$lag),6)) +
                        scale_y_continuous(name = element_blank(), 
                                           limits = c(min(df1$acf- qnorm((1+ci)/2)/sqrt(N),
                                                          df2$pacf- qnorm((1+ci)/2)/sqrt(N)),1)) +
                        labs(subtitle = "ACF") +
                        theme_bw()
                
                plot.pacf <- ggplot(data = df2, aes(x = lag, y = pacf)) +
                        geom_col(fill = clr_bar, width = 0.7) +
                        geom_hline(yintercept = qnorm((1+ci)/2)/sqrt(N), 
                                   colour = clr_line,
                                   linetype = "dashed") + 
                        geom_hline(yintercept = - qnorm((1+ci)/2)/sqrt(N), 
                                   colour = clr_line,
                                   linetype = "dashed") + 
                        scale_x_continuous(breaks = seq(0,max(df2$lag, na.rm = TRUE),6)) +
                        scale_y_continuous(name = element_blank(),
                                           limits = c(min(df1$acf- qnorm((1+ci)/2)/sqrt(N),
                                                          df2$pacf- qnorm((1+ci)/2)/sqrt(N)),1)) +
                        labs(subtitle = "PACF") +
                        theme_bw()
        }
        cowplot::plot_grid(plot.acf, plot.pacf, nrow = numrow)
}



resid_fn<-function(mod,data,colr1,colr2,head){
        p1<-ggplot(data = data,aes(x=Year,y=residuals(mod)))+
                geom_point(color=colr1,size=1.5)+
                xlab("Year")  + ylab("Residuals")+
                geom_line(color=colr2,size=1)+
                labs(title = head,
                     subtitle = "Residuals")+
                theme_bw()+
                theme(plot.title = element_text(hjust = 0.5))
        return(p1)
}


diag_fn<-function(mod,data,colr1,colr2,colr3,colr4){
        resid_plots<-map2(mod,names(mod),~resid_fn(mod = .x,data = df,colr1 = colr1,
                                                   colr2 = colr2,
                                                   head = .y))
        cor_plot<-map(mod,~gg_corgram(.x,large.sample.size = F,clr_bar = colr3,
                                      clr_line = colr4))
        p<-map2(resid_plots,cor_plot,~.x/.y)
        return(p)
        
}

# Getting predicted values and final tables Function
add_pred_fn<-function(df,model,names_var){
        select_fn<-function(model){
                l<-map(model,augment) %>% map(`[`,".fitted") %>% reduce(bind_cols)
                names_vec<-map_chr(names(model),~glue("pred_{.x}"))
                names(l)<-names_vec
                return(l)
        }
        a<-select_fn(model = model) 
        b<-cbind(df,a)
        lst<-names_var %>% map(.,~b %>% select(Year,time,level,trend,contains(.x)))
        names(lst)<-names(model)
        return(lst)
        
        
        
}

#Final Plots Function
plot_fn<-function(df,vars,ylims,model,break_date,break_index,point_clr,point_size,
                  dash_clr,dash_size,line_clr,line_size){
        data1<-subset(df,Year<break_date)
        data2<-subset(df,Year>=break_date)
        a<-df %>% ggplot(aes(x=Year,y=get(vars)))+
                geom_point(aes(color="a"),size=point_size,
                           key_glyph="point")+
                ylab(NULL)+xlab(NULL)+
                ggtitle(vars)+
                scale_x_date(date_labels = "%b-%Y",
                             date_breaks = "3 month")+
                geom_vline(xintercept = break_date,
                           color=dash_clr,
                           size=dash_size,
                           linetype="dashed")+
                scale_y_continuous(limits= ylims)
        b<-a+geom_segment(data = data1,x=data1[1,1],y=data1[1,6],
                          xend=data1[break_index-1,1],yend=data1[break_index-1,6],
                          aes(color="b"),size=line_size,
                          key_glyph="smooth")
        c<-b+geom_segment(data = data2,x=data2[1,1],y=data2[1,6],
                          xend=data2[nrow(data2),1],yend=data2[nrow(data2),6],
                          aes(color="b"),size=line_size,
                          key_glyph="smooth")
        d<-c+geom_segment(x=data1[1,1],y=model$coefficients[1]+model$coefficients[2],
                          xend=data2[nrow(data2),1],
                          yend=model$coefficients[1]+model$coefficients[2]*nrow(df),
                          aes(color="b"),size=line_size,linetype="dotted",
                          key_glyph="smooth")+
                scale_color_manual(name="",
                                   values = c("a"=point_clr,
                                              "b"=line_clr),
                                   labels=c("Actuals","Predicted"))+
                theme_wsj()+
                theme(plot.title = element_text(size=20),
                      plot.caption =element_text(size=12,face="italic",
                                                 hjust = 0),
                      axis.title = element_text(size=12),
                      plot.background = element_blank(),
                      panel.background = element_blank(),
                      legend.background = element_blank(),
                      legend.key = element_rect(fill="white"),
                      legend.key.size = unit(2, 'cm'),
                      legend.text = element_text(size=18))
        
        
        
        
}

#Change calculation function
calc_val_fn<-function(df,vars,model,break_index,period_end){
        df1<- tibble(Period=df[c(break_index,period_end),1])
        df1<-df1 %>% mutate("Actuals"=case_when(
                Period=="2020-04-01"~df[break_index,5],
                Period=="2021-06-01"~df[period_end,5]
        ))
        df1<-df1 %>% mutate("Trend Values"=case_when(
                Period=="2020-04-01"~model$coefficients[1]+model$coefficients[2]*break_index,
                Period=="2021-06-01"~model$coefficients[1]+model$coefficients[2]*period_end)
        )
        
        df1<-df1 %>% mutate("Change"=df1[,3,drop=T]-df1[,2,drop=T],
                            Variable=vars,
                            "% Change"=(Change/df1[,3,drop=T])*100) %>% select(Variable,everything())
        df1$Period<-format(as.Date(df1$Period),"%b-%Y")  
        df1[]<-df1[] %>% map_if(is.numeric,round,2)
}
