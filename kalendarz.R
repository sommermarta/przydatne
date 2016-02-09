library("ggplot2")
library("dplyr")

kalendarz <- function(zaznacz_od, zaznacz_do, filtr=""){
    
    rok <- substr(od, 1, 4) %>% as.numeric()
    
    seq(as.Date(zaznacz_od), as.Date(zaznacz_do), by=1) -> ktore
    
    seq(as.Date(paste(rok, "-01-01", sep="")), 
        as.Date(paste(rok, "-12-31", sep="")), 
        by=1) -> data
    
    as.POSIXlt(data)$wday -> dzien_tyg
    dzien_tyg[dzien_tyg==0] <- 7
    strftime(data, format="%W") %>% 
        as.numeric() %>% 
        "+"(1) -> nr_tyg
    
    data.frame(data, dzien_tyg, nr_tyg) %>% 
        mutate(ktore=ifelse(data %in% ktore, "1", "0"),
               miesiac=as.POSIXlt(data)$mon+1,
               rok=rok) %>%
        mutate(rok_polowa=ifelse(miesiac<=6, 1, 2)) %>% 
        group_by(miesiac) %>% 
        mutate(min_tyg=min(nr_tyg)) %>% 
        mutate(nr_tyg2=nr_tyg-min_tyg+1,
               dzien=as.numeric(strftime(data, format="%d"))) -> ramka
    
    ramka$dzien_tyg <- factor(ramka$dzien_tyg, levels=1:7,
                              labels=c("pon.","wt.","śr.","czw.","pt.","sob.","niedz."),
                              ordered=TRUE)
    ramka$miesiac <- factor(ramka$miesiac,
                            levels=as.character(1:12),
                            labels=c("Sty","Luty","Mar","Kw","Maj","Cze",
                                     "Lip","Sie","Wrz","Paź","Lis","Gru"),
                            ordered=TRUE)
    ramka$nr_tyg2 <- factor(ramka$nr_tyg2, levels=rev(1:max(ramka$nr_tyg2)),
                            labels=rev(as.character(1:max(ramka$nr_tyg2))),
                            ordered=TRUE)
    
    if(filtr=="") ramka2 <- ramka else ramka2 <- ramka %>% filter_(filtr)
    
    ggplot(ramka2, aes(x=dzien_tyg, y=nr_tyg2, fill=ktore))+ 
        geom_tile(colour="white")+
        geom_text(aes(label=dzien))+
        facet_grid(rok~miesiac)+
        theme_bw()+
        theme(axis.title = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.text.y = element_blank(),
              axis.text.x = element_text(angle=90),
              legend.position="none")+
        scale_fill_manual(values=c("gray89", "skyblue1"))
}
    
