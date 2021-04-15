#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(knitr)
library(dplyr)
library(FactoMineR)
library(base)
library(factoextra)
library(BurStMisc)
# Define UI for application that draws a histogram
ui <- fluidPage(
    theme=shinytheme("cerulean"),
    
    navbarPage(
        "TFG",
        tabPanel("Mapas de Riesgo",
                 
                 sidebarPanel(
                     
                     selectInput("fchosen","Elige una variable", choices = c("Ninguna"=0,"Est.Civ"=1,"Sexo"=2,
                                                                             "Grupo Cliente"=3,"Reg. Laboral"=4,
                                                                             "Cobro"=5,
                                                                             "Plan Com"=6,"Of.Cobradora"=7,
                                                                             "Forma Pago"=9,"Clase"=10,
                                                                             "Antigüedad"=11,"Segmento"=18,
                                                                             
                                                                             "Num.Polizas(Cancelar)"=12,"Num.Polizas(Impacto)"=13,
                                                                             "Provincia(Cancelar)"=14, "Provincia(Impacto)"=15,
                                                                             "Abrev.Cia(Cancelar)"=16, "Abrev.Cia(Impacto)"=17,
                                                                             "Clusters"=20) 
                               ),
                     
                     selectInput("escala","Elige una escala", choices = c("Percentil"=0,"Real"=1) 
                     ),
                     
                     
                     numericInput(inputId = "idc", label="Escribe el ID del Cliente",value=0),
                     
                     h4("¿Tienes dudas?"),
                     
                     h5("Variables"),
                     
                     "Debes seleccionar la variable por la que desees discriminar a los asegurados.A su vez, podrás ver el efecto de cada uno de los niveles de la variable seleccionada sobre ambas variables respuesta
                     en los gráficos debajo del de riesgo.Las variables tienen el siguiente significado:",
                    div(""),
                     tags$b("Est.Civ:"),"El estado Civil del Asegurado (C=casado, S=Soltero y O=Desconocido).",
                    div(""),
                    tags$b("Sexo:"),"Sexo del asegurado(F=Femenino,M=Masculino y S=Desconocido).",
                    div(""),
                    tags$b("Grupo Cliente:"),"Grupo al que pertenece el asegurado (Seguros y sin grupo).",
                    div(""),
                    tags$b("Reg.Laboral:"),"Regulación del asegurado ( Regulación general y otros).",
                    div(""),
                    tags$b("Cobro:"),"Muestra cómo se realizó el cobro (Efectivo y No efectivo).",
                    div(""),
                    tags$b("Plan Com:"),"Muestra si fue adquirido con un Plan Comercial o no (Sí y No).",
                    div(""),
                    tags$b("Of.Cobradora:"),"Oficina Cobradora del seguro (Segresegur-Zaragoza,Segresegur Central y Unió de Llauradors).",
                    div(""),
                    tags$b("Forma Pago:"),"Periodicidad del pago (Anual y no anual).",
                    div(""),
                    tags$b("Clase:"),"Muestra si este es el primer año del asegurado o no (PRODUCCIÓN=Primer año,CARTERA=Renovación después del primer año).",
                    
                    div(""),
                    tags$b("Antigüedad:"),"Muestra los años que lleva el asegurado dado de alta. (Los niveles están en la forma (x,y] donde x e y son números naturales cuya interpretación es que el usuario tiene más de x años de antigüedad pero no más de y. ))",
                    div(""),
                    tags$b("Segmento:"),"Muestra la categoría a la que pertenece el asegurado según el eBroker (Bronce o Oro, Plata o VIP).",
                    
                    div(""),
                    tags$b("Num.Polizas(Cancelar)*:"),"Número de Pólizas Contratadas categorizada de acuerdo a la variable.",
                    div(""),
                    tags$b("Num.Polizas(Impacto)*:"),"Número de Pólizas Contratadas categorizada de acuerdo a la variable del impacto ecónomico.",
                    div(""),
                    tags$b("Provincia(Cancelar)*:"),"Comunidad de residencia el asegurado(Aragón,Valencia,Cataluña y Otros).Categorizada de acuerdo a la variable de cancelación.",
                    div(""),
                    tags$b("Provincia(Impacto)*:"),"Comunidad de residencia el asegurado(Aragón,Valencia,Cataluña y Otros).Categorizada de acuerdo a la variable del impacto ecónomico.",
                    div(""),
                    tags$b("Abrev.Cia(Cancelar)*:"),"Abreviatura  de la compañía aseguradora(Axa,Allianz,Adeslas..).Categorizada de acuerdo a la variable de cancelación.",
                    div(""),
                    tags$b("Abrev.Cia(Impacto)*:"),"Abreviatura  de la compañía aseguradora(Axa,Allianz,Adeslas..).Categorizada de acuerdo a la variable del impacto ecónomico.",
                    div(""),
                    tags$b("Clusters:"),"Cluster o grupo, de los formados en el clustering, al que pertenecen los asegurados.(1=Cluster 1, 2=Cluster 2 y 3=Cluster 3).",
                    div(""),
                    h6("*La categorización de las variables acompañadas por un asterisco (*) es distinta en función de la variable de cancelación y la del impacto económico."),
                    
                    h5("Escala"),
                    "Debes seleccionar entre una de estas escalas:" ,
                    div(""),
                    tags$b("Percentil:"),"La posición del asegurado en el mapa está determinada por el percentil al que pertenece tanto de la probabilidad de cancelar como del impacto económico.",
                    div(""),
                    tags$b("Real:"),"La posición del asegurado en el mapa está determinada por su probabilidad de cancelar e impacto económico.",
                    
                    h5("Id del Cliente"),
                    "Debes escribir el Número del cliente que desee consultar y este se mostrará como un punto rojo en el mapa de riesgo (Si no se ha dado de baja). 
                    Además, podrás observar sus características en la tabla debajo del gráfico." ),
                 
                 
        mainPanel(
            h2("¿Qué hace esta App?", align="center"),
            p("Esta App te permite observar cómo se distribuyen los asegurados en el mapa de riesgo formado por 2 ejes, uno que muestra el impacto económico (Eje Y) y el otro la probabilidad de cancelar alguna póliza (Eje X). 
            A su vez, utilizando las opciones del panel de la izquierda, te permite seleccionar la escala del mapa, discriminar a los asegurados en función de un conjunto de variables categóricas, observar la situación exacta de un determinado cliente en el mapa y sus características."),
            h4("Mapa de riesgo"),
            "La posición de cada cliente (punto) viene determinada por el riesgo de cancelar alguna póliza (Eje X) y el impacto económico. De esta manera, conforme mayor sea el valor de 
            cade uno de los ejes mayor es el riesgo y el impacto del usuario, respectivamente. Nota que solo los clientes con póliza(s) en vigor están en el mapa.",
           plotOutput("mapR",height="800px"),
           h4("Gráfico de los efectos de las variables explicativas sobre las variables respuesta"),
           "Ambos gráficos muestran el efecto de cada uno de los niveles de las variables explicativas sobre la variable respuesta respecto a un cierto nivel de referencia. Este nivel de referencia es el nivel de la variable categórica que se omite.",
           "En caso de que la variable no sea categórica, como antigüedad en el gráfico de la variable impacto, se muestra el efecto por cada unidad que aumenta el valor de dicha variable numérica.",
           
           h5("Variable de Cancelación"),
           "El gráfico a continuación muestra el porcentaje de variación sobre las ODDS de cancelar alguna póliza al pasar del nivel de referencia, cuyo porcentaje siempre es 0, a otro nivel determinado.
           Entonces, un valor positivo implica que las odds son más elevadas en este nivel respecto al de referencia, mientras que  un valor negativo implica que las odds son más bajas.",
           div(""),
           "A la izquierda del gráfico se encuentra la información sobre la variable y el efecto que tiene cada uno de sus niveles sobre las odds escritos de la siguiente manera:",
           div(""),
           em("Variable | Nivel | % de variación"),
          
            div(""),
           
           "De esta manera, Antigüedad | (2,10] | -47% se debe leer como que los asegurados con una antigüedad superior a  2 y menor a 10 años tienen unas odds un 47% menores respecto al nivel de referencia, en este caso tener una antigüedad de menos de 2 años.",
           plotOutput("clevlogit",height="700px"),
           h5("Variable del impacto"),
           
           "El gráfico a continuación muestra el porcentaje de variación sobre la esperanza de la variable impacto al pasar del nivel de referencia,cuyo porcentaje siempre es 0, a otro nivel determinado.
           Entonces, un valor positivo implica que el impacto ecónomico es mayor y, uno  negativo implica que tiene un impacto inferior.",
           div(""),
           "A la izquierda del gráfica se encuentra la información sobre la variable y el efecto que tiene cada uno de sus niveles sobre la esperanza de la variable escritos de la siguiente manera:",
           div(""),
           em("Variable | Nivel | % de variación"),
           
           div(""),
           
           "De esta manera, Forma Pago | No Anual | -36% se debe leer como que los asegurados con una forma de pago No Anual tiene una media de impacto un 36% menor que los del nivel de referencia, es decir los quue han seleccionado la forma de pago Anual.
           ",
           "En caso de que la variable sea numérica, como sucede con la variable antigüedad, no tiene niveles y el porcentaje de variación se debe leer como la variación sobre la esperanza del impacto por cada unidad que aumenta la variable numérica.",
           
           plotOutput("clevgamma",height="700px"),
           h4("Características del cliente seleccionado"),
           "Aquí puedes ver las características del cliente seleccionado en el panel de la izquierda:",
           tableOutput("tablemr"),
        )
    ),
    
    tabPanel("Clustering", 
             
             
             sidebarPanel(
                 
                 sliderInput("rangox", "Selecciona el rango de Dim1:",
                             min = -5, max = 15,
                             value = c(-3,10)),
                 
                 sliderInput("rangoy", "Selecciona el rango de Dim2:",
                             min = -10, max = 10,
                             value = c(-7,5)),
                 selectInput("geomchosen","Elige cómo deseas representar las observaciones", choices = c("Puntos Y Nº de Cliente"=3,
                                                                                                         "Puntos"=1,
                                                                                                         "Nº de Cliente"=2) 
                 ),
                 numericInput(inputId = "idc2", label="Escribe el ID del Cliente",value=NA),
                 
                 h4("¿Tienes dudas?"),
                 h5("Sliders"),
                 "Debes variar los valores de los sliders para hacer zoom en la parte del mapa que desees observar con mayor nitidez.",
                 div(""),
                 h5("Representación de las observaciones"),
                 "Debes seleccionar si quieres que los asegurados se representen cómo puntos, con su número de cliente o ambos.",
                 h5("Id del Cliente"),
                 "Debes  escribir el Nº del cliente del que desees saber el cluster al que pertenece. La respuesta aparece justo debajo del gráfico."
             ),
             
             
             mainPanel(
                 h2("¿Qué hace esta App?", align="center"),
                 p("Esta App te permite observar los 3 clusters, que se formaron a partir del análisis de conglomerados, sobre los ejes artificiales Dim1 y Dim 2 que están muy correlacionados con el dinero que pagan los asegurados y el número de pólizas que tienen, respectivamente.
                   A su vez, utilizando las opciones del panel de la izquierda, te permite  centrarte en la parte del gráfico que desees y también observar al cluster al que pertenece el cliente seleccionado."),
                 plotOutput("clust",height="800px"),
                 textOutput("txtc")
             )
             
             )

    
    
    
    
    ),
)



# Define server logic required to draw a histogram
server <- function(input, output) {
    
    dx3<- reactive({
        
        load("dbd.Rdata")
        names(db.d)[22]<-"Abrev.Cia.x"
        names(db.d)[14]<-"Num Polizas"
        names(db.d)[1]<-"ind"
        
        db.d$F.Baja[is.na(db.d$F.Baja)]<-Sys.Date()
        
        db.d$F.Baja <- as.Date(db.d$F.Baja, format='%d/%m/%y') 
        
        db.d$anti<-as.numeric(difftime(db.d$F.Baja,db.d$F.Alta, units = "weeks")/52.25)
        
        ve<-c("Tot.Pr.Actual","Tot.Com.Bruta","Tot.Pr.Net", "yimp","PR. Media")
        
        dx3<- na.omit(db.d[,c("Est.Civ","Sexo","Grupo Cliente.x","Reg. Laboral","Segmento","Cobro","Provincia", "Plan Com..x", "Abrev.Cia.x","Of.Cobradora","yc","Fis/Jur","Forma Pago","Clase","anti","Num Polizas",ve )])
        
       
         l1<-quantile(dx3[,"Tot.Pr.Actual"],probs=0.75)+3*IQR(dx3[,"Tot.Pr.Actual"])
        
        
        l2<-quantile(dx3[,"Tot.Com.Bruta"],probs=0.75)+3*IQR(dx3[,"Tot.Com.Bruta"])
        
        
        l3<-quantile(dx3[,"Tot.Pr.Net"],probs=0.75)+3*IQR(dx3[,"Tot.Pr.Net"])
        
        
        l4<-quantile(dx3[,"yimp"],probs=0.75)+3*IQR(dx3[,"yimp"])
        
        
        l5<-quantile(dx3[,"PR. Media"],probs=0.75)+3*IQR(dx3[,"PR. Media"])
        
        
        dx3<- dx3[dx3[,"Tot.Pr.Actual"]<round(l1), ]  ## 432 outliers
        
        dx3<- dx3[dx3[,"Tot.Com.Bruta"]<round(l2), ]  #78 outliers
       
        dx3<- dx3[dx3[,"Tot.Pr.Net"]<round(l3), ] #35 outliers
        
        dx3<- dx3[dx3[,"yimp"]<round(l4), ]  ## #68 outliers
        
        dx3<- dx3[dx3[,"PR. Media"]<round(l5), ]  ## 204 outliers
        
        dx3[,ve]<-scale(dx3[,ve])
        
        dx3
    })
    
    
    
    k3<- reactive({
        
        dx3<-dx3()
        
        dx3$yc<-as.numeric(dx3$yc)
        
        res.pca <- PCA(dx3,quali.sup=c(1:10,12:16), quanti.sup = c(11,15,16), ncp = 2)  
        
        Psi = res.pca$ind$coord[,1:2]
        
        set.seed(123)
        
        kmeans(Psi,3,nstart = 25)}) 

    
    
    lmod1<- reactive({
        load("dx1.Rdata")
       
        
        names(dx1)[9]<- "Abrev.Cia.x"
   
        glm(yc~Est.Civ+Sexo+Provincia+`Reg. Laboral`+f.numpol+`Abrev.Cia.x`+Cobro + Of.Cobradora+`Plan Com..x`+`Forma Pago`+Clase+ fanti, data=dx1, family=binomial)
    })
    
    lmod2<- reactive({

        load("dx2.Rdata")
        names(dx2)[5]<- "Abrev.Cia.x"
        
        glm(yimp~`Fis/Jur`+Provincia+`Plan Com..x`+`Abrev.Cia.x`+Of.Cobradora+f.numpol+`Forma Pago`+anti,data=dx2, family=Gamma("log"), maxit=150)
        
        
    })
        
    x<- reactive({
        load("dx1.Rdata")
        load("dx2.Rdata")
        names(dx2)[5]<- "Abrev.Cia.x"
        names(dx1)[9]<- "Abrev.Cia.x"
        
        dx1$pc<-100*predict(lmod1(), type="response")
        
        dx2$pimp<- predict(lmod2(), type="response")
        
        inner_join(dx1,dx2,by="ind") 
    
    })
    
    
output$mapR<- renderPlot({
    
    load("dbd.Rdata")
    
    sel_noB<-data.frame(ind=db.d[which(is.na(db.d$F.Baja)),1])
    
    x<- inner_join(x(),sel_noB,by="ind")  # Solo los clientes que no tienen ninguna póliza en vigor tienen una fecha, es decir  no un Na.
    
    xr<-x[, c("yimp","pimp","yc","pc","ind")]
    
    xe<- x[,c(1:4,6,8,10,14,15,16,37,19,36,7,23,9,26,5,17)]
    
    xe$fanti.y<-as.character(xe$fanti.y)
    
    palet<-c("royalblue","darkblue","skyblue","turquoise4")
    
    if(as.numeric(input$fchosen)!=0) {
    
    if(as.numeric(input$fchosen)==20){
        
        vr<- data.frame(ind=as.numeric(names(k3()$cluster)), clu=as.character(k3()$cluster))
        
        xe[,20]<-inner_join(xe,vr, by="ind")$clu
        
        
        } 
        
    v<- unique(xe[,as.numeric(input$fchosen)])
    
    col<- xe[,as.numeric(input$fchosen)]
    
    for(j in 1:length(v)){
        
        col[xe[,as.numeric(input$fchosen)]==v[j]]<- palet[j]
    }
    
    }else{
        v<- "Todos"
        col<-"royalblue"}
    
        
    sel<-x$ind==input$idc
        
        
    if(as.numeric(input$escala)==0){
        
        pi<- ntile(xr$pimp,1000)
        pi2<- ntile(xr$pc,1000)
        
        l<-perimp<-c()
        l2<-perpc<-c()
        for(i in 1:1000){
            
            l[i]<- length(pi[[i]])
            perimp<- c(perimp, rep(i/10, l[i]))
            
            l2[i]<- length(pi2[[i]])
            perpc<- c(perpc, rep(i/10, l2[i]))
        }
        xr<- xr[order(xr$pimp),]
        xr$perimp<- perimp
        
        xr<- xr[order(xr$pc),]
        xr$perpc<- perpc
        
        
        plot(xr$perpc,xr$perimp, ylab="Percentil Impacto",xlab="Percentil Probabilidad de Cancelar ", col=col,cex=1.6,pch=19)
    
        points(xr$perpc[sel],xr$perimp[sel],col="red", pch=19,cex=2)
        
    }else{
        
        plot(xr$pc,xr$pimp, ylab="Impacto",xlab="Probabilidad de Cancelar", col=col,cex=1.6,pch=19)
        
        points(xr$pc[sel],xr$pimp[sel],col="red", pch=19,cex=2)
    }
    
    if(!is.na(input$idc) & sum(sel)>0){
        legend("topleft", legend=c(v,"Elegido"),col=c(unique(col),"red"), pch=19, cex=1.4,pt.cex=1.4) 
    } else{legend("topleft", legend=c(v),col=c(unique(col)), pch=19, cex=1.4,pt.cex=1.4) }
    
    })


output$tablemr<- renderTable({
    
    if(is.null(input$idc)| is.na(input$idc) ){
        
        
    } else {
        
        x1<- x()[,c(17,1:5,6,8,10,12,14,15,16,37,19,36,7,23,9,26)]
        
        names(x1)<-c("NºCliente","Est.Civ","Sexo", "Grupo Cliente","Reg.Laboral","Segmento","Cobro","Plan Com",
                     "Of Cobradora","PR. Media","Fis/Jur","Forma Pago","Clase","Antigüedad",
                     "Num Polizas(yc)","Num Polizas(Yimp)","Provincia(Yc)","Provincia(Yimp)","Abrev.Cia (Yc)","Abrev.Cia(Yimp)")
        
        x1$Cluster <- rep(0, nrow(x1))
    
        cl<- k3()$cluster[as.numeric(names(k3()$cluster))==input$idc]
        if(!is.null(cl)){  x1$Cluster[input$idc==x1[,1]]<-cl
        }else{
            x1$Cluster[input$idc==x1[,1]]<- NA                            
                                             }
        x1[input$idc==x()$ind,]
    }
})

output$clevlogit<- renderPlot({
    
    vlog<- c(1, 2, 
             14, 14,
             4, 12, 12, 
             16, 16, 
             5,7, 7, 
             6, 9,10, 11,11)
    
    
    
    dfb<-c("Est.Civ - O", "Sexo | S", 
           "Provincia | Otros", "Provincia | Valencia",
           "Reg.Laboral | Reg.General", "Núm Pólizas | 1", "Núm Pólizas | 2-3", 
           "Abrev.Cia | Axa o Mapfre", "Abrev.Cia | Otras", 
           "Cobro | No efectivo","Of.Cobradora | Segregur Central", "Of.Cobradora | Unió de Llauradors", 
           "Plan Com | Sí", "Forma Pago | No Anual","Clase | Producción","Antigüedad | (10,32]","Antigüedad | (2,10]")
    
    colores<-c("yellow3","orange", 
               "red","red",
               "violetred", "purple","purple",
               "green","green",
               "darkgreen","royalblue","royalblue",
               "blue","darkblue","brown","black","black")
    
    xyz<- data.frame(variable=vlog, valor=dfb ,colores=colores,xl=100*(exp(coef(lmod1())[-1])-1))

    lim<-c(-120,1250)
    bol<-xyz$variable==input$fchosen
    
    if(sum(bol)>0){
        
        xyz<- xyz[bol,]
    
    f<-max(xyz$xl)*min(xyz$xl)
    
    if(f>0){
     lim<-c(-5,max(xyz$xl)+5)
        if(max(xyz$xl)<0){lim<-c(min(xyz$xl)-5,5) }
    }
    
    }
    eti<-paste(xyz$valor, round(xyz$xl),sep=" | " )
    
    
    dotchart(xyz$xl,color=xyz$colores, cex=1.4, pch=16, main="Efecto de las variables" , xlab=expression(exp(beta)), labels=paste(eti,"%",sep=""), xlim=lim)
    abline(v=0, col=1)
    
})




output$clevgamma<- renderPlot({
    
    vg<- c(-4, 15, 15,15,
           6,17, 17, 17, 7,
           7,  13, 13,9,11)
    
    vv<- c("Fis/Jur | J", "Provincia | Cataluña", "Provincia | Otros","Provincia | Valencia",
           "Plan Com | Sí","Abrev.Cia | Generali ", "Abrev.Cia | Mapfre o Zurich", "Abrev.Cia | Otras", "Of.Cobradora | Segregur Central",
           "Of.Cobradora | Unió de Llauradors",  "Núm Pólizas | 1", "Núm Pólizas | 2-3","Forma Pago | No Anual","Antigüedad | Sin Niveles (Numérica) |")
    
    
    colores<-c("yellow3","orange", "orange","orange",
               "red","violetred","violetred", "violetred" ,"purple",
               "purple","green","green","darkgreen","royalblue")
    
    
    xyz<- data.frame(variable=vg, valor=vv ,colores=colores, xg=100*(exp(coef(lmod2())[-1])-1) )
    
    
    
    lim<-c(-65,140)
    bol<-xyz$variable==input$fchosen
    
    if(sum(bol)>0){
        
        xyz<- xyz[bol,]
        
        f<-max(xyz$xg)*min(xyz$xg)
        
        if(f>0){
            lim<-c(-5,max(xyz$xg)+5)
            if(max(xyz$xg)<0){lim<-c(min(xyz$xg)-5,5) }
        }
        
    }
    
    eti<-paste(xyz$valor, round(xyz$xg),sep=" | " )
    
    dotchart(xyz$xg,color=xyz$colores, cex=1.4, pch=16, main="Efecto de las variables" , xlab=expression(exp(beta)), labels=paste(eti,"%",sep=""), xlim=lim)
    abline(v=0, col=1)
    
})




output$clust<- renderPlot({
    
    geo<-c("point", "text")
    
    if(input$geomchosen!=3) {geo<-geo[as.numeric(input$geomchosen)]}
    
    ve<-c("Tot.Pr.Actual","Tot.Com.Bruta","Tot.Pr.Net", "yimp","PR. Media")
    
    fviz_cluster(k3(), geom =geo , data = dx3()[,ve], xlim=input$rangox,ylim=input$rangoy)+ theme(plot.title = element_text(face="bold", hjust=0.5,size=24))+ggtitle("Clustering")
})

output$txtc<- renderText({
    
    if(is.null(input$idc2)| is.na(input$idc2) ){
        "Por favor, seleccione el ID del cliente que desee consultar"
        
    } else {
        paste("El cliente pertenece al cluster",as.numeric(k3()$cluster)[as.numeric(names(k3()$cluster))==input$idc2],sep=" ")
    }
})




}


# Run the application 
shinyApp(ui = ui, server = server)
