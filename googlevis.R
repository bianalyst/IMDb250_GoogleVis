## install.packages('RJSONIO') 
## install.packages("googleVis", 'C:/Users/cw13/Documents/R/win-library/2.15')
library(googleVis)
data <- read.csv("./data/Top250 (unique).csv", sep="," , header=TRUE, skip=0, stringsAsFactors=FALSE)

### Rename Columns
nn<-names(data)
nn[1:16] <- c("position", "const","created", "modified", "description"
              ,"Title","Title.type","Directors","YouRated","IMDbRating"
              ,"Runtime","Year","Genres","NumVotes", "ReleaseDate", "URL")
names(data) <- nn

### Clean-up Data: ReleaseDate
correct <- nchar(data$ReleaseDate) == 10
## data[correct,]$ReleaseDate  <-as.Date(substr(data[correct,]$ReleaseDate,0,10), "%Y-%m-%d") 
## data[!correct,]$ReleaseDate <-as.Date(paste(substr(data[!correct,]$ReleaseDate,0,7), "01", sep="-"), "%Y-%m-%d")
data[correct,]$ReleaseDate  <-        substr(data[correct,]$ReleaseDate,1,10)
data[!correct,]$ReleaseDate <-  paste(substr(data[!correct,]$ReledaseDate,1,7), "01", sep="-")
##data[correct,]$ReleaseDate  <-  paste(substr(data[correct,]$ReleaseDate,1,4), substr(data[correct,]$ReleaseDate,6,7), sep="")
##data[!correct,]$ReleaseDate <-  paste(substr(data[!correct,]$ReleaseDate,1,4), substr(data[!correct,]$ReleaseDate,6,7), sep="")

Flag_19940512 <-   substr(data$ReleaseDate,1,3)=="-01"
data[Flag_19940512,]$ReleaseDate  <-             "1994-05-12"

data$ReleaseDate <- as.Date(data$ReleaseDate, "%Y-%m-%d")
## data$ReleaseDate <- as.numeric(data$ReleaseDate)
data$ReleaseDate <-paste(substr(data$ReleaseDate,1,4), substr(data$ReleaseDate,6,7), substr(data$ReleaseDate,9,10), sep ="/")
data$ReleaseDate <- as.Date(data$ReleaseDate, "%Y/%m/%d")

drop_columns <- c( "const", "created", "modified", "description", "Genres", "URL")
data= data[,!(names(data) %in% drop_columns)]

### Clean-up Data: Directors
Charles_Chaplin <- data$Directors ==              "Charles Chaplin"
data[Charles_Chaplin,]$Directors  <-              "Charlie Chaplin"
FFCoppola       <- data$Directors ==              "Francis Coppola"
data[FFCoppola,]$Directors        <-              "Francis Ford Coppola"
Danny_Boyle     <- substr(data$Directors,1,11) == "Danny Boyle"
data[Danny_Boyle,]$Directors      <-              "Danny Boyle"


### add Number of Movies per Director in Top 250
freq  <- as.data.frame(table(data$Directors))
data  <- data.frame <- merge(data, freq, by.x = "Directors", by.y = "Var1")
Total <- data$Freq * data$IMDbRating
data <- cbind(data,Total)

### add Running of Number Movies in Top 250
x<- data.frame(prop.table(table(data$Year, data$Directors)))
x1<-x[x$Freq!=0,]    #Drop unproductive years
perc<- as.data.frame(within(x1, { Percentage <- ave(Freq, Var2, FUN = cumsum)}))

perc$Var1<-as.numeric(levels(perc$Var1)[as.integer(perc$Var1)])  #Year
perc$Var2<-as.character(levels(perc$Var2)[as.integer(perc$Var2)]) #Director

result            <- merge(data, perc, by.x=c("Year","Directors"), by.=c("Var1", "Var2"))
result$Percentage <- result$Percentage*100

myStateSettings <-'
{"xZoomedDataMax":9.3,"nonSelectedAlpha":0.4
 ,"yZoomedIn":false
 ,"iconKeySettings":[{"key":{"dim0":"Sergio Leone"}},{"key":{"dim0":"Frank Capra"}},{"key":{"dim0":"Stanley Kubrick"}},{"key":{"dim0":"Charlie Chaplin"}},{"key":{"dim0":"Billy Wilder"}},{"key":{"dim0":"Christopher Nolan"}},{"key":{"dim0":"Peter Jackson"}},{"key":{"dim0":"Alfred Hitchcock"}},{"key":{"dim0":"Steven Spielberg"}},{"key":{"dim0":"Quentin Tarantino"}},{"key":{"dim0":"Akira Kurosawa"}},{"key":{"dim0":"Clint Eastwood"}},{"key":{"dim0":"Martin Scorsese"}}]
 ,"orderedByX":false,"yZoomedDataMin":0.004
 ,"dimensions":{"iconDimensions":["dim0"]}
 ,"yZoomedDataMax":0.04,"xZoomedIn":false
 ,"iconType":"BUBBLE", "xAxisOption":"3"
 ,"xLambda":1,"yLambda":1,"sizeOption":"5"
 ,"uniColorForNonSelected":false
 ,"playDuration":40000,"time":"1920","showTrails":false
 ,"yAxisOption":"2","duration":{"timeUnit":"Y","multiplier":1}
 ,"xZoomedDataMin":8,"colorOption":"_UNIQUE_COLOR","orderedByY":false}
'

### gvisMotionChart
M = gvisMotionChart(data=result
                    , idvar="Directors", timevar="Year"
                    , xvar="Percentage", yvar="IMDbRating", sizevar="NumVotes"
                     , options=list(state=myStateSettings)
                    , chartid="IMDB_Top250"
                    )
plot(M)
T <- gvisTable(result[,c(1:2,4)],options=list(
                                  # width=600, height=300, fontSize=8, page='disable'
                                              ))
TM<- gvisMerge(T, M, horizontal=TRUE,
                 tableOptions="bgcolor=\"#CCCCCC\" cellspacing=10")
plot(TM)

cat(M$html$chart, file="tmp.html")

