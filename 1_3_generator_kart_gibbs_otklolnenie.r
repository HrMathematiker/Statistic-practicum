# 100 на 100 5 городов и 10 шахт
# расст между городами 40+
#   у каждого города 2 шахты на расстоянии от 10 до 20
#   расстояние между шахтами 10+ ctrl shift c 
library(ggplot2)
type <- c("town","mine","mine","town","mine","mine",
            "town","mine","mine","town","mine","mine","town","mine","mine")
y <- runif(15,0,100)
x <- runif(15,0,100)
df <- data.frame(x,y, type) #координаты городов и шахт здесь

towns <- seq(1,13,by = 3) 
numer <- 1:5
df1 <- data.frame(numer, towns)

mines <- c(2,3,5,6,8,9,11,12,14,15)
nomer<-1:10
df2 <- data.frame(nomer, mines)

#gibbs +
#acceptance-rejectance 
for(town in towns) #общий цикл: перегенируем город и две его шахты
{
    print("TOWN #") 
    print(town)
    
    f = function(i) return (sqrt( (x[town]-x[i])^2 + (y[town]-y[i])^2 ))
    ind = function(x) 
      {
        if (x > 40) return (1)
        else return (0)
      }
       #генерим первый город чтоб выполнялось условие :
       # расстояние до других городов >40 
      while ( ind(f(1)) + ind(f(4)) +ind (f(7)) + ind(f(10)) + ind(f(13)) < 4 )
       {  
          x[town] <- runif(1,0,100) 
          y[town] = runif(1,0,100)
       }
    # Для проверки генерации городов   
    # for (t in towns)
    # {
    #   for (k in towns)
    #   {
    #     print(sqrt( (x[t]-x[k])^2 + (y[t]-y[k])^2 ))
    #   }
    # }
    # 
    # print(c(x[1],y[1]))
    # print(c(x[4],y[4]))
    # print(c(x[7],y[7]))
    # print(c(x[10],y[10]))
    # print(c(x[13],y[13]))

 
    
    
    
  # mines of town
    
  for (i in c(town+1, town+2))
  {    
      #f = function(i) return (sqrt( (x[town]-x[i])^2 + (y[town]-y[i])^2 ))
      g <- function(k) return (sqrt((x[i]-x[k])^2 + (y[i]-y[k])^2 ) ) 
      
      ind2 = function(x) 
      {
        if (x > 10) return (1)
        else return (0)
      }
      
      ind1 = function(x) 
      {
        if (x > 10 & x < 20) return (1)
        else return (0)
      }

        while (ind1(f(i)) +
                ind2(g(2)) + ind2(g(3)) + ind2(g(5)) + ind2(g(6)) + ind2(g(8)) 
                +ind2(g(9)) + ind2(g(11)) + ind2(g(12)) + ind2(g(14)) + ind2(g(15)) < 10  ) #условия на шахты
        {
          x[i] <- runif(1,0,100)
          y[i]<- runif(1,0,100)
        }
    }
   

    print("координаты сгенерированного города и двух его шахт")  
    print(x[town])
    print(y[town])
    print(x[town+1])
    print(y[town+1])
    print(x[town+2])
    print(y[town+2])
   
}    
    df <- data.frame(x,y, type)
    library(ggplot2)
    ggplot(df, aes(x,y))+
      geom_point(aes(shape = type, color = type, size =I(5)))+
      scale_x_continuous(breaks = seq(0, 100, 10)) +
      scale_y_continuous(breaks = seq(0, 100, 10)) +
      theme_light()
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  


  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  