library(readxl)
Data_1Y <- read_excel("somefile.xlsx")
#View(Data_1Y)
Data_1Y$td=(Data_1Y$...3)/3600
#data.frame(matrix(NA, nrow = 2, ncol = 3))
#M_24=matrix(0L, nrow = 24, ncol = 148, byrow=TRUE)
M24=matrix(NA, nrow = 24, ncol = 148)
View(M24)
for (row in 1:20) 
{
  t=Data_1Y[row,"td"]
  t_24=as.integer(t)
  print(t_24)
  for (sg in 1:10)
  { 
    print(sg)
    print(M24[t_24+1][sg])
    if (is.na(M24[t_24+1][sg]))                    # +1 because t_24 ranges from 0 to 22 or in theory, 0 to 23
    {                                             # The first row (row 1) indicates that  start time is in between 0 and 1    
                                                  # Similarly, the last ie 24th row indicates that sstart time is between 23 and 24
      M24[t_24+1][sg]=as.numeric(Data_1Y[row,sg+3])# sg+3 because sg ranges from 1 to 148 while the column for segment starts from 4
      print(M24[t_24+1][sg])  
    }
    else
    {
      
    }
  }
}
