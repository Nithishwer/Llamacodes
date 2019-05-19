library(readxl)
Data_1Y <- read_excel("somefile.xlsx")
#View(Data_1Y)
Data_1Y$td=(Data_1Y$...3)/3600
M24=matrix(0L, nrow = 24, ncol = 148, byrow=TRUE)
View(M24)
for (row in 1:nrow(Data_1Y)) 
{
  t=Data_1Y[row,"td"]
  t_24=as.integer(t)
  for (j in 1:148)
  { 
    print(t_24)
    print(j)
    print(M24[t_24+1][j])
    print(M24[t_24+1][j] == 0)
    if (M24[t_24+1][j] == 0)                    # +1 because t_24 ranges from 0 to 22 or in theory, 0 to 23
    {                                            # The first row (row 1) indicates that  start time is in between 0 and 1    
                                                 # Similarly, the last ie 24th row indicates that sstart time is between 23 and 24
      M24[t_24+1][j]=as.numeric(Data_1Y[row,j+3])  # j+3 because j ranges from 1 to 148 while the column for segment starts from 4
    }
  }
}
