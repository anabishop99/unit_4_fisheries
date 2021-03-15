library(tidyverse)

# toy data
data1 = data.frame(ID=c(1,2), 
                   X1 = c("a1", "a2"))
data2 = data.frame(ID = c(2,3), 
                   X2 = c("b1", "b2"))
data1
data2

# left_join()
data12_left = left_join(data1, data2)
data12_left

# with piping
data12_left = data1 %>%
  left_join(data2, by = "ID")
data12_left

# inner join()
data12_inner = data1 %>%
  inner_join(data2)
data12_inner

# full join()
data12_full = data1 %>%
  full_join(data2)
data12_full

# semi join()
data12_semi = data1 %>%
  semi_join(data2)
data12_semi

# anti join()
data12_anti = data1 %>%
  anti_join(data2)
data12_anti

##################
# Transformations

survey = data.frame(quadrat_ID = c(101, 102, 103, 104),
                    barnacle_n = c(2, 11, 8, 27),
                    chiton_n = c(1,0,0,2),
                    mussel_n = c(0,1,1,4))
survey

long = survey %>%
  pivot_longer(c("barnacle_n", "chiton_n", "mussel_n"), names_to = "taxon", values_to = "counts")
long

wide = long %>%
  pivot_wider(names_from = "taxon", values_from = "counts")
wide
