
library(glue)
library(tidyverse)

path <- './migrations'

files <- list.files(path)
file <- files[5]
# Cria pasta e copia arquivo pra cada pasta
for (file in files) {
  folder <- str_replace(file, '\\.markdown$|\\.Rmd$', '')
  new.file <- str_sub(folder, 12, -1)
  dir.create(glue('./_posts/{folder}'))
  file.copy(glue('{path}/{file}'), glue('./_posts/{folder}/{new.file}.Rmd'))
}
