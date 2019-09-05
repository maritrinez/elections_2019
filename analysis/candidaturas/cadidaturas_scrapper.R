# Scrapper of the pdf 
# I copy paste the whole pdf to a txt file
# manually remove the firs and las useless lines
# Here I get the circunscriptions and candidaures
# so all the municipalities parties can be found in a unique file



#------- LIBRARIES & WD ------
library(tidyverse)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# ------ Municipalities names -----
municipalities <- read_csv('../data/parties2019.csv') %>% 
  distinct(location_id, muni_name)

# ------ FUNCTIONS ----
files2table <- function (path, prov_id) {
  # Get files
  files <- list.files(path)
  
  # Init variables and table
  je <- NULL
  c <- NULL
  p_l <- NULL
  p_s <- NULL
  tb <- NULL
  
  # Buid the table
  for (file in files) {
    print(file)
    raw <- read_lines(glue::glue('{path}{file}'))
    
    # Remove those starting with a number
    tmp <- str_extract(raw, "^([^0-9].*)")
    tmp <-tmp[!is.na(tmp)]
    # tmp[1:20]
    
    
    # Remove 'Titulares', 'Suplentes'
    tmp <- str_extract(tmp, "^(?!Titular|Suplent|Sin candidatos|Página|Butlletí Oficial.+|B Butlletí Oficial).+")
    tmp <-tmp[!is.na(tmp)]
    # head(tmp, 20)
    # tail(tmp, 20)
    
    # Merge in one string
    tmp <- str_replace_all(toupper(paste(tmp, collapse="\n")), '\\n', '')
    # tmp
    
    # Trim spaces beforeand after slashes
    tmp <- str_replace_all(tmp, '- ', '-')
    tmp <- str_replace_all(tmp, ' -', '-')
    # tmp
    
    # Split lines lines by 'Circunscripción' & 'Candidatura'
    tmp <- str_replace_all(tmp, 'CANDIDATURA NÚM. \\d+: ', '\\\n')
    tmp <- str_replace_all(tmp, 'CIRCUNSCRIPCIÓN ELECTORAL', '\\\nCIRCUNSCRIPCION')
    tmp <- str_replace_all(tmp, 'JUNTA ELECTORAL DE ZONA DE', '\\\nJUNTA ELECTORAL')
    tmp <- unlist(str_split(tmp, '\n'))
    
    # Build a table 
    # for each line
    # if starts with 'JUNTA ELECTORAL', update value
    # if starts with 'CIRCUNSCRIPCION', update value
    # else, update party value & bind row to the data set
    for (l in tmp) {
      if (l == '') {
        next
      } else if (grepl('^JUNTA ELECTORAL', l)) {
        je <- str_replace_all(l, 'JUNTA ELECTORAL ', '')
      } else if (grepl('^CIRCUNSCRIPCION', l)) {
        c <- str_trim(str_replace_all(l, 'CIRCUNSCRIPCION ', ''))
      } else {
        p_l <- str_replace(str_trim(str_extract(l, "[^(]+")), '[0-9]+-', '')
        p_s <- str_trim(str_extract(l, "(?<=\\().*?(?=\\))"))
        row <- tibble(junta_electoral = je, muni_name = c, party_acronym_original = p_s, party_long = p_l)
        tb <- bind_rows(tb, row)
      }
    }
  }
  tb$province_id <- prov_id
  return(tb)
}
muni_left <- function(prov_id, completed) {
  p <- glue::glue('^{prov_id}')
  left <- municipalities %>% 
    filter(grepl(pattern = p, x = location_id)) %>% 
    filter(!(toupper(muni_name) %in% completed))
  return(left)
}

# ------ BCN -----
tb_bcn <- files2table('raw/bcn/', '08')
left <- muni_left('08', unique(tb_bcn$muni_name))

# ------ GIR -----
tb_gir <- files2table('raw/gir/', '17')
left <- muni_left('17', unique(tb_gir$muni_name))

# ------ LLE -----
tb_lle <- files2table('raw/lle/', '25')
left <- muni_left('25', unique(tb_lle$muni_name))

# ------ TAR -----
tb_tar <- files2table('raw/tar/', '43')
left <- muni_left('43', unique(tb_tar$muni_name))


aux <- tb_tar %>% filter(junta_electoral == 'GIRONA')
aux

# ------  WRITE FILE -----
tb_bcn %>% 
  bind_rows(tb_gir) %>% 
  bind_rows(tb_lle) %>% 
  bind_rows(tb_tar) %>% 
  write_csv('candidaturas.csv')
