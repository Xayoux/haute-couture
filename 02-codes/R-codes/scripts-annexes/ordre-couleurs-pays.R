# Définitions de l'ordre/couleurs des pays pour graphs ---------------------
# Ordre des pays
ordre_pays_exporter <- 
  list(
    general    = c("RDM", "Amérique","Moyen-Orient",
                   "Reste de l'Asie", "Chine et HK",   
                   "Suisse", "Reste de l'UE", "Italie", "France"),
    
    bijouterie = c("RDM", "Amérique", "USA", "Moyen-Orient",
                   "Turquie", "Reste de l'Asie", "Chine et HK",   
                   "Suisse", "Reste de l'UE", "Italie", "France")
  )

ordre_pays_importer <-
  list(
    general =
      c("RDM", "Amérique", "USA", "Moyen-Orient", "ARE",
        "Reste de l'Asie" , "Japon et Corée", "Chine et HK",
        "Suisse", "Reste de l'UE", "Italie", "France"),
    
    bijouterie =
      c("RDM", "Amérique", "USA", "Moyen-Orient", "ARE",
        "Reste de l'Asie" , "Japon et Corée", "Chine et HK",
        "Suisse", "Reste de l'UE", "Italie", "France")
  )


# Couleurs des pays
couleurs_pays_exporter <- 
  list(
    general = 
      c(
        "France"                    = "#006CA5",
        "Italie"                    = "#04B2DE",
        "Reste de l'UE"             = "#48CAE4",
        "Suisse"                    = "#90E0EF",
        "Chine et HK"               = "#ae4d4d",
        "Reste de l'Asie"           = "#F7B4BB",
        "Moyen-Orient"              = "#3AB0AA",
        "Amérique"                  = "#d499ed",
        "RDM"                       = "#D9D9D9"
      ),
    
    bijouterie = 
      c(
        "France"                    = "#006CA5",
        "Italie"                    = "#04B2DE",
        "Reste de l'UE"             = "#48CAE4",
        "Suisse"                    = "#90E0EF",
        "Chine et HK"               = "#ae4d4d",
        "Reste de l'Asie"           = "#F7B4BB",
        "Turquie"                   = "#008270",
        "Moyen-Orient"              = "#3AB0AA",
        "USA"                       = "#7600bc",
        "Amérique"                  = "#d499ed",
        "RDM"                       = "#D9D9D9"
      )
  )

couleurs_pays_importer <-
  list(
    general = 
      c(
        "France"                    = "#006CA5",
        "Italie"                    = "#04B2DE",
        "Reste de l'UE"             = "#48CAE4",
        "Suisse"                    = "#90E0EF",
        "Chine et HK"               = "#ae4d4d",
        "Japon et Corée"            = "#F46D75",
        "Reste de l'Asie"           = "#F7B4BB",
        "ARE"                       = "#008259",
        "Moyen-Orient"              = "#3AB0AA",
        "USA"                       = "#7600bc",
        "Amérique"                  = "#d499ed",
        "RDM"                       = "#D9D9D9"
      ),
    
    bijouterie = 
      c(
        "France"                    = "#006CA5",
        "Italie"                    = "#04B2DE",
        "Reste de l'UE"             = "#48CAE4",
        "Suisse"                    = "#90E0EF",
        "Chine et HK"               = "#ae4d4d",
        "Japon et Corée"            = "#F46D75",
        "Reste de l'Asie"           = "#F7B4BB",
        "ARE"                       = "#008259",
        "Moyen-Orient"              = "#3AB0AA",
        "USA"                       = "#7600bc",
        "Amérique"                  = "#d499ed",
        "RDM"                       = "#D9D9D9"
      )
  )


# Type de ligne des pays
linetype_exporter <- 
  list(
    general = c(
      "France"                    = "solid",
      "Italie"                    = "dashed",
      "Reste de l'UE"             = "dotted",
      "Suisse"                    = "longdash",
      "Chine et HK"               = "solid",
      "Reste de l'Asie"           = "dashed",
      "Moyen-Orient"              = "solid",
      "Amérique"                  = "solid",
      "RDM"                       = "solid"
    ),
    bijouterie = c(
      "France"                    = "solid",
      "Italie"                    = "dashed",
      "Reste de l'UE"             = "dotted",
      "Suisse"                    = "longdash",
      "Chine et HK"               = "solid",
      "Reste de l'Asie"           = "dashed",
      "Turquie"                   = "dashed",
      "Moyen-Orient"              = "solid",
      "USA"                       = "solid",
      "Amérique"                  = "solid",
      "RDM"                       = "solid"
    )
  )


# Définition de l'ordre pour les gammes ------------------------------------
ordre_gammes <- c("H", "M", "L")

