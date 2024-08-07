# Définitions de l'ordre/couleurs des pays pour graphs ---------------------
# Ordre des pays
ordre_pays_exporter <- 
  list(
    general    = c("RDM", "Amérique","Moyen-Orient",
                   "Reste de l'Asie", "Chine et HK",   
                   "Suisse", "Reste de l'UE", "Italie", "France"),
    
    bijouterie = c("RDM", "Amérique", "États-Unis", "Moyen-Orient",
                   "Turquie", "Reste de l'Asie", "Chine et HK",   
                   "Suisse", "Reste de l'UE", "Italie", "France")
  )

ordre_pays_importer <-
  list(
    general =
      c("RDM", "Amérique", "États-Unis", "Moyen-Orient", "Émirats arabes unis",
        "Reste de l'Asie" , "Japon et Corée", "Chine et HK",
        "Suisse", "Reste de l'UE", "Italie", "France"),
    
    bijouterie =
      c("RDM", "Amérique", "États-Unis", "Moyen-Orient", "Émirats arabes unis",
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
        "États-Unis"                = "#7600bc",
        "Amérique"                  = "#d499ed",
        "RDM"                       = "#D9D9D9"
      ),

    marge_extensive =
      c(
        "FRA"                       = "#006CA5",
        "ITA"                       = "#04B2DE",
        "CHN"                       = "#ae4d4d"
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
        "Émirats arabes unis"       = "#008259",
        "Moyen-Orient"              = "#3AB0AA",
        "États-Unis"                = "#7600bc",
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
        "Émirats arabes unis"       = "#008259",
        "Moyen-Orient"              = "#3AB0AA",
        "États-Unis"                = "#7600bc",
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
      "États-Unis"                = "solid",
      "Amérique"                  = "solid",
      "RDM"                       = "solid"
    )
  )

linetype_importer <- 
  list(
    general = c(
      "France"                    = "solid",
      "Italie"                    = "dashed",
      "Reste de l'UE"             = "dotted",
      "Suisse"                    = "longdash",
      "Chine et HK"               = "solid",
      "Japon et Corée"            = "longdash",
      "Reste de l'Asie"           = "dashed",
      "Émirats arabes unis"       = "solid",
      "Moyen-Orient"              = "dashed",
      "Amérique"                  = "dashed",
      "États-Unis"                = "solid",
      "RDM"                       = "solid"
    )
  )


# Définition de l'ordre pour les gammes ------------------------------------
ordre_gammes <- c("Haute", "Moyenne", "Basse")

