dl_gravity_fixed <- function(dl_folder, dl_zip = FALSE){

  # Lien vers la page de la base de données Gravity sur le site du CEPII
  html_gravity <- rvest::read_html("https://www.cepii.fr/CEPII/fr/bdd_modele/bdd_modele_item.asp?id=8")

  # Récupérer la version de Gravity pour télécharger la dernière version disponible
  version <-
    html_gravity |>
    rvest::html_nodes(xpath = '//*[@id="telechargement"]') |>
    rvest::html_nodes("em") |>
    rvest::html_text() |>
    stringr::str_extract("\\d{6}")

  
  # Demander à l'utilisateur s'il souhaite dl cette version de gravity
  question_dl <- stringr::str_glue("Voulez-vous t\u00E9l\u00E9charger Gravity {version} ? (Y/n) : ")
  reponse_dl <- svDialogs::dlg_message(question_dl, "yesnocancel")$res

  if (reponse_dl == "yes"){
    # Récupérer le lien de téléchargement du fichier zip de Gravity
    dl_link <-
      html_gravity |>
      rvest::html_nodes("a") |>
      rvest::html_attr("href") |>
      (\(links) grep("csv", links, value = TRUE, ignore.case = TRUE))() |>
                                                                    (\(link) grep(version, link, value = TRUE))()


    # Chemin d'accès au dossier contenant la dernière version de Gravity
    gravity_folder <-
      here::here(
        dl_folder,
        stringr::str_glue("Gravity_csv_V{version}")
      )

    # Chemin d'accès au fichier zip de Gravity dans sa dernière version
    gravity_zip <-
      here::here(
        gravity_folder,
        stringr::str_glue("Gravity_csv_V{version}.zip")
      )


    # Créer le dossier où sera mis le zip téléchargé, s'il n'existe pas déjà
    if (!dir.exists(here::here(dl_folder))) {
      dir.create(gravity_folder, recursive = TRUE)
    }


    # Télécharger le fichier zip de Gravity
    if (dl_zip == TRUE) { # Si TRUE télécharger dans tous les cas
      curl::multi_download(
        dl_link,
        gravity_zip
      )
    }
    else { # Si Faux télécharger le zip que s'il n'existe pas
      if (!file.exists(gravity_zip)) {
        # Si le fichier zip n'existe pas, télécharger BACI
        curl::multi_download(
          dl_link,
          gravity_zip
        )
      }
    }

    # Décompresser le fichier zip au même endroit
    print("Extraction des fichiers de Gravity")
    gravity_zip |>
      utils::unzip(exdir = gravity_folder)

    # Créer les formats parquet de Gravity
    print("Cr\uE9ation des fichiers parquet")
    analyse.competitivite::transfo_gravity_pq(
      csv_folder = gravity_folder,
      path_output = gravity_folder,
      version = version
    )

    print("Donn\u00E9es de Gravity t\u00E9l\u00E9charg\u00E9es")
  } else {
    print(stringr::str_glue("Refus de t\u00E9l\u00E9charger Gravity {version}"))
  } 
}
