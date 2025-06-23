dl_baci_fixed <- function(revision = "HS92", dl_folder, rm_csv = TRUE,
                    dl_zip = FALSE){


  ## Information sur BACI ----------------------------------------------------
  # Lien vers la page BACI du cepii
  html_baci <- rvest::read_html("https://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=37")

  # Récupérer l'information sur la dernière version de BACI
  version <- "202401b"

  # Demander si l'utilisateur veut télécharger cette version de baci
  question_dl <- stringr::str_glue("Voulez-vous t\u00E9l\u00E9charger BACI {version} ?")
  reponse_dl <- svDialogs::dlg_message(question_dl, "yesnocancel")$res

  if (reponse_dl == "yes"){
    # Créer le lien pour télécharger la dernière version de BACI
    dl_link <- stringr::str_glue("http://www.cepii.fr/DATA_DOWNLOAD/baci/data/BACI_{revision}_V{version}.zip")

    # # Tester si le fichier zip de BACI existe déjà

    # Si dl_zip == TRUE, télécharger le fichier zip
    # Télécharge le zip même s'il existe
    if (dl_zip == TRUE) {
      curl::multi_download(
        dl_link,
        here::here(dl_folder, stringr::str_glue("BACI_{revision}_V{version}.zip"))
      )
    }
    # Si dl_zip == FALSE, vérifier si le fichier zip existe.
    # S'il existe alors, on ne télécharge pas. Sinon, on télécharge.
    else {
      if (!file.exists(here::here(dl_folder, stringr::str_glue("BACI_{revision}_V{version}.zip")))) {
        # Si le fichier zip n'existe pas, télécharger BACI
        curl::multi_download(
          dl_link,
          here::here(dl_folder, stringr::str_glue("BACI_{revision}_V{version}.zip"))
        )
      }
    }

    # Décompresser le fichier zip au même endroit
    print("Extraction des fichier csv")
    here::here(dl_folder, stringr::str_glue("BACI_{revision}_V{version}.zip")) |>
      utils::unzip(exdir = dl_folder)

    # Créer les formats parquet pour BACI
    print("Cr\uE9ation des fichiers parquet")
    analyse.competitivite::transfo_baci_pq(
      csv_folder = dl_folder,
      path_output = dl_folder,
      version = version
    )

    # Supprimer les fichiers csv de BACI pour gain de place si rm_csv == TRUE
    if (rm_csv == TRUE) {
      dl_folder |>
        list.files(full.names = TRUE, pattern = "^BACI.*csv") |>
        purrr::walk(file.remove)
    }
    print("Donn\u00E9es de BACI t\u00E9l\uE900charg\u00E9es !")
  } else {
    print(stringr::str_glue("Refus de t\u00E9l\u00E9charger BACI {version}"))
  }
}
