# Script dont le but est d'exécuter une à une les fonctions de téléchargement
# des bases de données afin de pouvoir vérifier la dernière version disponible
# et répondre au cas par cas de façon consciente.

# Télécharger la base de données BACI
dl_baci(
  dl_folder = path_baci_folder_origine, rm_csv = TRUE
)

# Télécharger la base de données Gravity 
dl_gravity(dl_folder = here::here("..", "Gravity"), dl_zip = FALSE)




