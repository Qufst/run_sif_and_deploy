# run_sif_and_deploy
With this repository we want to run pictures.sif, which contains R or python environments, create in the repository [create_sif_repository](https://github.com/Qufst/create_apptainer.sif) in order to render some papers/articles with quarto.

# Utilisation

## Creation of apptainer environment.sif
Create an apptainer environnement on [create_sif_repository](https://github.com/Qufst/create_apptainer.sif) follow the README.

## Use this repository
- Clone the repository
- Copy the number of the run and write it in the workflows in the command "gh run download 10160047868 -n apptainer-image -R Qufst/create_apptainer.sif". 
You also could use an apptainer picture based on zenodo like https://github.com/Qufst/run_sif_and_deploy/actions/runs/10281112954.
- Replace image_quarto.sif by the name of the apptainer pictures you download in the workflows.
- import in the repository the qmd you want to render, with every dependencies you need. 


- You can also use multiple apptainer environments simultaneously as in the example: (code)[https://github.com/Qufst/run_sif_and_deploy/actions/runs/10250734965], and the result of the exemple: (exemple)[https://qufst.github.io/run_sif_and_deploy/].