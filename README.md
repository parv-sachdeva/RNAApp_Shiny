# App Strurcture

This is a R shiny application and the files and folders are used as follows:

- `global.R`: Used for package imports and global declarations.
- `server.R`: Contains server logic and function calls to other server components.
- `ui.R`: Contains overall ui components and function calls to other ui components.
- `R/`: Contains all dependency functions and modules of the app.
- `sample-data/`: Contains demo input files to run the app.
- `infoAppDescriptions.yaml`: Contains user guide which is presented in the app.

# Dependencies

Need to install all modules imported in `global.R`.

# Running the app

The app can be run using the command: `shiny::runApp('<path to app folder>')`

# Contact

Contact [Shreya Sharma](mailto:sshreya319@gmail.com)  for any information or issues.