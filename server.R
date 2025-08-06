server = function(input, output, session) {
	appDataObject <- reactiveValues()

	# Loading user help info file 
	infoDescriptions = read_yaml("infoAppDescriptions.yaml")
	# Adding user help section to global data object
	appDataObject$descriptions = infoDescriptions
	appDataObject$comparisons = list() # list to store paired-wise comparions 
	appDataObject$reportedBoxes = list() # list to store reported list 
	appDataObject$deCatColors <- c('#91cc75', '#fac858', '#aaaaaa',  '#73c0de','#ee6666')
	# Adding server component of bookmark service
	bookmarkServer("Bookmark", appDataObject)
	# Adding server component of data upload service
  	uploadDataset("Upload", parentSession = session, appDataObject)
	# Adding server component of sample exploration service
	sampleServer("Sample", parentSession = session, appDataObject)
	# Adding server component of gene view service
	geneServer("Gene",appDataObject)
	# Adding server component of differential expression service
	degServer("DE", parentSession=session, appDataObject)
	# Adding server component of app header
	headerServer("Header", appDataObject)
}