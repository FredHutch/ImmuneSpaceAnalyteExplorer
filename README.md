The purpose of this shiny app is to prototype the user experience for the Analyte Explorer feature in ImmuneSpace.

Implementation Notes:
Once the user experience has been finalized using the prototype, the expectation is to implement the feature in LabKeyModules/AnalyteExplorer in a similar architecture to LabKeyModules/ResourcesPage.  The key components of the architecture are a react + d3 front-end in the LabKeyModules' module and then the necessary API endpoints in the ImmuneSpaceLabKeyAPI R package that provides a PlumbeR service from the WebServe machines and pre-processing done within ImmuneSpaceCronjobs R package located on the RServe machines.

The result of the above implementation should be the front-end as a webpart or even full view within the LabKey Server framework.  This webpart or view should be visible to the public, i.e. available to users that have not logged in yet.  So far, all the endpoints within the ImmuneSpaceLabKeyAPI package use summary data and should be allowable.  However, this may be an issue to consider long-term - whether some endpoints will need to be private or inaccessible to users who are not logged in.

Response-Call Notes:
A main request has been segmentation of expression data by response status (e.g. low, mid, high).  In order to do this, ImmuneSpace will need a module "HIPCResponse" that does the following (modeled on the DGEA work):
- Creates a table for the response call data to be stored in the DB that has participant-level information such as assay used (HAI, NAb, or ELISA), baseline and later timepoint used in calculation, status (low, mid, high), discretization points (e.g. 30% and 70%), method (e.g. fold-change, adjusted-MFC based on titeR package) and a version number.  To see what sort of information is desired, it is best to look at the ImmuneSignatures2 package's response call methods and the output.
- A custom report that when run takes the available data and updates the response call table.  Ideally, the actual work should be done by a method that has been abstracted out to a separate package that is easier to version control and unit test. This abstracted function should simply take a studyid as input similar to how the bulk microarray and RNAseq function create-matrix.R wraps the actual pipeline: https://github.com/RGLab/LabKeyModules/blob/master/HIPCMatrix/pipeline/tasks/create-matrix.R#L13.
- Once the custom report is implemented and all studies with relevant data have the module turned on, the processing method described can then be run on all studies from the RServe or remotely by an admin user in the same way as https://github.com/RGLab/LabKeyModules/blob/master/HIPCMatrix/pipeline/tasks/runMxFromCL.R
- The reason for the above abstraction is to allow all studies to be re-run easily if one of the underlying methods or related logic have changed.  It should be expected that this will happen.

Version: 1.0

