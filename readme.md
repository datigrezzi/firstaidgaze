This is a shiny app that was developed mainly for the Many Babies 2 project (https://osf.io/jmuvd/).

To use it, the design should include some sort of a calibration validation stimulus (i.e. a target that changes location at known times).
The user should then upload the raw gaze data and an accumulative image of the calibration stimulus with the same name as the file used during the recording.

After the image is loaded, the user should click on the image to mark all target locations. Then the timestamp for each target location should be entered in the "Targets Onset" field as semicolon-separated-values.

After verifying that the data has been read correctly, the user can click on the "Start Calibration" button to train a regression model for the X and Y coordinates separately.
The procedure is simliar to that descrived by Frank and colleagues (2012).

Soon I will add the possiblity to download the data with the adjusted coordinates and a help section directly in the app.

More functionality is planned for this app, including noise reduction and missing-data interpolation.

Feel free to write me for any questions or comments through the contact form on my website (www.datigrezzi.com/contact).

If you use this app or the procedure described make sure to cite the following papers:

Aldaqre, I. (in prep) First Aid Gaze: An open-source application for cleaning eye tracking data.
Frank, M.C., Vul, E. & Saxe, R. (2012 )Measuring the Development of Social Attention Using Free-Viewing. Infancy, 17(4), 355–375.
