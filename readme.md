This app is designed to adjust systematic offset in screen-based eye tracking studies. It was developed mainly for the Many Babies 2 project (https://osf.io/jmuvd/). To use the app, the design must include some sort of a calibration-validation stimulus (i.e. a target with known positions at known times).

Feel free to write me for any questions or comments through the contact form on my website (www.datigrezzi.com/contact).

If you use this app or the procedure described make sure to cite the following papers:
- Aldaqre, I. (in prep) First Aid Gaze: An open-source application for cleaning eye tracking data.
- Frank, M.C., Vul, E. & Saxe, R. (2012) Measuring the Development of Social Attention Using Free-Viewing. Infancy, 17(4), 355–375.

# Instructions

### The Data Tab

In the left panel you can specify the **Eye Tracker** brand for the app to guess variable names.
Below that there are some specifics about data import (field **Seperator**, **Decimal** seperator and **NA String** or empty cells values).

In the right panel you can check if _variable names_ are correct, otherwise modify as needed.

Then, you can **Upload Gaze Data** that contains the eye tracking data. The app currently supports data sets that include a *single recording*. Data with multiple recordings (or participants) will be supported in the future.

After the data is uploaded successfully, the head of the data set is presented in a table at the bottom of the page for verification purposes.

### The Stimulus Tab

Here you can choose some information about the calibration-validation stimulus. The app will look for stimuli in the _MediaVariable_ that have the same name as the uploaded stimulus image, without the extension.

**Stimulus Type** The app currently supports two types of such stimuli:
1. A single stimulus that included either one or more validation point at a known time from stimulus start.
2. Multiple stimuli, each containing a single validation point at a known time from stimulus start.

**Targets Onset** should include the time at which each validation point was presented, relative to stimulus start. Multiple values should be separated by a semicolon. Here, you could also add a time offset of about 200ms to each onset time, to make sure participants were already looking at the target after it changed place.

**Target Duration** is the duration for which each validation point was presented. Both Target Onset and Target Duration should be specified in the same units as the Timestamp in the data.

**Exclude Stimulus** is helpful to specify stimuli that share same name of the calibration-validation stimulus but should not be included for the calibration.

You can then **Upload Stimulus Image**, which should be an accumulative image of the calibration stimulus with the same name as the file used during the recording for calibration validation (or at least the common portion of the file name, like “validation_.png” if validation stimuli were called "validation_1.png", "validation_2.png" and so on).

**Screen Width** and **Screen Height** should include the size of the presentation screen.

**Stimulus Top** and **Stimulus Left** should include the top-left point coordinates of the stimulus, relative to the screen.

After the image is loaded correctly, it will be presented below with the gaze data. Now you can click on the stimulus image to mark all target locations. It is **important** to click on the targets in the same order as the "Targets Onset" values.

**Max Point Distance** is the maximum distance between the target position and points to be considered in the calibration.

[check point]
the user can click on the "Start Calibration" button to train a regression model for the X and Y coordinates separately. The procedure is similar to that described by Frank and colleagues (2012).

After the calibration is finished, the plot in the [Validation] tab will be updated with the updated coordinates in red. The rest lot only presents data from the validation stimulus. Finally, the user can download the data with the adjusted coordinates in variables called “newx” and “newy”.
