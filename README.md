Author: Christian Blum, member of Thomas Bugnyar and Barbara Klump lab

This is a script to calculate Inter Observer Reliability Test Scores using ICCs and explore differences between coders.

Before running: download all the CSV files from Loopy for all the videos of all the coders you want to compare
and place them in the corerct folders as specified in the script comments

This script then merges all information into one dataframe and assigns Coder ID to every event, based on the title
(that is the title on Loopy at the time of downloading the csv, not the filename of the csv).

It also calculates durations and frequencies for all behaviours and corrects directionality for group protocols
(relevant for ad libitum coding). In doing so "A chasing B" and "B being chased by A" are no longer interpreted
as two different events.

It will then provide separate ICC results for durations and frequencies, for the entire dataset and some selected sub-sets to identify systemic errors in coding.
Report only "all data (dur)" and "all data (frq)" results in your publications, as the sub-sets are only used for training.

After that, the script does some additional data exploration, showing how much each behaviour differed between coders 
in duration and frequency, for the complete dataset and per video.

It then sums up all the findings in a PDF (for all plots) and an Excel file (for all data), wich are exported to
the project main-directory.

Manual edits to the script should not be required, besides specifying the project at the very beginning.
Coder names are derived dynamically from the input data, provided it follows lab-internal standards for file-names.

This script was written by Christian Blum for the labs of Thomas Bugnyar and Barbara Klump.
Intended to compare 2 coders at a time
