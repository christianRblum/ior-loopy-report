Author: Christian Blum, member of Thomas Bugnyar and Barbara Klump lab
last updated: 2026-04-24


Note: package load order matters, i haven't yet had the time to implement namespace operators (::)
restart R if you encounter problems and then rerun this script to ensure correct load order


This is a script to calculate Inter Observer Reliability Test Scores using ICCs and explore differences between coders.

Before running: download all the CSV files from Loopy for all the videos of all the coders you want to compare
and place them in the folders "reference" (=CoderA) and "trainee" (=CoderB)

This script then merges all information into one dataframe and assigns Coder ID to every event, based on the title
(that is the title on Loopy at the time of downloading the csv, not the filename of the csv).

It also calculates durations and frequencies for all behaviours and corrects directionality for group protocols
(relevant for ad libitum coding). In doing so "A chasing B" and "B being chased by A" are no longer interpreted
as two different events.

It will then provide ICC results for several categories of data (always for durations and frequencies separately):
  - all data, only social, only non-social, without termination, without modifiers
This gives a quick summary on where potential differences come from (relevant only for further training, not for
reporting IOR scores in publications etc.)
  - report only "all data" and "all frequencies" results in your publications

After that, the script does some additional data exploration, showing how much each behaviour differed between coders
in duration and frequency, for the complete dataset and per video.

It then sums up all the findings in a PDF (for all plots) and an Excel file (for all data), wich are exported to
the project main-directory.

Manual edits are not required

This script was written by Christian Blum for the labs of Thomas Bugnyar and Barbara Klump.
Intended to compare 2 coders at a time
