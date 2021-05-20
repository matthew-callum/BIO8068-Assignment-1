#Following section of code is to ensure all packages for the analysis are loaded
library(warbleR)
library(stringr)
library(behaviouR)
library(tuneR)
library(seewave)
library(ggplot2)
library(dplyr)
library(vegan)
source("nes8010.R")

#First step for the analysis is to download the files of the species to be
#analysed from Xeno-Canto, for this analysis we have selected: Great Tits, Blue Tits
#and Coal Tits for analysis

#Before we begin we need to ensure that we have enough (more than 50) recordings
#of these birds available to undergo the analysis
Great_Tit_Calls <- query_xc(qword = 'Parus major cnt:"united kingdom" type:call len:5-30', download = FALSE)
Blue_Tit_Calls <- query_xc(qword = 'Cyanistes caeruleus cnt:"united kingdom" type:call len:5-30', download = FALSE)
Coal_Tit_Calls <- query_xc(qword = 'Periparus ater cnt:"united kingdom" type:call len:5-30', download = FALSE)
#Each of the three species has enough records to undergo the analysis so we may continue

#Before we download these files we need to create the files that they will be downloaded into
dir.create(file.path("Great_Tit_Calls"))
dir.create(file.path("Blue_Tit_Calls"))
dir.create(file.path("Coal_Tit_Calls"))

#With the created file paths we can now download these files into the folders
#query_xc(X = Great_Tit_Calls, path="Great_Tit_Calls")
#query_xc(X = Blue_Tit_Calls, path="Blue_Tit_calls")
#query_xc(X = Coal_Tit_Calls, path="Coal_Tit_Calls")
#THESE ARE DOWNLOADED DO NOT REPEAT THIS CODE

#Now that the files are downloaded we will then rename them for audio analysis
#To get a correct class column for the MFCC analysis

#First we rename Great Tit Calls
old_files <- list.files("Great_Tit_Calls", full.names=TRUE)
new_files <- NULL
for(file in 1:length(old_files)){
  curr_file <- str_split(old_files[file], "-")
  new_name <- str_c(c(curr_file[[1]][1:2], "-song_", curr_file[[1]][3]), collapse="")
  new_files <- c(new_files, new_name)
}
file.rename(old_files, new_files)

#Next we rename Blue Tit Calls
old_files <- list.files("Blue_Tit_Calls", full.names=TRUE)
new_files <- NULL
for(file in 1:length(old_files)){
  curr_file <- str_split(old_files[file], "-")
  new_name <- str_c(c(curr_file[[1]][1:2], "-song_", curr_file[[1]][3]), collapse="")
  new_files <- c(new_files, new_name)
}
file.rename(old_files, new_files)

#Finally we rename Coal Tit Calls
old_files <- list.files("Coal_Tit_Calls", full.names=TRUE)
new_files <- NULL
for(file in 1:length(old_files)){
  curr_file <- str_split(old_files[file], "-")
  new_name <- str_c(c(curr_file[[1]][1:2], "-song_", curr_file[[1]][3]), collapse="")
  new_files <- c(new_files, new_name)
}
file.rename(old_files, new_files)

#Now as the files have been renamed we can paste the mp3s over to a new folder
#called "Tit_Audio"
dir.create(file.path("Tit_Audio"))

#Now copy the files from the 3 species folders to the new Tit_Audio folder
file.copy(from=paste0("Great_Tit_Calls/",list.files("Great_Tit_Calls")),
          to="Tit_Audio")
file.copy(from=paste0("Blue_Tit_Calls/",list.files("Blue_Tit_Calls")),
          to="Tit_Audio")
file.copy(from=paste0("Coal_Tit_Calls/",list.files("Coal_Tit_Calls")),
          to="Tit_Audio")

#Now that we have all of the renamed files within the same folder we can convert
#The files from the MP3 file format to the .WAV format that is used in the following
#analysis, and delete the .mp3 files that we aren't using

#mp32wav(path="Tit_Audio", dest.path="Tit_Audio")
#unwanted_mp3 <- dir(path="Tit_Audio", pattern="*.mp3")
#file.remove(paste0("Tit_Audio/", unwanted_mp3))
#DONE ALREADY DO NOT REPEAT THIS CODE

#With the .WAV files ready to go we can undergo feature extraction for the following
#analysis

#Tit_Audio_mfcc <- MFCCFunction(input.dir = "Tit_Audio", max.freq = 7000)
#DO NOT REPEAT AGAIN, RESULTS ALREADY GAINED

#Now that we have the feature extraction for the audio we can undergo pca analysis
#and then see the results of this analysis
Tit_Audio_pca <- ordi_pca(Tit_Audio_mfcc[, -1], scale=TRUE)
summary(Tit_Audio_pca)

#Finally we can create a graph to show the results for the analysis visually for the report
Tit_sco <- ordi_scores(Tit_Audio_pca, display="sites")
Tit_sco <- mutate(Tit_sco, group_code = Tit_Audio_mfcc$Class)

ggplot(Tit_sco, aes(x=PC1, y=PC2, colour=group_code)) +
  geom_point()


#Now we can work on the creation of a few separate oscillograms and spectographs of these species for comparisons
#For the Blue Tit we are using the file named: Cyanistescaeruleus-song_187829
#For the Great Tit we are using the file named: Parusmajor-song_95062
#For the Coal Tit we are using the file named: Periparusater-song_396928

#Next we read these selected files into the R Enviroment as sound waves
BlueTitWaveFile <- readWave("Tit_Audio/Cyanistescaeruleus-song_187829.wav")
GreatTitWaveFile <- readWave("Tit_Audio/Parusmajor-song_95062.wav")
CoalTitWaveFile <-readWave("Tit_Audio/Periparusater-song_396928.wav")

#Then we create Oscillograms using these selected read in files
oscillo(BlueTitWaveFile)
oscillo(GreatTitWaveFile)
oscillo(CoalTitWaveFile)

#Finally for the analysis we will use these selected files to create colour spectrograms
#To allow for comparisons between them
SpectrogramSingle(sound.file = "Tit_Audio/Cyanistescaeruleus-song_187829.wav", min.freq = 4000, max.freq = 8000, Colors = "Colors")
SpectrogramSingle(sound.file = "Tit_Audio/Parusmajor-song_95062.wav", Colors = "Colors")
SpectrogramSingle(sound.file = "Tit_Audio/Periparusater-song_396928.wav", min.freq = 3000, max.freq = 8000, Colors = "Colors")

