""" Create and save a subclip of videos according to the start and end times store in a csv file."""
# make a copy of videos with session togethers and differentiate their name
import os
import pandas as pd
import moviepy
from moviepy.editor import *

# set item
item = 'oil'

# set the name of csv file 
this_csv = '_' + item + '_both.csv'

# set percentage of videos to create
per_videos = .12

# path of folder containing the videos to a variable
videos_path  = 'C://Users//LPXJGB//Desktop//PC_CookingStudy//'
# path of folder where videos will be saved
output_path = 'C://Users//LPXJGB//Desktop//CHI-2020//videos_output//'
# path of file containing the start and end times
csv_path = 'C://Users//LPXJGB//Desktop//CHI-2020//cookingProject//outputs//'

# full path of csv file
file_path = csv_path+this_csv

# read csv file 
csv_file = pd.read_csv(file_path)

# subset columns of csv file
csv_data =  pd.DataFrame(csv_file, columns= ['participant', 'session', 'start',
            'end', 'duration'])
            
# get number of videos to be created            
subset = round(len(csv_data.participant) * per_videos)

# iterate over csv data to create videos
for i in range(subset):
    # iterate over rows in csv_data
    session = csv_data.session[i]
    participant = csv_data.participant[i]
    
    # concatenate values to create video name  
    if participant < 10:
        video_name = "p" + "0" + str(participant) + "-" + str(session) + ".mp4" 
    else:
        video_name = "p" + str(participant) + "-" + str(session) + ".mp4" 
      
    # set video path
    video = videos_path + video_name
    
    # get start time
    start = csv_data.start[i] - 2

    # get end time
    end = csv_data.end[i] + 2

    # get duration time
    duration = end - start
    
    # check duration video
    if (duration > 16):
        end = start + 16
        
    # make clip 
    clip = (VideoFileClip(video)
        .subclip(start, end) # select a segment of the video
        .resize(0.5)) # resize (keep aspect ratio)
        
    # make output name
    output_name = output_path + str(i+1) + '_' + item + '_' + video_name 
    
    # Write the result to a file
    clip.write_videofile(output_name)