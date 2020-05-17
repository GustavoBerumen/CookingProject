""" Create and save a subclip of videos according to the start and end times store in a csv file."""
# make a copy of videos with session togethers and differentiate their name
import os
import pandas as pd
import numpy as np
import moviepy
from moviepy.editor import *

def createVideos( item, typeItem ):
    
    #defines the max duration of the videos
    maxDur = 6
    
    item = str(item)
    
    #set the name of csv file 
    #this_csv = item + '_both_type.csv'
    this_csv = item + '_both.csv'
    
    # path of folder containing the videos to a variable
    videos_path  = 'C://Users//LPXJGB//Desktop//PC_CookingStudy//'
    
    # path of file containing the start and end times
    csv_path = 'C://Users//LPXJGB//Desktop//CHI-2020//CookingProject//outputs//' + str(typeItem) + '//'
    
    # path of folder where videos will be saved
    # output_path = 'C://Users//LPXJGB//Desktop//CHI-2020//videos_output//' + item + '// '
    output_path = 'C://Users//LPXJGB//Desktop//CHI-2020//videos_output//' + item + '// '

    # create folder directory to store videos  
    try:
        # Create target Directory
        os.mkdir(output_path)
        print("Directory " , output_path,  " Created ") 
    except FileExistsError:
        print("Directory " , output_path,  " already exists")
    
    # full path of csv file
    file_path = csv_path+this_csv
    
    # read csv file 
    csv_file = pd.read_csv(file_path)
    
    # subset columns of csv file
    csv_data =  pd.DataFrame(csv_file, columns= ['participant', 'session', 'start',
                'end'])
                
    # find position of row with NA // this is the numbers of videos that will be created it 
    index = int((list(map(tuple, np.where(np.isnan(csv_data.participant)))))[0][0])

    # get length of a column // this is the numbers of videos that will be created it     
    # index = len(csv_data.participant)
    
    
    # iterate over csv data to create videos
    for i in range(index):
        # iterate over rows in csv_data
        session = csv_data.session[i]
        participant = int(csv_data.participant[i])
        
        # concatenate values to create video name  
        if participant < 10:
            video_name = "p" + "0" + str(participant) + "-" + str(session) + ".mp4" 
        else:
            video_name = "p" + str(participant) + "-" + str(session) + ".mp4" 
        
        # set video path
        video = videos_path + video_name
        
        # get start time
        start = csv_data.start[i] - 2
        if start <0:
            start = 0
        
        # get end time
        end = csv_data.end[i] + 2
    
        # get duration time
        duration = end - start
        
        # check duration video
        if (duration > maxDur):
            middle = start + (duration/2)
            start = middle - maxDur/2
            end = middle + maxDur/2
            
        # make clip 
        clip = (VideoFileClip(video)
            .subclip(start, end) # select a segment of the video
            .resize(0.25)) # resize (keep aspect ratio)
            
        # make output name
        if i <9:
            output_name = output_path + "0" + str(i+1) + '_' + item + '_' + video_name 
        else:
            output_name = output_path + str(i+1) + '_' + item + '_' + video_name 
            
        # Write the result to a file
        clip.write_videofile(output_name)
        
       
    return
      
items = ["plate", "container"] 
 
lenItems = len(items)
     
for i in range(lenItems):
    thisItem = items[i]
    createVideos(thisItem, 'u')