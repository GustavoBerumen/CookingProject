# importing libraries 
import cv2 
import numpy as np 
   
# Create a VideoCapture object and read from input file 
cap = cv2.VideoCapture('p09-reg.mp4') 


# Check if camera opened successfully 
if (cap.isOpened()== False):  
  print("Error opening video  file") 
   
# Read until video is completed 
while(cap.isOpened()): 
      
  # Capture frame-by-frame 
  ret, frame = cap.read() 
  if ret == True: 
   
    # Display the resulting frame 
    cv2.imshow('Frame', frame) 
   
    # Press Q on keyboard to  exit 
    if cv2.waitKey(25) & 0xFF == ord('q'): 
      break
   
  # Break the loop 
  else:  
    break
   
# When everything done, release  
# the video capture object 
cap.release() 
   
# Closes all the frames 
cv2.destroyAllWindows()


import numpy as np
import cv2 as cv

cap = cv.VideoCapture('p09-reg.mp4')
while cap.isOpened():
    ret, frame = cap.read(500)
    # if frame is read correctly ret is True
    if not ret:
        print("Can't receive frame (stream end?). Exiting ...")
        break
    gray = cv.cvtColor(frame, cv.COLOR_BGR2GRAY)
    cv.imshow('frame', gray)
    if cv.waitKey(1) == ord('q'):
        break
cap.release()
cv.destroyAllWindows()