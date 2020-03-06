""" Convert all media assets located in a specified directory."""
# tburrows13
import glob
import os
from optparse import OptionParser

from moviepy.editor import VideoFileClip


def get_dir_files(dir_path, patterns=None):
    """Get all absolute paths for pattern matched files in a directory.

    Args:
        dir_path (str): The path to of the directory containing media assets.
        patterns (list of str): The list of patterns/file extensions to match.

    Returns:
        (list of str): A list of all pattern-matched files in a directory.
    """
    if not patterns or type(patterns) != list:
        print('No patterns list passed to get_dir_files, defaulting to patterns.')
        patterns = ['*.mp4', '*.avi', '*.mov', '*.flv']

    files = []
    for pattern in patterns:
        dir_path = os.path.abspath(dir_path) + '/' + pattern
        files.extend(glob.glob(dir_path))

    return files

def modify_clip(path, output):
    """Handle conversion of a video file.

    Args:
        path (str): The path to the directory of video files to be converted.
        output (str): The filename to associate with the converted file.
    """
    clip = VideoFileClip(path).subclip(10, 12)
    clip = clip.crop(x_center=540, y_center=960, width=1080, height=608)
    clip = clip.resize(0.6)

    clip.write_videofile(output, codec='libx264')
    print('File: {} should have been created.'.format(output))


if __name__ == '__main__':
    status = 'Failed!'
    parser = OptionParser(version='%prog 1.0.0')
    parser.add_option('-p', '--path', action='store', dest='dir_path',
                      default='.', type='string',
                      help='the path of the directory of assets, defaults to .')

    options, args = parser.parse_args()
    print('Running against directory path: {}'.format(options.dir_path))
    path_correct = raw_input('Is that correct?').lower()

    if path_correct.startswith('y'):
        dir_paths = get_dir_files(options.dir_path)
        for dir_path in dir_paths:
            output_filename = 'converted_' + os.path.basename(dir_path)
            modify_clip(path=dir_path, output=output_filename)

        status = 'Successful!'

    print('Conversion {}'.format(status))
    
    
    
    
    
    
    
    
#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""Convert all media assets located in a specified directory."""
import glob
import os
from optparse import OptionParser

from moviepy.editor import VideoFileClip


def get_dir_files(dir_path, patterns=None):
    """Get all absolute paths for pattern matched files in a directory.

    Args:
        dir_path (str): The path to of the directory containing media assets.
        patterns (list of str): The list of patterns/file extensions to match.

    Returns:
        (list of str): A list of all pattern-matched files in a directory.
    """
    if not patterns or type(patterns) != list:
        print('No patterns list passed to get_dir_files, defaulting to some video types.')
        patterns = ['*.mp4', '*.avi', '*.mov', '*.flv']

    files = []
    for pattern in patterns:
        dir_path = os.path.abspath(dir_path) + '/' + pattern
        files.extend(glob.glob(dir_path))

    return files

def modify_clip(path, output):
    """Handle conversion of a video file.

    Args:
        path (str): The path to the directory of video files to be converted.
        output (str): The filename to associate with the converted file.
    """
    clip = VideoFileClip(path)
    clip = clip.rotate(270)
    clip = clip.crop(x_center=540, y_center=960, width=1080, height=608)
    clip = clip.resize(width=1920)

    clip.write_videofile(output, codec='libx264')
    print('File: {} should have been created.'.format(output))


if __name__ == '__main__':
    status = 'Failed!'
    parser = OptionParser(version='%prog 1.0.0')
    parser.add_option('-p', '--path', action='store', dest='dir_path',
                      default='.', type='string',
                      help='the path of the directory of assets, defaults to .')

    options, args = parser.parse_args()
    print('Running against directory path: {}'.format(options.dir_path))
    path_correct = raw_input('Is that correct?').lower()

    if path_correct.startswith('y'):
        dir_paths = get_dir_files(options.dir_path)
        for dir_path in dir_paths:
            output_filename = 'converted_' + os.path.basename(dir_path)
            modify_clip(path=dir_path, output=output_filename)

        status = 'Successful!'

    print('Conversion {}'.format(status))