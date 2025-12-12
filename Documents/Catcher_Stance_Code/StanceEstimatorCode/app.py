import math
import os.path
import tempfile

import pandas as pd
import streamlit as st
import cv2
import StanceEstimatorOnline
import time

if "clicked" not in st.session_state:
    st.session_state.clicked = False

st.set_page_config(page_title = "File Uploader")

upload_containter = st.container()

if not st.session_state.clicked:
    with upload_containter:
        folder = st.file_uploader("Choose a Folder of Videos", accept_multiple_files="directory", type = "mp4")
        bottom_crop = 0
        image = None
        if folder:
            tfile = tempfile.NamedTemporaryFile(delete = False)
            tfile.write(folder[0].read())
            cap = cv2.VideoCapture(tfile.name)
            fps = cap.get(cv2.CAP_PROP_FPS)
            success, frame = cap.read()
            cap.release()

            if not success:
                st.error("Check Folder for errors")
            else:
                h, w, _ = frame.shape
                bottom_crop = st.slider("Bottom", 0, h, h)

                cropped = frame[0:bottom_crop, 0:w]
                st.image(cropped, channels="BGR")

        def mark_clicked():
            st.session_state.clicked = True
            st.session_state.final_crop = bottom_crop
            st.session_state.folder = []
            for f in folder:
                f.seek(0)
                st.session_state.folder.append({'contents': f.read(), 'number': f.name[0:-4]})

            upload_containter.empty()
            st.rerun()
        st.button(label = "Submit Crop", on_click = mark_clicked)

if st.session_state.clicked:
        processed_container = st.empty()
        time_container = st.empty()
        timeSum = 0
        filesProcessed = 0
        folderLength = len(st.session_state.folder)

        stances = {'pitch': [], 'stance': [], 'confidence': []}
        for file_contents in st.session_state.folder:
            curr = time.time()
            tfile = tempfile.NamedTemporaryFile(delete=False)
            tfile.write(file_contents.get('contents'))
            tfile.flush()
            tfile.close()
            cap = cv2.VideoCapture(tfile.name)
            fps = cap.get(cv2.CAP_PROP_FPS)
            success, frame = cap.read()
            stance = StanceEstimatorOnline.ProcessVideo(tfile.name, st.session_state.final_crop)
            stances['pitch'].append(file_contents.get('number'))
            stances['stance'].append(stance[0])
            stances['confidence'].append(stance[1])
            timeSum += time.time() - curr
            filesProcessed += 1
            secondsremaining = (timeSum/filesProcessed) * (folderLength -  filesProcessed)

            with processed_container:
                processed_container.empty()
                st.write(f"Files processed = {filesProcessed}")
            with time_container:
                time_container.empty()
                st.write(f"Time remaining = {math.floor(secondsremaining/60)} minutes")

        st.write(pd.DataFrame.from_dict(stances))

##folder = "/Users/noahlippman/Documents/Catcher_Vids_Xavier/video"
#st.video("/Users/noahlippman/Documents/Catcher_Vids_Xavier/video/132.mp4")