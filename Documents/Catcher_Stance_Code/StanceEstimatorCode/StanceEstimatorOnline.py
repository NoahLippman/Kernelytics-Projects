import os
from inference_sdk import InferenceHTTPClient
import cv2
import uuid
from ultralytics import YOLO
import pandas as pd
import streamlit as st
import time

# initialize the client
CLIENT = InferenceHTTPClient(
    api_url="https://serverless.roboflow.com",
    api_key="dQKcEYMXDMssKrAQV4ck"
)
ball_track_model = YOLO('https://data.balldatalab.com/index.php/s/YkGBwbFtsf34ky3/download/ball_tracking_v4-YOLOv11.pt')


def trim_video_for_model(input_path, output_path, vertical_crop, time_crop = -1, frame_crop = 0):
    cap = cv2.VideoCapture(input_path)

    # Get video properties
    fps = cap.get(cv2.CAP_PROP_FPS)
    width = int(cap.get(cv2.CAP_PROP_FRAME_WIDTH))
    height = int(cap.get(cv2.CAP_PROP_FRAME_HEIGHT))

    start_time = 3 - time_crop
    start_frame = int(start_time * fps) if time_crop != -1 else frame_crop
    cap.set(cv2.CAP_PROP_POS_FRAMES, start_frame)
    new_height = height - vertical_crop

    # Use H.264 codec (most compatible with models)
    fourcc = cv2.VideoWriter_fourcc(*'mp4v')  # or 'mp4v'
    out = cv2.VideoWriter(output_path, fourcc, fps, (width, new_height))

    frame_count = 0
    while True:
        ret, frame = cap.read()
        if not ret:
            break

        out.write(frame[:new_height, :])
        frame_count += 1

    cap.release()
    out.release()

    return [start_frame, output_path]

def estimateStance(coordinates : dict) -> str:
    try:
        preds = coordinates.get('predictions')
        keypoints = preds[0].get('keypoints')
        keypointList = [[i.get('x'), i.get('y')] for i in keypoints]

        LeftKnee = keypointList[0] if keypointList[0][0] < keypointList[2][0] else keypointList[2]
        RightKnee = keypointList[2] if keypointList[0][0] < keypointList[2][0] else keypointList[0]
        LeftAnkle = keypointList[1] if keypointList[0][0] < keypointList[2][0] else keypointList[3]
        RightAnkle = keypointList[3] if keypointList[0][0] < keypointList[2][0] else keypointList[1]

        if abs(LeftKnee[0] - RightKnee[0]) < 50:
            return "None"
        elif abs(LeftKnee[1] - RightKnee[1]) < 10:
            return "Two Knees Up"
        elif LeftKnee[1] > RightKnee[1]:
            if RightAnkle[0] > RightKnee[0]:
                return "Left Leg Kickstand"
            else:
                return "Right Knee Down"
        else:
            if LeftAnkle[0] < LeftKnee[0]:
                return "Right Leg Kickstand"
            else:
                return "Left Knee Down"
    except:
        pass

def ProcessVideo(path, vertical_crop):
    coords = findCoordinates(path, vertical_crop, 0)
    if coords is None:
        coords = findCoordinates(path, vertical_crop, 2)
        return coords if coords != None else ["None", 0]

    return coords

def findCoordinates(path, vertical_crop, time_crop):
    frame_count = 0
    frames_tracked = 0
    stanceCounts = {"Two Knees Up":0, "Right Knee Down":0, "Left Leg Kickstand":0, "Left Knee Down":0, "Right Leg Kickstand":0}

    # Trim video
    ball_identified = False
    frame_count_initial = 0
    uid1 = uuid.uuid4().hex[:8]
    start_frame, trimmed_path = trim_video_for_model(path, f'cropped.mp4', vertical_crop, time_crop=time_crop)
    results = ball_track_model.predict(trimmed_path, show=False, stream=True)
    for r in results:
        boxes = r.boxes  # Boxes object for bbox outputs
        try:
            conf = boxes.conf[0]
            if conf > .35:
                ball_identified = True
                break
        except:
            pass
        frame_count_initial += 1
        if (frame_count_initial > 400):
            return None
    cap = None
    if ball_identified:
        start_frame, new_path = trim_video_for_model(path, f"full_height.mp4", 0, frame_crop=start_frame + frame_count_initial)
        cap = cv2.VideoCapture(new_path)
        fps = cap.get(cv2.CAP_PROP_FPS)
    #placeholder = st.empty()
    stack = []
    while frame_count < 20:
        ret, frame = cap.read()
        if not ret:
            break
        if frame_count % 2 == 0:
            result = CLIENT.infer(frame, model_id="catching-stance-estimator-uimxn/6")
            stack.append(result.get("predictions")[0].get("keypoints"))
            stance = estimateStance(result)
            print(stance)
            try:
                if stance != "None":
                    stanceCounts[stance] = stanceCounts.get(stance) + 1
                    frames_tracked += 1
            except:
                pass

        try:
            for joint in stack[-1]:
                coords = (int(joint.get("x")), int(joint.get("y")))
                cv2.circle(frame, coords, 10, (0, 0, 0), cv2.FILLED)
        except:
            pass

        #with placeholder.container():
            #placeholder.image(frame, channels="BGR")

        frame_count += 1

    cv2.destroyAllWindows()

    mcs = max(stanceCounts, key = stanceCounts.get)
    return [mcs, stanceCounts.get(mcs)/frames_tracked]