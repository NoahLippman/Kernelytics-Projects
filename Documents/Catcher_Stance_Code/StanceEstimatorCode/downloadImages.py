import cv2
import os

def download(path, vidNum):
    output_dir = '/Users/noahlippman/Documents/Catcher_Pictures_cropped_4+Secs'
    os.makedirs(output_dir, exist_ok=True)

    cap = cv2.VideoCapture(path)

    fps = cap.get(cv2.CAP_PROP_FPS)
    total_frames = int(cap.get(cv2.CAP_PROP_FRAME_COUNT))
    duration = total_frames / fps
    start_time = duration - 4
    start_frame = int(start_time * fps)
    cap.set(cv2.CAP_PROP_POS_FRAMES, start_frame)

    frame_count = 0
    saved_count = 0
    interval = 10  # Save every 10th frame

    while True:
        ret, frame = cap.read()
        if not ret:
            break
        if saved_count < 10 and frame_count % interval == 0:
            cv2.imwrite(f'{output_dir}/vid{vidNum}frame_{saved_count:04d}.jpg', frame)
            saved_count += 1
        frame_count += 1
    cap.release()
    print(f'Extracted {saved_count} frames from {frame_count} total frames')

if __name__ == "__main__":
    folder_path = "/Users/noahlippman/Documents/Catcher_Vids_Xavier/video"
    i = 0
    for filename in os.listdir(folder_path):
        file_path = os.path.join(folder_path, filename)
        if os.path.isfile(file_path):
            download(file_path, i)
            i += 1