import os, keyboard, string
os.environ['PYGAME_HIDE_SUPPORT_PROMPT'] = 'hide'
import pygame

pygame.init()
pygame.mixer.init()

# Download sound files from Glarses: https://drive.google.com/drive/folders/1ND88io8f4qIt5Ku2TJ21_LQqB6ju8r2P
# See also: https://www.youtube.com/watch?v=P_9vXJZVT54
keys = list(string.ascii_uppercase) + ['BACKSPACE', 'CAPS LOCK', 'ENTER', 'SPACE']
keySounds = dict(map(lambda k: (k, pygame.mixer.Sound(f'NK Cream Sounds/{k}.mp3')), keys))

print("Leave this running in the background, playing keyboard clicks as you type in any foreground app.")
down = {}
while True:
    event = keyboard.read_event()
    key = event.name.upper()
    if event.event_type == keyboard.KEY_DOWN:
        if not key in down: # ignore repeating
            if key in keySounds: keySounds[key].play()
            else: keySounds['SPACE'].play()
        down[key] = True
    else: del down[key]
