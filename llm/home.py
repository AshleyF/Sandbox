import openai
import os
import asyncio
import speech_recognition as sr
import pyttsx3
from kasa import SmartBulb

openai.api_key = "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"

speech = pyttsx3.init()

def query(prompt):
    completion = openai.ChatCompletion.create(model="gpt-3.5-turbo", messages=[{"role": "user", "content": prompt}])
    return completion.choices[0].message.content

async def kasaBulb(name, on):
    #bulb = SmartBulb('192.168.4.193') # Livingroom Floor Lamp
    #await bulb.update()
    #print(bulb.alias)
    name = name.lower()
    if name == 'tall1' or name == 'sofa' or name == 'couch':
        if on: os.system('kasa --host 192.168.4.175 --type plug on') # Corner Living Room Floor Lamp
        else: os.system('kasa --host 192.168.4.175 --type plug off') # Corner Living Room Floor Lamp
    elif name == 'tall2' or name == 'bookcase':
        if on: os.system('kasa --host 192.168.4.193 --type plug on') # Livingroom Floor Lamp
        else: os.system('kasa --host 192.168.4.193 --type plug off') # Livingroom Floor Lamp
    elif name == 'overhanging' or name == 'reading' or name == 'black':
        if on: os.system('kasa --host 192.168.4.204 --type bulb on') # Living Room Black Lamp
        else: os.system('kasa --host 192.168.4.204 --type bulb off') # Living Room Black Lamp

def lamp(room, name, on):
    #print(f'Lamp: {room} {name} {on}')
    asyncio.run(kasaBulb(name, on))

description = '''
    My home has three lights in the living room: two tall white floor lamps, one by the sofa
    and one by a bookcase, and a tall overhanging black lamp for reading.'''

def automation(prompt, time):
    context = f"Pretend that you have access to Python APIs to control my home. {description} You can turn them on and off by calling a function lamp('living', 'tall1', True) or lamp('living', 'Tall2', false) for example. We like all the lights off at night and the living room lights on during the day. Respond to the following prompt with a message for the human and function calls to do it"

    print('----------------------------------------------------------------------')
    print(f'Prompt: {prompt}')
    response = query(f'{context}, given at {time}: {prompt}')
    print('----------------------------------------------------------------------')
    print(f'Response: {response}')

    code = query(f'Please extract just the code from this: {response}')
    print('----------------------------------------------------------------------')
    print(f'Code: {code}')
    for line in code.splitlines():
        try:
            exec(line.strip())
        except Exception as e:
            print(f'ERROR: {repr(e)}')

    human = query(f'Please extract just the part meant for humans (without including any mention of the code) from this: {response}')
    print('----------------------------------------------------------------------')
    print(f'Human: {human}')
    speech.say(human)
    speech.runAndWait()

#automation('its getting to be that time', '9:35pm')
#automation('Im getting tired', '8:45pm')
#automation('Im done reading', '7:20pm')
#automation('hey Im reading over here', '7:20pm')
#automation('its time for diner. we really dont need the livingroom to be so bright right now.', '7pm')
#automation('turn off all the livingroom lights please', '')
#automation('I have a headache', '')
#automation('good morning', '8:10am')
#automation('good night', '8:10pm')
#automation('illuminate', '9:13am')

#lamp('living', 'tall2', False)

# print(query('write python code to recognize speech from the microphone and print text'))
#print(query('write python code to speak some text'))
def main():
    while True:
        r = sr.Recognizer()
        with sr.Microphone() as source:
            r.adjust_for_ambient_noise(source)
            print()
            #input('Press enter to continue...')
            print('Say something...')
            audio = r.listen(source)
        try:
            prompt = r.recognize_google(audio)
            isQuestionForAI = 'yes' in query(f'Does the following sound like a question for an AI (please answer "yes" or "no"): {prompt}').lower()
            isAutomation = 'yes' in query(f'{description} Does the following have something to do with turning on or off the lights in the house or with it being too dim or to bright in the room (please answer "yes" or "no"): {prompt}').lower()
            if isQuestionForAI or isAutomation:
                if isAutomation:
                    print('AUTOMATION-RELATED')
                    automation(prompt, '8:30pm')
                else:
                    print(f'HEARD: {prompt}')
                    response = query(prompt)
                    print(f'RESPONSE: {response}')
                    speech.say(response)
                    speech.runAndWait()
            else:
                print(f'NOT FOR ME: {prompt}')

        except:
            print('I didn\'t catch that')
main()