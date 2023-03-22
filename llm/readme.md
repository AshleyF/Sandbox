pip install openai

brew install portaudio
https://stackoverflow.com/questions/68251169/unable-to-install-pyaudio-on-m1-mac-portaudio-already-installed

pip install pyttsx3

-- BEFORE ------------------------------------------------------------

        Curently seated at each screen are:
        Remote1: Rick Gutierrez, Andy Wilson, Richard Hughes
        Remote2: Sean Rintel, Ashley Feniello
        Remote3: John Tang
        Remote4: 
        Fake: Sasa Junuzovic, Quanquin Qi, Kori Inkpen
    
-- PROMPT ----------------------------------------------------------
Put Rick on the presenter screen

-- RESPONSE ----------------------------------------------------------

Sure, to put Rick Gutierrez on the presenter screen, I would call the `unseat()` function to remove him from wherever he is seated currently. If he is not seated, then nothing will happen. Then, I would call the `seat()` function to seat him at the presenter screen with the identifier "Remote4" using the following code:

```
unseat('Rick Gutierrez')
seat('Rick Gutierrez', 'Remote4')
```

After running this code, Rick Gutierrez would be moved from his current seat to the presenter screen.

-- EXEC --------------------------------------------------------------
unseat('Rick Gutierrez') # unseat Rick Gutierrez from Remote1
seat('Rick Gutierrez', 'Remote4') # seat Rick Gutierrez at Remote4
-- STATE -------------------------------------------------------------

        Curently seated at each screen are:
        Remote1: Andy Wilson, Richard Hughes
        Remote2: Sean Rintel, Ashley Feniello
        Remote3: John Tang
        Remote4: Rick Gutierrez
        Fake: Sasa Junuzovic, Quanquin Qi, Kori Inkpen
    
-- PROMPT ----------------------------------------------------------
Rick is done presenting. Please reseat him.
-- RESPONSE ----------------------------------------------------------

Sure, to reseat Rick Gutierrez, I will first check which screen has the least number of people and is not the presenter screen (Remote4). From the current seating arrangement, it looks like Remote1 has two people seated and Remote3 has one person seated. So, I will call the function seat('Rick Gutierrez', 'Remote3') to move him from the presenter screen to Remote3 screen.

After making the seating changes, the new seating arrangement will be:
        Remote1: Andy Wilson, Richard Hughes
        Remote2: Sean Rintel, Ashley Feniello
        Remote3: John Tang, Rick Gutierrez
        Remote4: (empty)
        Fake: Sasa Junuzovic, Quanquin Qi, Kori Inkpen

Note that Sasa Junuzovic, Quanquin Qi, and Kori Inkpen are still in the fake seat as they are physically present in the room. Also, no one was removed from any screen as everyone from the roster is still in a seat.

-- EXEC --------------------------------------------------------------
seat('Rick Gutierrez', 'Remote3') # move Rick Gutierrez from Remote4 to Remote3
-- STATE -------------------------------------------------------------

        Curently seated at each screen are:
        Remote1: Andy Wilson, Richard Hughes
        Remote2: Sean Rintel, Ashley Feniello
        Remote3: John Tang, Rick Gutierrez
        Remote4: 
        Fake: Sasa Junuzovic, Quanquin Qi, Kori Inkpen