#!/bin/bash

if [ "$1" == "gemini" ]; then
    google-chrome --app='https://gemini.google.com'
else
    google-chrome --app='https://chatgpt.com'
fi
